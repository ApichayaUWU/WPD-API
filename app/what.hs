{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Web.Scotty as S ( get, post, scotty, text, liftIO, param, rescue, file, put, middleware, files, json, setHeader)
import Network.HTTP.Client.MultipartFormData (formDataBody, partFileSource, partLBS)
import Network.HTTP.Client as C (RequestBody, Response(..), httpLbs, parseRequest, newManager, Request (requestBody))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple
    ( parseRequest,
      getResponseBody,
      httpLBS,
      setRequestBodyLBS,
      setRequestHeader,
      setRequestMethod )
import Data.Aeson (encode, decode', (.:), Value(..), Object)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics (Generic)
import Data.Text.Lazy (Text, pack, unpack)
import Data.Aeson.Types
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as BL
import System.Environment (getEnv)
import Configuration.Dotenv (loadFile, defaultConfig)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing, listDirectory)
import Control.Monad (ap)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Exception (SomeException)
import qualified Data.Text as StrictText
import Network.Wai.Parse (FileInfo(..))
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.Posix.Internals (puts)
import System.FilePath ((</>))
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

data Part = Part
  { text :: String
  } deriving (Show, Generic)

data Content = Content
  { parts :: [Part]
  } deriving (Show, Generic)

data Body = Body
  { contents :: [Content]
  } deriving (Show, Generic)

instance ToJSON Part
instance FromJSON Part

instance ToJSON Content
instance FromJSON Content

instance ToJSON Body
instance FromJSON Body

makeGeminiBody :: Text -> Text -> Value
makeGeminiBody prompt fileUri= 
  object ["contents" .= [object ["parts" .= [object ["text" .= prompt], object ["file_data" .= object ["mime_type" .= ("image/jpeg" :: Text), "file_uri" .= fileUri]]]]]]

makeOpenaiBody :: String -> String -> Value
makeOpenaiBody prompt fileUri = 
  object [ "model" .= ("gpt-4o-mini" :: String)
    , "messages" .= [object ["role" .= ("user" :: String), "content" .= [object
                        [ "type" .= ("text" :: Text),
                          "text" .= prompt
                        ],
                       object
                        [ "type" .= ("image_url" :: String),
                          "image_url"
                            .= object ["url" .= fileUri]
                        ]
                    ]]]
    , "temperature" .= (0.7 :: Double)
    , "response_format" .= ("json" :: String)
    ]
    

geminiApiKey :: IO String
geminiApiKey = getEnv "GEMINI_API_KEY"


openaiApiKey :: IO String
openaiApiKey = getEnv "OPENAI_API_KEY"

-- Create the uploads directory if it doesn't exist
createUploadsDirectory :: IO ()
createUploadsDirectory = do
  exists <- doesDirectoryExist "./uploads"
  if not exists
    then createDirectoryIfMissing True "./uploads"
    else return ()

-- non-pure function
createImageName :: Text -> IO Text
createImageName user = do
  currentTime <- getCurrentTime
  let dateStr = formatTime defaultTimeLocale "%Y%m%d%H%M%S" currentTime
  return $ pack (unpack user ++ dateStr ++ ".jpg")

-- Convert Image to Base64
encodeImageBase64 :: FilePath -> IO T.Text
encodeImageBase64 path = do
    imgData <- BL.readFile path
    return $ TE.decodeUtf8 $ B64.encode $ BL.toStrict imgData

-- Upload Image & Get Response from OpenAI
uploadOpenaiImage :: FilePath -> IO T.Text
uploadOpenaiImage imagePath = do
    apiKey <- openaiApiKey
    let imageUri = "https://effective-parakeet-v9wvq5jxxj4hw65-3000.app.github.dev/images/"


    let requestBody = encode $ makeOpenaiBody "What is this picture" imageUri

    -- Send HTTP Request
    manager <- newManager tlsManagerSettings
    request <- parseRequest "https://api.openai.com/v1/chat/completions"

    let request' = setRequestMethod "POST"
                $ setRequestHeader "Content-Type" ["application/json"]
                $ setRequestHeader "Authorization" [TE.encodeUtf8 $ T.pack ("Bearer " ++ apiKey)]
                $ setRequestBodyLBS requestBody request

    response <- httpLbs request' manager
    let body = responseBody response

    case decode' body of
        Just (responseJson :: Value) -> return $ TE.decodeUtf8 $ BL.toStrict body
        Nothing -> error "Failed to decode JSON response"

-- Function to upload the image and get the file URI
uploadGeminiImage :: FilePath -> IO Text
uploadGeminiImage imagePath = do
   -- Create a Manager
  manager <- newManager tlsManagerSettings

  -- Create a request to upload the image
  apiKey <- liftIO geminiApiKey
  request <- parseRequest $ "https://generativelanguage.googleapis.com/upload/v1beta/files?uploadType=media&key=" ++ apiKey
  fileContent <- BL.readFile imagePath
  let request' = setRequestMethod "POST"
               $ setRequestHeader "Content-Type" ["image/jpeg"]
               $ setRequestBodyLBS fileContent request

  -- Send the request and decode the JSON response
  response <- C.httpLbs request' manager
  let body = responseBody response
  case decode' body of
    Just obj -> case parseMaybe (withObject "root" (\o -> o .: "file" >>= (.: "uri"))) obj of
      Just uri -> return $ pack uri
      Nothing -> error "Failed to extract file URI from response"
    Nothing -> error "Failed to decode JSON response"

-- Function to send the image and prompt to the model
generateContent :: Text -> Text -> IO Text
generateContent prompt fileUri = do
   -- Create a Manager
  manager <- newManager tlsManagerSettings

  -- Create a request to generate content
  let requestBody = encode $ makeGeminiBody prompt fileUri

  apiKey <- liftIO geminiApiKey

  request <- parseRequest $ "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent?key=" ++ apiKey
  let request' = setRequestMethod "POST"
               $ setRequestHeader "Content-Type" ["application/json"]
               $ setRequestBodyLBS requestBody request

  -- Send the request and decode the JSON response
  response <- C.httpLbs request' manager

  -- liftIO $ putStrLn "Response Body:"
  -- liftIO $ L8.putStrLn $ responseBody response
  let body = responseBody response
  return $ decodeUtf8 body

main :: IO ()
main = do
  -- Load environment variables from .env file
  _ <- loadFile defaultConfig
  -- Ensure the uploads directory exists
  createUploadsDirectory
  
  S.scotty 3000 $ do
    -- Middleware for logging requests
    middleware logStdoutDev

    -- Serve static files from the ./uploads directory
    S.middleware $ staticPolicy (addBase "./uploads")

    -- Endpoint to list available images
    S.get "/images" $ do
        files <- liftIO $ listDirectory "uploads"
        S.json files

    -- Endpoint to serve a specific image
    S.get "/images/:filename" $ do
        filename <- S.param "filename"
        S.setHeader "Content-Type" "image/jpeg"
        S.file $ "uploads" </> filename

    -- Endpoint to handle file uploads
    S.post "/gemini" $ do
      -- Get the uploaded file from the request
      file <- files
      let (fileName, fileInfo) = head file  -- Assuming only one file is uploaded
      let content = fileContent fileInfo  -- Extract the file content
      imageName <- liftIO $ createImageName (pack $ StrictText.unpack fileName)
      let filePath = "./uploads/" ++ unpack imageName

      -- Save the file to the uploads directory
      liftIO $ BL.writeFile filePath content

      -- Log the uploaded file path
      liftIO $ putStrLn $ "Uploaded file: " ++ filePath

      uri <- liftIO $ uploadGeminiImage filePath
      -- S.text uri
      
      
      let uriPath = pack (unpack uri ) -- why this code?
      liftIO $ putStrLn $ "Image URI: " ++ unpack uriPath
      -- Send the image and prompt to the model
      generatedText <- liftIO $ generateContent "อาหารในรูปคืออะไร" uriPath

      -- Print the generated text
      S.text generatedText
      liftIO $ putStrLn $ "Generated Text: " ++ unpack generatedText

    S.post "/openai" $ do
      -- Get the uploaded file from the request
      file <- files
      let (fileName, fileInfo) = head file  -- Assuming only one file is uploaded
      let content = fileContent fileInfo  -- Extract the file content
      imageName <- liftIO $ createImageName (pack $ StrictText.unpack fileName)
      let filePath = "./uploads/" ++ unpack imageName

      -- Save the file to the uploads directory
      liftIO $ BL.writeFile filePath content

      -- Log the uploaded file path
      liftIO $ putStrLn $ "Uploaded file: " ++ filePath

      -- Upload the image to OpenAI
      response <- liftIO $ uploadOpenaiImage filePath

      -- Print the response
      S.text $ pack $ T.unpack response
      liftIO $ putStrLn $ "Response: " ++ T.unpack response
