{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

import Web.Scotty as S ( get, post, scotty, text, liftIO, put )
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
import System.Directory (doesFileExist)
import Control.Monad (ap)


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

exampleBody :: Body
exampleBody = Body
  [ Content
      [ Part "อาหารในรูปคืออะไร" ]
  ]

makeBody :: Text -> Text -> Value
makeBody fileUri prompt = 
  object ["contents" .= [object ["parts" .= [object ["text" .= prompt], object ["file_data" .= object ["mime_type" .= ("image/jpeg" :: Text), "file_uri" .= fileUri]]]]]]


apiKey :: Text
apiKey = pack "AIzaSyC0xCqsrg7x4HyRuRJPLaIZYvHqHKxsgm0"

-- Function to upload the image and get the file URI
uploadImage :: FilePath -> IO Text
uploadImage imagePath = do
   -- Create a Manager
  manager <- newManager tlsManagerSettings

  -- Create a request to upload the image
  request <- parseRequest $ "https://generativelanguage.googleapis.com/upload/v1beta/files?uploadType=media&key=" ++ unpack apiKey
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
generateContent fileUri prompt = do
   -- Create a Manager
  manager <- newManager tlsManagerSettings

  -- Create a request to generate content
  let requestBody = encode $ makeBody fileUri prompt

  request <- parseRequest $ "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent?key=" ++ unpack apiKey
  let request' = setRequestMethod "POST"
               $ setRequestHeader "Content-Type" ["application/json"]
               $ setRequestBodyLBS requestBody request

  -- Send the request and decode the JSON response
  response <- C.httpLbs request' manager

  liftIO $ putStrLn "Response Body:"
  liftIO $ L8.putStrLn $ responseBody response
  let body = responseBody response
  let decoded = decode' body 
  case decoded of
    Just (Object obj) -> case parseMaybe (.: "generatedText") obj of
      Just text -> return $ pack text  -- Successfully extracted the value
      Nothing -> error "Failed to extract generated text"
    _ -> error "Failed to decode JSON response or response is not an object"


main :: IO ()
main = S.scotty 3000 $ do

  S.get "/" $ do
    uri <- liftIO $ uploadImage "/workspaces/WPD-API/app/example.jpg"
    S.text uri

  S.post "/gemini" $ do
    uri <- liftIO $ uploadImage "/workspaces/WPD-API/app/example.jpg"
    -- S.text uri
    
    
    let uriPath = pack (unpack uri ) -- why this code?
    liftIO $ putStrLn $ "Image URI: " ++ unpack uriPath
    -- Send the image and prompt to the model
    generatedText <- liftIO $ generateContent uriPath "อาหารในรูปคืออะไร?"

    -- Print the generated text
    S.text generatedText
    
    -- let url = "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent?key=AIzaSyC0xCqsrg7x4HyRuRJPLaIZYvHqHKxsgm0" 
    -- initialRequest <- liftIO $ parseRequest url

    -- let requestBody = makeBody "what is 1+1"  -- JSON payload with the text question
    
    -- -- Set request method to POST
    -- let request = setRequestHeader "Content-Type" ["application/json"]
    --               $ setRequestMethod "POST"
    --               $ setRequestBodyLBS (L8.pack requestBody) initialRequest


    -- response <- liftIO $ httpLBS request

    -- -- Print response
    -- liftIO $ putStrLn "Response Body:"
    -- liftIO $ L8.putStrLn $ getResponseBody response
    -- S.text $ decodeUtf8 $ getResponseBody response