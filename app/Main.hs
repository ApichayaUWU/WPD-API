{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as CSV
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import GHC.Generics
import Network.HTTP.Simple
import System.FilePath
import Control.Monad.IO.Class
import Control.Monad (forM, void)
import System.Environment (getEnv, setEnv)
import Configuration.Dotenv (loadFile, defaultConfig)
import System.Directory.Internal.Prelude (getEnv)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import System.Environment.MrEnv (envAsBool, envAsInt, envAsInteger, envAsString)
import MyEnvi (setEnvironment)
import Web.Scotty as S (get, put, scotty, text, liftIO, param, rescue, file, middleware, files, json, setHeader)
import qualified Data.ByteString as BS
import Network.HTTP.Client (Response(responseBody))
import Network.HTTP.Simple (getResponseBody)
import qualified Data.Vector as V

-- Data types
data Item = Item
    { name :: T.Text
    , imagePath :: T.Text
    , sugar :: Maybe Double
    , carb :: Maybe Double
    , openai_sugar :: Maybe Double
    , gemini_sugar :: Maybe Double
    , openai_carb :: Maybe Double
    , gemini_carb :: Maybe Double
    } deriving (Show, Generic)

instance FromJSON Item
instance ToJSON Item

data OpenAIResponse = OpenAIResponse
    { openaiSugar :: Maybe Double
    , openaiCarb :: Maybe Double
    , explain :: T.Text
    } deriving (Show, Generic)

instance FromJSON OpenAIResponse
instance ToJSON OpenAIResponse

-- Configuration
data Config = Config
    { openaiApiKey :: T.Text
    , geminiApiKey :: T.Text
    } deriving (Show)

-- Helper functions
encodeImage :: FilePath -> IO T.Text
encodeImage path = do
    contents <- BL.readFile path
    return $ T.pack $ show $ B64.encode $ BL.toStrict contents

foodsPrompt :: T.Text -> T.Text
foodsPrompt name = "อาหารรูปนี้คือ " <> name <> "ให้ประเมินปริมาตรมาในหน่วย ml จากนั้น ให้ประเมินค่าน้ำตาลจากปริมตรที่ประเมินไว้ก่อนหน้า หากประเมินมาเป็นช่วงให้เลือกตอบ 1 ค่าเท่านั้น และอธิบายว่าทำไมถึงประเมินค่าได้แบบนั้น ตอบในรูปแบบตามที่กำหนดข้างล่าง ชื่อเมนู: ปริมาตร: น้ำตาล: อธิบายวิธีคิด:"

makeOpenaiBody :: T.Text -> T.Text -> Value
makeOpenaiBody prompt encodeImg = 
  object [ "model" .= ("gpt-4o-mini" :: String)
    , "messages" .= [object ["role" .= ("user" :: String), "content" .= [object
                        [ "type" .= ("text" :: T.Text),
                          "text" .= prompt
                        ],
                       object
                        [ "type" .= ("image_url" :: String),
                          "image_url"
                            .= object ["url" .= T.pack ("data:image/jpeg;base64," ++ T.unpack  encodeImg)]
                        ]
                    ]]]
    , "temperature" .= (0.7 :: Double)
    ]

openaiRequest :: Value -> T.Text -> IO (Maybe T.Text)
openaiRequest body apiKey = do
    let request = setRequestMethod "POST"
                . setRequestSecure True   
                . setRequestPort 443      
                . setRequestHost "api.openai.com"
                . setRequestPath "/v1/chat/completions"
                . setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 apiKey]
                . setRequestHeader "Content-Type" ["application/json"]
                . setRequestBodyJSON body
                $ defaultRequest
    response <- httpJSON request
    let responseBody = getResponseBody response :: Value
    case responseBody of
        Object obj -> case KM.lookup "choices" obj of
            Just (Array choices) -> case V.toList choices of
                (Object choice : _) -> case KM.lookup "message" choice of
                    Just (Object message) -> case KM.lookup "content" message of
                        Just (String content) -> return (Just content)
                        _ -> error "Failed to parse content"
                    _ -> error "Failed to parse message"
                _ -> error "Failed to parse choices"
            _ -> error "Failed to parse choices"
        _ -> error "Failed to parse response"

makeImagePath :: T.Text -> T.Text
makeImagePath name = "test_images/" <> name <> ".jpeg"

main :: IO ()
main = do
    setEnvironment
    
    apiKey <- T.pack <$> envAsString "OPENAI_API_KEY" "API key not found"
    
    S.scotty 3000 $ do
        S.get "/openai" $ do
            let imagePath = "test_images/add1.jpeg"
            encodedImage <- liftIO $ encodeImage imagePath
            let prompt = foodsPrompt "ไอศกรีมสเวนเซ่น ใส่กล้วย"
            let body = makeOpenaiBody prompt encodedImage
            response <- liftIO $ openaiRequest body apiKey
            S.text $ TL.pack $ maybe "No response" T.unpack response
            liftIO $ putStrLn $ "Prompt: " ++ T.unpack prompt
            liftIO $ putStrLn $ "Response: " ++ maybe "No response" T.unpack response