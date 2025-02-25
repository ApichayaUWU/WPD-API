{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Request ( encodeImage, foodsPrompt, makeGeminiBody, makeOpenaiBody, openaiRequest, geminiRequest) where


import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL (dropWhile, drop, pack, take, ByteString, readFile, toStrict, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Csv as CSV
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
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
import System.IO (hSetEncoding, stdout, utf8)
import Data.Word (Word8)



-- Helper functions
encodeImage :: FilePath -> IO T.Text
encodeImage path = do
    contents <- BL.readFile path
    return $ decodeUtf8 $ B64.encode $ BL.toStrict contents

foodsPrompt :: T.Text -> T.Text
foodsPrompt name = "อาหารรูปนี้คือ " <> name <> "ให้ประเมินปริมาตรมาในหน่วย ml จากนั้น ให้ประเมินค่าน้ำตาลและคาร์โบไฮเดรตจากปริมตรที่ประเมินไว้ก่อนหน้า หากประเมินมาเป็นช่วงให้เลือกตอบ 1 ค่าเท่านั้น และอธิบายว่าทำไมถึงประเมินค่าได้แบบนั้น ตอบในรูปแบบตามที่กำหนดข้างล่าง ชื่อเมนู: volume: sugar: carb: อธิบายวิธีคิด:"

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

makeGeminiBody :: T.Text -> T.Text -> Value
makeGeminiBody prompt encodedImage = 
  object ["contents" .= [
    object [
      "parts" .= [
        object ["text" .= prompt],
        object [
          "inline_data" .= object [
            "mime_type" .= ("image/jpeg" :: T.Text),
            "data" .= encodedImage
          ]
        ]
      ]
    ]
  ]]

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

geminiRequest :: Value -> T.Text -> IO (Maybe T.Text)
geminiRequest body apiKey = do
    let request = setRequestMethod "POST"
                . setRequestSecure True
                . setRequestPort 443
                . setRequestHost "generativelanguage.googleapis.com"
                . setRequestPath (encodeUtf8 $ T.pack ("/v1beta/models/gemini-1.5-flash:generateContent?key=" <> T.unpack apiKey))
                . setRequestHeader "Content-Type" ["application/json"]
                . setRequestBodyJSON body
                $ defaultRequest
    response <- httpJSON request
    let responseBody = getResponseBody response :: Value
    
    -- Debug print
    liftIO $ putStrLn $ "Response: " ++ show responseBody
    
    -- Parse the response similar to openaiRequest
    case responseBody of
        Object obj -> case KM.lookup "candidates" obj of
            Just (Array candidates) -> case V.toList candidates of
                (Object candidate : _) -> case KM.lookup "content" candidate of
                    Just (Object content) -> case KM.lookup "parts" content of
                        Just (Array parts) -> case V.toList parts of
                            (Object part : _) -> case KM.lookup "text" part of
                                Just (String text) -> return (Just text)
                                _ -> return Nothing
                            _ -> return Nothing
                        _ -> return Nothing
                    _ -> return Nothing
                _ -> return Nothing
            _ -> return Nothing
        _ -> return Nothing