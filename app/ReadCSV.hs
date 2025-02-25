{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ReadCSV (Item(..), printHeaders, printItems, readCsvFile, removeBOM, processItemWithOpenAI) where

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
import Configuration.Dotenv (loadFile, defaultConfig)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString as BS
import Network.HTTP.Client (Response(responseBody))
import Network.HTTP.Simple (getResponseBody)
import qualified Data.Vector as V
import System.IO (hSetEncoding, stdout, utf8)
import Data.Word (Word8)
import Request (encodeImage, foodsPrompt, makeOpenaiBody, makeGeminiBody, openaiRequest, geminiRequest)

-- Data types
data Item = Item
    { name :: T.Text
    , imagePath :: Maybe T.Text
    , sugar :: Maybe Double
    , carb :: Maybe Double
    , openai_sugar :: Maybe Double
    , gemini_sugar :: Maybe Double
    , openai_carb :: Maybe Double
    , gemini_carb :: Maybe Double
    } deriving (Show, Generic)

instance ToJSON Item
instance FromJSON Item

instance CSV.FromRecord Item
instance CSV.ToRecord Item

-- Instance for CSV parsing
instance CSV.FromNamedRecord Item where
    parseNamedRecord r = Item
        <$> r CSV..: "name"
        <*> r CSV..: "imagePath"
        <*> r CSV..: "sugar"
        <*> r CSV..: "carb"
        <*> r CSV..: "openai_sugar"
        <*> r CSV..: "gemini_sugar"
        <*> r CSV..: "openai_carb"
        <*> r CSV..: "gemini_carb"
       
-- Debug function to print CSV headers
printHeaders :: BL.ByteString -> IO ()
printHeaders csvData = do
    putStrLn "Detected CSV headers:"
    case CSV.decodeByName csvData :: Either String (CSV.Header, V.Vector Item) of
        Left err -> putStrLn $ "Error reading headers: " ++ err
        Right (headers, _) -> print headers

-- Read CSV file and return Vector of Items
readCsvFile :: FilePath -> IO (Either String (V.Vector Item))
readCsvFile filepath = do
    -- Read the file as UTF-8
    contents <- removeBOM <$> BL.readFile filepath
    liftIO $ putStrLn $ "First few bytes: " ++ show (BL.unpack $ BL.take 10 contents)
    -- Ensure stdout can handle UTF-8 for debugging
    hSetEncoding stdout utf8
    -- Try to decode the CSV
    case CSV.decodeByName contents of
        Left err -> do
            -- More detailed error reporting
            liftIO $ putStrLn $ "Headers detected: " ++ show (BL8.lines contents !! 0)
            return $ Left err
        Right (_, items) -> return $ Right items

-- Helper function to print all items
printItems :: V.Vector Item -> IO ()
printItems items = 
    V.forM_ items $ \Item{..} -> do
        TIO.putStrLn $ "Name: " <> name
        TIO.putStrLn $ "Image Path: " <> maybe "None" id imagePath
        putStrLn $ "Sugar: " ++ maybe "None" show sugar
        putStrLn $ "Carb: " ++ maybe "None" show carb
        putStrLn $ "OpenAI Sugar: " ++ maybe "None" show openai_sugar
        putStrLn $ "Gemini Sugar: " ++ maybe "None" show gemini_sugar
        putStrLn $ "OpenAI Carb: " ++ maybe "None" show openai_carb
        putStrLn $ "Gemini Carb: " ++ maybe "None" show gemini_carb
        putStrLn "------------------------"

-- Helper function to remove BOM
removeBOM :: BL.ByteString -> BL.ByteString
removeBOM bs = if BL.take 3 bs == BL.pack [0xEF, 0xBB, 0xBF]
               then BL.drop 3 bs
               else bs

processItemWithOpenAI :: Item -> T.Text -> IO (Item, Maybe T.Text)
processItemWithOpenAI item@Item{..} apiKey = do
    case imagePath of
        Just path -> do
            let fullPath = "test_images/" <> T.unpack path  -- Construct full path
            putStrLn $ "Processing image: " ++ fullPath
            encodedImage <- encodeImage fullPath
            let prompt = foodsPrompt name
            let body = makeOpenaiBody prompt encodedImage
            response <- openaiRequest body apiKey
            putStrLn $ "Processed " ++ T.unpack name
            return (item, response)
        Nothing -> do
            putStrLn $ "No image path for item: " ++ T.unpack name
            return (item, Nothing)