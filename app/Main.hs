{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL (dropWhile, drop, pack, take, ByteString, readFile, writeFile, toStrict, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Csv as CSV
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
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
import ReadCSV (Item(..), printHeaders, printItems, readCsvFile, removeBOM, processItemWithOpenAI)
import Request ( encodeImage, foodsPrompt, makeOpenaiBody, makeGeminiBody, openaiRequest, geminiRequest)
import Metrics (eval)
import Web.Scotty as S (get, put, scotty, text, liftIO, param, rescue, file, middleware, files, json, setHeader)
import qualified Data.ByteString as BS
import Network.HTTP.Client (Response(responseBody))
import Network.HTTP.Simple (getResponseBody)
import qualified Data.Vector as V
import System.IO (hSetEncoding, stdout, utf8)
import Data.Word (Word8)
import Network.Wai.Middleware.Timeout (timeout)


writeResultsToJson :: FilePath -> [(Item, Maybe T.Text)] -> IO ()
writeResultsToJson filePath results = do
    let jsonResults = map (\(item, response) -> object [
            "item" .= item,
            "response" .= response
            ]) results
    BL.writeFile filePath (encodePretty jsonResults)

handleOpenaiEval :: String -> FilePath -> T.Text -> IO (Either String Value)
handleOpenaiEval name csvFile gptApiKey = do
    csvResult <- readCsvFile csvFile
    case csvResult of
        Left err -> return $ Left err
        Right items -> do
            results <- V.forM items $ \item -> do
                processItemWithOpenAI item gptApiKey
            let resultsList = V.toList results
            writeResultsToJson ("results_" ++ name ++ ".json") resultsList
            return $ Right $ toJSON $ map (\(item, response) -> object [
                "item" .= item,
                "response" .= response
                ]) resultsList

main :: IO ()
main = do
    setEnvironment
    
    gptApiKey <- T.pack <$> envAsString "OPENAI_API_KEY" "API key not found"
    geminiApiKey <- T.pack <$> envAsString "GEMINI_API_KEY" "API key not found"

    -- csvData <- readCsvFile "fv_short.csv"
    -- case csvData of 
    --     Left err -> putStrLn $ "Error reading CSV: " ++ err
    --     Right items -> do
    --         putStrLn "Successfully read CSV file:"
    --         printItems items

    -- liftIO $ handleOpenaiEval "drinks" "drinks.csv" gptApiKey
    -- liftIO $ handleOpenaiEval "fv" "fv.csv" gptApiKey
    -- liftIO $ handleOpenaiEval "food_inter" "food_inter.csv" gptApiKey
    -- liftIO $ handleOpenaiEval "food_thai" "food_thai.csv" gptApiKey
    -- liftIO $ handleOpenaiEval "sweets" "sweets.csv" gptApiKey

    Metrics.eval "results_fv.json"
    Metrics.eval "results_drinks.json"
    Metrics.eval "results_food_inter.json"
    Metrics.eval "results_food_thai.json"
    Metrics.eval "results_sweets.json"



    -- case result of
    --     Left err -> putStrLn $ "Error reading CSV: " ++ err
    --     Right jsonResult -> putStrLn $ "Successfully processed " ++ show (length jsonResult) ++ " items"
    


    
   
    S.scotty 3000 $ do
        S.middleware $ timeout 900000000
        S.get "/items" $ do
            S.setHeader "Content-Type" "application/json; charset=utf-8"
            csvData <- liftIO $ readCsvFile "fv_short.csv"
            case csvData of
                Left err -> do
                    liftIO $ putStrLn $ "Error: " ++ err
                    S.text $ TL.pack $ "Error reading CSV: " ++ err
                Right items -> do
                    liftIO $ putStrLn $ "Successfully read " ++ show (V.length items) ++ " items"
                    S.json $ V.toList items

        S.get "/openai_eval" $ do
            result <- liftIO $ handleOpenaiEval "fv" "fv.csv" gptApiKey
            case result of
                Left err -> do
                    liftIO $ putStrLn $ "Error reading CSV: " ++ err
                    S.text $ TL.pack $ "Error reading CSV: " ++ err
                Right jsonResult -> S.json jsonResult

        S.get "/openai" $ do
            let imagePath = "test_images/add1.jpeg"
            encodedImage <- liftIO $ encodeImage imagePath
            let prompt = foodsPrompt "ไอศกรีมสเวนเซ่น ใส่กล้วย"
            let body = makeOpenaiBody prompt encodedImage
            response <- liftIO $ openaiRequest body gptApiKey
            S.text $ TL.pack $ maybe "No response" T.unpack response
            liftIO $ putStrLn $ "Prompt: " ++ T.unpack prompt
            liftIO $ putStrLn $ "Response: " ++ maybe "No response" T.unpack response

        S.get "/gemini" $ do
            let imagePath = "test_images/add1.jpeg"
            encodedImage <- liftIO $ encodeImage imagePath
            let prompt = foodsPrompt "ไอศกรีมสเวนเซ่น ใส่กล้วย"
            let body = makeGeminiBody prompt encodedImage
            response <- liftIO $ geminiRequest body geminiApiKey
            S.text $ TL.pack $ maybe "No response" T.unpack response
            liftIO $ putStrLn $ "Prompt: " ++ T.unpack prompt
            liftIO $ putStrLn $ "Response: " ++ maybe "No response" T.unpack response