{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Metrics (eval) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import GHC.Generics
import Control.Monad (forM, forM_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hSetEncoding, stdout, utf8)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Data.List (find)

data Item = Item
    { carb :: Maybe Double
    , gemini_carb :: Maybe Double
    , gemini_sugar :: Maybe Double
    , imagePath :: Maybe Text
    , name :: Maybe Text
    , openai_carb :: Maybe Double
    , openai_sugar :: Maybe Double
    , sugar :: Maybe Double
    } deriving (Show, Generic)

data Response = Response
    { item :: Item
    , response :: Text
    } deriving (Show, Generic)

instance FromJSON Item
instance FromJSON Response

-- Calculate MAE, MAPE, and RMSE
calculateMetrics :: [Double] -> [Double] -> (Double, Double, Double)
calculateMetrics actual predicted = (mae, mape, rmse)
  where
    n = fromIntegral $ length actual
    diffs = zipWith (-) actual predicted
    absDiffs = map abs diffs
    absPctDiffs = zipWith (\a p -> abs (a - p) / a) actual predicted
    sqDiffs = map (^2) diffs
    mae = sum absDiffs / n
    mape = (sum absPctDiffs / n) * 100
    rmse = sqrt $ sum sqDiffs / n

-- Parse sugar value from response text
parseSugarFromResponse :: Text -> Maybe Double
parseSugarFromResponse responseText =
    let lines = T.lines responseText
        sugarLine = find (T.isPrefixOf "sugar:") lines
    in sugarLine >>= (readMaybe . T.unpack . T.dropWhile (not . (`elem` ['0'..'9'])))

eval :: String -> IO ()
eval file = do
    hSetEncoding stdout utf8
    contents <- BL.readFile file
    let responses = eitherDecode contents :: Either String [Response]
    case responses of
        Left err -> putStrLn $ "Error: " ++ err
        Right resps -> do
            let actualSugar = map (fromMaybe 0 . sugar . item) resps
            let predictedSugar = map (fromMaybe 0 . openai_sugar . item) resps
            let parsedSugar = map (fromMaybe 0 . parseSugarFromResponse . response) resps
            let actualCarb = map (fromMaybe 0 . carb . item) resps
            let predictedCarb = map (fromMaybe 0 . openai_carb . item) resps

            let (maeSugar, mapeSugar, rmseSugar) = calculateMetrics actualSugar parsedSugar
            let (maeCarb, mapeCarb, rmseCarb) = calculateMetrics actualCarb predictedCarb

             -- Print the name of each item
            forM_ resps $ \resp -> do
                let itemName = fromMaybe "Unknown" (name (item resp))
                putStrLn $ T.unpack itemName

            putStrLn $ "Sugar - MAE: " ++ show maeSugar ++ ", MAPE: " ++ show mapeSugar ++ "%, RMSE: " ++ show rmseSugar
            putStrLn $ "Carb - MAE: " ++ show maeCarb ++ ", MAPE: " ++ show mapeCarb ++ "%, RMSE: " ++ show rmseCarb