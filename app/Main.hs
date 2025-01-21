{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

import Web.Scotty ( get, html, pathParam, scotty, text )

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    text "Haskell says: Hello Earth!"
    html $ mconcat ["Haskell says: Hello Earth!"]
  get "/:word" $ do
    beam <- pathParam "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]