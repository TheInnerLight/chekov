{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Web.Scotty
import Chekov.Scotty
import Chekov
import Data.Text
import Data.Text.Lazy (fromStrict)
import Data.Monoid (mconcat)
import Network.HTTP.Types.Method

main :: IO ()
main = scotty 3000 $ do
  runRoutes $ [
      makeRoute (constantPath "transport" // var) GET $ \(target :: Text) -> 
        html $ mconcat ["<h1>Scotty, beam up ", fromStrict target,  "!</h1>"]
    , makeRoute (constantPath "don't" // constantPath "transport" // var) GET $ \(target :: Text) -> 
        html $ mconcat ["<h1>Scotty, don't beam up ", fromStrict target,  "!</h1>"]
    ]
 

