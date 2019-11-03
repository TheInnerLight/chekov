{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleInstances, GADTs, ExistentialQuantification, RankNTypes #-}

module Chekov where

import Data.Text
import Data.List
import Text.Read
import Web.Scotty
import Network.HTTP.Types.Method

data Path a b where
  ConstantPath :: Text -> Path a a
  ParsePath :: (Text -> Maybe b) -> Path (b -> a) a
  CombinePath :: Path a c -> Path c b -> Path a b

data Route r where
  Route :: Path a r -> StdMethod -> a -> Route r

class PathComponent a where
  parse :: Text -> Maybe a

instance PathComponent Text where
  parse = Just

instance PathComponent Int where
  parse = readMaybe . unpack

constantPath :: Text -> Path a a
constantPath = ConstantPath

var :: PathComponent a => Path (a -> b) b
var = ParsePath parse

(//) :: Path a c -> Path c b -> Path a b
(//) = CombinePath

makeRoute :: Path a r -> StdMethod -> a -> Route r
makeRoute = Route

runPath :: Path a b -> [Text] -> a -> Maybe (b, [Text])
runPath (ParsePath p) [] f =
  Nothing
runPath (ParsePath p) (x:xs) f =
  fmap (\r -> (f r, xs)) $ p x
runPath (CombinePath p1 p2) lst f = do
  (a, lst') <- runPath p1 lst f
  runPath p2 lst' a
runPath (ConstantPath constStr) [] f =
    Nothing
runPath (ConstantPath constStr) (x:xs) f 
  | x == constStr = Just(f, xs)
  | otherwise = Nothing
