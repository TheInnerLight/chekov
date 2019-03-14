module Chekov.Scotty
  ( runRoutes
  ) where

import Control.Monad
import Chekov
import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method
import Web.Scotty

runPartialRoute :: Path a (ActionM ()) -> StdMethod -> a -> ActionM (Maybe ())
runPartialRoute p method act = do
  req <- request
  let reqPath = pathInfo req
  let reqMethod = requestMethod req
  case runPath p reqPath act of
    Just(res, _) -> 
      if renderStdMethod method == reqMethod then do
        _ <- res
        _ <- status status200
        return $ Just ()
      else do
        _ <- status methodNotAllowed405
        return Nothing
    Nothing      -> return Nothing

runRoutes :: [Route (ActionM ())] -> ScottyM ()
runRoutes rs = 
  notFound $ fmap (const ()) $ foldM folder Nothing rs
  where 
    folder :: Maybe () -> Route (ActionM ()) -> ActionM (Maybe ())
    folder Nothing (Route p m act) = runPartialRoute p m act
    folder (Just ()) _ = return $ Just ()
