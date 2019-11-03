module Chekov.Scotty
  ( runRoutes
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Chekov
import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method
import Web.Scotty.Trans
import Web.Scotty.Internal.Types

runPartialRoute :: (ScottyError e, Monad m) => Path a (ActionT e m ()) -> StdMethod -> a -> ActionT e m (Maybe ())
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
    Nothing -> 
      return Nothing

runRoutes :: (ScottyError e, MonadIO m) => [Route (ActionT e m ())] -> ScottyT e m ()
runRoutes rs = 
  notFound $ foldM_ folder Nothing rs
  where 
    folder :: (ScottyError e, MonadIO m) => Maybe () -> Route (ActionT e m ()) -> ActionT e m (Maybe ())
    folder Nothing (Route p m act) = runPartialRoute p m act
    folder (Just ()) _ = return $ Just ()
