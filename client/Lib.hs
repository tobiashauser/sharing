module Lib where

import Miso
import Language.Javascript.JSaddle 
import Control.Monad

-------------------------------------------------------------------------------
--- Actions         
-------------------------------------------------------------------------------

-- | Return an @action@ to the update function after a delay @ms@.
--
-- > updateModel _ = delay 1000 SayHello
delay :: Int -> action -> Effect parent model action
delay ms action = withSink $ \sink -> do
  win <- jsg "window"
  cb  <- asyncCallback $ sink action
  void $ win # "setTimeout" $ (cb, ms)

-- | Optionally return a result if a condition is met.
--
-- > updateModel _ = for $ pure $ inCase (1 < 2) SayHello
inCase :: Bool -> result -> Maybe result
inCase predicate result
  | predicate = Just result
  | otherwise = Nothing

-------------------------------------------------------------------------------
--- Files and Folders
-------------------------------------------------------------------------------

data Item = File
          | Folder { items :: [ Item ] }

-- data DataTransfer =

-- dataTransferDecoder :: Decoder DataTransfer
