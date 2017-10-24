-- |
-- Module: Network.Poloniex.Types
--
-- Data declarations for Poloniex response types
module Network.Poloniex.Types where

import           Data.Aeson  (ToJSON (..))
import           Servant.API (ToHttpApiData (..))

data Period =
  P300
  | P900
  | P1800
  | P7200
  | P14400
  | P86400
  deriving (Eq, Show)

toInt :: Period -> Int
toInt P300   = 300
toInt P900   = 900
toInt P1800  = 1800
toInt P7200  = 7200
toInt P14400 = 14400
toInt P86400 = 86400

instance ToJSON Period where
  toJSON = toJSON . toInt

instance ToHttpApiData Period where
  toQueryParam = toQueryParam . toInt
