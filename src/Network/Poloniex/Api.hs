{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module: Network.Poloniex.Api
--
-- This module contains the Poloniex API specification
module Network.Poloniex.Api (
  -- * Public API
    PublicAPI
  , publicAPI

  -- * Trading API
  , TradingAPI

  -- * Auxiliary types
  , Period (..)
  ) where

import           Data.Aeson         (ToJSON (..), Value)
import           Data.Map           (Map)
import           Data.Proxy
import           Data.Text          (Text)
import qualified Data.Text          as T
import           GHC.TypeLits       (KnownSymbol, Symbol, symbolVal)
import           Servant.API
import           Servant.Client     (HasClient (..))
import           Servant.Common.Req (appendToQueryString)

--
-- Public API
--

-- | The combined Public API
type PublicAPI =
  ReturnTicker
  :<|> Return24hVolume
  :<|> ReturnOrderBook
  :<|> ReturnTradeHistory
  :<|> ReturnChartData
  :<|> ReturnCurrencies
  :<|> ReturnLoanOrders

-- | Proxy value for the Public API
publicAPI :: Proxy PublicAPI
publicAPI = Proxy

-- | Ticker data endpoint
type ReturnTicker =
  "public" :>
  HardParam "command" "returnTicker" :>
  Get '[JSON] (Map Text Value)

-- | 24h volume endpoint
type Return24hVolume =
  "public" :>
  HardParam "command" "return24hVolume" :>
  Get '[JSON] (Map Text Value)

-- | Order book endpoint
type ReturnOrderBook =
  "public" :>
  HardParam "command" "returnOrderBook" :>
  QueryParam "currencyPair" Text :>
  QueryParam "depth" Int :>
  Get '[JSON] Value

-- | Trade history endpoint
type ReturnTradeHistory =
  "public" :>
  HardParam "command" "returnTradeHistory" :>
  QueryParam "currencyPair" Text :>
  QueryParam "start" Int :>
  QueryParam "end" Int :>
  Get '[JSON] [Value]

-- | Chart data endpoint
type ReturnChartData =
  "public" :>
  HardParam "command" "returnChartData" :>
  QueryParam "currencyPair" Text :>
  QueryParam "period" Period :>
  QueryParam "start" Int :>
  QueryParam "end" Int :>
  Get '[JSON] [Value]

-- | Currencies endpoint
type ReturnCurrencies =
  "public" :>
  HardParam "command" "returnCurrencies" :>
  Get '[JSON] Value

-- | Loan orders endpoint
type ReturnLoanOrders =
  "public" :>
  HardParam "command" "returnLoanOrders" :>
  QueryParam "currency" Text :>
  Get '[JSON] Value

--
-- Trading API
--

type TradingAPI = ()

--
-- HardParam
--

data HardParam (k :: Symbol) (v :: Symbol)

instance (KnownSymbol k, KnownSymbol v, HasClient api) => HasClient (HardParam k v :> api) where

  type Client (HardParam k v :> api) = Client api

  clientWithRoute Proxy req = clientWithRoute (Proxy :: Proxy api) $
    appendToQueryString name (Just val) req where
      name = T.pack $ symbolVal (Proxy :: Proxy k)
      val  = T.pack $ symbolVal (Proxy :: Proxy v)

--
-- Helper types
--

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
