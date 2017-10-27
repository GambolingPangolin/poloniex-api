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
  , tradingAPI
  ) where

import           Data.Aeson             (Value)
import           Data.Map               (Map)
import           Data.Proxy
import           Data.Text              (Text)
import qualified Data.Text              as T
import           GHC.TypeLits           (KnownSymbol, Symbol, symbolVal)
import           Network.Poloniex.Types
import           Servant.API
import           Servant.Client         (HasClient (..))
import           Servant.Common.Req     (appendToQueryString)

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
  Get '[JSON] (Map Text Value)

-- | Loan orders endpoint
type ReturnLoanOrders =
  "public" :>
  HardParam "command" "returnLoanOrders" :>
  QueryParam "currency" Text :>
  Get '[JSON] Value

--
-- Trading API
--

type TradingAPI =
  ReturnBalances
  :<|> ReturnCompleteBalances
  :<|> ReturnDepositAddresses
  :<|> GenerateNewAddress
  :<|> ReturnDepositsWithdrawals
  :<|> ReturnOpenOrders
  :<|> ReturnTradeHistory
  :<|> ReturnOrderTrades
  :<|> Buy
  :<|> Sell
  :<|> CancelOrder
  :<|> MoveOrder
  :<|> Withdraw
  :<|> ReturnFeeInfo
  :<|> ReturnAvailableAccountBalances
  :<|> ReturnTradableBalances
  :<|> TransferBalance
  :<|> ReturnMarginAccountSummary
  :<|> MarginBuy
  :<|> MarginSell
  :<|> GetMarginPosition
  :<|> CloseMarginPosition
  :<|> CreateLoanOffer
  :<|> CancelLoanOffer
  :<|> ReturnOpenLoanOffers
  :<|> ReturnActiveLoans
  :<|> ReturnLendingHistory
  :<|> ToggleAutoRenew

-- | Proxy value for the trading API
tradingAPI :: Proxy TradingAPI
tradingAPI = Proxy

type ReturnBalances =
  "public" :>
  HardParam "command" "returnBalances" :>
  Get '[JSON] Value

type ReturnCompleteBalances =
  "public" :>
  HardParam "command" "returnCompleteBalances" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

type ReturnDepositAddresses =
  "public" :>
  HardParam "command" "returnDepositAddresses" :>
  Get '[JSON] Value

type GenerateNewAddress =
  "public" :>
  HardParam "command" "generateNewAddress" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

type ReturnDepositsWithdrawals =
  "public" :>
  HardParam "command" "returnDepositsWithdrawals" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

type ReturnOpenOrders =
  "public" :>
  HardParam "command" "returnOpenOrders" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

type ReturnTradeHistory =
  "public" :>
  HardParam "command" "returnTradeHistory" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

type ReturnOrderTrades =
  "public" :>
  HardParam "command" "returnOrderTrades" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

type Buy =
  "public" :>
  HardParam "command" "buy" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

type Sell =
  "public" :>
  HardParam "command" "sell" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

type CancelOrder =
  "public" :>
  HardParam "command" "cancelOrder" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

type MoveOrder =
  "public" :>
  HardParam "command" "moveOrder" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

type Withdraw =
  "public" :>
  HardParam "command" "withdraw" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

type ReturnFeeInfo =
  "public" :>
  HardParam "command" "returnFeeInfo" :>
  Get '[JSON] Value

type ReturnAvailableAccountBalances =
  "public" :>
  HardParam "command" "returnAvailableAccountBalances" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

type ReturnTradableBalances =
  "public" :>
  HardParam "command" "returnTradableBalances" :>
  Get '[JSON] Value

type TransferBalance =
  "public" :>
  HardParam "command" "transferBalance" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

type ReturnMarginAccountSummary =
  "public" :> HardParam "command" "returnMarginAccountSummary"

type MarginBuy =
  "public" :>
  HardParam "command" "marginBuy" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

type MarginSell =
  "public" :>
  HardParam "command" "marginSell" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

type GetMarginPosition =
  "public" :>
  HardParam "command" "getMarginPosition" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

type CloseMarginPosition =
  "public" :>
  HardParam "command" "closeMarginPosition" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

type CreateLoanOffer =
  "public" :>
  HardParam "command" "createLoanOffer" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

type CancelLoanOffer =
  "public" :>
  HardParam "command" "cancelLoanOffer" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

type ReturnOpenLoanOffers =
  "public" :> HardParam "command" "returnOpenLoanOffers" :>
  Get '[JSON] Value

type ReturnActiveLoans =
  "public" :>
  HardParam "command" "returnActiveLoans" :>
  Get '[JSON] Value

type ReturnLendingHistory =
  "public" :> HardParam "command" "returnLendingHistory" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

type ToggleAutoRenew =
  "public" :>
  HardParam "command" "toggleAutoRenew" :>
  ReqBody '[JSON] Value :>
  Post '[JSON] Value

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
