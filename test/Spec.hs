{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent      (threadDelay)
import           Data.Aeson              (Value)
import           Data.Either             (isLeft, isRight)
import           Data.Map                (Map)
import           Data.Text               (Text)
import           Network.HTTP.Client.TLS (newTlsManager)
import           Network.Poloniex
import           Servant.Client          (ClientEnv (..), ClientM, ServantError,
                                          runClientM)
import           Test.Hspec


main :: IO ()
main = do
  mgr <- newTlsManager
  let

    cenv = (ClientEnv mgr pxBaseUrl)

    runC :: ClientM a -> IO (Either ServantError a)
    runC session = do
      -- Poloniex limits requests to 6/sec
      threadDelay $ 5 * 10^5
      runClientM session cenv

  hspec $ do
    describe "Public endpoints" $ do
      it "Ticker" $ do
        res <- runC returnTicker
        res `shouldSatisfy` isRight

      it "24 h volume" $ do
        res <- runC return24hVolume
        res `shouldSatisfy` isRight

      it "Order book" $ do
        res <- runC $ returnOrderBook "XMR_BTC" Nothing
        res `shouldSatisfy` isRight

      it "Trade history" $ do
        res <- runC $ returnTradeHistory "BTC_LTC" Nothing
        res `shouldSatisfy` isRight

      it "Chart data" $ do
        res <- runC $ returnChartData "BTC_XMR" P14400 1405699200 9999999999
        res `shouldSatisfy` isRight

      it "Currencies" $ do
        res <- runC $ returnCurrencies
        res `shouldSatisfy` isRight

      it "Loan orders" $ do
        res <- runC $ returnLoanOrders "BTC_LTC"
        res `shouldSatisfy` isRight
