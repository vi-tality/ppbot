module Main where

import Calamity
import Calamity.Cache.InMemory (runCacheInMemory)
import Calamity.Metrics.Noop (runMetricsNoop)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Di
import DiPolysemy
import qualified Polysemy as P
import System.Environment (getEnv)
import System.Random (randomRIO)

main :: IO ()
main = do
  token <- T.pack <$> getEnv "PPBOT_TOKEN"
  Di.new $ \di ->
    void
    . P.runFinal
    . P.embedToFinal
    . runDiToIO di
    . runCacheInMemory
    . runMetricsNoop
    . runBotIO (BotToken token) defaultIntents
    $ do
      react @'MessageCreateEvt $ \m -> do
        when (T.toLower (m ^. #content) == T.pack "show pp") do
          len <- liftIO $ randomRIO (0, 15)
          aName <- maybe "unknown" (^. #username) <$> upgrade (m ^. #author)
          let canvas = aName <> "'s pp:\n8" <> T.replicate len "=" <> "D"
          void $ tell @Text m canvas
