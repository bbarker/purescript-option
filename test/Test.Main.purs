module Test.Main (main) where

import Prelude
import Control.Monad.Except.Trans (runExceptT)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect as Effect
import Effect.Console (logShow)
import Foreign (readString, unsafeToForeign)
import Option as Opt
import Option (Option(..))
import Test.Assert (assert)
import Test.Unit.Console (print)

main :: Effect.Effect Unit
main = do
  insertTest1
  setTest1


insertTest1 :: Effect.Effect Unit
insertTest1 = do
  logShow anotherOption
  assert $ Just bar1 == (Opt.get (SProxy :: _ "bar") anotherOption)
  where
    someOption :: Opt.Option ( foo :: Boolean )
    someOption = Opt.empty
    bar1 :: Int
    bar1 = 31
    anotherOption :: Opt.Option ( foo :: Boolean, bar :: Int )
    anotherOption = Opt.insert (SProxy :: _ "bar") bar1 someOption

setTest1 :: Effect.Effect Unit
setTest1 = do
  logShow anotherOption
  -- logShow $ show $ runExceptT $ readString $ unsafeToForeign anotherOption
  assert $ Just bar1 == (Opt.get (SProxy :: _ "bar") anotherOption)
  where
    someOption :: Opt.Option ( foo :: Boolean, bar :: Int )
    someOption = Opt.empty
    bar1 :: Int
    bar1 = 31
    anotherOption :: Opt.Option ( foo :: Boolean, bar :: Int )
    anotherOption = Opt.set (SProxy :: _ "bar") bar1 someOption
