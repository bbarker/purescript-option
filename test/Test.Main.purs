module Test.Main (main) where

import Prelude
import Control.Monad.Except.Trans (runExceptT)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect as Effect
import Effect.Console (logShow)
import Foreign (readString, unsafeToForeign)
import Option as Opt
import Test.Assert (assert)
import Test.Unit.Console (print)

someOption :: Opt.Option ( foo :: Boolean, bar :: Int )
someOption = Opt.empty
bar1 :: Int
bar1 = 31
anotherOption :: Opt.Option ( foo :: Boolean, bar :: Int )
anotherOption = Opt.set (SProxy :: _ "bar") bar1 someOption

main :: Effect.Effect Unit
main = do
  logShow anotherOption
  logShow $ show $ runExceptT $ readString $ unsafeToForeign anotherOption
  assert $ Just bar1 == (Opt.get (SProxy :: _ "bar") anotherOption)
