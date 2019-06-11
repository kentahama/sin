import Test.Hspec
import Test.QuickCheck
import Sine

import Debug.Trace

infix 4 ~=
(~=) :: (RealFloat a) => a -> a -> Bool
x ~= y = x - y < e where
  e = 1.0e-13

main :: IO ()
main = hspec $ do
  describe "Sine.sine" $ do
    it "should be equal to built-in sin function" $ do
      property $ \x -> sine x ~= sin x
    it "is an odd function" $ do
      property $ \x -> sine (-x) ~= -sine x
    it "has 2pi period" $ do
      property $ \x -> sine x ~= sine (x + 2 * pi)
    it "satisfies sum identity" $ do
      property $ \a b -> sin (a + b) ~= sin a * cos b + cos a * sin b where
          sin x = sine x
          cos x = sine (pi / 2 - x)
