module Sine (sine) where

sine :: Double -> Double
sine x = sine_r 0 1 False where
  sine_r sum i isneg = if sum == sum'
                       then sum
                       else sine_r sum' (i + 2) (not isneg) where
    sum' = sum + if isneg then -t else t
    t = (x' ** i) / fact i
    x' = x + if x >= pi then -(tweak x) else if x <= -pi then tweak (-x) else 0
    tweak x = 2 * pi * (fromIntegral . truncate $  x / (2 * pi))
    fact n = if n == 0 then 1 else n * fact (n - 1)
