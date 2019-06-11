import Sine

main = do
  x <- read <$> getLine
  print $ sine x
