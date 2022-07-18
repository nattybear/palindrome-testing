import Data.Char
import Lib
import Test.QuickCheck

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test
                                          then putStrLn passStatement
                                          else putStrLn failStatement

prop_punctuationInvariant text = preprocess text == preprocess noPuncText
  where noPuncText = filter (not . isPunctuation) text

prop_reverseInvaraint text = isPalindrome text == isPalindrome (reverse text)

main :: IO ()
main = do
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
  quickCheck prop_reverseInvaraint
  putStrLn "done!"
