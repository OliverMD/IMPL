module Test_2_4 where
import Test.QuickCheck
import Solution_2_4

test_zerofy :: IO ()
test_zerofy = quickCheck tz
              where tz xs = zerofy xs == map(const 0) xs

test_copies :: IO ()
test_copies = quickCheck tz
              where tz xs = (length (copies 'a' xs)) == xs
