{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}

-- from 'doctest'
import Test.DocTest

main :: IO ()
main = doctest [ "-isrc", "src"]
