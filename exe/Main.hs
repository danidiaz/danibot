module Main (
        main
    ) where

import Network.Danibot (dumbHandler)
import Network.Danibot.Main (mainWith)

main :: IO ()
main = mainWith dumbHandler 


