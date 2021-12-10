module Main where

import Lib ( plainHandler, serveLog )

main :: IO a
main = serveLog "80" plainHandler
