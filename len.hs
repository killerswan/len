-- |
-- Module      : Main
-- Copyright   : (c) 2011 Kevin Cantu
--
-- License     : BSD-style
-- Maintainer  : Kevin Cantu <me@kevincantu.org>
-- Stability   : experimental

{-# LANGUAGE DoAndIfThenElse, OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Lazy.IO as TIO
import Math.Statistics as Stat
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO


data Options = Options { }

defaultOpts :: Options
defaultOpts = Options { }


-- command line description
-- this format is kinda bone headed:
--   [Option short [long] (property setter-function hint) description]
options :: [ OptDescr (Options -> IO Options) ]
options = [ Option "h" ["help"] 
                   (NoArg  $ \_ -> do
                        prg <- getProgName
                        let header = "Usage: " ++ prg ++ " [file1 file2 ...]"
                        hPutStrLn stderr $ usageInfo header options
                        exitWith ExitSuccess)
                   "display this help"
          ]


-- apply a function (which uses the path) to a list of files and/or stdin
fileMapWithPath :: ( FilePath -> B.ByteString -> IO () ) -> [FilePath] -> IO ()
fileMapWithPath f paths = 
    let    
        -- apply f to stdin
        stdinF :: IO ()
        stdinF = B.getContents >>= f "-"

        -- apply f to a file (or stdin, when "-")
        fileF :: FilePath -> IO ()
        fileF "-"  = stdinF
        fileF path = (B.readFile path >>= f path) `catch` (hPutStrLn stderr . show )
    in
        case paths of
            [] -> stdinF
            _  -> mapM_ fileF paths 


-- count and then display word frequencies
countAndDisplay :: FilePath -> B.ByteString -> IO ()
countAndDisplay path content =
   let
      -- list of line lengths
      lineCounts = map fromIntegral . filter (> 0) . map T.length $ (T.lines . TE.decodeUtf8 $ content)

      m :: Double
      m = Stat.mean lineCounts

      s :: Double
      s = Stat.stddev lineCounts

      mx :: Double -- ?
      mx = foldl max 0.0 lineCounts
   in
      do
         TIO.putStrLn . T.concat . (map T.pack) $ [     "max: ",    show mx, 
                                                    ",\t mean: ",   show m,
                                                    ",\t stddev: ", show s,
                                                    ",\t path: ",   path ]


main :: IO ()
main = 
   do 
      args <- getArgs
      let (actions, nonOptions, _) = getOpt Permute options args
      _ <- foldl (>>=) (return defaultOpts) actions

      fileMapWithPath countAndDisplay nonOptions


