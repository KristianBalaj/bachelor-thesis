{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.IO
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B

data Codons = Codons {
    nonpolar :: [String],
    polar :: [String],
    basic :: [String],
    acidic :: [String],
    termination :: [String]
} deriving (Show, Generic)

instance FromJSON Codons

parseCodons :: FilePath -> IO (Maybe Codons)
parseCodons path = fmap decode (B.readFile path)

main :: IO ()
main = parseCodons "codons.json" >>= print
