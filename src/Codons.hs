{-# LANGUAGE DeriveGeneric #-}
module Codons where

import Data.List (elemIndex)
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as BL

import Vec2

data Codons = Codons {
    nonpolar :: [String],
    polar :: [String],
    basic :: [String],
    acidic :: [String],
    termination :: [String]
} deriving (Show, Generic)

instance FromJSON Codons

parseCodons :: FilePath -> IO (Maybe Codons)
parseCodons path = fmap decode (BL.readFile path)

codonDirection :: Codons -> String -> Maybe Vec2
codonDirection allCodons codon =
    let
        ordCodons = orderedCodons allCodons
        codonID = fmap (+1) (elemIndex codon ordCodons) -- +1 because we don't want to start from (0, 1) direction
        anglesCount = length ordCodons + 2  -- plus 2 because (0, 1) and (0, -1) cannot be used (overlapping)
    in
        case codonID of
            Nothing -> Nothing
            Just index ->
                let
                    ratio = fromIntegral index / fromIntegral anglesCount
                    angle = pi / 2 - ratio * pi   -- going from pi/2 down 
                in
                    Just (cos angle, sin angle)
    where
        orderedCodons :: Codons -> [String]
        orderedCodons a =
            nonpolar a ++
            polar a ++
            basic a ++
            acidic a ++
            termination a