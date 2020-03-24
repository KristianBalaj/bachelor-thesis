{-# LANGUAGE DeriveGeneric #-}

module Codons (codonLookup, parseCodons, CodonsLookup, Codons(..)) where

import Data.List (elemIndex)
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM

import Vec2

type CodonsLookup = HM.HashMap String (Maybe Vec2)
type LookupSize = Int

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

codonLookup :: Codons -> CodonsLookup
codonLookup codons = 
    let 
        codonToIndex = HM.fromList $ zip (orderedCodons codons) [0..] :: HM.HashMap String Int
        indexLookupSize = HM.size codonToIndex :: Int
    in
        HM.mapWithKey (\codon index -> calculateVecFromIndex indexLookupSize (HM.lookup codon codonToIndex)) codonToIndex
    where
        calculateVecFromIndex :: LookupSize -> Maybe Int -> Maybe Vec2
        calculateVecFromIndex lookupSize index =
            let
                codonID = (+1) <$> index -- +1 because we don't want to start from (0, 1) direction
                anglesCount = lookupSize + 2  -- plus 2 because (0, 1) and (0, -1) cannot be used (overlapping)
            in
                case codonID of
                    Nothing -> Nothing
                    Just index ->
                        let
                            ratio = fromIntegral index / fromIntegral anglesCount
                            angle = pi / 2 - ratio * pi   -- going from pi/2 down 
                        in
                            Just (cos angle, sin angle)

orderedCodons :: Codons -> [String]
orderedCodons codons = 
    nonpolar codons ++
    polar codons ++
    basic codons ++
    acidic codons ++
    termination codons
        