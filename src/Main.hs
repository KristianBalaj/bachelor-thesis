{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.List (elemIndex)
import System.Directory (getDirectoryContents)
import System.IO
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import qualified FastaParser as Fasta
import Data.Maybe

data Codons = Codons {
    nonpolar :: [String],
    polar :: [String],
    basic :: [String],
    acidic :: [String],
    termination :: [String]
} deriving (Show, Generic)

instance FromJSON Codons

type Vec2 = (Float, Float)

parseCodons :: FilePath -> IO (Maybe Codons)
parseCodons path = fmap decode (BL.readFile path)

-- main :: IO ()
-- main = parseCodons "codons.json" >>= print

main :: IO ()
main = Fasta.parseFasta <$> B.readFile "data/assembled-ecoli/536.fasta" >>= print

-- main :: IO ()
-- main = getDirectoryContents "data/assembled-ecoli" >>= print

addVec2 :: Vec2 -> Vec2 -> Vec2
addVec2 (a, b) (c, d) = (a + c, b + d)

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

calculateSeqVertices :: Vec2 -> String -> (String -> Maybe Vec2) -> [Vec2]
calculateSeqVertices origin seq codonDirFn =
    case seq of
        [] -> [origin]
        (a:b:c:xs) -> let codonDir = codonDirFn [a, b, c] in
                if isNothing codonDir 
                    then calculateSeqVertices origin xs codonDirFn 
                    else origin : calculateSeqVertices (addVec2 origin (fromJust codonDir)) xs codonDirFn
        _ -> [origin]

-- smth = do
--     fast <- Fasta.parseFasta <$> readFile "data/assembled-ecoli/536.fasta"
--     codons <- parseCodons "codons.json"
--     return $ calculateSeqVertices (0, 0) (Fasta.fastaSeq (head $ fromJust fast)) (codonDirection $ fromJust codons)

    --  calculateSeqVertices (0, 0) (Fasta.parseFasta <$> readFile "data/assembled-ecoli/1test1.fasta")