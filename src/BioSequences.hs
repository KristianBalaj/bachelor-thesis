module BioSequences (sequencesDistances) where

import Data.Maybe
import Control.Monad (filterM)
import qualified Data.Bifunctor (second)
import qualified Data.ByteString.Char8 as B

import qualified FastaParser as Fasta
import Metrics (Metric)
import Codons (Codons, codonDirection)
import Descriptors
import Vec2

type SequenceVertices = [Vec2]

sequencesDistances :: Metric -> [Fasta.FastaSeq] -> Codons -> [(String, String, Float)]
sequencesDistances metric lst codons =
    let
        sequencesPowerset = filterM (const [True, False]) $ sequenceFeaturesVectors $ allSequenceVerts lst codons
        sequencesPairs = filter (\x -> length x == 2) sequencesPowerset
    in
        foldr pairwiseDistance [] sequencesPairs
    where 
        pairwiseDistance [(a, featuresVec1), (b, featuresVec2)] accumulator = (a, b, metric featuresVec1 featuresVec2):accumulator

sequenceFeaturesVectors :: [(String, SequenceVertices)] -> [(String, [Float])]
sequenceFeaturesVectors = map (Data.Bifunctor.second featuresVector)

allSequenceVerts :: [Fasta.FastaSeq] -> Codons -> [(String, SequenceVertices)]
allSequenceVerts fastas codons = map processFasta fastas
    where
        processFasta :: Fasta.FastaSeq -> (String, SequenceVertices)
        processFasta fasta =
            (
                B.unpack $ Fasta.fastaHeader fasta,
                calculateSeqVertices (0, 0) (codonDirection codons) (B.unpack $ Fasta.fastaSeq fasta)
            )

calculateSeqVertices :: Vec2 -> (String -> Maybe Vec2) -> String -> SequenceVertices
calculateSeqVertices origin codonDirFn seq =
    case seq of
        (a:b:c:xs) -> let codonDir = codonDirFn [a, b, c] in
                if isNothing codonDir
                    then calculateSeqVertices origin codonDirFn xs
                    else origin : calculateSeqVertices (addVec2 origin (fromJust codonDir)) codonDirFn xs
        _ -> [origin]