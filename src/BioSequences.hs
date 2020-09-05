module BioSequences (sequencesDistances) where

import Data.Maybe
import Control.Monad (filterM, join)
import qualified Data.Bifunctor (second)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.List.Split

import qualified FastaParser as Fasta
import Metrics (Metric)
import qualified Codons (Codons, CodonsLookup, codonLookup)
import Descriptors
import Vec2

type SequenceVertices = [Vec2]

sequencesDistances :: Metric -> [Fasta.FastaSeq] -> Codons.Codons -> [(String, String, Float)]
sequencesDistances metric lst codons =
    let
        sequencesPowerset = filterM (const [True, False]) $ sequenceFeaturesVectors $ allSequenceVerts lst (Codons.codonLookup codons)
        sequencesPairs = filter (\x -> length x == 2) sequencesPowerset
    in
        foldr pairwiseDistance [] sequencesPairs
    where 
        pairwiseDistance [(a, featuresVec1), (b, featuresVec2)] accumulator = (a, b, metric featuresVec1 featuresVec2):accumulator

sequenceFeaturesVectors :: [(String, SequenceVertices)] -> [(String, [Float])]
sequenceFeaturesVectors = map (Data.Bifunctor.second featuresVector)

allSequenceVerts :: [Fasta.FastaSeq] -> Codons.CodonsLookup -> [(String, SequenceVertices)]
allSequenceVerts fastas codonsLookup = map processFasta fastas
    where
        processFasta :: Fasta.FastaSeq -> (String, SequenceVertices)
        processFasta fasta =
            (
                B.unpack $ Fasta.fastaHeader fasta,
                calculateSeqVertices (Vec2 0 0) (join . flip HM.lookup codonsLookup) (B.unpack $ Fasta.fastaSeq fasta)
            )

calculateSeqVertices :: Vec2 -> (String -> Maybe Vec2) -> String -> SequenceVertices
calculateSeqVertices origin codonDirFn seq =
    case seq of
        (a:b:c:xs) -> let codonDir = codonDirFn [a, b, c] in
                if isNothing codonDir
                    then calculateSeqVertices origin codonDirFn xs
                    else origin : calculateSeqVertices (addVec2 origin (fromJust codonDir)) codonDirFn xs
        _ -> [origin]

        -- Remove the explicit recursion and evaluate it strictly not to store large thunks in memory