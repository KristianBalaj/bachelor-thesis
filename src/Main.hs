module Main where

import System.Directory (getDirectoryContents)
import System.IO
import qualified Data.ByteString.Char8 as B
import Data.List.Split (splitOn)
import Control.Monad (join)

import qualified BioSequences
import qualified FastaParser as Fasta

-- main :: IO ()
-- main = parseCodons "codons.json" >>= print

-- main :: IO ()
-- main = Fasta.parseFasta <$> B.readFile "data/assembled-ecoli/536.fasta" >>= print

-- main :: IO ()
-- main = smth >>= print

-- main :: IO ()
-- main = do
--     fast <- Fasta.parseFasta <$> B.readFile "data/assembled-ecoli/536.fasta"
--     print (B.unpack $ Fasta.fastaSeq (head $ fromJust fast))

main :: IO ()
main = getDirectoryContents "data/assembled-ecoli" >>= print

allSequences :: FilePath -> [String] -> IO (Maybe [Fasta.FastaSeq])
allSequences directoryPath fileEndings =
    let
        allFiles = getDirectoryContents directoryPath :: IO [FilePath]
        sequencesFiles = filter isCorrectFile <$> allFiles :: IO [String]
        sequencesFilesPaths = map (directoryPath ++) <$> sequencesFilesPaths :: IO [String]
        filesContents = traverse B.readFile =<< sequencesFilesPaths :: IO [B.ByteString]
        parsedFiles = fmap (Fasta.parseFasta <$>) filesContents :: IO [Maybe [Fasta.FastaSeq]]
    in
        fmap join . sequence <$> parsedFiles
    where
        isCorrectFile :: String -> Bool
        isCorrectFile fileName = last (splitOn "." fileName) `elem` fileEndings

-- smth = do
--     fast <- Fasta.parseFasta <$> B.readFile "data/assembled-ecoli/536.fasta"
--     codons <- parseCodons "codons.json"
--     let verts = calculateSeqVertices (0, 0) (B.unpack $ Fasta.fastaSeq (head $ fromJust fast)) (codonDirection $ fromJust codons)
--     return $ length verts

    -- return $ calculateSeqVertices (0, 0) (B.unpack $ Fasta.fastaSeq (head $ fromJust fast)) (codonDirection $ fromJust codons)

    --  calculateSeqVertices (0, 0) (Fasta.parseFasta <$> readFile "data/assembled-ecoli/1test1.fasta")