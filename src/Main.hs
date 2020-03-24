module Main where

import Data.Maybe (fromJust, maybe)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.IO
import qualified Data.ByteString.Char8 as B
import Data.List (isSuffixOf)
import Control.Monad (join)
import System.FilePath (combine)

import qualified BioSequences
import qualified Codons (parseCodons)
import qualified Metrics
import qualified FastaParser as Fasta
import qualified TsvSerializer
import qualified CmdLine

main :: IO ()
main = do
    args <- CmdLine.getCmd
    putStrLn $ "Codons file: \"" ++ CmdLine.cmdCodonsFile args ++ "\""
    putStrLn $ "Sequence files dir: \"" ++ CmdLine.cmdSequencesDir args ++ "\""
    putStrLn $ "Result file: \"" ++ CmdLine.cmdResultFile args ++ "\""
    putStrLn $ "Sequence file endings: " ++ show (CmdLine.cmdSequenceFileEndings args)
    putStrLn "Processing..."
    --
    codons <- Codons.parseCodons $ CmdLine.cmdCodonsFile args
    sequences <- allSequences args
    let distances = BioSequences.sequencesDistances Metrics.euclideanDistance <$> sequences <*> codons
    writeFile (CmdLine.cmdResultFile args) $ fromJust $ TsvSerializer.serializeTsv <$> distances
    --
    putStrLn $ "Sequences count = " ++ show (maybe 0 length sequences)
    putStrLn $ "Resulting distances count = " ++ show (maybe 0 length distances)
    putStrLn "Finished"

allSequences :: CmdLine.Cmd -> IO (Maybe [Fasta.FastaSeq])
allSequences CmdLine.Cmd{CmdLine.cmdSequencesDir = dir, CmdLine.cmdSequenceFileEndings = fileEndings} =
    let
        sequencesFiles = filter isCorrectFile <$> allFiles :: IO [String]
        sequencesFilesPaths = map (combine dir) <$> sequencesFiles :: IO [String]
        filesContents = traverse B.readFile =<< sequencesFilesPaths :: IO [B.ByteString]
        parsedFiles = fmap (Fasta.parseFasta <$>) filesContents :: IO [Maybe [Fasta.FastaSeq]]
    in
        fmap join . sequence <$> parsedFiles
    where
        allFiles :: IO [FilePath]
        allFiles = do
            exists <- doesDirectoryExist dir
            if exists && not (null dir)
                then getDirectoryContents dir 
                else fail $ "No such directory exists (directory=\"" ++ dir ++ "\")!"
        isCorrectFile :: String -> Bool
        isCorrectFile fileName = any (`isSuffixOf` fileName) fileEndings