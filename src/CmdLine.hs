{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module CmdLine (getCmd, Cmd(..)) where

import System.Console.CmdArgs.Implicit
import Data.Typeable
import Data.Data

getCmd :: IO Cmd
getCmd = cmdArgs mode >>= processCmd

data Cmd = Cmd {
    cmdCodonsFile :: String,
    cmdSequencesDir :: String,
    cmdResultFile :: String,
    cmdSequenceFileEndings :: [String]
} deriving (Show, Data, Typeable)

mode = Cmd { 
    cmdCodonsFile = def &= explicit &= name "codons-file" &= help "File path to the codons json file. (default = ./codons.json)",
    cmdSequencesDir = def &= explicit &= name "sequences-directory" &= help "File path to the directory with sequences files.",
    cmdResultFile = def &= explicit &= name "result-file" &= help "Defaults to ./result.tsv",
    cmdSequenceFileEndings = def &= args &= typ "FILE ENDINGS"
} &= program "bachelor-thesis"

processCmd :: Cmd -> IO Cmd
processCmd cmd = codonsFile cmd >>= resultFile
    where
        codonsFile cmd = return $ if null $ cmdCodonsFile cmd then cmd{cmdCodonsFile="./codons.json"} else cmd
        resultFile cmd = return $ if null $ cmdResultFile cmd then cmd{cmdResultFile="./result.tsv"} else cmd