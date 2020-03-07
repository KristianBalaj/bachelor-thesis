module FastaParser (parseFasta) where 

import Text.ParserCombinators.ReadP
import Data.Char (isSpace)

data FastaSeq = FastaSeq {
    fastaHeader :: String,
    fastaSeq :: String
} deriving (Show)

fastaSeqStart :: (Char -> Bool)
fastaSeqStart c = c `elem` ">;"

recordStart :: ReadP String
recordStart = munch1 fastaSeqStart

word :: ReadP String
word = munch1 $ not . isSpace

header :: ReadP String
header = do
    _ <- recordStart
    _ <- munch isSpace
    header <- word
    _ <- munch isSpace
    return header

fastaSequence :: ReadP [String]
fastaSequence = (munch (\c -> not (isSpace c) && not (fastaSeqStart c))) `sepBy1` satisfy isSpace

singleFastaSeq :: ReadP FastaSeq
singleFastaSeq = do
    hdr <- header
    seq <- concat <$> fastaSequence
    return (FastaSeq hdr seq)

fasta :: ReadP [FastaSeq]
fasta = do
    res <- many singleFastaSeq
    eof
    return res

parseFasta :: String -> Maybe [FastaSeq]
parseFasta contents = 
    case readP_to_S fasta contents of
        [] -> Nothing
        ((fastas, rest):xs) -> Just fastas