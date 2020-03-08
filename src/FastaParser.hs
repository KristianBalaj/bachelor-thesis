module FastaParser (parseFasta, FastaSeq(..)) where 

import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString as A
import Data.Word8

data FastaSeq = FastaSeq {
    fastaHeader :: B.ByteString,
    fastaSeq :: B.ByteString
} deriving (Show)

fastaSeqStart :: (Word8 -> Bool)
fastaSeqStart c = c `elem` [_semicolon, _greater]

recordStart :: Parser B.ByteString
recordStart = A.takeWhile1 fastaSeqStart

word :: Parser B.ByteString
word = A.takeWhile1 $ not . isSpace

header :: Parser B.ByteString
header = do
    _ <- recordStart
    _ <- A.takeWhile isSpace
    header <- word
    _ <- A.takeWhile isSpace
    return header

fastaSequence :: Parser [B.ByteString]
fastaSequence = A.takeWhile (\c -> not (isSpace c) && not (fastaSeqStart c)) `A.sepBy1` satisfy isSpace

singleFastaSeq :: Parser FastaSeq
singleFastaSeq = do
    hdr <- header
    seq <- B.concat <$> fastaSequence
    return (FastaSeq hdr seq)

fasta :: Parser [FastaSeq]
fasta = do
    res <- A.many' singleFastaSeq
    A.endOfInput 
    return res

parseFasta :: B.ByteString -> Maybe [FastaSeq]
parseFasta contents = 
    case A.parseOnly fasta contents of
        Left _ -> Nothing
        Right fastas -> Just fastas