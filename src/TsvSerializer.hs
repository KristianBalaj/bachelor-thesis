module TsvSerializer (serializeTsv) where

serializeTsv :: [(String, String, Float)] -> String
serializeTsv seqDistances = 
    case seqDistances of
        [] -> ""
        ((seqA, seqB, dist):xs) -> seqA ++ "\t" ++ seqB ++ "\t" ++ show dist ++ "\n" ++ serializeTsv xs