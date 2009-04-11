{-
 -      ``System/Disk/Free''
 -      (c) 2009 James Cook
 -}

module System.Disk.Free where

import System.Process
import System.IO

b,kb,mb,gb,tb :: Num a => a
b  = 1
kb = 1024 * b
mb = 1024 * kb
gb = 1024 * mb
tb = 1024 * gb

data DfRecord = DfRecord 
    { dfFilesystem  :: String
    , dfSize        :: Integer
    , dfUsed        :: Integer
    , dfAvail       :: Integer
    , dfUsePct      :: Integer
    , dfMountPt     :: String
    } deriving (Eq, Show)

parseDfRecord :: Integer -> String -> DfRecord
parseDfRecord blkSz = parse . wordsWithSpaces
    where
        isNum x = case reads x :: [(Integer, String)] of
            [(_,"")] -> True
            _   -> False
        parse xs = case xs of
            (fs:spc:notNum:rest)
                | not (isNum notNum) -> parse ((fs ++ spc ++ notNum) : rest)
            (fs:_:blks:_:used:_:avail:_:pct:_:rest) -> DfRecord
                { dfFilesystem  = fs
                , dfSize        = blkSz * read blks
                , dfUsed        = blkSz * read used
                , dfAvail       = blkSz * read avail
                , dfUsePct      = read (filter (/= '%') pct)
                , dfMountPt     = concat rest
                }
            _ -> error "parseDfRecord: no parse"
        
        wordsWithSpaces [] = []
        wordsWithSpaces xs@(' ':_) = spaces : wordsWithSpaces rest
            where (spaces, rest) = break (/=' ') xs
        wordsWithSpaces xs = word : wordsWithSpaces rest
            where (word, rest) = break (==' ') xs

df :: IO [DfRecord]
df = do
    let blkSz = 512
    
    (i,o,e,pid) <- runInteractiveProcess "df" ["-P"] Nothing
        (Just [("POSIXLY_CORRECT", "YES")])
    
    contents <- hGetContents o
    return . map (parseDfRecord blkSz) . drop 1 . lines $ contents
