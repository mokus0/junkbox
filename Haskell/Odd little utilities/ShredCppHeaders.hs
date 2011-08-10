module ShredCppHeaders where

import System.Environment
import System.Process
import Text.ParserCombinators.Parsec
import Data.List

main = do
    (hdr:args) <- getArgs
    processed <- readProcess "cpp" (["-xc++"] ++ args)
        ("#include " ++ show hdr)
    
    case parse header "<cpp output>" processed of
        Left e  -> print e
        Right e -> putStr (shredHeader e)

data Header = Header [Fragment]
    deriving (Eq, Show)

data Fragment = 
    Fragment Int String [String]
    deriving (Eq, Show)

data Decl
    = Namespace String [Decl]
    | Other String
    deriving (Eq, Show)

interestingNamespaces = ["rti13", "RTI"]
interestingFragment (Fragment _ file _) = interestingFile file
interestingFile file = "include/" `isPrefixOf` file

shredHeader (Header fragments) = cleanLines . unlines . map showFrag . filter interestingFragment $ fragments
cleanLines ('\n':rest@('\n':'\n':_)) = cleanLines rest
cleanLines (x:xs) = x:cleanLines xs
cleanLines [] = []
showFrag (Fragment _ _ text) = unlines text

shredDecls = concatMap shredDecl 

shredDecl (Namespace ns decls)
    | ns `elem` interestingNamespaces
    = shredDecls decls
    | otherwise = ("<namespace elided: " ++ ns ++ "\n")
shredDecl (Other string) = string

header :: CharParser st Header
header = fmap Header (many fragment)

fragment = do
        (line, file) <- lineDecl
        text <- many fragmentLine
        return (Fragment line file text)
    <?> "fragment"

fragmentLine :: CharParser st String
fragmentLine = try (pragma >> return "") <|> do
    c <- noneOf "#"
    if c == '\n'
        then return [c]
        else do
            rest <- many (noneOf "\n")
            eol
            return (c:rest)
    

-- decls = sepBy blankStuff decl
-- 
-- blankStuff = many1 blankThing
-- blankThing = space <|> newline
-- 
-- decl  =  namespace
--      <|> otherDecl
-- 
-- namespace = do
--     string "namespace"
--     blankStuff
--     identifier
--     
--     inCurlys decls

integer :: Integral a => CharParser st a
integer = do
    i <- many1 digit
    return (fromInteger (read i))

quotedString :: CharParser st String
quotedString = do
    char '"'
    string <- many quotedChar
    char '"' <?> "end of quoted string"
    
    return string
    
    <?> "quoted string"

-- conservative... add cases as needed
quotedChar 
    =   alphaNum
    <|> oneOf "/.+<>-_ "
    <|> (char '\\' >> char '\"')
    <?> "quoted char"

eol = char '\n' >> return ()

newline = do
    eol 
    optional lineDecl
    optional pragma

pragma = do
    string "#pragma"
    text <- many (noneOf "\n")
    eol
    return text
    <?> "pragma"

lineDecl = do
    let space = char ' ' <?> "space"
    string "# "
    line <- integer
    many1 space
    file <- quotedString
    many (many1 space >> integer)
    eol
    
    pos <- getPosition
    setPosition (flip setSourceLine line . flip setSourceName file $ pos)
    return (line, file)
    <?> "line declaration"
    
