{-

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- | Provide a simplistic parser for Daikon. 

      * Incomplete! (does not parse all features)
      * It won't do full parsing (see the AST)
      
    For now, intended to work only on Daikon files produced by Haslog.

-}

module Eu.DaikonUtils.DaikonSimpleParser

where

import Prelude as P hiding (takeWhile,readFile,writeFile)
import System.Environment
import Data.Text.IO 
-- import System.IO
import Data.Attoparsec.Text
import qualified Data.Text as T
import Control.Applicative
-- import Control.Monad

-- AST
type Line = T.Text

type Header = Line             -- so called Declaration related records, 
                               -- currently only support a single line version decl

type PPtDecl  = (Line,[PPtInfo],[VarDecl])  -- Program point declaratin: ppt-name, ppt-infos, 
                                            -- var-decls                               
type PPtInfo = Line
type VarDecl = (Line,[Line])    -- Name, var-infos
                            
type PPt = (Line,[Line],[Var]) -- Program point: first line, nonce-lines, variables' values
type Var = (Line,Line,Line)
type Daikon = (Header,[PPtDecl],[PPt])

-- Top level parsers

parseFromText :: T.Text -> Daikon      
parseFromText t = case parseOnly parseDaikon t of
                    Left err -> error err
                    Right r  -> r
                    
parseFromString :: String -> Daikon     
parseFromString s = parseFromText (T.pack s)

parseFromFile :: FilePath -> IO Daikon
parseFromFile f = do {
      content <- readFile f ;
      return . parseFromText $ content
    }

-- many1 p = (:) <$> p <*> many p

doubleSlash = string (T.pack "//")
hash_       = string (T.pack "#")
commentLine = (hash_ <|> doubleSlash) *>  many (notChar '\n') *> endOfLine

hspace = T.pack <$> many (satisfy isHorizontalSpace)

parseEmptyLine   = (hspace *> endOfLine) <|> commentLine
parseEmptyLines1 =  many1 parseEmptyLine 

parseLine = T.cons
            <$> satisfy (/= '\n') 
            <*> takeWhile (/= '\n') 
            <*  endOfLine

parseHeader :: Parser Header
parseHeader =  parseLine 
            
parsePPtDecl :: Parser PPtDecl
parsePPtDecl = parseEmptyLines1 *>
               ((\n infos vars -> (n,infos,vars))
                 <$> parsePPtDeclName 
                 <*> many parsePPtInfo 
                 <*> many parseVarDecl  )

append3 s t u =  s `T.append` t `T.append` u   

ppt_  =  T.pack "ppt"            
pppt_ =  string ppt_         
parsePPtDeclName = append3 <$> hspace <*> pppt_ <*> parseLine

parsePPtInfo = append3 <$> hspace <*> kword <*> parseLine
    where
    kword = string (T.pack "ppt-type")
            <|> string (T.pack "flags")
            <|> string (T.pack "parent")
            
parseVarDecl = (\n infos-> (n,infos))
               <$> (append3 <$> hspace <*> string (T.pack "variable") <*> parseLine)
               <*>  many parseVarInfoDecl

parseVarInfoDecl = append3 <$> hspace <*> kword <*> parseLine
    where
    kword = foldr1 (<|>) [string k | k <- varInfoKwords]
    
varInfoKwords = map T.pack
               ["var-kind", "enclosing-var", "reference-type",
                 "array", "dec-type", "rep-type", "flags",
                 "comparability", "parent", "constant",
                 "function-args"
                ]
    
parsePPt :: Parser PPt
parsePPt = parseEmptyLines1 *>
           ((\pptname nonceline nonce vars -> (pptname, [nonceline,nonce], vars)) 
           <$> 
           parseLine <*> parseLine <*> parseLine <*> many parseVar)

parseVar :: Parser Var
parseVar = (\vname val modf -> (vname,val,modf)) <$> parseLine <*> parseLine <*> parseLine

parseDaikon :: Parser Daikon
parseDaikon = (\h decls ppts -> (h,decls,ppts)) 
              <$> parseHeader
              <*> many parsePPtDecl
              <*> many parsePPt
              
testP p s = parseTest p (T.pack s)

--
-- Pretty printers
--

ppDaikon :: Daikon -> T.Text
ppDaikon (header,decls,ppts) = T.concat [ b `T.append` tEmptyline | b <- all]
   where
   all     = header : decls' ++ ppts'
   decls'  = map pp_Decl decls
   ppts'   = map pp_PPt ppts
   

tNewline   =  T.singleton '\n'
tEmptyline =  T.pack "\n\n"
   
pp_Decl (n,infos,vars) = T.intercalate tNewline (n : infos ++ vars')
   where 
   vars' = concat [ vname:infos | (vname,infos) <- vars ]

pp_PPt (name,nonces,vars) = T.intercalate tNewline ppt_
   where
   ppt_  = name : nonces ++ vars'
   vars' = concat [ [n,val,m] | (n,val,m) <- vars ]

ppDaikon_toString d = T.unpack . ppDaikon $ d
   
saveDaikon :: FilePath -> Daikon -> IO ()
saveDaikon f d = writeFile f (ppDaikon d)



