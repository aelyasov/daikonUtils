{-

Author: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

-}

{- |

   Small tool transform a Daikon log. The resulting log is the same,
   except it may not include all variables. Only specified variables
   will be included.
   
   At the moment, vars are only removed from the ppts. We doesn't fix
   the ppts' declarations.

-}
module Main

where

import Eu.DaikonUtils.DaikonSimpleParser
import Data.List
import qualified Data.Text as T
import System.Environment

{-
filterVars vars (h,decls,ppts) = (h, decls, ppts')
   where
   ppts'          = [ (pn,nonces,filter ok vars) | (pn,nonces,vars) <- ppts ]
   ok (vname,_,_) = or [ n `isSuffixOf` T.unpack vname | n <- vars ]
-}
   
filterVars vars (h,decls,ppts) = (h, decls', ppts')
   where
   decls' = map (filterVars2 vars) decls
   ppts'          = [ (pn,nonces,filter ok vars) | (pn,nonces,vars) <- ppts ]
   ok (vname,_,_) = or [ n `isSuffixOf` T.unpack vname | n <- vars ]
   
filterVars2 vars (pptName,infos,vardecls) = (pptName,infos, filter ok vardecls)
   where
   ok (vname,infos) = or [ n `isSuffixOf` T.unpack vname | n <- vars ]
   
mkName fname = reverse f' ++ "filt." ++ reverse ext'
   where
   (ext',f') = span (/= '.') (reverse fname)
   

main :: IO()
main = do { args <- getArgs ; main_ args }

mainf args = main_ (words args)

main_ args = do {
   case args of
      ("--help" : _)   ->  putStrLn "varsfilter filename [name]+"
      (fname : names)  ->  do {
                              putStrLn ("** varsfilter " ++ fname ++ ", " ++ show names) ;
                              d <- parseFromFile fname ;
                              let
                                 d' = filterVars names d
                                 f'  = mkName fname
                              in
                              do { saveDaikon f' d' ;
                                   putStrLn ("** Done, saved in "  ++ f');
                                 }
                           }
   }
   