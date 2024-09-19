{-# LANGUAGE ImpredicativeTypes, Rank2Types, TupleSections, RecordWildCards #-}
-- | This module handles the mutation of different patterns.
module Test.MuCheck.Mutation where

import Data.Generics (Typeable, mkMp, listify)
import Data.List(nub, (\\), permutations, partition)
import Control.Monad (liftM)
import qualified GHC.Types.SrcLoc as GHC

import Test.MuCheck.Tix
import qualified Test.Mendel.Mutation as M
import Test.Mendel.MutationOperator
import qualified  Test.Mendel.Parser as M
import Test.MuCheck.Utils.Syb
import Test.MuCheck.Utils.Common
import Test.Mendel.Config
import Test.MuCheck.TestAdapter
import System.Directory.Internal.Prelude (exitFailure)

-- | The `genMutants` function is a wrapper to genMutantsWith with standard
-- configuraton
genMutants ::
     FilePath           -- ^ The module we are mutating
  -> FilePath           -- ^ Coverage information for the module
  -> IO (Int,[Mutant]) -- ^ Returns the covering mutants produced, and original length.
genMutants = genMutantsWith defaultConfig

-- | The `genMutantsWith` function takes configuration function to mutate,
-- function to mutate, filename the function is defined in, and produces
-- mutants in the same directory as the filename, and returns the number
-- of mutants produced.
genMutantsWith ::
     Config                     -- ^ The configuration to be used
  -> FilePath                   -- ^ The module we are mutating
  -> FilePath                   -- ^ Coverage information for the module
  -> IO (Int, [Mutant])         -- ^ Returns the covered mutants produced, and the original number
genMutantsWith _config filename  tix = (-1, genMutantsForSrc defaultConfig (readFile filename))

-- | The `genMutantsForSrc` takes the function name to mutate, source where it
-- is defined, and returns the mutated sources
genMutantsForSrc ::
     Config                   -- ^ Configuration
  -> String                   -- ^ The module we are mutating
  -> IO ([Mutant]) -- ^ Returns the mutants
genMutantsForSrc config src = map toMutant $ M.programMutants config ast
  where ast = getASTFromStr src

-- AST/module-related operations

-- | Returns the AST from the file
getASTFromStr :: String -> IO (Module_)
getASTFromStr fname = do 
    mmod <- M.parseModule fname
    case mmod of 
      Just (GHC.L _ mod) -> mod
      Nothing -> exitFailure