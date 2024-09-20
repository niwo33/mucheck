{-# LANGUAGE ImpredicativeTypes, Rank2Types, TupleSections, RecordWildCards #-}
-- | This module handles the mutation of different patterns.
module Test.MuCheck.Mutation where

import Data.Generics (listify)
import Data.List(partition)

import qualified GHC.Types.SrcLoc as GHC
import qualified Language.Haskell.Syntax.Decls as GHC
import qualified GHC.Hs as GHC
import qualified GHC.Types.Name.Occurrence as GHC
import qualified GHC.Types.Name.Reader as GHC
import qualified GHC.Data.FastString as GHC
import qualified GHC.Utils.Outputable as GHC

import Test.MuCheck.Tix
import qualified Test.Mendel.Mutation as M
import Test.Mendel.MutationOperator
import qualified  Test.Mendel.Parser as M
import Test.MuCheck.Utils.Common
import Test.Mendel.Config
import Test.Mendel.Printer
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
genMutantsWith _config filename tix = do
  mutants <- genMutantsForSrc defaultConfig filename
  return (-1, mutants)

-- | The `genMutantsForSrc` takes the function name to mutate, source where it
-- is defined, and returns the mutated sources
genMutantsForSrc ::
     Config                   -- ^ Configuration
  -> String                   -- ^ Path to the module we are mutating
  -> IO [Mutant] -- ^ Returns the mutants
genMutantsForSrc config path = do
  origAst <- getASTFromStr path

  let (onlyAnn, noAnn) = splitAnnotations origAst
      ast = putDecl origAst noAnn
      mutants = M.programMutants config ast
      annmutants = map (apTh (withAnn onlyAnn)) mutants

  pure (map (toMutant . apTh (GHC.renderWithContext GHC.defaultSDocContext . GHC.ppr)) annmutants)

withAnn :: [GHC.LHsDecl GHC.GhcPs] -> Module_ -> Module_
withAnn decls modu = putDecl modu $ getDecla modu ++ decls


-- AST/module-related operations

-- | Returns the AST from the file
getASTFromStr :: String -> IO (Module_)
getASTFromStr fname = do 
    mmod <- M.parseModule fname
    case mmod of 
      Just (GHC.L _ modu) -> pure modu
      Nothing -> exitFailure


-- Annotation related operations

-- | Split declarations of the module to annotated and non annotated.
splitAnnotations :: Module_ -> ([GHC.LHsDecl GHC.GhcPs], [GHC.LHsDecl GHC.GhcPs])
splitAnnotations ast = partition fn $ getDecla ast
  where fn x = functionName x `elem` getAnnotatedTests ast --(functionName x ++ pragmaName x) `elem` getAnnotatedTests ast
        -- only one of pragmaName or functionName will be present at a time.

-- | get all annotated functions
getAnn :: Module_ -> String -> [String]
getAnn m s =  [conv ann | ann <- listify isAnn m]
  where isAnn :: GHC.AnnDecl GHC.GhcPs -> Bool
        isAnn (GHC.HsAnnotation _ _ (GHC.L _ (GHC.HsLit _ (GHC.HsString _ e)))) = e == GHC.mkFastString s
        isAnn _ = False
        conv (GHC.HsAnnotation _ (GHC.ValueAnnProvenance (GHC.L _ (GHC.Unqual n))) _) = GHC.occNameString n
        conv (GHC.HsAnnotation _ (GHC.TypeAnnProvenance (GHC.L _ (GHC.Unqual n))) _) = GHC.occNameString n
        conv (GHC.HsAnnotation _ GHC.ModuleAnnProvenance _) = ""

-- | Get the embedded declarations from a module.
getDecla :: Module_ -> [GHC.LHsDecl GHC.GhcPs]
getDecla (GHC.HsModule _ _ _ _ ldecls) = ldecls

-- | Put the given declarations into the given module
putDecl :: Module_ -> [GHC.LHsDecl GHC.GhcPs] -> Module_
putDecl (GHC.HsModule a b c d _) = GHC.HsModule a b c d

-- | The name of a function
functionName :: GHC.LHsDecl GHC.GhcPs -> String
functionName (GHC.L _ (GHC.ValD _ (GHC.FunBind _l (GHC.L _ (GHC.Unqual n)) _))) = GHC.occNameString n
-- we also consider where clauses
-- functionName (GHC.ValD _ (GHC.PatBind _l (GHC.L _ (GHC.Unqual n)) _)) = GHC.occNameString n
functionName _                                   = []

-- | The identifier of declared pragma
-- pragmaName :: GHC.HsDecl GHC.GhcPs -> String
-- pragmaName (AnnPragma _ (Ann _l (Ident _li n) (Lit _ll (String _ls _t _)))) = n
-- pragmaName _ = []


-- Find tests 

-- | Returns the annotated tests and their annotations
getAnnotatedTests :: Module_ -> [String]
getAnnotatedTests ast = concatMap (getAnn ast) ["Test","TestSupport"]

-- | given the module name, return all marked tests
getAllTests :: String -> IO [String]
getAllTests modname = do
    ast <- getASTFromStr modname
    
    return (getAnn ast "Test")