{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nixpkgs.Haskell.FromStack where

import Control.Lens
import Stackage.BuildPlan
import Stackage.Types (CabalFileInfo(..),PackageConstraints(..), DepInfo(..), SimpleDesc(..), TestState(..))
import HackageGit
import Distribution.Compiler (CompilerInfo(..))
import Distribution.System (Platform(..))
import Distribution.Package (PackageName(..), PackageIdentifier(..), Dependency(..))
import Distribution.PackageDescription (GenericPackageDescription(..))
import Distribution.Nixpkgs.Haskell.FromStack.Package
import Distribution.Nixpkgs.Haskell.PackageSourceSpec
import Distribution.Nixpkgs.Haskell.FromCabal
import Distribution.Nixpkgs.Haskell.Derivation

import qualified Data.Map as Map
import qualified Data.Set as Set

data PackageSetConfig = PackageSetConfig
  { haskellResolver :: HaskellResolver
  , nixpkgsResolver :: NixpkgsResolver
  , packageLoader   :: Maybe SHA1Hash -> PackageIdentifier -> IO Package
  , targetPlatform  :: Platform
  , targetCompiler  :: CompilerInfo
  }

removeTests :: GenericPackageDescription -> GenericPackageDescription
removeTests gd = gd { condTestSuites = [] }

planDependencies :: PackagePlan -> [Dependency]
planDependencies = map makeDependency . Map.toList . sdPackages . ppDesc
 where
  makeDependency (name, depInfo) = Dependency name (diRange depInfo)

buildNodeM :: PackageSetConfig -> PackageName -> PackagePlan -> IO Node
buildNodeM conf name plan = do
  let
    cabalHashes = maybe mempty cfiHashes $ ppCabalFileInfo plan
    mGitSha1 = Map.lookup "GitSHA1" cabalHashes
  pkg <- packageLoader conf mGitSha1 $ PackageIdentifier name (ppVersion plan)
  pure $ buildNode conf plan pkg

buildNode :: PackageSetConfig -> PackagePlan -> Package -> Node
buildNode conf plan pkg =
  let
    constraints = ppConstraints plan
    testsEnabled = pcTests constraints == ExpectSuccess
    haddocksEnabled
       = pcHaddocks constraints == ExpectSuccess
      && not (Set.null (sdModules (ppDesc plan)))
    configureTests
      | pcTests constraints == Don'tBuild = removeTests
      | otherwise = id
    genericDrv = fromGenericPackageDescription
      (haskellResolver conf)
      (nixpkgsResolver conf)
      (targetPlatform conf)
      (targetCompiler conf)
      (Map.toList (pcFlagOverrides constraints))
      (planDependencies plan)
      (configureTests (pkgCabal pkg))
    drv = genericDrv
      & src .~ pkgSource pkg
      & doCheck .~ testsEnabled
      & runHaddock .~ haddocksEnabled
  in mkNode drv
