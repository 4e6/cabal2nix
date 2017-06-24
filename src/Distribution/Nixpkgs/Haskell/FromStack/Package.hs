{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Distribution.Nixpkgs.Haskell.FromStack.Package where

import Control.Lens
import Data.Graph (Graph, Vertex)
import Data.Foldable as F
import Data.Maybe
import Data.Monoid
import Data.Ord as O
import Distribution.Text (display, disp)
import Distribution.Package (PackageName(..), PackageIdentifier(..))
import Distribution.Nixpkgs.Haskell.BuildInfo
import Distribution.Nixpkgs.Haskell.Derivation
import Language.Nix.PrettyPrinting (onlyIf)
import Language.Nix as Nix
import Stackage.Types (SystemInfo(..))
import Text.PrettyPrint.HughesPJClass hiding ((<>))


import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Set as Set

data Node = Node
  { _nodeDerivation   :: Derivation
  , _nodeTestDepends  :: Set.Set String
  , _nodeOtherDepends :: Set.Set String }

makeLenses ''Node

mkNode :: Derivation -> Node
mkNode _nodeDerivation = Node{..}
  where
    haskellDependencies s = Set.map (view (localName . ident))
      . Set.filter isFromHackage
      $ view (s . (haskell <> tool)) _nodeDerivation
    _nodeTestDepends = haskellDependencies testDepends
    _nodeOtherDepends = haskellDependencies (executableDepends <> libraryDepends)

nodeName :: Node -> String
nodeName = unPackageName . pkgName . view (nodeDerivation . pkgid)

nodeDepends :: Node -> Set.Set String
nodeDepends = _nodeTestDepends <> _nodeOtherDepends

findCycles :: [Node] -> [[Node]]
findCycles nodes = mapMaybe cyclic $
  Graph.stronglyConnComp [(node, nodeName node, Set.toList $ nodeDepends node) | node <- nodes]
 where
  cyclic (Graph.AcyclicSCC _) = Nothing
  cyclic (Graph.CyclicSCC c)  = Just c

breakCycle :: [Node] -> [String]
breakCycle [] = []
breakCycle c = nodeName breaker : concatMap breakCycle (findCycles remaining)
 where
  breaker = F.maximumBy (O.comparing rate) c
  remaining = map updateNode c
  updateNode n
    | nodeName n == nodeName breaker = n & nodeTestDepends .~ mempty
    | otherwise = n
  names = Set.fromList $ map nodeName c
  rate node = - Set.size (view nodeOtherDepends node `Set.intersection` names)

type FromVertex = Vertex -> (Node, String, [String])
type FromKey = String -> Maybe Vertex

buildNodeGraph :: [Node] -> (Graph, FromVertex, FromKey)
buildNodeGraph nodes = Graph.graphFromEdges
  [(node, nodeName node, Set.toList $ nodeDepends node) | node <- nodes]

reachableDependencies :: [Node] -> [Node] -> [Node]
reachableDependencies keyNodes nodes = view _1 . fromVertex <$> reachableVertices
  where
    (graph, fromVertex, fromKey) = buildNodeGraph nodes
    keys = mapMaybe (fromKey . nodeName) keyNodes
    reachableVertices = concatMap F.toList $ Graph.dfs graph keys

-- pretty printing

isFromHackage :: Binding -> Bool
isFromHackage b = case view (reference . Nix.path) b of
  ["self",_] -> True
  _          -> False


pPrintOutConfig :: SystemInfo -> [Node] -> Doc
pPrintOutConfig systemInfo nodes = vcat
  [ "{ pkgs }:"
  , ""
  , "with pkgs.haskell.lib; self: super: {"
  , ""
  , "  # core packages"
  , nest 2 $ vcat $
      Map.toList (siCorePackages systemInfo) <&> \(pkg, _version) ->
        onlyIf (pkg /= "ghc")
          $ hsep [doubleQuotes (text (display pkg)), equals, text "null"] <> semi
  , ""
  , nest 2 $ vcat $ pPrintBreakCycle <$> findCycles nodes
  , ""
  , "}"
  ]

pPrintBreakCycle :: [Node] -> Doc
pPrintBreakCycle c = vcat
  [ text $ "# break cycle: " ++ unwords (map nodeName c)
  , vcat $ breakCycle c <&> \breaker ->
      hsep [doubleQuotes (text breaker), equals, text "dontCheck", text "super." <> text breaker] <> semi
  ]

pPrintOutPackages :: [Derivation] -> Doc
pPrintOutPackages drvs = vcat
  [ "{ pkgs, stdenv, callPackage }:"
  , ""
  , "self: {"
  , ""
  , nest 2 $ vcat (pPrintPackageOverride <$> drvs)
  , ""
  , "}"
  ]

pPrintPackageOverride :: Derivation -> Doc
pPrintPackageOverride drv =
  let
    name = drv ^. pkgid . to pkgName
    overrides = fsep
      [ disp bind <> semi
      | bind <- Set.toList $ view (dependencies . each <> extraFunctionArgs) drv
      , not $ isFromHackage bind ]
  in
    hang (hsep [doubleQuotes (text (display name)), equals, text "callPackage"]) 2
    $ parens (pPrint drv) <+> (braces overrides <> semi)
