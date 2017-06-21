{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HackageGit where

import Control.Lens hiding ( (<.>) )
import Control.Monad
import Data.Aeson
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map as Map
import Data.Set as Set
import Data.String
import Data.String.UTF8 ( toString, fromRep )
import Data.Text as T
import Distribution.Nixpkgs.Hashes
import Distribution.Nixpkgs.Haskell.OrphanInstances ( )
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse ( parsePackageDescription, ParseResult(..) )
import Distribution.Text
import Distribution.Version
import OpenSSL.Digest ( digest, digestByName )
import System.Directory
import System.FilePath
import Git.Libgit2 as Libgit2
import Git

type Hackage = Map PackageName (Set Version)

readHackage :: FilePath -> IO Hackage
readHackage path = getSubDirs path >>= foldM discoverPackageVersions mempty
  where
    discoverPackageVersions :: Hackage -> String -> IO Hackage
    discoverPackageVersions db pkg = do
      vs <- getSubDirs (path </> pkg)
      return (Map.insert (PackageName pkg) (Set.fromList (Prelude.map fromString vs)) db)

getSubDirs :: FilePath -> IO [FilePath]
getSubDirs path = do
  let isDirectory p = doesDirectoryExist (path </> p)
  getDirectoryContents path >>= filterM isDirectory . Prelude.filter (\x -> Prelude.head x /= '.')

decodeUTF8 :: ByteString -> String
decodeUTF8 = toString . fromRep

type SHA256Hash = String
type SHA1Hash = T.Text

readPackageByHash :: FilePath -> SHA1Hash -> IO (GenericPackageDescription, SHA256Hash)
readPackageByHash repoDir sha1Hash = do
  let repoOpts = defaultRepositoryOptions { repoPath = repoDir }
  repo <- openLgRepository repoOpts
  buf <- runLgRepository repo $ do
    BlobObj (Blob _ contents) <- lookupObject =<< parseOid sha1Hash
    case contents of
      BlobString bs -> return bs
      BlobStringLazy lbs -> return $ LBS.toStrict lbs
      _ -> fail $ "Git SHA1 " ++ show sha1Hash ++ ": expected single Blob"
  cabal <- case parsePackageDescription (decodeUTF8 buf) of
    ParseOk _ a     -> return a
    ParseFailed err -> fail ("Git SHA1 " ++ show sha1Hash ++ ": " ++ show err)
  let
    hash = printSHA256 (digest (digestByName "sha256") buf)
    pkg  = setCabalFileHash hash cabal
  return (pkg, hash)

readPackageFile :: FilePath -> PackageIdentifier -> IO (GenericPackageDescription, SHA256Hash)
readPackageFile repoDir (PackageIdentifier name version) = do
  let cabalFile = repoDir </> unPackageName name </> display version </> unPackageName name <.> "cabal"
  buf <- BS.readFile cabalFile
  cabal <- case parsePackageDescription (decodeUTF8 buf) of
    ParseOk _ a     -> return a
    ParseFailed err -> fail (cabalFile ++ ": " ++ show err)
  let
    hash = printSHA256 (digest (digestByName "sha256") buf)
    pkg  = setCabalFileHash hash cabal
  return (pkg, hash)

setCabalFileHash :: SHA256Hash -> GenericPackageDescription -> GenericPackageDescription
setCabalFileHash hash cabal = cabal
  { packageDescription = (packageDescription cabal)
    { customFieldsPD = ("X-Cabal-File-Hash", hash) : customFieldsPD (packageDescription cabal)
    }
  }

declareLenses [d|
  data Meta = Meta { hashes :: Map String String
                   , locations :: [String]
                   , pkgsize :: Int
                   }
    deriving (Show)
  |]

instance FromJSON Meta where
  parseJSON (Object v) = Meta
                         <$> v .: "package-hashes"
                         <*> v .: "package-locations"
                         <*> v .: "package-size"
  parseJSON o          = fail ("invalid Cabal metadata: " ++ show o)

readPackageMeta :: FilePath -> PackageIdentifier -> IO Meta
readPackageMeta dirPrefix (PackageIdentifier name version) = do
  let metaFile = dirPrefix </> unPackageName name </> display version </> unPackageName name <.> "json"
  buf <- BS.readFile metaFile
  case eitherDecodeStrict buf of
    Left msg -> fail (metaFile ++ ": " ++ msg)
    Right x  -> return $ over (hashes . ix "SHA256") (printSHA256 . packHex) x
