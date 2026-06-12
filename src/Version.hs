module Version where

import Data.Version (showVersion)
import Import
import Paths_espial qualified

#ifdef USE_BUILDINFO
import BuildInfo qualified
#else
import Development.GitRev
#endif

appVersion :: String
appVersion = showVersion Paths_espial.version

gitSha :: String
#ifdef USE_BUILDINFO
gitSha = BuildInfo.gitSha
#else
gitSha = $gitHash
#endif

gitShaShort :: String
gitShaShort = take 7 gitSha

versionSpec :: Text
versionSpec = "espial-" <> pack appVersion <> " (" <> pack gitShaShort <> ")"