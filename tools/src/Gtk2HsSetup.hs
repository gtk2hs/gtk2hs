{-# LANGUAGE CPP, ViewPatterns #-}
-- | Build a Gtk2hs package.
--
module Gtk2HsSetup (
  gtk2hsUserHooks,
  getPkgConfigPackages,
  checkGtk2hsBuildtools,
  typeGenProgram,
  signalGenProgram,
  c2hsLocal
  ) where

import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.InstalledPackageInfo ( importDirs,
                                           showInstalledPackageInfo,
                                           libraryDirs,
                                           extraLibraries,
                                           extraGHCiLibraries )
import Distribution.Simple.PackageIndex ( lookupUnitId )
import Distribution.PackageDescription as PD ( PackageDescription(..),
                                               updatePackageDescription,
                                               BuildInfo(..),
                                               emptyBuildInfo, allBuildInfo,
                                               Library(..),
                                               explicitLibModules, hasLibs)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(withPackageDB, buildDir, localPkgDescr, installedPkgs, withPrograms),
                                           InstallDirs(..),
                                           ComponentLocalBuildInfo,
                                           componentPackageDeps,
                                           absoluteInstallDirs,
                                           relocatable,
                                           compiler)
import Distribution.Simple.Compiler  ( Compiler(..) )
import Distribution.Simple.Program (
  Program(..), ConfiguredProgram(..),
  runDbProgram, getDbProgramOutput, programName, programPath,
  c2hsProgram, pkgConfigProgram, gccProgram, requireProgram, ghcPkgProgram,
  simpleProgram, lookupProgram, rawSystemProgramStdout, ProgArg)
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Simple.Program.HcPkg ( defaultRegisterOptions )
import Distribution.Types.PkgconfigDependency ( PkgconfigDependency(..) )
import Distribution.Types.PkgconfigName
import qualified Distribution.Types.LocalBuildInfo as LBI (componentsConfigs)  -- TODO will be removed in Cabal 2.2
#endif
import Distribution.ModuleName ( ModuleName, components, toFilePath )
import Distribution.Simple.Utils
import Distribution.Simple.Setup (CopyFlags(..), InstallFlags(..), CopyDest(..),
                                  defaultCopyFlags, ConfigFlags(configVerbosity),
                                  fromFlag, toFlag, RegisterFlags(..), flagToMaybe,
                                  fromFlagOrDefault, defaultRegisterFlags)
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Simple.BuildPaths ( autogenModulesDir )
#endif
import Distribution.Simple.Install ( install )
import Distribution.Simple.Register ( generateRegistrationInfo, registerPackage )
import Distribution.Text ( simpleParse, display )
import System.FilePath
import System.Exit (exitFailure)
import System.Directory ( doesFileExist, getDirectoryContents, doesDirectoryExist )
import Distribution.Version (Version(..))
import Distribution.Verbosity
import Control.Monad (when, unless, filterM, liftM, forM, forM_)
import Data.Maybe ( isJust, isNothing, fromMaybe, maybeToList, catMaybes )
import Data.List (isPrefixOf, isSuffixOf, nub, minimumBy, stripPrefix, tails )
import Data.Ord as Ord (comparing)
import Data.Char (isAlpha, isNumber)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Distribution.PackageDescription as PD
import qualified Distribution.Simple.LocalBuildInfo as LBI
import qualified Distribution.InstalledPackageInfo as IPI
       (installedUnitId)
import Distribution.Simple.Compiler (compilerVersion)

import Control.Applicative ((<$>))

import Distribution.Simple.Program.Find ( defaultProgramSearchPath )
import Gtk2HsC2Hs (c2hsMain)
import HookGenerator (hookGen)
import TypeGen (typeGen)
import UNames (unsafeResetRootNameSupply)

#if !MIN_VERSION_Cabal(2,0,0)
versionNumbers :: Version -> [Int]
versionNumbers = versionBranch
#endif

onDefaultSearchPath f a b = f a b defaultProgramSearchPath
#if MIN_VERSION_Cabal(2,5,0)
libraryConfig lbi = case [clbi | (LBI.CLibName _, clbi, _) <- LBI.componentsConfigs lbi] of
#else
libraryConfig lbi = case [clbi | (LBI.CLibName, clbi, _) <- LBI.componentsConfigs lbi] of
#endif
  [clbi] -> Just clbi
  _ -> Nothing

-- the name of the c2hs pre-compiled header file
precompFile = "precompchs.bin"

gtk2hsUserHooks = simpleUserHooks {
    -- hookedPrograms is only included for backwards compatibility with older Setup.hs.
    hookedPrograms = [typeGenProgram, signalGenProgram, c2hsLocal],
    hookedPreProcessors = [("chs", ourC2hs)],
    confHook = \pd cf ->
      (fmap adjustLocalBuildInfo (confHook simpleUserHooks pd cf)),
    postConf = \args cf pd lbi -> do
      genSynthezisedFiles (fromFlag (configVerbosity cf)) pd lbi
      postConf simpleUserHooks args cf pd lbi,
    buildHook = \pd lbi uh bf -> fixDeps pd >>= \pd ->
                                 buildHook simpleUserHooks pd lbi uh bf,
    copyHook = \pd lbi uh flags -> copyHook simpleUserHooks pd lbi uh flags >>
      installCHI pd lbi (fromFlag (copyVerbosity flags)) (fromFlag (copyDest flags)),
    instHook = \pd lbi uh flags ->
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
      installHook pd lbi uh flags >>
      installCHI pd lbi (fromFlag (installVerbosity flags)) NoCopyDest,
    regHook = registerHook
#else
      instHook simpleUserHooks pd lbi uh flags >>
      installCHI pd lbi (fromFlag (installVerbosity flags)) NoCopyDest
#endif
  }

------------------------------------------------------------------------------
-- Lots of stuff for windows ghci support
------------------------------------------------------------------------------

getDlls :: [FilePath] -> IO [FilePath]
getDlls dirs = filter ((== ".dll") . takeExtension) . concat <$>
    mapM getDirectoryContents dirs

fixLibs :: [FilePath] -> [String] -> [String]
fixLibs dlls = concatMap $ \ lib ->
    case filter (isLib lib) dlls of
                dlls@(_:_) -> [dropExtension (pickDll dlls)]
                _          -> if lib == "z" then [] else [lib]
  where
    -- If there are several .dll files matching the one we're after then we
    -- just have to guess. For example for recent Windows cairo builds we get
    -- libcairo-2.dll libcairo-gobject-2.dll libcairo-script-interpreter-2.dll
    -- Our heuristic is to pick the one with the shortest name.
    -- Yes this is a hack but the proper solution is hard: we would need to
    -- parse the .a file and see which .dll file(s) it needed to link to.
    pickDll = minimumBy (Ord.comparing length)
    isLib lib dll =
        case stripPrefix ("lib"++lib) dll of
            Just ('.':_)                -> True
            Just ('-':n:_) | isNumber n -> True
            _                           -> False

-- The following code is a big copy-and-paste job from the sources of
-- Cabal 1.8 just to be able to fix a field in the package file. Yuck.

installHook :: PackageDescription -> LocalBuildInfo
                   -> UserHooks -> InstallFlags -> IO ()
installHook pkg_descr localbuildinfo _ flags = do
  let copyFlags = defaultCopyFlags {
                      copyDistPref   = installDistPref flags,
                      copyDest       = toFlag NoCopyDest,
                      copyVerbosity  = installVerbosity flags
                  }
  install pkg_descr localbuildinfo copyFlags
  let registerFlags = defaultRegisterFlags {
                          regDistPref  = installDistPref flags,
                          regInPlace   = installInPlace flags,
                          regPackageDB = installPackageDB flags,
                          regVerbosity = installVerbosity flags
                      }
  when (hasLibs pkg_descr) $ register pkg_descr localbuildinfo registerFlags

registerHook :: PackageDescription -> LocalBuildInfo
        -> UserHooks -> RegisterFlags -> IO ()
registerHook pkg_descr localbuildinfo _ flags =
    if hasLibs pkg_descr
    then register pkg_descr localbuildinfo flags
    else setupMessage verbosity
           "Package contains no library to register:" (packageId pkg_descr)
  where verbosity = fromFlag (regVerbosity flags)

register :: PackageDescription -> LocalBuildInfo
         -> RegisterFlags -- ^Install in the user's database?; verbose
         -> IO ()
register pkg@PackageDescription { library       = Just lib  } lbi regFlags
  = do
    let clbi = LBI.getComponentLocalBuildInfo lbi
#if MIN_VERSION_Cabal(2,5,0)
                   (LBI.CLibName $ PD.libName lib)
#else
                    LBI.CLibName
#endif

    absPackageDBs       <- absolutePackageDBPaths packageDbs
    installedPkgInfoRaw <- generateRegistrationInfo
                           verbosity pkg lib lbi clbi inplace reloc distPref
                           (registrationPackageDB absPackageDBs)

    dllsInScope <- getSearchPath >>= (filterM doesDirectoryExist) >>= getDlls
    let libs = fixLibs dllsInScope (extraLibraries installedPkgInfoRaw)
        installedPkgInfo = installedPkgInfoRaw {
                                extraGHCiLibraries = libs }

    when (fromFlag (regPrintId regFlags)) $ do
      putStrLn (display (IPI.installedUnitId installedPkgInfo))

     -- Three different modes:
    case () of
     _ | modeGenerateRegFile   -> writeRegistrationFile installedPkgInfo
       | modeGenerateRegScript -> die "Generate Reg Script not supported"
       | otherwise             -> do
           setupMessage verbosity "Registering" (packageId pkg)
           registerPackage verbosity (compiler lbi) (withPrograms lbi)
#if MIN_VERSION_Cabal(2,0,0)
             packageDbs installedPkgInfo defaultRegisterOptions
#else
             False packageDbs installedPkgInfo
#endif

  where
    modeGenerateRegFile = isJust (flagToMaybe (regGenPkgConf regFlags))
    regFile             = fromMaybe (display (packageId pkg) <.> "conf")
                                    (fromFlag (regGenPkgConf regFlags))
    modeGenerateRegScript = fromFlag (regGenScript regFlags)
    inplace   = fromFlag (regInPlace regFlags)
    reloc     = relocatable lbi
    packageDbs = nub $ withPackageDB lbi
                    ++ maybeToList (flagToMaybe  (regPackageDB regFlags))
    distPref  = fromFlag (regDistPref regFlags)
    verbosity = fromFlag (regVerbosity regFlags)

    writeRegistrationFile installedPkgInfo = do
      notice verbosity ("Creating package registration file: " ++ regFile)
      writeUTF8File regFile (showInstalledPackageInfo installedPkgInfo)

register _ _ regFlags = notice verbosity "No package to register"
  where
    verbosity = fromFlag (regVerbosity regFlags)


------------------------------------------------------------------------------
-- This is a hack for Cabal-1.8, It is not needed in Cabal-1.9.1 or later
------------------------------------------------------------------------------

#if MIN_VERSION_Cabal(2,0,0)
adjustLocalBuildInfo :: LocalBuildInfo -> LocalBuildInfo
adjustLocalBuildInfo = id
#else
adjustLocalBuildInfo :: LocalBuildInfo -> LocalBuildInfo
adjustLocalBuildInfo lbi =
  let extra = (Just libBi, [])
      libBi = emptyBuildInfo { includeDirs = [ autogenModulesDir lbi
                                             , buildDir lbi ] }
   in lbi { localPkgDescr = updatePackageDescription extra (localPkgDescr lbi) }
#endif

------------------------------------------------------------------------------
-- Processing .chs files with our local c2hs.
------------------------------------------------------------------------------

#if MIN_VERSION_Cabal(2,0,0)
ourC2hs :: BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
ourC2hs bi lbi _ = PreProcessor {
#else
ourC2hs :: BuildInfo -> LocalBuildInfo -> PreProcessor
ourC2hs bi lbi = PreProcessor {
#endif
  platformIndependent = False,
  runPreProcessor = runC2HS bi lbi
}

runC2HS :: BuildInfo -> LocalBuildInfo ->
           (FilePath, FilePath) -> (FilePath, FilePath) -> Verbosity -> IO ()
runC2HS bi lbi (inDir, inFile)  (outDir, outFile) verbosity = do
  -- have the header file name if we don't have the precompiled header yet
  header <- case lookup "x-c2hs-header" (customFieldsBI bi) of
    Just h -> return h
    Nothing -> die ("Need x-c2hs-Header definition in the .cabal Library section "++
                    "that sets the C header file to process .chs.pp files.")

  -- c2hs will output files in out dir, removing any leading path of the input file.
  -- Thus, append the dir of the input file to the output dir.
  let (outFileDir, newOutFile) = splitFileName outFile
  let newOutDir = outDir </> outFileDir
  -- additional .chi files might be needed that other packages have installed;
  -- we assume that these are installed in the same place as .hi files
  let chiDirs = [ dir |
                  ipi <- maybe [] (map fst . componentPackageDeps) (libraryConfig lbi),
                  dir <- maybe [] importDirs (lookupUnitId (installedPkgs lbi) ipi) ]
  (gccProg, _) <- requireProgram verbosity gccProgram (withPrograms lbi)
  unsafeResetRootNameSupply
  c2hsMain $
       map ("--include=" ++) (outDir:chiDirs)
    ++ [ "--cpp=" ++ programPath gccProg, "--cppopts=-E" ]
    ++ ["--cppopts=" ++ opt | opt <- getCppOptions bi lbi]
    ++ ["--output-dir=" ++ newOutDir,
        "--output=" ++ newOutFile,
        "--precomp=" ++ buildDir lbi </> precompFile,
        header, inDir </> inFile]
  return ()

getCppOptions :: BuildInfo -> LocalBuildInfo -> [String]
getCppOptions bi lbi
    = nub $
      ["-I" ++ dir | dir <- PD.includeDirs bi]
   ++ [opt | opt@('-':c:_) <- PD.cppOptions bi ++ PD.ccOptions bi, c `elem` "DIU"]

installCHI :: PackageDescription -- ^information from the .cabal file
        -> LocalBuildInfo -- ^information from the configure step
        -> Verbosity -> CopyDest -- ^flags sent to copy or install
        -> IO ()
installCHI pkg@PD.PackageDescription { library = Just lib } lbi verbosity copydest = do
  let InstallDirs { libdir = libPref } = absoluteInstallDirs pkg lbi copydest
  -- cannot use the recommended 'findModuleFiles' since it fails if there exists
  -- a modules that does not have a .chi file
  mFiles <- mapM (findFileWithExtension' ["chi"] [buildDir lbi] . toFilePath)
                   (PD.explicitLibModules lib)

  let files = [ f | Just f <- mFiles ]
  installOrdinaryFiles verbosity libPref files


installCHI _ _ _ _ = return ()

------------------------------------------------------------------------------
-- Generating the type hierarchy and signal callback .hs files.
------------------------------------------------------------------------------

genSynthezisedFiles :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
genSynthezisedFiles verb pd lbi = do
  cPkgs <- getPkgConfigPackages verb lbi pd

  let xList = maybe [] (customFieldsBI . libBuildInfo) (library pd)
              ++customFieldsPD pd
      typeOpts :: String -> [ProgArg]
      typeOpts tag = concat [ map (\val -> '-':'-':drop (length tag) field ++ '=':val) (words content)
                            | (field,content) <- xList,
                              tag `isPrefixOf` field,
                              field /= (tag++"file")]
              ++ [ "--tag=" ++ tag
#if MIN_VERSION_Cabal(2,0,0)
                 | PackageIdentifier name version <- cPkgs
                 , let major:minor:_ = versionNumbers version
#else
                 | PackageIdentifier name (Version (major:minor:_) _) <- cPkgs
#endif
                 , let name' = filter isAlpha (display name)
                 , tag <- name'
                        :[ name' ++ "-" ++ show maj ++ "." ++ show d2
                          | (maj, d2) <- [(maj,   d2) | maj <- [0..(major-1)], d2 <- [0,2..20]]
                                      ++ [(major, d2) | d2 <- [0,2..minor]] ]
                 ]

      signalsOpts :: [ProgArg]
      signalsOpts = concat [ map (\val -> '-':'-':drop 10 field++'=':val) (words content)
                        | (field,content) <- xList,
                          "x-signals-" `isPrefixOf` field,
                          field /= "x-signals-file"]

      genFile :: ([String] -> IO String) -> [ProgArg] -> FilePath -> IO ()
      genFile prog args outFile = do
         res <- prog args
         rewriteFile outFile res

  forM_ (filter (\(tag,_) -> "x-types-" `isPrefixOf` tag && "file" `isSuffixOf` tag) xList) $
    \(fileTag, f) -> do
      let tag = reverse (drop 4 (reverse fileTag))
      info verb ("Ensuring that class hierarchy in "++f++" is up-to-date.")
      genFile typeGen (typeOpts tag) f

  case lookup "x-signals-file" xList of
    Nothing -> return ()
    Just f -> do
      info verb ("Ensuring that callback hooks in "++f++" are up-to-date.")
      genFile hookGen signalsOpts f

  writeFile "gtk2hs_macros.h" $ generateMacros cPkgs

-- Based on Cabal/Distribution/Simple/Build/Macros.hs
generateMacros :: [PackageId] -> String
generateMacros cPkgs = concat $
  "/* DO NOT EDIT: This file is automatically generated by Gtk2HsSetup.hs */\n\n" :
  [ concat
    ["/* package ",display pkgid," */\n"
    ,"#define VERSION_",pkgname," ",show (display version),"\n"
    ,"#define MIN_VERSION_",pkgname,"(major1,major2,minor) (\\\n"
    ,"  (major1) <  ",major1," || \\\n"
    ,"  (major1) == ",major1," && (major2) <  ",major2," || \\\n"
    ,"  (major1) == ",major1," && (major2) == ",major2," && (minor) <= ",minor,")"
    ,"\n\n"
    ]
  | pkgid@(PackageIdentifier name version) <- cPkgs
  , let (major1:major2:minor:_) = map show (versionNumbers version ++ repeat 0)
        pkgname = map fixchar (display name)
  ]
  where fixchar '-' = '_'
        fixchar '.' = '_'
        fixchar c   = c

--FIXME: Cabal should tell us the selected pkg-config package versions in the
--       LocalBuildInfo or equivalent.
--       In the mean time, ask pkg-config again.

getPkgConfigPackages :: Verbosity -> LocalBuildInfo -> PackageDescription -> IO [PackageId]
getPkgConfigPackages verbosity lbi pkg =
  sequence
    [ do version <- pkgconfig ["--modversion", display pkgname]
         case simpleParse version of
           Nothing -> die "parsing output of pkg-config --modversion failed"
#if MIN_VERSION_Cabal(2,0,0)
           Just v  -> return (PackageIdentifier (mkPackageName $ unPkgconfigName pkgname) v)
    | PkgconfigDependency pkgname _
#else
           Just v  -> return (PackageIdentifier pkgname v)
    | Dependency pkgname _
#endif
    <- concatMap pkgconfigDepends (allBuildInfo pkg) ]
  where
    pkgconfig = getDbProgramOutput verbosity
                  pkgConfigProgram (withPrograms lbi)

------------------------------------------------------------------------------
-- Dependency calculation amongst .chs files.
------------------------------------------------------------------------------

-- Given all files of the package, find those that end in .chs and extract the
-- .chs files they depend upon. Then return the PackageDescription with these
-- files rearranged so that they are built in a sequence that files that are
-- needed by other files are built first.
fixDeps :: PackageDescription -> IO PackageDescription
fixDeps pd@PD.PackageDescription {
          PD.library = Just lib@PD.Library {
            PD.exposedModules = expMods,
            PD.libBuildInfo = bi@PD.BuildInfo {
              PD.hsSourceDirs = srcDirs,
              PD.otherModules = othMods
            }}} = do
  let findModule m = findFileWithExtension [".chs.pp",".chs"] srcDirs
                       (joinPath (components m))
  mExpFiles <- mapM findModule expMods
  mOthFiles <- mapM findModule othMods

  -- tag all exposed files with True so we throw an error if we need to build
  -- an exposed module before an internal modules (we cannot express this)
  let modDeps = zipWith (ModDep True []) expMods mExpFiles++
                zipWith (ModDep False []) othMods mOthFiles
  modDeps <- mapM extractDeps modDeps
  let (othMods, expMods) = span (not . mdExposed) $ reverse $ sortTopological modDeps
  return pd { PD.library = Just lib {
    PD.exposedModules = map mdOriginal (reverse expMods),
    PD.libBuildInfo = bi { PD.otherModules = map mdOriginal (reverse othMods) }
  }}

data ModDep = ModDep {
  mdExposed :: Bool,
  mdRequires :: [ModuleName],
  mdOriginal :: ModuleName,
  mdLocation :: Maybe FilePath
}

instance Show ModDep where
  show x = show (mdLocation x)

instance Eq ModDep where
  ModDep { mdOriginal = m1 } == ModDep { mdOriginal = m2 } = m1==m2
instance Ord ModDep where
  compare ModDep { mdOriginal = m1 } ModDep { mdOriginal = m2 } = compare m1 m2

-- Extract the dependencies of this file. This is intentionally rather naive as it
-- ignores CPP conditionals. We just require everything which means that the
-- existance of a .chs module may not depend on some CPP condition.
extractDeps :: ModDep -> IO ModDep
extractDeps md@ModDep { mdLocation = Nothing } = return md
extractDeps md@ModDep { mdLocation = Just f } = withUTF8FileContents f $ \con -> do
  let findImports acc (('{':'#':xs):xxs) = case (dropWhile (' ' ==) xs) of
        ('i':'m':'p':'o':'r':'t':' ':ys) ->
          case simpleParse (takeWhile ('#' /=) ys) of
            Just m -> findImports (m:acc) xxs
            Nothing -> die ("cannot parse chs import in "++f++":\n"++
                            "offending line is {#"++xs)
         -- no more imports after the first non-import hook
        _ -> return acc
      findImports acc (_:xxs) = findImports acc xxs
      findImports acc [] = return acc
  mods <- findImports [] (lines con)
  return md { mdRequires = mods }

-- Find a total order of the set of modules that are partially sorted by their
-- dependencies on each other. The function returns the sorted list of modules
-- together with a list of modules that are required but not supplied by this
-- in the input set of modules.
sortTopological :: [ModDep] -> [ModDep]
sortTopological ms = reverse $ fst $ foldl visit ([], S.empty) (map mdOriginal ms)
  where
  set = M.fromList (map (\m -> (mdOriginal m, m)) ms)
  visit (out,visited) m
    | m `S.member` visited = (out,visited)
    | otherwise = case m `M.lookup` set of
        Nothing -> (out, m `S.insert` visited)
        Just md -> (md:out', visited')
          where
            (out',visited') = foldl visit (out, m `S.insert` visited) (mdRequires md)

-- Included for backwards compatibility with older Setup.hs.
checkGtk2hsBuildtools :: [Program] -> IO ()
checkGtk2hsBuildtools programs = do
  programInfos <- mapM (\ prog -> do
                         location <- onDefaultSearchPath programFindLocation prog normal
                         return (programName prog, location)
                      ) programs
  let printError name = do
        putStrLn $ "Cannot find " ++ name ++ "\n"
                 ++ "Please install `gtk2hs-buildtools` first and check that the install directory is in your PATH (e.g. HOME/.cabal/bin)."
        exitFailure
  forM_ programInfos $ \ (name, location) ->
    when (isNothing location) (printError name)

-- Included for backwards compatibility with older Setup.hs.
typeGenProgram :: Program
typeGenProgram = simpleProgram "gtk2hsTypeGen"

-- Included for backwards compatibility with older Setup.hs.
signalGenProgram :: Program
signalGenProgram = simpleProgram "gtk2hsHookGenerator"

-- Included for backwards compatibility with older Setup.hs.
-- We are not going to use this, so reporting the version we will use
c2hsLocal :: Program
c2hsLocal = (simpleProgram "gtk2hsC2hs") {
    programFindVersion = \_ _ -> return . Just $
#if MIN_VERSION_Cabal(2,0,0)
      mkVersion [0,13,13]
#else
      Version [0,13,13] []
#endif
  }
