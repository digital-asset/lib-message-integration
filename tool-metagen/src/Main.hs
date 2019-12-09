-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generate DAML types and metadata for XML XSD schema.
module Main where

import           Control.Applicative        ((<$>))
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified DA.Daml.GenDaml            as GenDaml (ppModule)
import           DA.Daml.TypeModel
import qualified DA.XML.Schema.Convert      as XSD
import qualified DA.XML.Schema.Metadata     as XSD
import           DA.XML.Schema.Parse        (parseXsd)
import qualified DA.XML.Schema.Schema       as XSD
import qualified DA.CDM.Rosetta.Convert     as Rosetta
import qualified DA.CDM.Rosetta.Metadata    as Rosetta
import qualified DA.CDM.Rosetta.Schema      as Rosetta
import           DA.CDM.Rosetta.Parse       (parseRosetta)
import           DA.Swagger.Parse           (parseSwagger)
import           Data.Swagger               (Swagger)
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.ByteString.Lazy       as LB
import           Data.Foldable
import qualified Data.List                  as List
import           Data.List.Extra            (splitOn)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import           Data.Version               (showVersion)
import           Options.Applicative
import           Options.Applicative.Types  (readerAsk, readerError)
import qualified System.Directory           as Dir
import           System.Exit                (exitFailure)
import           System.FilePath
import           Text.PrettyPrint.HughesPJ  (Doc, render)

-- cabal generated
import Paths_metagen (version)

------------------------------------------------------------

data Options = Options
  { optCommand     :: Command
  , optOutputDir   :: FilePath
  , optDamlPackage :: String
  , optJsonPackage :: String -- use to set dir relative to output dir
  , optLogLevel    :: LogLevel
  }

data Command
  = UseXSD { inputXsdFile     :: FilePath
           , outputModuleName :: String
           }
  | UseCDM { inputDir :: FilePath
           }
  | UseSwagger { inputSwaggerFile :: FilePath
               }

optParser :: Parser Options
optParser = Options
    <$> subparser
        (  command "xsd" (info xsdOptionsP (progDesc "Use an XML XSD schema file."))
        <> command "cdm" (info cdmOptionsP (progDesc "Use a folder of CDM Rosetta files."))
        <> command "swagger" (info swaggerOptionsP (progDesc "Use a Swagger JSON schema file."))
        )
    <*> strOption
        ( long "output-dir"
        <> short 'o'
        <> metavar "OUTPUT-DIR"
        <> help "output directory for generated artefacts")
    <*> strOption
        ( long "daml-package"
        <> short 'p'
        <> metavar "DAML-PACKAGE"
        <> help "package name for DAML output target")
    <*> strOption
        ( long "json-package"
        <> short 'j'
        <> metavar "JSON-PACKAGE"
        <> help "package name for Json output target" )
    <*> option readLogLevel
            (long "log-level"
             <> value LevelInfo
             <> metavar "debug|info|warn|error (default: info)"
             <> help "log level")
  where
    readLogLevel :: ReadM LogLevel
    readLogLevel = do
      s <- readerAsk
      case s of
        "debug" -> return LevelDebug
        "info"  -> return LevelInfo
        "warn"  -> return LevelWarn
        "error" -> return LevelError
        _       -> readerError "log-level must be one of debug|info|warn|error"


xsdOptionsP :: Parser Command
xsdOptionsP = UseXSD
    <$> strOption
        ( long "xsd-schema"
          <> short 's'
          <> metavar "XSD-SCHEMA"
          <> help "input XML XSD schema file"
        )
    <*> strOption
        ( long "module-name"
        <> short 'n'
        <> metavar "MODULE-NAME"
        <> help "Module name for DAML and Json targets")

cdmOptionsP :: Parser Command
cdmOptionsP = UseCDM <$> strOption
    ( long "input-dir"
      <> short 'd'
      <> metavar "INPUT-DIR"
      <> help "input directory"
    )

swaggerOptionsP :: Parser Command
swaggerOptionsP = UseSwagger
  <$> strOption
    ( long "swagger-schema"
      <> short 's'
      <> metavar "SWAGGER-SCHEMA"
      <> help "input Swagger schema file"
    )

argParser :: ParserInfo Options
argParser = info (optParser <**> helper)
    (   fullDesc
    <>  header (unwords ["metagen", showVersion version])
    <>  progDesc "Convert schemas to DAML types and encoder/decoder metadata" )

main :: IO ()
main = do
    opts <- execParser argParser
    runLog (optLogLevel opts) $ runProgram opts

runProgram :: Options -> LoggingT IO ()
runProgram opts = do
    curDir <- liftIO Dir.getCurrentDirectory
    logDebugN $ "Current working directory: " <> T.pack curDir
    case optCommand opts of
        UseCDM dir          -> runRosetta opts dir
        UseXSD file modName -> runXSD opts file modName
        UseSwagger file     -> runSwagger opts file

runLog :: MonadIO m => LogLevel -> LoggingT m a -> m a
runLog level m =
    runStdoutLoggingT $
        filterLogger (\_ ll -> ll >= level) m

packageToPath :: String -> FilePath
packageToPath pkg = joinPath $ splitOn "." pkg

writeJson
    :: (MonadIO m, MonadLogger m, Aeson.ToJSON a)
    => FilePath
    -> a
    -> m ()
writeJson file obj = do
    liftIO $ do
        Dir.createDirectoryIfMissing True (takeDirectory file)
        LB.writeFile file $ Aeson.encodePretty obj
    logInfoN $ "Wrote '" <> T.pack file <> "'."

writeDoc
    :: (MonadIO m, MonadLogger m)
    => FilePath
    -> Doc
    -> m ()
writeDoc file doc = do
    liftIO $ do
        Dir.createDirectoryIfMissing True (takeDirectory file)
        writeFile file $ render doc
    logInfoN $ "Wrote '" <> T.pack file <> "'."

writeDaml
    :: (MonadIO m, MonadLogger m, Show a)
    => FilePath
    -> Module a
    -> m ()
writeDaml file modu =
    writeDoc file $ GenDaml.ppModule modu

------------------------------------------------------------
-- CDM Rosetta support

runRosetta :: Options -> FilePath -> LoggingT IO ()
runRosetta Options{..} baseDir = do

    let damlOutDir = optOutputDir </> "daml" </> packageToPath optDamlPackage
        jsonOutDir = optOutputDir </> "metadata" </> packageToPath optJsonPackage

    -- find all rosetta files
    files   <- filter isRosetta <$> liftIO (Dir.listDirectory baseDir)

    schemas <- forM (files List.\\ blacklist) $ readRosetta baseDir

    let schemaEnums = Rosetta.Schema
            { schemaNamespace = head $ map Rosetta.schemaNamespace schemas -- TODO
            , schemaDecls     = [ e | e@Rosetta.EnumDecl{} <- concatMap Rosetta.schemaDecls schemas ]
            }

    let schemaClasses = Rosetta.Schema
            { schemaNamespace = head $ map Rosetta.schemaNamespace schemas -- TODO
            , schemaDecls     = [ c | c@Rosetta.ClassDecl{} <- concatMap Rosetta.schemaDecls schemas ]
            }

    let env = Rosetta.mkEnv schemaEnums <>
              Rosetta.mkEnv schemaClasses

    let writeDaml' name imports env schema = do
            modu <- runReaderT (Rosetta.convert (optDamlPackage ++ "." ++ name) schema) env
            writeDaml (damlOutDir </> name ++ ".daml") $
                modu { module_imports = imports }


    -- For DAML, split into two files (as we have always done so far)
    writeDaml' "Enums" [] env schemaEnums
    writeDaml' "Classes" [Unqualified $ optDamlPackage++".Enums"] env schemaClasses

    -- For Json metadata, just emit one file.
    let schema = Rosetta.Schema
                     { schemaNamespace = head $ map Rosetta.schemaNamespace schemas -- TODO
                     , schemaDecls = concatMap Rosetta.schemaDecls schemas
                     }

    logDebugN $ "Schema types count: " <> T.pack (show . length $ Rosetta.schemaDecls schema)

    modu <- runReaderT (Rosetta.convert "CDM" schema) env
    writeJson (jsonOutDir </> "CDM.json")
        . Rosetta.genMetadata
        $ modu

  where
    isRosetta f = takeExtension f == ".rosetta"

    blacklist = [ "model-cdm-config.rosetta"
                , "model-cdm-calculations.rosetta"
                ]


readRosetta :: (MonadLogger m, MonadIO m) => FilePath -> FilePath -> m Rosetta.Schema
readRosetta baseDir file = do
    str <- liftIO $ readFile $ baseDir </> file
    case parseRosetta file str of
        Right s  -> do
            logDebugN $ T.pack file <> " : successfully parsed"
            return s
        Left  err -> do
            logErrorN $ T.pack file <> " : FAILED TO PARSE: " <> T.pack (show err)
            liftIO exitFailure


------------------------------------------------------------
-- XSD support

runXSD :: Options -> FilePath -> String -> LoggingT IO ()
runXSD Options{..} inputFile modName = do
    let baseDir    = takeDirectory inputFile
        inFile     = takeFileName  inputFile

        damlOutDir = optOutputDir </> "daml" </> packageToPath optDamlPackage
        jsonOutDir = optOutputDir </> "metadata" </> packageToPath optJsonPackage

    schemas <- chaseXsdImports baseDir inFile
    let (Just mainSchema) = Map.lookup inFile schemas

    -- produce a global environment from all dependencies
    let env = mconcat $ map XSD.mkEnv $ Map.elems schemas

    -- produce a list of decls for each file, using the
    -- global enviroment
    decls <- fmap fold $ forM (Map.elems schemas) $ \ s -> do
        modu <- runReaderT (XSD.convert inFile s) env
        let f d | Just t <- getTypeName d = [(t, d)]
                | otherwise = []
        return $ Map.fromList $ concatMap f (module_decls modu)

    logDebugN $ "Schema types count: " <> T.pack (show $ Map.size decls)

    let damlModuleName = optDamlPackage ++ "." ++ modName

    let modu
          = Module damlModuleName [] (Map.elems decls)
                (Comment $ Just $ "Generated by metagen from: " ++ inFile)

    let damlOutFile    = damlOutDir </> modName ++ ".daml"
    writeDaml damlOutFile modu

    let jsonOutFile    = jsonOutDir </> modName ++ ".json"
    writeJson jsonOutFile
        . XSD.genMetadata (XSD.mkXmlTopLevelMeta env mainSchema)
        $ modu

chaseXsdImports
    :: (MonadLogger m, MonadIO m)
    => FilePath
    -> FilePath
    -> m (Map FilePath XSD.Schema)
chaseXsdImports baseDir = go mempty
  where
    -- XSD permits circular imports, so we must check as we recurse
    -- that we haven't already captured a dependency.
    go m file
        | file `Map.member` m = return m
        | otherwise = do
              s <- readXsd baseDir file
              let (deps,_) = unzip $ XSD.gatherImports s
              foldM go (Map.insert file s m) deps

readXsd :: (MonadLogger m, MonadIO m) => FilePath -> FilePath -> m XSD.Schema
readXsd baseDir file = do
    str <- liftIO $ readFile $ baseDir </> file
    case parseXsd file str of
        Right s  -> do
            logDebugN $ T.pack file <> " : successfully parsed"
            return s
        Left  err -> do
            logErrorN $ T.pack file <> " : FAILED TO PARSE: " <> T.pack (show err)
            liftIO exitFailure

-- | get all transitive file dependencies, non-inclusive
getDependencies :: Map FilePath XSD.Schema -> FilePath -> Set FilePath
getDependencies schemas fp = Set.delete fp $ go mempty fp
  where
    go fps fp
        | fp `Set.notMember` fps =
          let deps = maybe mempty getDeps $ Map.lookup fp schemas
          in List.foldl' go (Set.insert fp fps) deps
        | otherwise = fps
    getDeps s = Set.fromList . map fst $ XSD.gatherImports s

--------------------------------------------------------------------------------
-- Swagger Support

runSwagger :: Options -> FilePath -> LoggingT IO ()
runSwagger Options{..} inputFile = do
  let -- baseDir = takeDirectory inputFile
      -- inFile = takeFileName inputFile

      damlOutDir = optOutputDir </> "daml" </> packageToPath optDamlPackage
      damlOutFile = damlOutDir </> "replacewithschemaname" ++ ".daml"
  schema <- readSwagger inputFile
  mod <- parseSwagger schema
  writeDaml damlOutFile mod

-- Parses JSON into a data type
readSwagger :: (MonadLogger m, MonadIO m) => FilePath -> m Swagger
readSwagger file = do 
  eitherSchema <- liftIO $ Aeson.eitherDecodeFileStrict' file
  case eitherSchema of
    Right s -> do
      logDebugN $ T.pack file <> " : successfully parsed"
      return s
    Left err -> do
      logErrorN $ T.pack file <> " : FAILED TO PARSE : " <> T.pack (show err)
      liftIO exitFailure
   
