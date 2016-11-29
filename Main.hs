module Main where

import ElmLensGenerator
import Options.Applicative
import Data.Monoid

data CLIArguments = CLIArguments
    { outputPath :: String
    , moduleName :: String
    , recordString :: String
    }

sampleParser :: Parser CLIArguments
sampleParser =
    let
        outputPathParser =
            strOption
                (  long "output-path"
                <> short 'o'
                <> metavar "DIRECTORY"
                <> help "Where to place the automatically generated lens module."
                )
        moduleNameParser =
            strOption
                (  long "module-name"
                <> short 'm'
                <> metavar "MODULE-NAME"
                <> help "The name to give the module containing the lenses."
                )
        recordArgParser =
            strArgument
                (  metavar "RECORD-STRING"
                )
    in
        CLIArguments
            <$> (outputPathParser <|> pure "./")
            <*> moduleNameParser
            <*> recordArgParser

main :: IO ()
main =
    let
        options =
            info (helper <*> sampleParser)
                (  fullDesc
                <> header "Elm Monocole Lens Generator"
                <> progDesc "Generate a standalone Elm module containing a set of lens functions that encode a given Elm record."
                )
    in
        execParser options >>= main'

main' :: CLIArguments -> IO ()
main' args =
    case parseRecord (recordString args) of
        Right records -> do
            putStrLn $ createModuleHeader (moduleName args)
            mapM_ (putStrLn . createLens) records
        Left parseError ->
            print parseError
