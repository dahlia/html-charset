module Main where

import Data.Semigroup ((<>))

import qualified Data.ByteString.Lazy
import Options.Applicative

import Text.Html.Encoding.Detection

newtype Detector = Detector
    { onFailure :: String
    }

runDetector :: Detector -> IO ()
runDetector Detector { onFailure = onFailure' } = do
    html <- Data.ByteString.Lazy.getContents
    case detect html of
        Just encoding -> putStrLn encoding
        Nothing -> putStrLn onFailure'

detector :: Parser Detector
detector = Detector
    <$> strOption
        ( long "on-failure"
        <> short 'f'
        <> help "String to print when it fails to detect the character encoding"
        <> showDefault
        <> value ""
        )

detectorOpts :: ParserInfo Detector
detectorOpts = info (detector <**> helper)
    ( fullDesc
    <> progDesc "Detect character encoding of HTML documents/fragments"
    )

main :: IO ()
main =
    runDetector =<< execParser detectorOpts
