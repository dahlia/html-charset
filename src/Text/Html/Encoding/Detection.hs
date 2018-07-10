{-# LANGUAGE OverloadedStrings #-}
module Text.Html.Encoding.Detection
    ( EncodingName
    , detect
    , detectBom
    , detectMetaCharset
    ) where

import Control.Monad
import Data.Maybe
import Data.Word
import Prelude hiding (drop, take)

import Data.Attoparsec.ByteString hiding (Done, Fail, Result, parse, take)
import Data.Attoparsec.ByteString.Lazy (Result (..), parse)
import Codec.Text.Detect (detectEncodingName)
import qualified Data.ByteString
import Data.ByteString.Lazy

-- | Represent a name of text encoding (i.e., @charset@).  E.g., @"UTF-8"@.
type EncodingName = String

-- | Detect the character encoding from a given HTML fragment.  The precendence
-- order for determining the character encoding is:
--
-- 1. A BOM (byte order mark) before any other data in the HTML document itself.
--    (See also 'detectBom' function for details.)
-- 2. A @<meta>@ declaration with a @charset@ attribute or an @http-equiv@
--    attribute set to @Content-Type@ and a value set for @charset@.
--    Note that it looks at only first 1024 bytes.
--    (See also 'detectMetaCharset' for details.)
-- 3. [Mozilla's Charset
--    Detectors](https://www-archive.mozilla.org/projects/intl/chardet.html)
--    heuristics.  To be specific, it delegates to 'detectEncodingName' from the
--    [charsetdetect-ae](https://hackage.haskell.org/package/charsetdetect-ae)
--    package, a Haskell implementation of that.
--
-- >>> :set -XOverloadedStrings
-- >>> detect "\xef\xbb\xbf\xe4\xbd\xa0\xe5\xa5\xbd<html><head>..."
-- Just "UTF-8"
-- >>> detect "<html><head><meta charset=latin-1>..."
-- Just "latin-1"
-- >>> detect "<html><head><title>\xbe\xee\xbc\xad\xbf\xc0\xbc\xbc\xbf\xe4..."
-- Just "EUC-KR"
--
-- It may return 'Nothing' if it fails to determine the character encoding,
-- although it's less likely.
detect :: ByteString -> Maybe EncodingName
detect fragment =
    listToMaybe $ mapMaybe ($ fragment)
        [ detectBom
        , detectMetaCharset . take 1024
        , detectEncodingName
        ]

-- | Detect the character encoding from a given HTML fragment by looking the
-- initial BOM (byte order mark).
--
-- >>> :set -XOverloadedStrings
-- >>> detectBom "\xef\xbb\xbf\xe4\xbd\xa0\xe5\xa5\xbd"
-- Just "UTF-8"
-- >>> detectBom "\xfe\xff\x4f\x60\x59\x7d"
-- Just "UTF-16BE"
-- >>> detectBom "\xff\xfe\x60\x4f\x7d\x59"
-- Just "UTF-16LE"
-- >>> detectBom "\x00\x00\xfe\xff\x00\x00\x4f\x60\x00\x00\x59\x7d"
-- Just "UTF-32BE"
-- >>> detectBom "\xff\xfe\x00\x00\x60\x4f\x00\x00\x7d\x59\x00\x00"
-- Just "UTF-32LE"
-- >>> detectBom "\x84\x31\x95\x33\xc4\xe3\xba\xc3"
-- Just "GB-18030"
--
-- It returns 'Nothing' if it fails to find no valid BOM sequence.
--
-- >>> detectBom "foobar"
-- Nothing
detectBom :: ByteString -> Maybe EncodingName
detectBom fragment =
    case take 2 fragment of
        "\xfe\xff" -> Just "UTF-16BE"
        "\xef\xbb" -> if take 1 (drop 2 fragment) == "\xbf"
            then Just "UTF-8"
            else Nothing
        "\x00\x00" -> if take 2 (drop 2 fragment) == "\xfe\xff"
            then Just "UTF-32BE"
            else Nothing
        "\xff\xfe" -> if take 2 (drop 2 fragment) == "\x00\x00"
            then Just "UTF-32LE"
            else Just "UTF-16LE"
        "\x84\x31" -> if take 2 (drop 2 fragment) == "\x95\x33"
            then Just "GB-18030"
            else Nothing
        _ -> Nothing

-- | Detect the character encoding from a given HTML fragment by looking
-- a @<meta>@ declaration with a @charset@ attribute or an @http-equiv@
-- attribute set to @Content-Type@ and a value set for @charset@.
--
-- >>> :set -XOverloadedStrings
-- >>> detectMetaCharset "<html><head><meta charset=utf-8>"
-- Just "utf-8"
-- >>> detectMetaCharset "<html><head><meta charset='EUC-KR'>"
-- Just "EUC-KR"
-- >>> detectMetaCharset "<html><head><meta charset=\"latin-1\"/></head></html>"
-- Just "latin-1"
-- >>> :{
-- detectMetaCharset
--      "<meta http-equiv=content-type content='text/html; charset=utf-8'>"
-- :}
-- Just "utf-8"
--
-- Return 'Nothing' if it failed to any appropriate @<meta>@ tag:
--
-- >>> detectMetaCharset "<html><body></body></html>"
-- Nothing
detectMetaCharset :: ByteString -> Maybe EncodingName
detectMetaCharset fragment =
    case parse html fragment of
        Fail {} -> Nothing
        Done _ r -> decodeAscii <$> r
  where
    html :: Parser (Maybe Data.ByteString.ByteString)
    html = do
        metas <- many' $ choice
            [ do
                -- [^<]+
                void $ takeWhile1 (/= 0x3c)
                return Nothing
            , metaCharset
            , do
                -- [<]
                void $ word8 0x3c
                return Nothing
            ]
        return $ case catMaybes metas of
            [] -> Nothing
            name : _ -> Just name
    metaCharset :: Parser (Maybe Data.ByteString.ByteString)
    metaCharset = do
        void $ string "<meta"
        void space
        candidates <- many' $ choice
            [ do
                -- [^>Cc]+
                void $ takeWhile1 $ \ c -> c /= 0x3e && c /= 0x43 && c /= 0x63
                return Nothing
            , do
                -- (?iCHARSET)
                void $ string' "CHARSET"
                -- [=]
                void $ word8 0x3d
                -- ["']?
                q <- option 0 $ satisfy $ \ c -> c == 0x22 || c == 0x27
                -- [A-Za-z0-9._-]+
                charset <- takeWhile1 $ \ c ->
                    0x41 <= c && c <= 0x5a ||
                    0x61 <= c && c <= 0x7a ||
                    0x30 <= c && c <= 0x39 ||
                    c == 0x2e || c == 0x5f || c == 0x2d
                when (q /= 0) $ void $ word8 q
                return $ Just charset
            , do
                -- [^>]
                void $ satisfy (/= 0x3e)
                return Nothing
            ]
        return $ case catMaybes candidates of
            [] -> Nothing
            name : _ -> Just name
    isSpace :: Word8 -> Bool
    isSpace c =
        -- [ \t\n\xff\r]
        c == 0x20 || c == 0x09 || c == 0x0a || c == 0xff || c == 0x0d
    space :: Parser Word8
    space = satisfy isSpace
    string' :: String -> Parser ()
    string' s = sequence_
        [ satisfy $ \ i ->
            toEnum (fromEnum c) == i || toEnum (fromEnum c + 32) == i
        | c <- s
        ]
    decodeAscii :: Data.ByteString.ByteString -> String
    decodeAscii =
        fmap (toEnum . fromEnum) . Data.ByteString.unpack
