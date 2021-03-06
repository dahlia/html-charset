html-charset: Determine character encoding of HTML bytes
========================================================

[![Hackage](https://img.shields.io/hackage/v/html-charset.svg)][html-charset]

This provides a [Haskell library][html-charset] and a CLI executable to
determine character encoding (i.e., so-called "charset") from given HTML bytes.

The precendence order for determining the character encoding is:

 1. A BOM (byte order mark) before any other data in the HTML document itself.
 2. A `<meta>` declaration with a `charset` attribute or an `http-equiv`
    attribute set to `Content-Type` and a value set for `charset`.
    Note that it looks at only first 1024 bytes.
 3. [Mozilla's Charset Detectors][chardet] heuristics.  To be specific,
    it delegates to the [charsetdetect-ae] package, a Haskell implementation
    of that.

[html-charset]: https://hackage.haskell.org/package/html-charset
[chardet]: https://www-archive.mozilla.org/projects/intl/chardet.html
[charsetdetect-ae]: https://hackage.haskell.org/package/charsetdetect-ae


API
---

The package is available on Hackage: *[html-charset]*.

~~~~ haskell
>>> import Data.ByteString.Lazy
>>> import Text.Html.Encoding.Detection
>>> detect "\xef\xbb\xbf\xe4\xbd\xa0\xe5\xa5\xbd<html><head>..."
Just "UTF-8"
>>> detect "<html><head><meta charset=latin-1>..."
Just "latin-1"
>>> detect "<html><head><title>\xbe\xee\xbc\xad\xbf\xc0\xbc\xbc\xbf\xe4..."
Just "EUC-KR"
~~~~

Note that the `detect` function takes a lazy bytestring, not strict.

Read the [API docs] for details.

[API docs]: https://hackage.haskell.org/package/html-charset/docs/Text-Html-Encoding-Detection.html


CLI
---

We currently doesn't provide any official binaries.
The CLI program can be installed using Cabal or Stack: *[html-charset]*.

~~~~
$ curl https://www.haskell.org/onlinereport/ | html-charset
ASCII
$ curl http://www.bunka.go.jp/kokugo_nihongo/sisaku/joho/joho/ | html-charset
shift_jis
~~~~

Although it's less likely, `html-charset` may fail to determine the character
encoding, and for the case it prints nothing (only a line feed, exactly).
You can customize the string to print when it fails by configuring
`-f`/`--on-failure` option.

Author and license
------------------

Witten by [Hong Minhee].  Licensed under [LGPL 2.1] or higher.

[Hong Minhee]: https://hongminhee.org/
[LGPL 2.1]: https://www.gnu.org/licenses/lgpl-2.1.html
