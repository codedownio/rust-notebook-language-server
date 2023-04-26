{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, exceptions, filepath, ghc, ghc-parser, ghc-paths, hpack, lens
, lens-regex-pcre, lib, lsp-types, monad-logger, mtl, myers-diff
, network-uri, optparse-applicative, pcre-light, process
, QuickCheck, random, regex-base, regex-pcre-builtin, retry, safe
, sandwich, sandwich-quickcheck, string-interpolate, text
, text-rope, time, unix, unliftio, unliftio-core, vector
}:
mkDerivation {
  pname = "rust-notebook-language-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring containers filepath ghc ghc-parser
    ghc-paths lens lens-regex-pcre lsp-types monad-logger mtl
    myers-diff network-uri optparse-applicative pcre-light process
    random regex-base regex-pcre-builtin retry safe string-interpolate
    text text-rope time unix unliftio unliftio-core vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers filepath ghc ghc-parser
    ghc-paths lens lens-regex-pcre lsp-types monad-logger mtl
    myers-diff network-uri optparse-applicative pcre-light process
    random regex-base regex-pcre-builtin retry safe string-interpolate
    text text-rope time unix unliftio unliftio-core vector
  ];
  testHaskellDepends = [
    aeson attoparsec base bytestring containers exceptions filepath ghc
    ghc-parser ghc-paths lens lens-regex-pcre lsp-types monad-logger
    mtl myers-diff network-uri optparse-applicative pcre-light process
    QuickCheck random regex-base regex-pcre-builtin retry safe sandwich
    sandwich-quickcheck string-interpolate text text-rope time unix
    unliftio unliftio-core vector
  ];
  doCheck = false;
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
