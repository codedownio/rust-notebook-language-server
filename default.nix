{ mkDerivation, aeson, base, bytestring, containers, exceptions
, filepath, hpack, lens, lens-regex-pcre, lib, lsp-types
, monad-logger, mtl, myers-diff, network-uri, optparse-applicative
, pcre-light, process, QuickCheck, random, retry, safe, sandwich
, string-interpolate, text, text-rope, time, unix, unliftio
, unliftio-core
}:
mkDerivation {
  pname = "rust-notebook-language-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers filepath lens lens-regex-pcre lsp-types
    monad-logger mtl myers-diff network-uri pcre-light random safe
    string-interpolate text text-rope time unliftio unliftio-core
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring lens lsp-types monad-logger mtl
    optparse-applicative process retry safe string-interpolate text
    unix unliftio unliftio-core
  ];
  testHaskellDepends = [
    base exceptions lsp-types monad-logger QuickCheck sandwich
    string-interpolate text unliftio
  ];
  doCheck = false;
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
