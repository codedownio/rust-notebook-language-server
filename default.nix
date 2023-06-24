{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, exceptions, filepath, hpack, lens, lens-regex-pcre, lib
, lsp-types, monad-logger, mtl, myers-diff, network-uri
, optparse-applicative, pcre-light, process, QuickCheck, random
, regex-base, regex-pcre-builtin, retry, safe, sandwich
, sandwich-quickcheck, string-interpolate, text, text-rope, time
, unix, unliftio, unliftio-core
}:
mkDerivation {
  pname = "rust-notebook-language-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring containers filepath lens
    lens-regex-pcre lsp-types monad-logger mtl myers-diff network-uri
    optparse-applicative pcre-light process random regex-base
    regex-pcre-builtin retry safe string-interpolate text text-rope
    time unix unliftio unliftio-core
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers filepath lens
    lens-regex-pcre lsp-types monad-logger mtl myers-diff network-uri
    optparse-applicative pcre-light process random regex-base
    regex-pcre-builtin retry safe string-interpolate text text-rope
    time unix unliftio unliftio-core
  ];
  testHaskellDepends = [
    aeson attoparsec base bytestring containers exceptions filepath
    lens lens-regex-pcre lsp-types monad-logger mtl myers-diff
    network-uri optparse-applicative pcre-light process QuickCheck
    random regex-base regex-pcre-builtin retry safe sandwich
    sandwich-quickcheck string-interpolate text text-rope time unix
    unliftio unliftio-core
  ];
  doCheck = false;
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
