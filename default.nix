{ mkDerivation, aeson, auto-update, base, bytestring, containers
, exceptions, filepath, lens, lens-regex-pcre, lib, lsp-types
, monad-logger, mtl, myers-diff, network-uri, optparse-applicative
, pcre-light, process, QuickCheck, quickcheck-instances, random
, retry, row-types, safe, sandwich, sandwich-quickcheck
, string-interpolate, text, text-rope, time, unix, unliftio
, unliftio-core, uuid
}:
mkDerivation {
  pname = "rust-notebook-language-server";
  version = "0.2.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson auto-update base containers filepath lens lens-regex-pcre
    lsp-types monad-logger mtl myers-diff network-uri pcre-light random
    row-types safe string-interpolate text text-rope time unliftio
    unliftio-core uuid
  ];
  executableHaskellDepends = [
    aeson base bytestring lens lsp-types monad-logger mtl
    optparse-applicative process retry safe string-interpolate text
    unix unliftio unliftio-core
  ];
  testHaskellDepends = [
    base exceptions lsp-types monad-logger myers-diff QuickCheck
    quickcheck-instances row-types sandwich sandwich-quickcheck
    string-interpolate text text-rope unliftio
  ];
  doCheck = false;
  license = "unknown";
  mainProgram = "rust-notebook-language-server";
}
