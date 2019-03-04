{ mkDerivation, base, containers, hpack, hspec, lens, parsec
, parsers, QuickCheck, stdenv, tasty, tasty-hspec
}:
mkDerivation {
  pname = "useless-lang";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers lens parsec parsers ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base containers lens parsec parsers ];
  testHaskellDepends = [
    base containers hspec lens parsec parsers QuickCheck tasty
    tasty-hspec
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/githubuser/useless-lang#readme";
  license = stdenv.lib.licenses.bsd3;
}
