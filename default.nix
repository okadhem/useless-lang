{ mkDerivation, base, bound, containers, deriving-compat, hpack
, hspec, lens, parsec, parsers, QuickCheck, stdenv, tasty
, tasty-hspec
}:
mkDerivation {
  pname = "useless-lang";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bound containers deriving-compat lens parsec parsers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bound containers deriving-compat lens parsec parsers
  ];
  testHaskellDepends = [
    base bound containers deriving-compat hspec lens parsec parsers
    QuickCheck tasty tasty-hspec
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/githubuser/useless-lang#readme";
  license = stdenv.lib.licenses.bsd3;
}
