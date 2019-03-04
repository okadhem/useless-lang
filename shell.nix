{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, hpack, hspec, lens, parsec
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
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
