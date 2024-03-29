{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell rec {
  packages = [
    ghc
    cabal-install
    haskell-language-server
    haskellPackages.ormolu
    haskellPackages.cabal2nix
    haskellPackages.haskell-dap
    haskellPackages.haskell-debug-adapter
    haskellPackages.ghci-dap
    haskellPackages.cabal-fmt
  ];
  buildInputs = [
    zlib
  ];
  LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
  shellHook = ''
    alias b="cabal build"
    alias c="cabal clean"
    alias fmt="ormolu -i ./**/*.hs"
    alias repl="cabal repl"
    alias run="cabal run --"
    alias t="cabal test"
  '';
}
