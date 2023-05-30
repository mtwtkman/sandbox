with import <nixpkgs> {};
mkShell {
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
    zlib
  ];
  shellHook = ''
    alias b="cabal build"
    alias c="cabal clean"
    alias fmt="ormolu -i ./**/*.hs"
    alias repl="cabal repl"
    alias run="cabal run --"
    alias t="cabal test"
    alias fmtc="cabal-fmt -i servant-trial.cabal"
    function gen() {
      mkdir $1
      cp template $1/Main.hs
      echo "
  executable $1
    import:           warnings
    main-is:          Main.hs
    build-depends:
      , aeson
      , base
      , http-client
      , servant
      , servant-client
      , servant-server
      , wai
      , warp

    hs-source-dirs: $1
    default-language: Haskell2010" >> servant-trial.cabal
      cabal-fmt -i ./servant-trial.cabal
    }
  '';
}
