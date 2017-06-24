let
  nixpkgs = import <nixpkgs> {};
in
rec {
  hello = import ./glow.nix {
    llvm = nixpkgs.llvm;
    ghc = nixpkgs.haskell.compiler.ghc7102;
    gcc = nixpkgs.gcc;
    cabal = nixpkgs.cabal-install;
    libpath = ./lib;
    inherit nixpkgs;
  };
}
