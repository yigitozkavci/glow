{ nixpkgs, llvm, ghc, gcc, cabal, libpath }:

let
  com = ''
    echo "Hello World!";
  '';
in

nixpkgs.stdenv.mkDerivation {
  name = "hello-derivation";
  builder = ./builder.sh;
  inherit llvm ghc gcc cabal libpath;
}
