function check_glowpath {
  if [ -z . ]; then
    echo "glowpath env variable is not set. Set it to your glow project directory."
    exit 1
  fi
}

function compile_lib_files {
  gcc -fPIC -shared $libpath/cbits.c $libpath/cbits2.c -o $libpath/lib.so
}

function create_path {
  local sl_file=$libpath/lib.so
  if ! [[ $LD_LIBRARY_PATH == *$sl_file* ]]; then
    LD_LIBRARY_PATH=LD_LIBRARY_PATH:$sl_file
  fi
}

function link_and_compile_main {
  for s_object in $libpath/*.so; do
    ghc $libpath/lib.so --make $src/Main.hs -package-db ./cabal-sandbox/x86_64-linux-ghc-7.10.3-packages.conf.d -o $out/glow
  done
}

function clear {
  echo "[.] Clearing..."
  # -f option is for not giving a warning about file not existing
  rm -f src/*.hi
  rm -f src/*.o
  rm -f lib/*.so
  rm -rf ./build/*
  echo "[.] Cleared."
}

function compile {
  # check_glowpath
  echo "[.] Compiling..."
  create_path
  compile_lib_files
  link_and_compile_main
  echo "[.] Compilation complete."
}

function run {
  check_glowpath
  EXECUTABLE=./build/Main
  if ! [[ -f $EXECUTABLE ]]; then
    echo "[.] Need to compile before running glow. Compiling..."
    compile
  fi
  echo "[.] Running glow..."
  ./build/Main
}

source $stdenv/setup
PATH=$gcc/bin:$PATH
PATH=$ghc/bin:$PATH
PATH=$llvm/bin:$PATH
PATH=$cabal/bin:$PATH
touch $out
compile
