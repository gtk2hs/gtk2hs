{ compiler-nix-name = "ghc922";
  overlays = [ (import ./overlays/gtk-debug.nix) ];
  shell.tools.cabal = {};
  shell.withHoogle = false;
  # shell.tools.hoogle = {};
  modules = [{ nonReinstallablePkgs = ["rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base" "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell" "bytestring" "ghc-bignum"]; }];
}
