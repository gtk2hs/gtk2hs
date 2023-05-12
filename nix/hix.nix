{ compiler-nix-name = "ghc961";
  overlays = [ (import ./overlays/gtk-debug.nix) ];
  shell.tools.cabal = {};
  shell.withHoogle = false;
  # shell.tools.hoogle = {};
}
