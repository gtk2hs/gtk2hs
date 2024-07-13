{ compiler-nix-name = "ghc9101";
  overlays = [ (import ./overlays/gtk-debug.nix) ];
  shell.tools.cabal = {};
  shell.withHoogle = false;
  # shell.tools.hoogle = {};
}
