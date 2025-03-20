{pkgs, ...}: { compiler-nix-name = "ghc912";
  flake.variants.ghc810.compiler-nix-name = pkgs.lib.mkForce "ghc810";
  flake.variants.ghc96.compiler-nix-name = pkgs.lib.mkForce "ghc96";
  flake.variants.ghc98.compiler-nix-name = pkgs.lib.mkForce "ghc98";
  flake.variants.ghc910.compiler-nix-name = pkgs.lib.mkForce "ghc910";
  overlays = [ (import ./overlays/gtk-debug.nix) ];
  shell.tools.cabal = {};
  shell.tools.haskell-ci = { compiler-nix-name = "ghc910"; src = pkgs.inputs.haskell-ci; };
  shell.withHoogle = false;
  # shell.tools.hoogle = {};
}
