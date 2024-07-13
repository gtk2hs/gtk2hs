{pkgs, ...}: { compiler-nix-name = "ghc9101";
  flake.variants.ghc8107.compiler-nix-name = pkgs.lib.mkForce "ghc8107";
  flake.variants.ghc966.compiler-nix-name = pkgs.lib.mkForce "ghc966";
  flake.variants.ghc982.compiler-nix-name = pkgs.lib.mkForce "ghc982";
  overlays = [ (import ./overlays/gtk-debug.nix) ];
  shell.tools.cabal = {};
  shell.tools.haskell-ci.src = pkgs.inputs.haskell-ci;
  shell.withHoogle = false;
  # shell.tools.hoogle = {};
}
