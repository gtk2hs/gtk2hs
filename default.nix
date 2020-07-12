{ sourcesOverride ? {}
, sources ? import ./nix/sources.nix {} // sourcesOverride
, nixpkgs ? (import sources."haskell.nix" {}).sources.nixpkgs
, haskellNixpkgsArgs ? (import sources."haskell.nix" {}).nixpkgsArgs
, pkgs ? import nixpkgs (haskellNixpkgsArgs // {
    overlays = haskellNixpkgsArgs.overlays ++ [ (import ./nix/overlays/gtk-debug.nix) ];
  } // (if system == null then {} else { inherit system; }))
, compiler-nix-name ? "ghc8101"
, system ? null
}:
  pkgs.haskell-nix.project {
    inherit compiler-nix-name;
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "gtk2hs";
      src = ./.;
    };
  }
