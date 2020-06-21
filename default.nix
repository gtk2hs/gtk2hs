{ sourcesOverride ? {}
, sources ? import ./nix/sources.nix {} // sourcesOverride
, nixpkgs ? (import sources."haskell.nix" {}).sources.nixpkgs-default
, haskellNixpkgsArgs ? (import sources."haskell.nix" { defaultCompilerNixName = haskellCompiler; }).nixpkgsArgs
, pkgs ? import nixpkgs (haskellNixpkgsArgs // {
    overlays = haskellNixpkgsArgs.overlays ++ [ (import ./nix/overlays/gtk-debug.nix) ];
  } // (if system == null then {} else { inherit system; }))
, haskellCompiler ? "ghc883"
, system ? null
}:
  pkgs.haskell-nix.project' {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "gtk2hs";
      src = ./.;
    };
    modules = [{reinstallableLibGhc = true;}];
  }

