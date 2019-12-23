{ pkgs ? import nixpkgs (haskellNixpkgsArgs // {
    overlays = haskellNixpkgsArgs.overlays ++ [ (import ./nix/overlays/gtk-debug.nix) ];
  } // (if system == null then {} else { inherit system; }))
, nixpkgs ? haskellNixSrc + "/nixpkgs"
, haskellNixpkgsArgs ? import haskellNixSrc
, haskellNixSrc ? builtins.fetchTarball {
    url = "https://github.com/input-output-hk/haskell.nix/archive/1dd6951d6a56115fd368c5b37b8dab47a90ec45c.tar.gz";
    sha256 = "02nhjy90xwg9wrp81cnlczndxags966c71wlbz6n91423a13mc99";
  }
, haskellCompiler ? "ghc881"
, system ? null
}:
  pkgs.haskell-nix.cabalProject' {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    ghc = pkgs.haskell-nix.compiler.${haskellCompiler};
  }
