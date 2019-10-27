{ pkgs ? import nixpkgs (haskellNixpkgsArgs // {
    overlays = haskellNixpkgsArgs.overlays ++ [ (import ./nix/overlays/gtk-debug.nix) ];
  } // (if system == null then {} else { inherit system; }))
, nixpkgs ? builtins.fetchTarball {
    url = "https://github.com/input-output-hk/nixpkgs/archive/d08a74315610096187a4e9da5998ef10e80de370.tar.gz";
    sha256 = "16i3n5p4h86aswj7y7accmkkgrrkc0xvgy7fl7d3bsv955rc5900";
  }
, haskellNixpkgsArgs ? import (builtins.fetchTarball {
    url = "https://github.com/input-output-hk/haskell.nix/archive/4a24da99c1c9783414372bdd0686f79f4e60bfe9.tar.gz";
    sha256 = "10w8mixdkqd194i68h22y11nl46imnc26f8xgxra33slr040paxb";
  })
, haskellCompiler ? "ghc865"
, system ? null
}:
  pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    ghc = pkgs.buildPackages.pkgs.haskell.compiler.${haskellCompiler};
  }
