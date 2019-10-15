{ pkgs ? import nixpkgs (import (builtins.fetchTarball {
      url = "https://github.com/input-output-hk/haskell.nix/archive/3bdc2a068498baac48f3be296a611f750033d551.tar.gz";
      sha256 = "0jjis8y1p3wj45v72alhcnrcxg1f2p6r6bi0383qwxlmngf3cizz";
    }))
, nixpkgs ? builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/f91f506680077924e5a1f0c861d10e946a5c7f8d.tar.gz";
    sha256 = "0bmx8ig3r8r5qskyyn7bzjfvz38k19yvhjfz3lkcglpqlcbmah4p";
  }
, haskellCompiler ? "ghc865"
}:
  pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    ghc = pkgs.buildPackages.pkgs.haskell.compiler.${haskellCompiler};
  }
