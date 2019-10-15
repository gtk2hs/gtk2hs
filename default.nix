{ pkgs ? import nixpkgs (import (builtins.fetchTarball {
      url = "https://github.com/input-output-hk/haskell.nix/archive/3bdc2a068498baac48f3be296a611f750033d551.tar.gz";
      sha256 = "0jjis8y1p3wj45v72alhcnrcxg1f2p6r6bi0383qwxlmngf3cizz";
    }))
, nixpkgs ? builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/393e96b3fac51139c134209c5d4f2738506b140e.tar.gz";
    sha256 = "0mlgwmrqd4734f2fjxqm44jxpr78qm7yy3kzh1zynd5mn9fpqw0b";
  }
, haskellCompiler ? "ghc865"
}:
  pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    ghc = pkgs.buildPackages.pkgs.haskell.compiler.${haskellCompiler};
  }
