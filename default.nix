{ pkgs ? import nixpkgs ({
    overlays = import ((builtins.fetchTarball {
      url = "https://github.com/input-output-hk/haskell.nix/archive/ee945efd4cb22e2cb5c58dce501312562d8ac960.tar.gz";
      sha256 = "0g5d7f4rcg5shqj4m9wb2dxjkf671nbj6yx671pnq2ncwl00dczy";
    }) + "/overlays");
  })
, nixpkgs ? builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/afbb73ee7177a7a0833335e49bfa7e567eaf7534.tar.gz";
    sha256 = "01a1ylyizsg1xvadrbhq60lfdjl8fll5p14zf35as7gfai9g9ycl";
  }
, haskellCompiler ? "ghc865"
}:
let
  project = pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    ghc = pkgs.buildPackages.pkgs.haskell.compiler.${haskellCompiler};
  };
  shells = {
    ghc = (project.shellFor {}).overrideAttrs (oldAttrs: {
      shellHook = (oldAttrs.shellHook or "") + ''
        unset CABAL_CONFIG
      '';
    });
  };
in
  project // {
    inherit shells;
  }

