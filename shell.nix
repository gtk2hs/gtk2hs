{ haskellCompiler ? "ghc881" }:
(import ./. { inherit haskellCompiler; }).hsPkgs.shellFor {}
    
