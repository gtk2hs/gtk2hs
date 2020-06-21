{ sourcesOverride ? {}
, haskellCompiler ? "ghc883" }:
(import ./. { inherit sourcesOverride haskellCompiler; }).hsPkgs.shellFor {
  tools = { cabal = "3.2.0.0"; };
}

