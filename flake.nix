{
  description = "personal wiki!";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    orgmode-parse.url = "github:jakeisnt/orgmode-parse/master";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, orgmode-parse, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        compiler = "ghc8104";
        package = with pkgs.haskell.packages.${compiler};
          callPackage ./build.nix {
            # orgmode-parse = orgmode-parse;
          };
      in {
        defaultPackage = package;
        devShell = package.env;
      });
}
