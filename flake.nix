{
  description = "personal wiki!";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    orgmode-parse.url = "github:jakeisnt/orgmode-parse/master";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, orgmode-parse, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        defaultPackage =
          pkgs.callPackage ./default.nix { inherit orgmode-parse; };
        devShell = pkgs.mkShell {
          buildInputs = {

          };
        };
      });
}
