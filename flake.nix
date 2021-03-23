{
  description = "personal wiki!";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        inherit (lib) attrValues;
        compiler = "ghc8104";
        lib = nixpkgs.lib;
        pkgs = import nixpkgs { inherit system; };
        package = with pkgs.haskellPackages;
          callPackage (callCabal2nix "wikigen" ./.) { };
      in {
        defaultPackage = package;
        devShell = package.env;
      });
}
