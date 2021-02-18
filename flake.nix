{
  description = "personal wiki!";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    orgmode-parse.url = "github:jakeisnt/orgmode-parse/master";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        inherit (lib) attrValues;
        compiler = "ghc8104";
        lib = nixpkgs.lib;
        orgmode-parse = with pkgs.haskellPackages;
          callPackage ./orgmode-parse.nix { };

        overlay = (self: super:
          let changeset = { orgmode-parse = orgmode-parse; };
          in {
            haskellPackages = super.haskellPackages.override {
              overrides = hself: hsuper: hsuper // changeset;
            };
          });

        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
        package = with pkgs.haskellPackages; callPackage (callCabal2nix "wikigen" ./.) { };
      in {
        defaultPackage = package;
        defaultOverlay = overlay;
        devShell = package.env;
      });
}
