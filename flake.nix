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
        overlay = (self: super: {
          haskellPackages = super.haskellPackages.extend (hself: hsuper:
            let inherit (super.haskell.lib) unmarkBroken;
            in {
              # harg = unmarkBroken hsuper.harg;
              # higgledy = unmarkBroken hsuper.higgledy;
            });
        });

        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
        package = with pkgs.haskellPackages;
          callPackage (callCabal2nix "wikigen" ./.) { };
      in {
        defaultPackage = package;
        devShell = package.env;
      });
}
