{ mkDerivation, pkgs, base, lib, orgmode-parse, lens, universum }:

let
  # https://stackoverflow.com/questions/54810851/how-can-i-build-a-haskell-dependency-from-a-github-source-nix-file-using-nix
  hs = pkgs.haskellPackages.extend (self: super: { # (1) extend the package set
    my-orgmode-parse = orgmode-parse; # {
    # inherit pkgs;
    # }; # self.callPackage ./orgmode-parse.nix {};
    # orgmode-parse.defaultPackage."x86_64-linux".lib; # pkgs.haskell.lib.dontCheck (self.callPackage .orgmode-parse { }); # use my local orgmode-parse
  });

in mkDerivation {
  pname = "wikigen";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hs.orgmode-parse lens universum ];
  license = "MIT";
  hydraPlatforms = lib.platforms.none;
}
