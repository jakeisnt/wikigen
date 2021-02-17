{ mkDerivation, pkgs, base, lib }:

mkDerivation {
  pname = "wikigen";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = with pkgs.haskellPackages; [
    base
    my-orgmode-parse
    lens
    universum
    time
    lucid
    text
    attoparsec
  ];
  license = "MIT";
  hydraPlatforms = lib.platforms.none;
}
