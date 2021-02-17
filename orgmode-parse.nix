{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, fetchgit, free, hashable, HUnit, insert-ordered-containers, lib
, neat-interpolation, old-locale, semigroups, tasty, tasty-hunit
, text, thyme, unordered-containers
}:
mkDerivation {
  pname = "orgmode-parse";
  version = "0.3.0";
  src = fetchgit {
    url = "https://github.com/jakeisnt/orgmode-parse.git";
    sha256 = "0qxddng5bm5sagapssq6n5jr8n5kg3p2mf825vma19hjk4kaypji";
    rev = "009bb90e8f2ecdd9a6f0d6aa4481cd69c9a00435";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson attoparsec base bytestring containers free hashable
    insert-ordered-containers old-locale semigroups text thyme
    unordered-containers
  ];
  testHaskellDepends = [
    aeson attoparsec base bytestring containers free hashable HUnit
    insert-ordered-containers neat-interpolation old-locale semigroups
    tasty tasty-hunit text thyme unordered-containers
  ];
  description = "A collection of Attoparsec combinators for parsing org-mode flavored documents";
  license = lib.licenses.bsd3;
}
