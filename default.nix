{ mkDerivation, base, containers, lib, megaparsec, optics
, optparse-applicative, tasty, tasty-hunit, text
}:
mkDerivation {
  pname = "cleaner-allocs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers megaparsec optics text ];
  executableHaskellDepends = [
    base megaparsec optics optparse-applicative text
  ];
  testHaskellDepends = [ base megaparsec tasty tasty-hunit text ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
