{ mkDerivation, base, lib, megaparsec, tasty, tasty-hunit, text }:
mkDerivation {
  pname = "cleaner-allocs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base megaparsec text ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base megaparsec tasty tasty-hunit text ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
