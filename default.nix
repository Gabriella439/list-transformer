{ mkDerivation, base, doctest, mtl, stdenv }:
mkDerivation {
  pname = "list-transformer";
  version = "1.0.2";
  src = ./.;
  libraryHaskellDepends = [ base mtl ];
  testHaskellDepends = [ base doctest ];
  homepage = "https://github.com/Gabriel439/Haskell-List-Transformer-Library";
  description = "List monad transformer";
  license = stdenv.lib.licenses.bsd3;
}
