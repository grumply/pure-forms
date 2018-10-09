{ mkDerivation, base, containers, hashable, mtl, pure, stdenv
, transformers
}:
mkDerivation {
  pname = "pure-forms";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers hashable mtl pure transformers
  ];
  homepage = "github.com/grumply/pure-forms";
  description = "A polymorphic constraint experiment with forms.";
  license = stdenv.lib.licenses.bsd3;
}
