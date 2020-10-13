{ mkDerivation, base, pure, pure-state, pure-json, pure-txt-trie, mtl, transformers, stdenv
}:
mkDerivation {
  pname = "pure-forms";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base pure pure-state pure-json pure-txt-trie mtl transformers
  ];
  homepage = "github.com/grumply/pure-forms";
  license = stdenv.lib.licenses.bsd3;
}
