{ mkDerivation, base, pure-state, pure-json, pure-txt-trie, mtl, transformers, stdenv
}:
mkDerivation {
  pname = "pure-forms";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base pure-state pure-json pure-txt-trie mtl transformers
  ];
  homepage = "github.com/grumply/pure-forms";
  license = stdenv.lib.licenses.bsd3;
}
