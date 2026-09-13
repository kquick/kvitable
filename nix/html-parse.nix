{ mkDerivation, attoparsec, base, containers, criterion, deepseq
, fetchgit, hspec, hspec-discover, lib, QuickCheck
, quickcheck-instances, string-conversions, tagsoup, text
}:
mkDerivation {
  pname = "html-parse";
  version = "0.2.2.0";
  src = fetchgit {
    url = "https://github.com/bgamari/html-parse";
    sha256 = "0rz0rdl873jwcbzq59hgq8v8iva7pavcd6w1m91285x3yg6nps8r";
    rev = "bd354c36586406efbedf67650dbf17b57bec53ef";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base containers deepseq text
  ];
  testHaskellDepends = [
    base containers hspec hspec-discover QuickCheck
    quickcheck-instances string-conversions text
  ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [
    attoparsec base criterion deepseq tagsoup text
  ];
  homepage = "http://github.com/bgamari/html-parse";
  description = "A high-performance HTML tokenizer";
  license = lib.licenses.bsd3;
}
