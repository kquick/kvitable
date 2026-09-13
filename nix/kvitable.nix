{ mkDerivation, base, containers, html-parse, lib, lucid, microlens
, named-text, prettyprinter, sayable, tasty, tasty-hunit
, template-haskell, text
}:
mkDerivation {
  pname = "kvitable";
  version = "1.2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers lucid microlens named-text prettyprinter sayable
    text
  ];
  testHaskellDepends = [
    base html-parse microlens tasty tasty-hunit template-haskell text
  ];
  homepage = "https://github.com/kquick/kvitable";
  description = "Key/Value Indexed Table container and formatting library";
  license = lib.licenses.isc;
}
