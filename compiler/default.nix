{ mkDerivation
, alex
, array
, base
, bytestring
, happy
, hspec
, lens }:
mkDerivation {
    pname = "wla";
    version = "0.0.0.0";
    license = null;
    src = ./.;
    buildDepends = [
        alex
        array
        base
        bytestring
        happy
        hspec
        lens
    ];
}
