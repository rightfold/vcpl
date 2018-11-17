{ nixpkgs ? import ./nix/nixpkgs.nix {} }:
{
    compiler = nixpkgs.haskellPackages.callPackage ./compiler {};
}
