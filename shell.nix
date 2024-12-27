# Enter a shell for this project using reflex-platform (which provides ghc8107)
# or nixpkgs (which provides ghc943)
{ compiler ? "ghc810" # or "ghc943"
}:
let pkgs = (import ./dep/reflex-platform { }).nixpkgs;
in
pkgs.mkShell {
  name = "shell-${compiler}";
  buildInputs = [
    pkgs.cabal-install
    pkgs.ghcid

    pkgs.haskellPackages.reflex-vty
  ];

}
