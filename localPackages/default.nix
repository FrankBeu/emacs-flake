{ nixpkgs, ... }:
let localPackages = with import nixpkgs { system = "x86_64-linux"; }; stdenv.mkDerivation {
  name = "packagesLocal";
  src = ./src;

  phases = [ "unpackPhase" "installPhase" ];

  unpackPhase = ''
    cp -r $src/* .
  '';

  ### DEBUG: list sources
  # configurePhase = ''
  #   ls -lR .
  # '';

  installPhase = ''
    cp -r . $out/
  '';
};
in
localPackages
