{ nixpkgs, emacs-overlay, ... }:
let emacsConfig = with import nixpkgs { system = "x86_64-linux"; overlays = [ emacs-overlay.overlay ]; }; stdenv.mkDerivation {
  name = "emacs-config";
  # buildInputs = [ emacsNativeComp ];   ### latestTag
  buildInputs = [ emacsGitNativeComp ];  ### master
  src = ../emacs.d;

  unpackPhase = ''
    cp -r $src/* .
  '';

  ### DEBUG: list sources
  # configurePhase = ''
  #   ls -lR .
  # '';

  ### 1. create config.org.org including all INCLUDE:d files
  ### 2. tangle config.org.org to init.el
  buildPhase = ''
    emacs \
    --quick \
    --batch \
    --eval "(require 'org)" \
    --eval "(progn (find-file \"config.org\") (org-org-export-to-org) (kill-buffer))"

    emacs \
    --quick \
    --batch \
    --eval "(require 'org)" \
    --eval '(org-babel-tangle-file "config.org.org" "init.el")'
  '';

  installPhase = ''
    cp -r . $out/
  '';
};
in
emacsConfig
