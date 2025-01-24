### in order to let the consumer controll systemPkgs (apply overlays ...)
### config and pkgs have to be provided by the consumer;
{ config, pkgs, ... }:
{
  # environment.systemPackages = with pkgs; [
  home.packages = with pkgs; [
    ###
    ##
    # ** misc
    #
    cask
    notmuch                                                       ### https://notmuchmail.org/
    python3                                                       ### dependency for lsp-treemacs
    # texlive.combined.scheme-full
    (texlive.combine { inherit (texlive) scheme-full graphics; })
    ripgrep                                                       ### https://github.com/BurntSushi/ripgrep
    yamllint                                                      ### https://github.com/adrienverge/yamllint
    puppet-lint
    emacs-all-the-icons-fonts                                     ### https://github.com/domtronn/all-the-icons.el

    ###
    ##
    # ** lsp
    #    https://emacs-lsp.github.io/lsp-mode/page/languages/
    #
    ansible-language-server

    cuelsp                                          ### https://github.com/dagger/cuelsp
    nodejs

    nodePackages.bash-language-server               ### https://github.com/bash-lsp/bash-language-server#readme                     ### TODO

    nodePackages.dockerfile-language-server-nodejs  ### https://github.com/rcjsuen/dockerfile-language-server-nodejs#readme         ### TODO

    gopls                                           ### https://github.com/golang/tools/tree/master/gopls

    jdt-language-server                             ### https://github.com/eclipse/eclipse.jdt.ls

    vscode-langservers-extracted                    ### https://github.com/hrsh7th/vscode-langservers-extracted
    #TODO nodePackages.vscode-css-languageserver-bin      ### https://github.com/vscode-langservers/vscode-css-languageserver-bin#readme  ### TODO

    #TODO nodePackages.vscode-html-languageserver-bin     ### https://github.com/vscode-langservers/vscode-html-languageserver-bin#readme ### TODO

    #TODO nodePackages.vscode-json-languageserver

    nodePackages.yaml-language-server               ### https://github.com/redhat-developer/yaml-language-server

    # python3Packages.python-language-server        ### https://github.com/palantir/python-language-server
    # python-language-server                        ### https://github.com/microsoft/python-language-server

    texlab                                          ### https://github.com/latex-lsp/texlab

    ###
    ##
    # ***  pyright
    #      goes only to stub instead of implementation
    #
    # TODO nodePackages.pyright                          ### https://github.com/microsoft/pyright installes currently pyright 1.1.235
    pyright                                        ### https://github.com/microsoft/pyright installes currently pyright 1.1.235

    ###
    ##
    # ***  python-lsp-server
    #
    python3Packages.python-lsp-server              ### https://github.com/python-lsp/python-lsp-server
    python3Packages.python-lsp-black               ### https://github.com/python-lsp/python-lsp-black                              ### fork from rupert
    # # python3Packages.pyls-black                 ### https://github.com/rupert/pyls-black                                        ### ALTERNATIVE
    python3Packages.pyls-isort                     ### https://github.com/paradoxxxzero/pyls-isort
    python3Packages.pyls-flake8                    ### https://github.com/emanspeaks/pyls-flake8
    # python3Packages.pyls-mypy                    ### https://github.com/Richardk2n/pylsp-mypy                 NOTWORKING         ### static type checking


    # rnix-lsp                                        ### https://github.com/nix-community/rnix-lsp
    nil                                             ### https://github.com/oxalica/nil/tree/main
    nixpkgs-fmt                                     ### https://nix-community.github.io/nixpkgs-fmt/

    # rls                                           ### https://github.com/rust-lang/rls/                                           ### DEPRECATED
    rust-analyzer                                   ### https://rust-analyzer.github.io/
    ### from projectFlake
    # rustfmt
    # cargo

    # sqls                                          ### https://emacs-lsp.github.io/lsp-mode/page/lsp-sqls/                         ### TODO not in nix

    # nodePackages.yaml-language-server             ### https://github.com/redhat-developer/yaml-language-server#readme             ### currently installed via lsp


    ###
    ##
    # ** babel
    #
    # dart
    (import ./dart.nix { inherit pkgs fetchurl; })    ### https://dart.dev/get-dart/archive

    nodePackages.mermaid-cli

    rust-script
    plantuml
    # python3Packages.numpy                           ###
    # python3Packages.matplotlib                      ###
    # phantomjs                                       ### TODO used for what - phantomjs is deprecated -write tests for all functionality
    graphviz

    ###
    ##
    # ** golang
    #
    ### errcheck                                    ### error-linter https://github.com/kisielk/errcheck
    gotests                                         ### https://github.com/cweill/gotests
    golangci-lint                                   ### https://golangci-lint.run/
    delve
    godef                                           ### https://github.com/rogpeppe/godef


    ###
    ##
    # ** python
    #
    python3


    ###
    ##
    # ** reveal.js
    #
    # # nodePackages.revealjs                           ### TODO:new not available any more
    # nodePackages."reveal.js"                           ### TODO:new not available any more
    # reveal.js                           ### TODO:new not available any more
    python3Packages.qrcode                         ### https://pypi.org/project/qrcode/

    ### FONTS
    emacs-all-the-icons-fonts
    fira-code
    fira-code-symbols
    hack-font
    iosevka-bin
    roboto-mono
  ];


  # fonts.packages = with pkgs; [
  #   emacs-all-the-icons-fonts
  #   fira-code
  #   fira-code-symbols
  #   hack-font
  #   iosevka-bin
  #   roboto-mono
  # ];
}
