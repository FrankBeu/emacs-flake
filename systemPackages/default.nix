{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
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

    ###
    ##
    # ** lsp
    #    https://emacs-lsp.github.io/lsp-mode/page/languages/
    #
    nodePackages.bash-language-server               ### https://github.com/bash-lsp/bash-language-server#readme                     ### TODO

    nodePackages.dockerfile-language-server-nodejs  ### https://github.com/rcjsuen/dockerfile-language-server-nodejs#readme         ### TODO

    gopls                                           ### https://github.com/golang/tools/tree/master/gopls

    nodePackages.vscode-css-languageserver-bin      ### https://github.com/vscode-langservers/vscode-css-languageserver-bin#readme  ### TODO

    nodePackages.vscode-html-languageserver-bin     ### https://github.com/vscode-langservers/vscode-html-languageserver-bin#readme ### TODO

    nodePackages.vscode-json-languageserver-bin     ### https://github.com/vscode-langservers/vscode-json-languageserver-bin#readme ### TODO

    # python39Packages.python-language-server       ### https://github.com/palantir/python-language-server
    # python-language-server                        ### https://github.com/microsoft/python-language-server

    ###
    ##
    # ***  pyright
    #      goes only to stub instead of implementation
    #
    nodePackages.pyright                          ### https://github.com/microsoft/pyright installes currently pyright 1.1.235

    ###
    ##
    # ***  python-lsp-server
    #
    python39Packages.python-lsp-server              ### https://github.com/python-lsp/python-lsp-server
    python39Packages.python-lsp-black               ### https://github.com/python-lsp/python-lsp-black                              ### fork from rupert
    # # python39Packages.pyls-black                 ### https://github.com/rupert/pyls-black                                        ### ALTERNATIVE
    python39Packages.pyls-isort                     ### https://github.com/paradoxxxzero/pyls-isort
    python39Packages.pyls-flake8                    ### https://github.com/emanspeaks/pyls-flake8
    # python39Packages.pyls-mypy                    ### https://github.com/Richardk2n/pylsp-mypy                 NOTWORKING         ### static type checking


    rnix-lsp                                        ### https://github.com/nix-community/rnix-lsp
    nixpkgs-fmt                                     ### https://nix-community.github.io/nixpkgs-fmt/

    rls                                             ### https://github.com/rust-lang/rls/                                           ### TODO

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
    # python39Packages.numpy                          ###
    # python39Packages.matplotlib                     ###
    phantomjs
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
    python39Packages.qrcode                         ### https://pypi.org/project/qrcode/

  ];


  fonts.fonts = with pkgs; [
    emacs-all-the-icons-fonts
    fira-code
    fira-code-symbols
    hack-font
    iosevka-bin
    roboto-mono
  ];

}
