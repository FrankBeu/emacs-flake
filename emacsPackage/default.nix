{ self
, nixpkgs
, emacs-overlay
, ...
}:
with import nixpkgs { system = "x86_64-linux"; overlays = [ emacs-overlay.overlay ]; };
let emacsPackage =
  (pkgs.emacsWithPackagesFromUsePackage {
    ### Your Emacs config file. Org mode babel files are also
    ### supported.
    ### NB: Config files cannot contain unicode characters, since
    ###     they're being parsed in nix, which lacks unicode
    ###     support.
    # config = /etc/nixos/config/users/config/homemanager/emacs/emacs.d/init.el;
    config = "${self.emacsConfig}/init.el";
    ### TODO: currently packages are imported explicitly

    ### Package is optional, defaults to pkgs.emacs
    # package = pkgs.emacsGit;
    # package = pkgs.emacsNativeComp;   ### latestTag
    package = pkgs.emacsGitNativeComp;  ### master

    ### By default emacsWithPackagesFromUsePackage will only pull in
    ### packages with `:ensure`, `:ensure t` or `:ensure <package name>`.
    ### Setting `alwaysEnsure` to `true` emulates `use-package-always-ensure`
    ### and pulls in all use-package references not explicitly disabled via
    ### `:ensure nil` or `:disabled`.
    ### Note that this is NOT recommended unless you've actually set
    ### `use-package-always-ensure` to `t` in your config.
    # alwaysEnsure = true;
    alwaysEnsure = false;

    ### For Org mode babel files, by default only code blocks with
    ### `:tangle yes` are considered. Setting `alwaysTangle` to `true`
    ### will include all code blocks missing the `:tangle` argument,
    ### defaulting it to `yes`.
    ### Note that this is NOT recommended unless you have something like
    ### `#+PROPERTY: header-args:emacs-lisp :tangle yes` in your config,
    ### which defaults `:tangle` to `yes`.
    # alwaysTangle = true;
    alwaysTangle = false;

    ### Optionally provide extra packages not in the configuration file.
    extraEmacsPackages = epkgs: [
      ###
      ##
      # ** PLAIN
      #
      epkgs.org-contrib                           ### https://elpa.nongnu.org/nongnu/org-contrib.html

      ###
      ##
      # ** ELPA
      #
      epkgs.elpaPackages.ace-window               ### https://github.com/abo-abo/ace-window
      epkgs.elpaPackages.auctex                   ### ; LaTeX mode                                                                                          # TODO
      epkgs.elpaPackages.beacon                   ### https://github.com/Malabarba/beacon                  highlight my cursor when scrolling               # TODO
      epkgs.elpaPackages.bug-hunter               ### https://elpa.gnu.org/packages/bug-hunter.html
      epkgs.elpaPackages.dired-git-info           ### https://github.com/clemera/dired-git-info
      epkgs.elpaPackages.rainbow-mode             ### https://elpa.gnu.org/packages/rainbow-mode.html
      epkgs.elpaPackages.csv-mode                 ### https://elpa.gnu.org/packages/csv-mode.html
      epkgs.elpaPackages.undo-tree                ### https://www.emacswiki.org/emacs/UndoTree             <C-x u> to show the undo tree

      ###
      ##
      # ** MELPA
      #
      epkgs.melpaPackages.all-the-icons           ### https://github.com/domtronn/all-the-icons.el
      epkgs.melpaPackages.all-the-icons-ivy-rich  ### https://github.com/seagle0128/all-the-icons-ivy-rich
      epkgs.melpaPackages.arduino-mode            ### https://github.com/stardiviner/arduino-mode/
      epkgs.melpaPackages.assess                  ### https://github.com/phillord/assess                                                                    # TODO
      epkgs.melpaPackages.auctex-latexmk
      epkgs.melpaPackages.auto-yasnippet          ### https://github.com/abo-abo/auto-yasnippet                                                             # TODO
      epkgs.melpaPackages.avy                     ### https://github.com/abo-abo/avy

      epkgs.melpaPackages.base16-theme            ### https://github.com/belak/base16-emacs                                                                 # TODO
      epkgs.melpaPackages.blacken                 ### https://github.com/pythonic-emacs/blacken
      epkgs.melpaPackages.buttercup               ### https://github.com/jorgenschaefer/emacs-buttercup                                                     # TOOD

      epkgs.melpaPackages.calfw                   ### https://github.com/kiwanami/emacs-calfw
      epkgs.melpaPackages.calfw-org               ### https://github.com/kiwanami/emacs-calfw
      epkgs.melpaPackages.cask                    ### https://github.com/cask/cask                                                                          # TOOD
      epkgs.melpaPackages.command-log-mode        ### https://github.com/lewang/command-log-mode           show {event,command}-history
      epkgs.melpaPackages.company                 ### https://github.com/company-mode/company-mode                                                          # TODO
      epkgs.melpaPackages.company-box             ### https://github.com/sebastiencs/company-box
      epkgs.melpaPackages.company-auctex          ### https://github.com/alexeyr/company-auctex                                                           # TODO
      epkgs.melpaPackages.company-box             ### https://github.com/sebastiencs/company-box
      epkgs.melpaPackages.company-solidity        ### https://github.com/ethereum/emacs-solidity
      epkgs.melpaPackages.counsel                 ### https://github.com/abo-abo/swiper
      epkgs.melpaPackages.counsel-projectile      ### https://github.com/ericdanan/counsel-projectile
      epkgs.melpaPackages.csharp-mode             ### https://github.com/emacs-csharp/csharp-mode                                                           # TODO
      epkgs.melpaPackages.cucumber-goto-step      ### https://github.com/gstamp/cucumber-goto-step                                                          # TODO
      ###   cue-mode                              ### https://github.com/phaer/cue-mode.el                         TODO: NOT-AVAILABLE

      ###   dap-dart                              ### https://github.com/Dart-Code/Dart-Code                       TODO: NOT-AVAILABLE                                        # TODO NOT AVAILABLE
      epkgs.melpaPackages.dap-mode                ### https://github.com/emacs-lsp/dap-mode                                                                 # TODO
      epkgs.melpaPackages.dart-mode               ### https://github.com/bradyt/dart-mode                                                                   # TODO
      epkgs.melpaPackages.dired-hide-dotfiles     ### https://github.com/mattiasb/dired-hide-dotfiles
      epkgs.melpaPackages.dired-k                 ### https://github.com/emacsorphanage/dired-k
      epkgs.melpaPackages.dired-rifle             ### https://github.com/Vifon/dired-rifle.el
      epkgs.melpaPackages.direnv                  ### https://github.com/wbolster/emacs-direnv
      epkgs.melpaPackages.docker                  ### https://github.com/Silex/docker.el                                                                    # TODO
      epkgs.melpaPackages.dockerfile-mode         ### https://github.com/spotify/dockerfile-mode
      epkgs.melpaPackages.doom-modeline           ### https://github.com/seagle0128/doom-modeline
      epkgs.melpaPackages.doom-themes             ### https://github.com/hlissner/emacs-doom-themes
      # epkgs.melpaPackages.doom-todo-ivy           ### https://github.com/jsmestad/doom-todo-ivy                 TODO: NOT-AVAILABLE via melpa

      epkgs.melpaPackages.ecukes                  ### https://github.com/ecukes/ecukes                                                                      # TODO
      epkgs.melpaPackages.edit-server             ### https://github.com/stsquad/emacs_chrome                                                               # TODO
      epkgs.melpaPackages.editorconfig            ### https://github.com/editorconfig/editorconfig-emacs
      epkgs.melpaPackages.ein                     ### https://github.com/millejoh/emacs-ipython-notebook#ob-ein   TODO
      epkgs.melpaPackages.evil                    ### https://github.com/emacs-evil/evil                   vim-keybindings
      epkgs.melpaPackages.emmet-mode              ### https://github.com/smihica/emmet-mode
      epkgs.melpaPackages.envrc                   ### https://github.com/purcell/envrc
      epkgs.melpaPackages.evil-collection         ### https://github.com/emacs-evil/evil-collection        evil for other modes
      epkgs.melpaPackages.evil-escape             ### https://github.com/syl20bnr/evil-escape
      epkgs.melpaPackages.evil-nerd-commenter     ### https://github.com/redguardtoo/evil-nerd-commenter
      epkgs.melpaPackages.evil-numbers            ### https://github.com/cofi/evil-numbers
      epkgs.melpaPackages.evil-org                ### https://github.com/Somelauw/evil-org-mode
      epkgs.melpaPackages.evil-surround           ### https://github.com/emacs-evil/evil-surround
      epkgs.melpaPackages.eyebrowse               ### https://depp.brause.cc/eyebrowse/

      epkgs.melpaPackages.feature-mode            ### https://github.com/michaelklishin/cucumber.el                                                         # TODO
      epkgs.melpaPackages.fira-code-mode          ### https://github.com/jming422/fira-code-mode           enable fira-code-ligatures
      epkgs.melpaPackages.flutter                 ### https://github.com/amake/flutter.el
      epkgs.melpaPackages.flycheck                ### https://github.com/flycheck/flycheck                                                                  # TODO
      epkgs.melpaPackages.flycheck-golangci-lint  ### https://github.com/weijiangan/flycheck-golangci-lint
      epkgs.melpaPackages.forge                   ### https://github.com/magit/forge                       magit for issues and pull request                # TODO

      epkgs.melpaPackages.general                 ### https://github.com/noctuid/general.el                create leader-keys
      epkgs.melpaPackages.go-gen-test             ### https://github.com/s-kostyaev/go-gen-test
      epkgs.melpaPackages.go-mode                 ### https://github.com/dominikh/go-mode.el
      epkgs.melpaPackages.gotest                  ### https://github.com/nlamirault/gotest.el
      epkgs.melpaPackages.graphviz-dot-mode       ### https://github.com/ppareit/graphviz-dot-mode

      epkgs.melpaPackages.haskell-mode            ### https://github.com/haskell/haskell-mode
      epkgs.melpaPackages.helpful                 ### https://github.com/Wilfred/helpful
      epkgs.melpaPackages.hl-todo                 ### https://github.com/tarsius/hl-todo
      epkgs.melpaPackages.hover                   ### https://github.com/ericdallo/hover.el
      epkgs.melpaPackages.hydra                   ### https://github.com/abo-abo/hydra

      epkgs.melpaPackages.imenu-list              ### https://github.com/bmag/imenu-list
      epkgs.melpaPackages.ivy                     ### https://github.com/abo-abo/swiper
      epkgs.melpaPackages.ivy-avy                 ### https://github.com/abo-abo/swiper
      epkgs.melpaPackages.ivy-hydra               ### https://github.com/abo-abo/swiper
      epkgs.melpaPackages.ivy-rich                ### https://github.com/Yevgnen/ivy-rich
      epkgs.melpaPackages.ivy-yasnippet           ### https://github.com/mkcms/ivy-yasnippet                                                                # TODO

      epkgs.melpaPackages.jsonnet-mode            ### https://github.com/tminor/jsonnet-mode

      epkgs.melpaPackages.k8s-mode                ### https://github.com/TxGVNN/emacs-k8s-mode
      epkgs.melpaPackages.kotlin-mode             ### https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode                                          # TODO
      epkgs.melpaPackages.kubernetes              ### https://github.com/chrisbarrett/kubernetes-el                                                         # TODO
      epkgs.melpaPackages.kubernetes-evil         ### https://github.com/chrisbarrett/kubernetes-el                                                         # TODO
      epkgs.melpaPackages.kubernetes-helm         ### https://github.com/abrochard/kubernetes-helm                                                          # TODO
      epkgs.melpaPackages.kubernetes-tramp        ### https://github.com/gruggiero/kubernetes-tramp                                                         # TODO

      epkgs.melpaPackages.lsp-dart                ### https://github.com/emacs-lsp/lsp-dart                                                                 # TODO
      epkgs.melpaPackages.lsp-docker              ### https://github.com/emacs-lsp/lsp-docker                                                               # TODO
      epkgs.melpaPackages.lsp-ivy                 ### https://github.com/emacs-lsp/lsp-ivy                                                                  # TODO
      epkgs.melpaPackages.lsp-java                ### https://github.com/emacs-lsp/lsp-java
      epkgs.melpaPackages.lsp-latex               ### https://github.com/ROCKTAKEY/lsp-latex                                                                # TODO
      epkgs.melpaPackages.lsp-mode                ### https://emacs-lsp.github.io/lsp-mode/                                                                 # TODO
      epkgs.melpaPackages.lsp-origami             ### https://github.com/emacs-lsp/lsp-origami                                                              # TODO
      epkgs.melpaPackages.lsp-pyright             ### https://github.com/emacs-lsp/lsp-pyright
      epkgs.melpaPackages.lsp-treemacs            ### https://github.com/emacs-lsp/lsp-treemacs                                                             # TODO
      epkgs.melpaPackages.lsp-ui                  ### https://github.com/emacs-lsp/lsp-ui                                                                   # TODO

      epkgs.melpaPackages.magit                   ### https://github.com/magit/magit
      epkgs.melpaPackages.magit-delta             ### https://github.com/dandavison/magit-delta
      epkgs.melpaPackages.magit-todos             ### https://github.com/alphapapa/magit-todos
      epkgs.melpaPackages.mermaid-mode            ### https://github.com/abrochard/mermaid-mode

      epkgs.melpaPackages.nix-mode                ### https://github.com/NixOS/nix-mode
      epkgs.melpaPackages.no-littering            ### https://github.com/emacscollective/no-littering
      epkgs.melpaPackages.notmuch                 ### https://melpa.org/#/notmuch                         from main packages set                            # TODO

      epkgs.melpaPackages.ob-browser              ### https://github.com/krisajenkins/ob-browser                                                            # TODO
      epkgs.melpaPackages.ob-dart                 ### https://github.com/mzimmerm/ob-dart                                                                   # TODO
      epkgs.melpaPackages.ob-go                   ### https://github.com/pope/ob-go                                                                         # TODO
      epkgs.melpaPackages.ob-graphql              ### https://github.com/jdormit/ob-graphql                                                                 # TODO
      epkgs.melpaPackages.ob-http                 ### https://github.com/zweifisch/ob-http                                                                  # TODO
      epkgs.melpaPackages.ob-kotlin               ### https://github.com/zweifisch/ob-kotlin                                                                # TODO
      epkgs.melpaPackages.ob-mermaid              ### https://github.com/arnm/ob-mermaid                                                                    # TODO
      epkgs.melpaPackages.ob-mongo                ### https://github.com/krisajenkins/ob-mongo                                                              # TODO
      epkgs.melpaPackages.ob-restclient           ### https://github.com/alf/ob-restclient.el                                                               # TODO
      epkgs.melpaPackages.ob-rust                 ### https://github.com/micanzhang/ob-rust                                                                 # TODO
      epkgs.melpaPackages.ob-swift                ### https://github.com/zweifisch/ob-swift                                                                 # TODO
      epkgs.melpaPackages.ob-translate            ### https://github.com/krisajenkins/ob-translate
      epkgs.melpaPackages.ob-typescript           ### https://github.com/lurdan/ob-typescript
      epkgs.melpaPackages.origami                 ### https://github.com/gregsexton/origami.el
      # epkgs.orgPackages.org                       ### https://orgmode.org/
      epkgs.melpaPackages.org-download            ### https://github.com/abo-abo/org-download             import png2orgfile                                # TODO
      epkgs.melpaPackages.org-drill               ### https://gitlab.com/phillord/org-drill/
      epkgs.melpaPackages.org-evil                ### https://github.com/GuiltyDolphin/org-evil                                                             # TODO
      epkgs.melpaPackages.org-make-toc            ### https://github.com/alphapapa/org-make-toc
      epkgs.melpaPackages.org-pomodoro            ### https://github.com/marcinkoziej/org-pomodoro
      epkgs.melpaPackages.org-ql                  ### https://github.com/alphapapa/org-ql                                                                   # TODO
      epkgs.melpaPackages.org-superstar           ### https://github.com/integral-dw/org-superstar-mode
      epkgs.melpaPackages.org-wild-notifier       ### https://github.com/akhramov/org-wild-notifier.el
      epkgs.melpaPackages.origami                 ### https://github.com/gregsexton/origami.el                                                              # TODO
      ###?? epkgs.melpaPackages.ox                      ###
      epkgs.melpaPackages.ox-hugo                 ### https://github.com/kaushalmodi/ox-hugo                                                                # TODO
      epkgs.melpaPackages.ox-json                 ### https://github.com/jlumpe/ox-json                                                                     # TODO
      epkgs.melpaPackages.ox-pandoc               ### https://github.com/kawabata/ox-pandoc                                                                 # TODO
      epkgs.melpaPackages.org-re-reveal           ### https://gitlab.com/oer/org-re-reveal

      epkgs.melpaPackages.persp-mode              ### https://github.com/Bad-ptr/persp-mode.el                                                              # TODO
      epkgs.melpaPackages.plantuml-mode           ### https://github.com/skuro/plantuml-mode
      epkgs.melpaPackages.popup                   ### https://github.com/auto-complete/popup-el                                                             # required by google-translate
      epkgs.melpaPackages.popwin                  ### https://github.com/emacsorphanage/popwin                                                              # USEFULL?
      epkgs.melpaPackages.prescient               ### https://github.com/raxod502/prescient.el                                                              # TODO
      epkgs.melpaPackages.projectile              ### https://github.com/bbatsov/projectile
      epkgs.melpaPackages.protobuf-mode           ### https://github.com/protocolbuffers/protobuf                                                           # TODO
      epkgs.melpaPackages.py-isort                ### https://gitlab.com/python-mode-devs/python-mode/
      epkgs.melpaPackages.python-mode             ### https://gitlab.com/python-mode-devs/python-mode/
      epkgs.melpaPackages.python-pytest           ### https://github.com/wbolster/emacs-python-pytest

      epkgs.melpaPackages.rainbow-delimiters      ### https://github.com/Fanael/rainbow-delimiters
      epkgs.melpaPackages.ripgrep                 ### https://github.com/nlamirault/ripgrep.el
      epkgs.melpaPackages.rust-mode               ### https://github.com/rust-lang/rust-mode                                                                # TODO
      epkgs.melpaPackages.rustic                  ### https://github.com/brotzeit/rustic

      epkgs.melpaPackages.sass-mode               ### https://github.com/nex3/sass-mode
      epkgs.melpaPackages.solaire-mode            ### https://github.com/hlissner/emacs-solaire-mode
      epkgs.melpaPackages.solidity-flycheck       ### https://github.com/ethereum/emacs-solidity
      epkgs.melpaPackages.solidity-mode           ### https://github.com/ethereum/emacs-solidity
      epkgs.melpaPackages.string-inflection       ### https://github.com/akicho8/string-inflection
      epkgs.melpaPackages.swift-mode              ### https://github.com/swift-emacs/swift-mode                                                             # TODO
      epkgs.melpaPackages.swiper                  ### https://github.com/abo-abo/swiper

      epkgs.melpaPackages.treemacs                ### https://github.com/Alexander-Miller/treemacs
      epkgs.melpaPackages.treemacs-all-the-icons  ### https://github.com/Alexander-Miller/treemacs
      epkgs.melpaPackages.treemacs-evil           ### https://github.com/Alexander-Miller/treemacs
      epkgs.melpaPackages.treemacs-icons-dired    ### https://github.com/Alexander-Miller/treemacs              ### use treemacs-icons in dired
      epkgs.melpaPackages.treemacs-magit          ### https://github.com/Alexander-Miller/treemacs
      epkgs.melpaPackages.treemacs-persp          ### https://github.com/Alexander-Miller/treemacs
      epkgs.melpaPackages.treemacs-projectile     ### https://github.com/Alexander-Miller/treemacs
      epkgs.melpaPackages.typescript-mode         ### https://github.com/emacs-typescript/typescript.el                                                     # TODO

      epkgs.melpaPackages.use-package             ### https://github.com/jwiegley/use-package

      epkgs.melpaPackages.vimrc-mode              ### https://github.com/mcandre/vimrc-mode
      epkgs.melpaPackages.vyper-mode              ### https://github.com/ralexstokes/vyper-mode

      epkgs.melpaPackages.web-mode                ### https://github.com/fxbois/web-mode
      epkgs.melpaPackages.which-key               ### https://github.com/justbur/emacs-which-key
      epkgs.melpaPackages.workgroups2             ### https://github.com/pashinin/workgroups2
      epkgs.melpaPackages.wrap-region             ### https://github.com/rejeep/wrap-region.el
      epkgs.melpaPackages.writeroom-mode          ### https://github.com/joostkremers/writeroom-mode

      epkgs.melpaPackages.yaml-mode               ### https://github.com/yoshiki/yaml-mode
      epkgs.melpaPackages.yasnippet               ### https://github.com/joaotavora/yasnippet
      epkgs.melpaPackages.yasnippet-snippets      ### https://github.com/AndreaCrotti/yasnippet-snippets

      epkgs.melpaPackages.zerodark-theme          ### https://github.com/NicolasPetton/zerodark-theme


      ###
      ##
      # *** ALTERNATIVES
      #
      # helm                          ### https://github.com/emacs-helm/helm                    ALT: ivy                                                 # TODO
      # org-alert                     ### https://github.com/spegoraro/org-alert                ALT: org-wild-notifier
      # epkgs.melpaPackages.yapfify   ### https://github.com/JorisE/yapfify                     ALT: blacken


      ###
      ##
      # *** OUTDATED
      #
      ### company-lsp                        ### https://github.com/tigersoldier/company-lsp           OUTDATED:   company-capf will be picked by lsp-mode
      ### dart-server                        ### https://github.com/bradyt/dart-server                 OUTDATED:   use lsp
      ### evil-commentary                    ### https://github.com/linktohack/evil-commentary         OUTDATED:   use evil-nerd-commenter
      ### evil-magit                         ### https://github.com/emacs-evil/evil-magit              DEPRECATED: integrated into evil-collection
      ### flycheck-yamllint                  ### https://github.com/krzysztof-magosa/flycheck-yamllint DEPRECATED: already integrated into flycheck
      ### flymake                            ### https://github.com/flymake/emacs-flymake              OUTDATED:   use flycheck
      ### flymd                              ### https://github.com/mola-T/flymd                       OUTDATED:   not working with firefox >68.0
      ### neotree                            ### https://github.com/jaypei/emacs-neotree               OUTDATED:   use treemacs
      ### ob-ipython                         ### https://github.com/gregsexton/ob-ipython
      ### ox-reveal                          ### https://github.com/yjwen/org-reveal                   DEPRECATED: use org-re-reveal instead
      ### dired-single                       ### https://github.com/crocket/dired-single               SOLVED:     commands provide a much easier solution
      ### all-the-icons-dired                ### https://github.com/jtbm37/all-the-icons-dired         DUPLICATES: treemacs-icons-dired

      ### epkgs.orgPackages.org-plus-contrib ### https://orgmode.org/worg/org-contrib/                 DEPRECATED: use epkgs.org-contrib instead
    ];


    ### Optionally override derivations.
    override = epkgs: epkgs // {
      # weechat = epkgs.melpaPackages.weechat.overrideAttrs(old: {
        #   patches = [ ./weechat-el.patch ];
        # });
    };
  });
in
emacsPackage
