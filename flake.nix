{
  description = "A flake for a configured emacs-daemon";

  nixConfig.extra-experimental-features = "nix-command flakes";

  inputs = {
    nixpkgs.url   = "github:nixos/nixpkgs/nixos-unstable";
    emacs-overlay = {
      url    = "github:nix-community/emacs-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs = { self, nixpkgs, emacs-overlay }: {
    aliases            = ./aliases;

    emacsConfig        = import ./emacsConfig   { inherit      nixpkgs emacs-overlay; };
    emacsPackage       = import ./emacsPackage  { inherit self nixpkgs emacs-overlay; };

    localPackages      = import ./localPackages { inherit      nixpkgs              ; };
    systemPackages     = ./systemPackages;
  };
}
