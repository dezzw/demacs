{
  description = "Desmond's Emacs (demacs) Configuration";
  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
      "https://dezzw.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "dezzw.cachix.org-1:5YXdWpaFXkULUAJ30oEaGHCZlC2Tt7SZMW8r9kmR83E="
    ];
  };
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
    };
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-utils,
      emacs-overlay,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlay ];
        };
        mps = pkgs.callPackage ./mps.nix {};
      in
      rec {
        emacs-patched =
          (pkgs.emacs-git.override {
            withImageMagick = true;
          }).overrideAttrs
            (old: rec {

              name = "emacs-${version}";
              version = "igc";

              src = pkgs.fetchFromGitHub {
                owner = "emacs-mirror";
                repo = "emacs";
                rev = "a19e818265e80d3dd2043a6a4ef2c4e8a8014f77";
                hash = "sha256-s73Km3rBhafd8cJXbJi+AWopdXqQPLMANy7wlR0XkqY=";
              };
              
              configureFlags = (old.configureFlags or [ ]) ++ [
                # "--with-xwidgets" # withXwidgets failed with mps enabled
                "--with-mps"
                "--with-native-compilation=aot"
              ];

              buildInputs =
                old.buildInputs
                ++ [mps];
                # ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [ pkgs.darwin.apple_sdk_11_0.frameworks.WebKit ];

              patches = (old.patches or [ ]) ++ [
                # Fix OS window role so that yabai can pick up Emacs
                (pkgs.fetchpatch {
                  url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
                  sha256 = "+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
                })
                
                # Add setting to enable rounded window with no decoration (still have to alter default-frame-alist)
                (pkgs.fetchpatch {
                  url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/round-undecorated-frame.patch";
                  sha256 = "uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
                })
                
                # Make Emacs aware of OS-level light/dark mode
                (pkgs.fetchpatch {
                  url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/system-appearance.patch";
                  sha256 = "3QLq91AQ6E921/W9nfDjdOUWR8YVsqBAT/W9c1woqAw=";
                })

                ./patches/ns-alpha-background.patch
                # ./patches/cursor-animation.patch
              ];
            });

        emacs-augmented = (
          (pkgs.emacsPackagesFor emacs-patched).emacsWithPackages (
            epkgs: with epkgs; [
              (callPackage ./site-packages/lsp-bridge/lsp-bridge.nix {
                inherit (pkgs) fetchFromGitHub;
              })
              
              vterm
              pdf-tools
              pkgs.emacsPackages.treesit-grammars.with-all-grammars
            ]
          )
        );

        packages.demacs = emacs-augmented;

        apps.demacs = flake-utils.lib.mkApp {
          drv = packages.demacs;
          name = "demacs";
          exePath = "/bin/emacs";
        };
        packages.default = packages.demacs;
        apps.default = apps.demacs;
      }
    );
}
