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
    emacs-src = {
      url = "github:emacs-mirror/emacs";
      flake = false;
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, emacs-overlay, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlay ];

        };
      in rec {
        dependencies = with pkgs; [
          git

          # Language Server
          ccls

          ruff-lsp

          nodePackages.pyright
          nodePackages.typescript-language-server
          nodePackages.vscode-langservers-extracted
          nodePackages.bash-language-server
          nodePackages.vscode-css-languageserver-bin
          nodePackages.vscode-html-languageserver-bin
          nodePackages.svelte-language-server

          nodePackages.eslint

          python39Packages.pylint

          rnix-lsp
          nil

          texlab

          zls

          universal-ctags

          # Code Formating
          nixfmt

          # dirvish
          imagemagick
          ffmpegthumbnailer
          mediainfo

          # org-download
          pngpaste

          # Spelling checking
          # enchant

          emacs-all-the-icons-fonts
        ];

        emacs-patched = (pkgs.emacs-git.override {
          withXwidgets = false;
          withSQLite3 = true;
          withNS = true;
          withWebP = true;
        }).overrideAttrs (old: {
          # https://github.com/cmacrae/emacs/blob/03b4223e56e10a6d88faa151c5804d30b8680cca/flake.nix#L75
          buildInputs = old.buildInputs
            ++ pkgs.lib.optionals pkgs.stdenv.isDarwin
            [ pkgs.darwin.apple_sdk.frameworks.WebKit ];
          patches = (old.patches or [ ]) ++ [
            # Fix OS window role (needed for window managers like yabai)
            (pkgs.fetchpatch {
              url =
                "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
              sha256 = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
            })
            # Use poll instead of select to get file descriptors
            (pkgs.fetchpatch {
              url =
                "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/poll.patch";
              sha256 = "sha256-jN9MlD8/ZrnLuP2/HUXXEVVd6A+aRZNYFdZF8ReJGfY=";
            })
            # Enable rounded window with no decoration
            (pkgs.fetchpatch {
              url =
                "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/round-undecorated-frame.patch";
              sha256 = "sha256-uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
            })
            # Make Emacs aware of OS-level light/dark mode
            (pkgs.fetchpatch {
              url =
                "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/system-appearance.patch";
              sha256 = "sha256-oM6fXdXCWVcBnNrzXmF0ZMdp8j0pzkLE66WteeCutv8=";
            })
          ];
        });

        emacs-augmented =
          ((pkgs.emacsPackagesFor emacs-patched).emacsWithPackages (epkgs:
            with epkgs; [
              (callPackage ./site-packages/lsp-bridge/lsp-bridge.nix {
                inherit (pkgs) fetchFromGitHub;
              })

              pkgs.emacsPackages.treesit-grammars.with-all-grammars
            ]));

        packages.demacs = emacs-augmented;

        apps.demacs = flake-utils.lib.mkApp {
          drv = packages.demacs;
          name = "demacs";
          exePath = "/bin/emacs";
        };
        packages.default = packages.demacs;
        apps.default = apps.demacs;
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [ packages.demacs ] ++ dependencies;

          shellHook = ''
            if [ ! -d $HOME/.emacs.d/ ]; then
              mkdir -p $HOME/.emacs.d
              git clone git@github.com:dezzw/demacs.git $HOME/.emacs.d 
            else
              cd $HOME/.emacs.d/
              git pull
            fi
          '';
        };
      });
}
