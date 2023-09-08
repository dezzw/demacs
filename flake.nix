{
  description = "Desmond's Emacs (demacs) Configuration";
  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    fenix.url = "github:nix-community/fenix";
    fenix.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
  };
  outputs = inputs @ {
    self,
      nixpkgs,
      flake-utils,
      emacs-overlay,
      ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          emacs-overlay.overlay
        ];
      };
    in rec {
      dependencies = with pkgs;
        [
          # Language Server
          ccls

          nodePackages.pyright
          nodePackages.typescript-language-server
          nodePackages.vscode-langservers-extracted
          nodePackages.bash-language-server
          nodePackages.vscode-css-languageserver-bin
          nodePackages.vscode-html-languageserver-bin

          nodePackages.eslint

          python39Packages.pylint
          
          rnix-lsp
          nil
          
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
        ];

      emacs-augmented = (pkgs.emacs-git).overrideAttrs (old: {
        # https://github.com/cmacrae/emacs/blob/03b4223e56e10a6d88faa151c5804d30b8680cca/flake.nix#L75
        buildInputs = old.buildInputs ++ dependencies ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [ pkgs.darwin.apple_sdk.frameworks.WebKit ];
        patches =
          (old.patches or [])
          ++ [
            # Fix OS window role (needed for window managers like yabai)
            (pkgs.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
              sha256 = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
            })
            # Use poll instead of select to get file descriptors
            (pkgs.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/poll.patch";
              sha256 = "sha256-jN9MlD8/ZrnLuP2/HUXXEVVd6A+aRZNYFdZF8ReJGfY=";
            })
            # Enable rounded window with no decoration
            (pkgs.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/round-undecorated-frame.patch";
              sha256 = "sha256-uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
            })
            # Make Emacs aware of OS-level light/dark mode
            (pkgs.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/system-appearance.patch";
              sha256 = "sha256-oM6fXdXCWVcBnNrzXmF0ZMdp8j0pzkLE66WteeCutv8=";
            })
          ];
      });
      packages.demacs = pkgs.emacsWithPackagesFromUsePackage {
        config = ./init.el;

        # Whether to include your config as a default init file.
        # If being bool, the value of config is used.
        # Its value can also be a derivation like this if you want to do some
        # substitution:
        # defaultInitFile = pkgs.substituteAll { #
        #   name = "default.el";
        #   src = ./test.el;
        #   # inherit (config.xdg) configHome dataHome;
        # };
        # defaultInitFile = true;

        package = emacs-augmented;

        # By default emacsWithPackagesFromUsePackage will only pull in
        # packages with `:ensure`, `:ensure t` or `:ensure <package name>`.
        # Setting `alwaysEnsure` to `true` emulates `use-package-always-ensure`
        # and pulls in all use-package references not explicitly disabled via
        # `:ensure nil` or `:disabled`.
        # Note that this is NOT recommended unless you've actually set
        # `use-package-always-ensure` to `t` in your config.
        alwaysEnsure = true;

        # For Org mode babel files, by default only code blocks with
        # `:tangle yes` are considered. Setting `alwaysTangle` to `true`
        # will include all code blocks missing the `:tangle` argument,
        # defaulting it to `yes`.
        # Note that this is NOT recommended unless you have something like
        # `#+PROPERTY: header-args:emacs-lisp :tangle yes` in your config,
        # which defaults `:tangle` to `yes`.
        alwaysTangle = true;

        # Optionally provide extra packages not in the configuration file.
        extraEmacsPackages = epkgs: with epkgs; [
          auctex
          vterm
          rainbow-delimiters
          pdf-tools
          jinx

          (callPackage ./site-packages/holo-layer/holo-layer.nix {
            inherit (pkgs) fetchFromGitHub;
          })

          (callPackage ./site-packages/lsp-bridge/lsp-bridge.nix {
            inherit (pkgs) fetchFromGitHub;
          })
        ];

        # Optionally override derivations.
        # override = epkgs: epkgs // {
        #    lsp-mode = epkgs.melpaPackages.lsp-mode.overrideAttrs(old: {
        #     # Apply fixes here
        #     postinstall =
        #   });
        # };
        #
        # override = epkgs: epkgs // {
        #   holo-layer = pkgs.callPackage ./site-packages/holo-layer.nix {
        #     inherit (pkgs) fetchFromGitHub;
        #     inherit (epkgs) trivialBuild;
        #   };
        # };
      };

      apps.demacs = flake-utils.lib.mkApp {
        drv = packages.demacs;
        name = "demacs";
        exePath = "/bin/emacs";
      };
      packages.default = packages.demacs;
      apps.default = apps.demacs;
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [packages.demacs] ++ dependencies;
      };
    });
}
