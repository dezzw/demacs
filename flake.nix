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
      url = "github:dezzw/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
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
          git

          # Language Server
          ccls

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

      emacs-patched = (pkgs.emacs-git).overrideAttrs (old: {
        # https://github.com/cmacrae/emacs/blob/03b4223e56e10a6d88faa151c5804d30b8680cca/flake.nix#L75
        buildInputs = old.buildInputs ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [ pkgs.darwin.apple_sdk.frameworks.WebKit ];
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

      emacs-augmented =  ((pkgs.emacsPackagesFor  emacs-patched).emacsWithPackages (epkgs: with epkgs;
        [
	        gcmh
	        exec-path-from-shell
	        posframe
	        
          # UI
	        (doom-themes.overrideAttrs (final: prev: {
            patches = (prev.patches or [])
                      ++ [./patches/override-theme.patch];
          }))
	        doom-modeline
	        nerd-icons
	        ligature
	        hl-todo
	        diff-hl
	        rainbow-delimiters
	        highlight-indent-guides
	        rainbow-mode

	        # (callPackage ./site-packages/holo-layer/holo-layer.nix {
          #   inherit (pkgs) fetchFromGitHub;
          # })

	        # frame/windows management
	        beframe
	        ace-window
	        popper
	        
	        # Edit
	        evil
	        evil-nerd-commenter
	        evil-escape
	        evil-visualstar
	        evil-surround
	        evil-multiedit
	        evil-mc
	        evil-matchit
	        evil-collection
	        evil-tex

	        vundo
	        hungry-delete
	        avy

	        general

	        #dired
	        dired-single
	        dired-hide-dotfiles
	        dirvish

	        # org
	        org-superstar
	        visual-fill-column
	        valign
	        org-appear
	        # org-modern-indent
	        org-super-agenda
	        org-roam
	        org-roam-ui
	        org-download

	        # completion
	        vertico
	        orderless
	        consult
	        consult-dir
	        marginalia
	        embark
	        embark-consult
	        corfu
	        cape
	        kind-icon
	        tempel
	        citre

	        # Productivity
	        vdiff
	        helpful
	        format-all
	        restclient
	        password-store
	        pdf-tools
          
          # (callPackage ./site-packages/mind-wave/mind-wave.nix {
          #   inherit (pkgs) fetchFromGitHub;
          # })

	        # Developing Tools
	        (callPackage ./site-packages/lsp-bridge/lsp-bridge.nix {
            inherit (pkgs) fetchFromGitHub;
          })

          (callPackage ./site-packages/dape {
            inherit (pkgs) fetchFromGitHub;
          })

	        envrc

	        # language
	        web-mode
	        js2-mode
	        rjsx-mode
	        add-node-modules-path
	        scss-mode
          svelte-mode
	        nix-mode
	        sly
	        haskell-mode
	        cdlatex
	        auctex
	        markdown-mode
	        gdscript-mode
	        swift-mode
	        docker
	        emmet-mode
          zig-mode
	        
	        # term/shell
          vterm
	        multi-vterm
	        vterm-toggle
          eat

	        # eshell
	        eshell-prompt-extras
	        eshell-up
	        eshell-syntax-highlighting
	        eshell-z
	        esh-help

	        # git
	        magit
	        magit-delta

	        # irc cheet
	        circe

	        # new packages want to try
          jinx
          pkgs.emacsPackages.treesit-grammars.with-all-grammars
        ]));

      packages.demacs = emacs-augmented;
      #   pkgs.emacsWithPackagesFromUsePackage {
      #   config = ./init.el;

      #   defaultInitFile = true;

      #   package = emacs-augmented;

      #   alwaysEnsure = false;
      #   alwaysTangle = false;
      # };
      
      apps.demacs = flake-utils.lib.mkApp {
        drv = packages.demacs;
        name = "demacs";
        exePath = "/bin/emacs";
      };
      packages.default = packages.demacs;
      apps.default = apps.demacs;
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [packages.demacs] ++ dependencies;

        shellHook = ''
          if [ ! -d $HOME/.emacs.d/ ]; then
            mkdir -p $HOME/.emacs.d
            git clone git@github.com:dezzw/demacs.git $HOME/.emacs.d 
          fi
        '';
      };
    });
}
