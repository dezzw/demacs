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
                rev = "0756b1f2f5452d715396f66d887c137776e360ca";
                hash = "sha256-nMGNkjxJMV2DQqI4YFeV8vK2cIvYeccUOX+h210wop4=";
              };
              
              configureFlags = (old.configureFlags or [ ]) ++ [
                # "--with-xwidgets" # withXwidgets failed with mps enabled
                "--with-mps"
                "--with-native-compilation=aot"
              ];

              # NIX_CFLAGS_COMPILE = "${old.NIX_CFLAGS_COMPILE or ""} -I${mps}/include";
              # NIX_LDFLAGS = "${old.NIX_LDFLAGS or ""} -L${mps}/lib";

              # preConfigure = ''
              #   export CPPFLAGS="-I${mps}/include $CPPFLAGS}"
              #   export LDFLAGS="-L${mps}/lib $LDFLAGS"
              # '';

              buildInputs =
                old.buildInputs
                ++ [mps];
              # ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [ pkgs.darwin.apple_sdk_11_0.frameworks.WebKit ];

              plistBuddy = "/usr/libexec/PlistBuddy";
              
              postInstall = old.postInstall + ''
                app="$out/Applications/Emacs.app"
                plist="$app/Contents/Info.plist"
                path="/opt/homebrew/bin:/opt/homebrew/sbin:/Users/dez/.nix-profile/bin:/etc/profiles/per-user/dez/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/usr/sbin:/bin:/sbin:/Users/dez/.orbstack/bin:/Users/dez/Library/Application Support/JetBrains/Toolbox/scripts:/Users/dez/.local/bin:/Users/dez/.zsh/plugins/powerlevel10k:/Users/dez/.zsh/plugins/autopair:/Users/dez/.zsh/plugins/zsh-nix-shell"

                echo "Injecting PATH into $plist"

                # Use plistBuddy to modify the plist file
                ${plistBuddy} -c 'Add :LSEnvironment dict' "$plist" || true
                ${plistBuddy} -c "Add :LSEnvironment:PATH string '$path'" "$plist" || \
                ${plistBuddy} -c "Set :LSEnvironment:PATH '$path'" "$plist"

                # Update modification time
                touch "$app"
              '';

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
                # ./patches/blur.patch
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
