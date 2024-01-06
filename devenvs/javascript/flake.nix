# How to use:
# Add to the project .envrc:
# use flake path:$HOME/.config/home-manager/devenvs/javascript --impure
{
  inputs = { nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable"; };
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.permittedInsecurePackages = [ "openssl-1.1.1w" ];
      };
      lib = nixpkgs.lib;
    in {
      devShell.${system} = pkgs.mkShell {
        packages = with pkgs; [
          git
          # volta
          corepack
          docker
          google-cloud-sdk
          python3
          buf
        ];
        shellHook = ''
          # VOLTA_HOME="$HOME/.volta";
          # PATH="$VOLTA_HOME/bin/:$PATH";
          YARN_NM_MODE=hardlinks-global
        '';
        MONGOMS_DISTRO = "ubuntu-22.04";
        NIX_LD_LIBRARY_PATH = lib.makeLibraryPath
          (with pkgs; [ stdenv.cc.cc openssl_1_1 openssl_3 curlFull xz ]);
        NIX_LD =
          builtins.readFile "${pkgs.stdenv.cc}/nix-support/dynamic-linker";
      };
    };
}
