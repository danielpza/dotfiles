{ pkgs, ... }:
let configDir = "/home/daniel/.config/home-manager";
in {
  home.packages = with pkgs; [ slack firefox ];

  home.shellAliases = {
    "hm" = "home-manager switch --flake path:${configDir}#c1";
    "nu" = "nix flake update ${configDir}";
    "nr" = "sudo nixos-rebuild switch --flake path:${configDir}#nixos";
  };

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      monospace-font-name = "Source Code Pro 20";
    };
  };
}
