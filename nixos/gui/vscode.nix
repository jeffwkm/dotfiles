{ config, lib, pkgs, ... }: {
  environment.sessionVariables = { NIXOS_OZONE_WL = "1"; };
  environment.systemPackages = [
    (pkgs.vscode-with-extensions.override {
      vscode = pkgs.vscodium;
      vscodeExtensions = with pkgs.vscode-extensions;
        [
          bbenoist.nix
          betterthantomorrow.calva
          brettm12345.nixfmt-vscode
          codezombiech.gitignore
          editorconfig.editorconfig
          graphql.vscode-graphql
          kahole.magit
          ms-vscode-remote.remote-ssh
          matklad.rust-analyzer
        ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [{
          name = "remote-ssh-edit";
          publisher = "ms-vscode-remote";
          version = "0.47.2";
          sha256 = "1hp6gjh4xp2m1xlm1jsdzxw9d8frkiidhph6nvl24d0h8z34w49g";
        }];
    })
  ];
}
