{ config, lib, pkgs, ... }: {
  programs.zsh = {
    enable = true;
    sessionVariables = { };
    history = { };

    envExtra = "";
    profileExtra = "";
    loginExtra = "";
    logoutExtra = "";

    prezto = {
      enable = true;
      pmodules = [
        "environment"
        "terminal"
        "editor"
        "history"
        "directory"
        "spectrum"
        "utility"
        "git"
        "completion"
        "autosuggestions"
        "command-not-found"
        "prompt"
      ];
      # pmoduleDirs = [ "${config.programs.home-manager.path}/dotfiles/zsh/prompts" ];
      pmoduleDirs = [ ../../../dotfiles/zsh/prompts ];
      prompt.theme = "jeffw";
      ssh.identities = [ "id_rsa" ];
      terminal.autoTitle = true;
      utility.safeOps = false;
      # gnuUtility.prefix = "g";
      extraConfig = "";
    };
  };
}
