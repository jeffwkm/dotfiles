{ config, lib, pkgs, ... }: {
  programs.zsh = {
    enable = true;
    sessionVariables = { };
    history = { };

    envExtra = ''
      fpath+=( ~/.zsh/completions )
      export FPATH
    '';
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
      pmoduleDirs = [ ../../dotfiles/zsh/prompts ];
      prompt.theme = lib.mkDefault "jeffw";
      ssh.identities = [ "id_rsa" ];
      terminal.autoTitle = true;
      utility.safeOps = false;
      # gnuUtility.prefix = "g";
      extraConfig = "";
    };
  };
}
