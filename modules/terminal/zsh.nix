{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let
  inherit (config) host user;
  inherit (config.host) darwin;

  lsd_completion =
    builtins.toFile "_lsd" (builtins.readFile ../../dotfiles/_lsd);

  makeUserConfig = extra: {
    enable = true;
    sessionVariables = { };
    history = { };
    initExtraBeforeCompInit = "";
    envExtra = ''
      fpath+=( ~/.zsh/completions )
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
        "history-substring-search"
        "directory"
        "spectrum"
        "utility"
        "git"
        "archive"
        "completion"
        "fasd"
        "autosuggestions"
        "command-not-found"
        "prompt"
      ];
      pmoduleDirs = [ ../../dotfiles/zsh/prompts ];
      prompt.theme = lib.mkDefault "jeffw";
      # prompt.theme = lib.mkDefault "powerlevel10k";
      ssh.identities = [ "id_rsa" ];
      terminal.autoTitle = true;
      utility.safeOps = false;
    };

    shellAliases = {
      _ = "sudo";
      __ = "sudo -i";
      dmesg = "sudo dmesg";
      top = "sudo htop";
      ".." = "cd ..";
      e = "emacsclient -t -a emacs";
      ee = "emacs -nw";
      v = "vim";
      ### nix
      ss = "rippkgs";
      ns = "nix search nixpkgs";
      ### tmux
      tmux = "tmux -2";
      tn = "tmux new-session -s";
      tl = "tmux list-sessions";
      ta = "tmux attach -t ";
      ### lsd
      lsd = "lsd --date relative";
      lsize = "sudo lsd --date relative --total-size";
      ls = "lsd --group-dirs first";
      ll = "ls -l";
      lr = "lsd --tree";
      lls = "lsize -l --blocks user,size,date,name";
      llS = "lsize -lSr";
      llt = "lsd -ltr --no-symlink --blocks permission,user,size,date,name";
      lt = "lsd -ltr --no-symlink --blocks size,date,name";
      ltr = "lsd -lt --no-symlink --blocks size,date,name";
      lts = "lsize -ltr --blocks size,date,name";
      l = "lsd -l --blocks user,size,date,name";
      l1 = "lsd -l1 --blocks name";
      lS = "lsize -l1 --blocks size,date,name --no-symlink";
      ldu = "lsize -lSr --blocks size,date,name";
      lduq = "lsize -lSr --blocks size,name --no-symlink";
      lst = "lsd --tree";
      lsd_help = ''alias | grep -Ee "(lsd|lsize |'ll |'lt )" | cat'';
      ### git
      gwS = "git status --ignore-submodules=$_git_status_ignore_submodules";
      gws = "gwS --untracked-files=no";
      gwsa = "gws --short";
      ### mpv
      mpvf = "mpv --input-ipc-server=";
      mpv-hlg =
        "mpv --icc-profile= --icc-profile-auto=no --target-trc=hlg --target-prim=bt.2020 --target-peak=1000 --hdr-compute-peak=no";
      # mpv2 = "mpv --icc-profile=~/.local/share/icc/p2715q.icc";
      ### misc
      sr-psql = "psql -h localhost -U postgres sysrev";
      sr-psql-any = "psql -h localhost -U postgres";
      sr-tunnels = ''
        open-tunnel ubuntu sysrev.com 5470 5432
        open-tunnel ubuntu staging.sysrev.com 5471 5432
      '';
      greg = "greg -cf ~/.greg.conf";
      H = "history -50000 | grep -i";
      n = "cd ${config.host.config-dir}";
      zsh-profile = "ZPROFILE=1 zsh -i -c exit";
    } // (extra.shellAliases or { });

    shellGlobalAliases = {
      _g = "| grep";
      _ge = "| grep -Ee";
      _gi = "| grep -i";
      _G = "| grep -iEe";
      _cl = "| wc -l";
      _cb = "| wc -c";
      _h = "| head -n";
      _t = "| tail -n";
      _l = "| less";
      EC2AUTH = "-O $AWS_ACCESS_KEY_ID -W $AWS_SECRET_ACCESS_KEY";
      # mpv options
      V = "--volume";
      V50 = "--volume=50";
      SHF = "--shuffle";
      FS0 = "--fs --fs-screen=0";
      FS1 = "--fs --fs-screen=1";
      FS2 = "--fs --fs-screen=2";
      F24 = "--display-fps=24";
      HLG =
        "--target-trc=hlg --target-prim=bt.2020 --hdr-compute-peak=yes --tone-mapping=hable";
      SVP = "--input-ipc-server=/tmp/mpvsocket";
      NOSVP = "--input-ipc-server=";
      ICC = "--icc-profile-auto=yes";
      NOICC = "--icc-profile-auto=no";
    } // (extra.shellGlobalAliases or { });

    initExtraFirst = ''
      export USING_P10K=0
      [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return
      export ZSHCONFIG="${config.host.config-dir}/dotfiles/zsh"
      fpath+=( "$ZSHCONFIG/prompts" "${config.user.home}/.zsh/completions" )
      ${extra.initExtraFirst or ""}
      [[ -n "$ZPROFILE" ]] && zmodload zsh/zprof
      if [[ "$USING_P10K" -eq 1 ]]; then
        if [[ -r "$HOME/.cache/p10k-instant-prompt-$USER.zsh" ]]; then
          source "$HOME/.cache/p10k-instant-prompt-$USER.zsh"
        fi
      fi
    '';

    initExtra = ''
      function _zsh_nix_profiles() {
        echo "$(realpath ~/.nix-profile)"
      }

      export _ZSH_NIX_PROFILES="$(_zsh_nix_profiles)"

      source ${lsd_completion} 2> /dev/null

      if [[ "$USING_P10K" -ne 1 ]] ; then
        setopt PROMPT_CR
      fi
      setopt PROMPT_SP

      [[ -n "$ZPROFILE" ]] && zprof

      if [[ "$USING_P10K" -eq 1 ]]; then
        [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
        p10k finalize
      fi

      ${extra.initExtra or ""}
    '';
  };

  userLinuxConfig = makeUserConfig {
    shellAliases = {
      iotop = "sudo iotop";
      virsh = "VISUAL='emacs -nw' sudo -E virsh";
      ### systemctl
      sd = "sudo systemctl";
      sds = "sd status";
      sdr = "sd restart";
      ### systemctl --user
      sdu = "systemctl --user";
      sdus = "sdu status";
      sdur = "sdu restart";
      ### systemctl daemon-reload
      sdd = ''
        echo -n 'daemon-reload:'
        sudo systemctl daemon-reload
        echo -n ' [system]'
        systemctl --user daemon-reload
        echo -n ' [user]'
        echo'';
      ### cpupower
      cpinfo = "sudo cpupower frequency-info";
      cpgov = "sudo cpupower frequency-set -g";
    };
    initExtraFirst = ''
      export NIXOS=/etc/nixos
    '';
    initExtra = "";
  };
  userDarwinConfig = makeUserConfig {
    shellAliases = {
      brew = "/opt/homebrew/bin/brew";
      ibrew = "arch -x86_64 /usr/local/homebrew/bin/brew";
      mv = "/bin/mv";
    };
    initExtraFirst = "";
    initExtra = ''
      if [ -z $DISPLAY ]; then export DISPLAY=:0.0; fi
    '';
  };
  userConfig = if darwin then userDarwinConfig else userLinuxConfig;
  cfg = config.modules.zsh;
in {
  options.modules.zsh = { enable = mkBoolOpt true; };

  config = mkIf cfg.enable {
    programs.zsh.enable = true;
    environment.shells = with pkgs; [ zsh bash ];
    nixpkgs.overlays = [ (final: prev: { zsh = optimize config prev.zsh; }) ];

    home-manager.users.${user.name} = {
      programs.zsh = userConfig;
      home.packages = with pkgs; [
        nix-zsh-completions
        starship
        zsh-better-npm-completion
      ];
    };
  };
}
