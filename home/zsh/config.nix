{ config, extra }:
let lsd_completion = builtins.toFile "_lsd" (builtins.readFile ./_lsd);
in {
  cfg = {
    shellAliases = {
      _ = "sudo";
      __ = "sudo -i";
      dmesg = "sudo dmesg";
      top = "sudo htop";
      ".." = "cd ..";
      e = "emacsclient -t -a emacs";
      ee = "emacs -nw";
      # vim = "e";
      v = "vim";
      ### nix
      ss = "nix search nixpkgs";
      ### fd
      fd = "fd -HIL";
      fdd = "env fd";
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
      n = "cd ${config.home.local.nix-repo-path}";
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
      [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return
      export ZSHCONFIG="${config.home.local.nix-repo-path}/dotfiles/zsh"
      fpath+=( "$ZSHCONFIG/prompts" "${config.home.homeDirectory}/.zsh/completions" )
      ${extra.initExtraFirst or ""}
      [[ -n "$ZPROFILE" ]] && zmodload zsh/zprof
    '';

    initExtra = ''
      function _zsh_nix_profiles() {
        echo "$(realpath ~/.nix-profile)"
      }

      export _ZSH_NIX_PROFILES="$(_zsh_nix_profiles)"

      source ${lsd_completion} 2> /dev/null

      eval "$(rbenv init -)"

      setopt PROMPT_CR
      setopt PROMPT_SP

      [[ -n "$ZPROFILE" ]] && zprof

      ${extra.initExtra or ""}
    '';
  };
}
