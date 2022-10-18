{ config, lib, pkgs, ... }: {
  systemd.user.services.ssh-agent = {
    Unit = { Description = "SSH key agent"; };
    Install = { WantedBy = [ "default.target" ]; };
    Service = {
      Type = "forking";
      Environment = "SSH_AUTH_SOCK=%t/ssh-agent.socket";
      ExecStart = "${pkgs.openssh}/bin/ssh-agent -a $SSH_AUTH_SOCK";
    };
  };

  systemd.user.services.ssh-add = {
    Unit = {
      Description = "SSH key add";
      Requires = [ "ssh-agent.service" ];
      After = [ "ssh-agent.service" ];
    };
    Install = { WantedBy = [ "default.target" ]; };
    Service = {
      Type = "oneshot";
      Environment = "SSH_AUTH_SOCK=%t/ssh-agent.socket";
      ExecStart = "${pkgs.bash}/bin/bash %h/bin/do-ssh-add";
    };
  };
}
