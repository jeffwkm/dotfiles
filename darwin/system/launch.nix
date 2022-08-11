{ config, lib, pkgs, ... }:

{
  # Set of files that have to be linked in '/Library/LaunchAgents'
  environment.launchAgents = {};
  # Set of files that have to be linked in '/Library/LaunchDaemons'
  environment.launchDaemons = {
    # ./com.jeff.pre-login.plist
  };
  # Set of files that have to be linked in '~/Library/LaunchDaemons'
  environment.userLaunchAgents = {};
}
