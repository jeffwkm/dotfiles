{ yabai, apps, keys, modifiers, appKeys, commands, extra }:
with keys;
with modifiers;
with apps; {
  cfg = ''
    # launch apps
    # ${launch} - ${appKeys.terminal} : ${commands.launchTerminal}
    # focus apps
    # ${focus} - ${appKeys.editor}    : ${commands.activateApp} ${editor}
    # ${focus} - ${appKeys.browser}   : ${commands.activateApp} ${browser}
    # ${focus} - ${appKeys.terminal}  : ${commands.activateApp} ${terminal}
    # ${focus} - ${appKeys.music}     : ${commands.activateApp} ${music}
    # ${focus} - ${appKeys.video}     : ${commands.activateApp} ${video}
    # ${focus} - l                    : ${commands.activateApp} Slack
    # focus window
    ${focus} - ${prevAlt}           : ${yabai} -m window --focus stack.prev
    ${focus} - ${nextAlt}           : ${yabai} -m window --focus stack.next
    # ${focus} - ${recent}            : ${yabai} -m window --focus recent
    # ${focus} - h                    : ${yabai} -m window --focus west
    # ${focus} - j                    : ${yabai} -m window --focus south
    # ${focus} - k                    : ${yabai} -m window --focus north
    # ${focus} - l                    : ${yabai} -m window --focus east
    # ${focus} + shift - h                    : ${yabai} -m window --swap west
    # ${focus} + shift - j                    : ${yabai} -m window --swap south
    # ${focus} + shift - k                    : ${yabai} -m window --swap north
    # ${focus} + shift - l                    : ${yabai} -m window --swap east
    # cmd + ctrl - left : ${yabai} -m window --focus west
    # cmd + ctrl - down : ${yabai} -m window --focus south
    # cmd + ctrl - up : ${yabai} -m window --focus north
    # cmd + ctrl - right : ${yabai} -m window --focus east
    cmd - left : ${yabai} -m window --focus west
    cmd - down : ${yabai} -m window --focus south
    cmd - up : ${yabai} -m window --focus north
    cmd - right : ${yabai} -m window --focus east
    cmd + shift - left : ${yabai} -m window --swap west
    cmd + shift - down : ${yabai} -m window --swap south
    cmd + shift - up : ${yabai} -m window --swap north
    cmd + shift - right : ${yabai} -m window --swap east
    # focus space
    # alt + ctrl - ${recent}          : ${yabai} -m space --focus recent
    ${focus} - ${prev}              : ${yabai} -m space --focus prev
    ${focus} - ${next}              : ${yabai} -m space --focus next
    ${focus} - left                 : ${yabai} -m space --focus prev
    ${focus} - right                : ${yabai} -m space --focus next
    ${focus} - ${first}             : ${yabai} -m space --focus first
    ${focus} - ${last}              : ${yabai} -m space --focus last
    cmd - 1                    : ${yabai} -m space --focus 1
    cmd - 2                    : ${yabai} -m space --focus 2
    cmd - 3                    : ${yabai} -m space --focus 3
    cmd - 4                    : ${yabai} -m space --focus 4
    cmd - 5                    : ${yabai} -m space --focus 5
    cmd - 6                    : ${yabai} -m space --focus 6
    cmd - 7                    : ${yabai} -m space --focus 7
    cmd - 8                    : ${yabai} -m space --focus 8
    cmd - 9                    : ${yabai} -m space --focus 9
    cmd - 0                    : ${yabai} -m space --focus 10
    alt - 1                    : ${yabai} -m space --focus 11
    alt - 2                    : ${yabai} -m space --focus 12
    alt - 3                    : ${yabai} -m space --focus 13
    alt - 4                    : ${yabai} -m space --focus 14
    alt - 5                    : ${yabai} -m space --focus 15
    alt - 6                    : ${yabai} -m space --focus 16
    alt - 7                    : ${yabai} -m space --focus 17
    alt - 8                    : ${yabai} -m space --focus 18
    alt - 9                    : ${yabai} -m space --focus 19
    alt - 0                    : ${yabai} -m space --focus 20
    # focus display
    ${focus} - z                    : ${yabai} -m display --focus prev
    ${focus} - c                    : ${yabai} -m display --focus next
    # move window to space
    ${move} - 1                     : ${yabai} -m window --space  1; ${yabai} -m space --focus 1
    ${move} - 2                     : ${yabai} -m window --space  2; ${yabai} -m space --focus 2
    ${move} - 3                     : ${yabai} -m window --space  3; ${yabai} -m space --focus 3
    ${move} - 4                     : ${yabai} -m window --space  4; ${yabai} -m space --focus 4
    ${move} - 5                     : ${yabai} -m window --space  5; ${yabai} -m space --focus 5
    ${move} - 6                     : ${yabai} -m window --space  6; ${yabai} -m space --focus 6
    ${move} - 7                     : ${yabai} -m window --space  7; ${yabai} -m space --focus 7
    ${move} - 8                     : ${yabai} -m window --space  8; ${yabai} -m space --focus 8
    ${move} - 9                     : ${yabai} -m window --space  9; ${yabai} -m space --focus 9
    ${move} - 0                     : ${yabai} -m window --space 10; ${yabai} -m space --focus 10
    # move space to display
    # ${display} - ${recent}          : ${yabai} -m space --display recent
    ${display} - ${prev}            : ${yabai} -m space --display prev
    ${display} - ${next}            : ${yabai} -m space --display next
    # move window to display
    # ${display} - ${recent}        : ${yabai} -m window --display recent; ${yabai} -m display --focus recent
    ${display} - ${prevAlt}         : ${yabai} -m window --display prev; ${yabai} -m display --focus prev
    ${display} - ${nextAlt}         : ${yabai} -m window --display next; ${yabai} -m display --focus next
    # toggle window sticky (show on all spaces)
    # ${misc} - ${sticky}             : ${yabai} -m window --toggle sticky
    # toggle window topmost (keep above other windows)
    # ${misc} - ${top}                : ${yabai} -m window --toggle topmost
    alt - f :                       : ${yabai} -m window --toggle float
    # change layout for space
    ${layout} - ${stack}            : ${yabai} -m space --layout stack
    ${layout} - ${float}            : ${yabai} -m space --layout float
    ${layout} - ${bsp}              : ${yabai} -m space --layout bsp
    # resize floating window - full screen, left half, right half
    # ${resize} - ${up}               : ${yabai} -m window --grid 1:1:0:0:1:1
    # ${resize} - ${left}             : ${yabai} -m window --grid 1:2:0:0:1:1
    # ${resize} - ${right}            : ${yabai} -m window --grid 1:2:1:0:1:1
    # reorder stacked windows
    #cmd + ctrl + shift - left       : ${yabai} -m window --swap stack.prev
    #cmd + ctrl + shift - right      : ${yabai} -m window --swap stack.next

    # alt - p : ${yabai} -m window --toggle pip ;\
    #           ${yabai} -m window --toggle topmost ;\
    #           ${yabai} -m window --toggle sticky

    ${extra}
    cmd + ctrl - o     : ~/bin/disp -s mac -t windows
  '';
}
