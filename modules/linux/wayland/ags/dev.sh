#!/usr/bin/env bash
fd . -e .ts ~/.config/ags | entr -rs "ags $*"
