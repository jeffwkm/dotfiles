{ config, lib, pkgs, ... }: {
  home.packages = with pkgs; [
    adapta-gtk-theme
    adementary-theme
    arc-icon-theme
    arc-theme
    ant-theme
    base16-builder # :: Base16 Builder is a nimble command-line tool that generates themes for your favourite programs.
    base16-shell-preview # :: Browse and preview Base16 Shell themes in your terminal
    beauty-line-icon-theme
    canta-theme
    capitaine-cursors # :: An x-cursor theme inspired by macOS and based on KDE Breeze
    clearlooks-phenix # :: GTK3 port of the Clearlooks theme
    equilux-theme
    faba-icon-theme
    flat-remix-gtk
    flat-remix-icon-theme
    #gnome.adwaita-icon-theme
    #gnome-breeze
    #gnome-icon-theme
    gruvbox-dark-gtk
    gtk-engine-murrine
    gtk_engines
    humanity-icon-theme
    juno-theme
    lounge-gtk-theme
    kora-icon-theme
    lambda-mod-zsh-theme # :: ??
    marwaita
    matcha-gtk-theme
    materia-theme
    mojave-gtk-theme
    moka-icon-theme
    nordic
    numix-cursor-theme
    numix-gtk-theme
    numix-icon-theme
    numix-icon-theme-circle
    numix-icon-theme-square
    numix-solarized-gtk-theme
    numix-sx-gtk-theme
    openzone-cursors
    orchis
    pantheon.elementary-icon-theme
    paper-gtk-theme
    paper-icon-theme
    papirus-icon-theme
    papirus-maia-icon-theme
    plano-theme
    plasma5Packages.qtstyleplugin-kvantum
    plata-theme
    pop-gtk-theme
    pop-icon-theme
    qogir-icon-theme
    qogir-theme
    shades-of-gray-theme
    sierra-gtk-theme
    skeu
    snowblind
    solarc-gtk-theme
    sound-theme-freedesktop
    stilo-themes
    sweet
    tela-icon-theme
    theme-obsidian2
    theme-vertex
    ubuntu-themes
    venta
    vimix-gtk-themes
    vivid # :: A generator for LS_COLORS with support for multiple color themes
    yaru-theme # :: Ubuntu community theme 'yaru' - default Ubuntu theme since 18.10
    zuki-themes
  ];
}
