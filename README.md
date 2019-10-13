# README

The default setup is
based on plain xmonad together with xmobar. There are also configs for Gnome
and Xfce4, but they are a bit shaky at times.

## Installation

Edit ``.xsession`` to your fit your needs. Log out and choose the ``XMonad``
session.

## Using gnome-session

Running xmonad under session management has some advantages:
    * Automatic startup of various services (settings manager, keyring daemon,
      etc.)
    * Controlled logout using gnome-session-quit
    * Keyboard layout indicator/selector in the system tray

## Themes

To make things look nice, you may have to specify the themes manually:

``~/.gtkrc-2.0``::

    gtk-theme-name = "Adwaita"
    gtk-icon-theme-name = "Ubuntu-mono-dark"
    gtk-font-name = "Ubuntu 11"

``~/.config/gtk-3.0/settings.ini``::

    [Settings]
    gtk-application-prefer-dark-theme=0
    gtk-theme-name = "Adwaita"
    gtk-icon-theme-name = "Ubuntu-mono-dark"
    gtk-font-name = "Ubuntu 11"
