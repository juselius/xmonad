README
=======

This xmonad setup works with (at least) Ubuntu 14.04. The defualt setup is
based on plain xmonad together with xmobar. There are also configs for Gnome
and Xfce4, but they are a bit shakey at times.

Installation
-------------

::

    $ sudo apt-get install xmonad xmobar libghc-xmonad-contrib-dev
    $ sudo apt-get install trayer volti suckless-tools
    $ sudo install xcompmgr scrot gnome-do conky gsimplecal
    $ sudo apt-get install xfce4-terminal
    $ git clone git@source.uit.no:jonas/xmonad .xmonad
    $ ln -s .xmonad/xmobarrc .xmobarrc
    $ ln -s .xmonad/xsessionrc .xsessionrc
    $ ln -s .xmonad/conkyrc .conkyrc

Edit ``.xsessionrc`` to your fit your needs. Log out and choose the ``XMonad``
session.

Using gnome-session
-------------------

Running xmonad under gnome-session has some advantages:
    * Automatic startup of various services (settings manager, keyring daemon,
    etc.)
    * Controlled logout using gnome-session-quit
    * Keyboard layout indicator/selector in the system tray

To enable gnome-session (without gnome-panel)::

    $ sudo cp .xmonad/xmonad-session.desktop /usr/share/xsessions/
    $ sudo cp .xmonad/xmonad-session /usr/bin/
    $ sudo cp .xmonad/xmonad-plain.session /usr/share/gnome-session/sessions/

Themes
-------

To make things look nice, you may have to speciy the themes manually:

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
