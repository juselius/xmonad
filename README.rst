README
=======

This xmonad setup works with (at least) Ubuntu 14.10. The default setup is
based on plain xmonad together with xmobar. There are also configs for Gnome
and Xfce4, but they are a bit shaky at times.

Installation
-------------

::

    $ sudo apt-get install xmonad xmobar libghc-xmonad-contrib-dev
    $ sudo apt-get install trayer volti suckless-tools
    $ sudo apt-get install xcompmgr scrot conky gsimplecal
    $ sudo apt-get install xfce4-terminal

    $ git clone git@source.uit.no:jonas/xmonad ~/.xmonad
    $ ln -s .xmonad/xmobarrc .xmobarrc
    $ ln -s .xmonad/xsessionrc .xsessionrc
    $ ln -s .xmonad/conkyrc .conkyrc

    $ cabal install yeganesh

    $ cp launchbox/launchbox.py /opt/bin
    $ cd /tmp; git clone https://github.com/olemb/launchbox.git

Edit ``.xsessionrc`` to your fit your needs. Log out and choose the ``XMonad``
session.

Using unity/gnome-session
-------------------

Running xmonad under session management has some advantages:
    * Automatic startup of various services (settings manager, keyring daemon,
      etc.)
    * Controlled logout using gnome-session-quit
    * Keyboard layout indicator/selector in the system tray

To enable session management (without gnome-panel)::

    $ sudo cp .xmonad/xmonad-session.desktop /usr/share/xsessions/
    $ sudo cp .xmonad/xmonad-plain.session /usr/share/gnome-session/sessions/
    $ sudo cp .xmonad/xmonad-plain.desktop /usr/share/applications/
    $ sudo cp .xmonad/xmonad-session /usr/bin/

Themes
-------

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
