{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
in
  {
    gnome-session = pkgs.stdenv.mkDerivation {
      name = "gnome-session";
      src = ./Xmonad.destop;
      builder = pkgs.writeText "builder.sh" ''
        . $stdenv/setup
        mkdir -p $out/share
        mkdir -p $out/share/gnome-session/sessions
        refpkg=${pkgs.gnome3.gnome_session}
        file=share/gnome-session/sessions/gnome.session
        ln -sf $refpkg/bin $out
        ln -sf $refpkg/libexec $out
        find $refpkg/share -maxdepth 1 \
          -not -name gnome-session -exec ln -sf {} $out/share \;
        sed 's/org.gnome.Shell/Xmonad/' $refpkg/$file > $out/$file
        mkdir -p $out/share/applications
        cp $src $out/share/applications/Xmonad.desktop
      '';
      buildInputs = [ pkgs.gnome3.gnome_session pkgs.gnome3.gnome_settings_daemon ];
    };
  # xmonad = pkgs.stdenv.mkDerivation {
  #   name = "xmonad-desktop";
  #   src = ./xmonad.desktop;
    # src = pkgs.fetchFromGitHub {
    #   owner = "juselius";
    #   repo = "xmonad";
    #   rev = "28c2f74";
    #   sha256 = "10b308a7c4a6285bfff8298a3f9e7e3119c652f21fbb3d31beb92d2f8b57eee8";
    # };
  #   buildInputs = with pkgs.haskellPackages; [
  #     xmonad
  #     xmonad-extras
  #     xmonad-contrib
  #     yeganesh
  #     xmobar
  #     # taffybar
  #     pkgs.dmenu2
  #   ];
  #   buildPhase = ''
  #     ln -s . .xmonad
  #     HOME=`pwd`
  #     set +e
  #     xmonad
  #     :
  #   '';
  #   installPhase = ''
  #     mkdir -p $out/share/applications
  #     mkdir -p $out/xmonad
  #     cp $src/xmonad.desktop $out/share/applications
  #     cp -a * $out/xmonad
  #   '';
    # buildCommand = ''
    #   mkdir -p $out/share/applications
    #   cp $src $out/share/applications/xmonad.desktop
    # '';
  # };
  }
