{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
    xmonad
    xmonad-contrib
    xmonad-extras
    yeganesh
    taffybar
  ]);

in
  {
    gnome-session = pkgs.stdenv.mkDerivation {
      name = "gnome-session";
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
        sed 's/org.gnome.Shell/xmonad/' $refpkg/$file > $out/$file
      '';
      buildInputs = [ pkgs.gnome3.gnome_session ];
    };
  xmonad = pkgs.stdenv.mkDerivation {
    name = "xmonad";
    src = pkgs.fetchFromGitHub {
      owner = "juselius";
      repo = "xmonad";
      rev = "28c2f74";
      sha256 = "10b308a7c4a6285bfff8298a3f9e7e3119c652f21fbb3d31beb92d2f8b57eee8";
    };
    buildInputs = with pkgs.haskellPackages; [
      xmonad
      xmonad-extras
      xmonad-contrib
      yeganesh
      taffybar
      pkgs.dmenu2
    ];
    buildPhase = ''
      eval $(egrep '^export' ${ghc}/bin/ghc)
      ln -s . .xmonad
      HOME=`pwd`
      set +e
      xmonad
      :
    '';
    installPhase = ''
      mkdir -p $out/share/applications
      mkdir -p $out/xmonad
      cp $src/xmonad.desktop $out/share/applications
      cp -a * $out/xmonad
    '';
  };
  }
