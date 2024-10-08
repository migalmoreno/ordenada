{
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.lib.ordenada;

{
  options = {
    ordenada.features.bemenu = {
      enable = lib.mkEnableOption "the bemenu feature";
      height = lib.mkOption {
        type = lib.types.int;
        description = "The height of the dmenu prompt.";
        default = 34;
      };
    };
  };
  config = {
    home-manager = mkHomeConfig config "bemenu" (user: {
      programs.bemenu = {
        enable = true;
        settings = with user.features.theme.scheme.withHashtag; {
          line-height = 34;
          ignorecase = true;
          hp = 10;
          cw = 1;
          ch = 20;
          tf = base05;
          tb = base02;
          ff = base05;
          fb = base01;
          nf = base05;
          nb = base01;
          af = base05;
          ab = base01;
          cf = base05;
          cb = base01;
          hf = base01;
          hb = base0D;
          fn = "${user.features.fontutils.fonts.monospace.name} 11";
        };
      };
      wayland.windowManager.sway.config.menu = with user.features.theme.scheme.withHashtag; ''
        ${pkgs.j4-dmenu-desktop}/bin/j4-dmenu-desktop \
         --dmenu="${pkgs.bemenu}/bin/bemenu -i -H ${toString user.features.bemenu.height} \
         --fn '${user.features.fontutils.fonts.monospace.name} ${toString user.features.fontutils.fonts.monospace.size}' \
         --hp 10 --cw 1 --ch 20 \
         --tf '${base05}' --tb '${base02}' --ff '${base05}' --fb '${base01}' \
         --nf '${base05}' --nb '${base01}' --af '${base05}' --ab '${base01}' \
         --cf '${base05}' --cb '${base01}' --hf '${base01}' --hb '${base0D}'"
      '';
    });
  };
}
