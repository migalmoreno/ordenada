{
  lib,
  mkFeature,
  ...
}:

let
  mkOpts =
    opts:
    toString (
      lib.mapAttrsToList (
        name: value:
        "--${name} ${
          if builtins.isInt value then
            (toString value)
          else if builtins.isBool value then
            ""
          else
            "'${value}'"
        }"
      ) opts
    );
  mkSettings =
    config: with config.ordenada.features.theme.scheme.withHashtag; {
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
      fn = with config.ordenada.features.fontutils.fonts.monospace; "${name} ${toString size}";
    };
  menuPass =
    config: pkgs: menuExec:
    pkgs.stdenv.mkDerivation {
      pname = "menupass";
      version = "20250124";

      src = pkgs.fetchFromGitHub {
        owner = "aehabdelouadoud";
        repo = "menupass";
        rev = "9e94543ee43117b42e211bd0845863efc44a622b";
        sha256 = "psNyzFWUYnnDoqzshh/Nrv0pKdfQNqzZdzE7sFsoTro=";
      };

      nativeBuildInputs = [
        pkgs.makeWrapper
        pkgs.gnused
        pkgs.patch
      ];

      dontBuild = true;

      installPhase =
        let
          runtimeDeps =
            with pkgs;
            [
              coreutils
              findutils
              gnugrep
              gnused
              libnotify
              pass
            ]
            ++ (if config.ordenada.globals.wayland then [ wl-clipboard ] else [ xclip ]);
        in
        # sh
        ''
            runHook p runHook preInstall

            install -Dm755 $src/menupass.sh $out/bin/menupass
            patchShebangs $out/bin/menupass

            cat > menu <<EOF
          MENU=${lib.strings.escapeShellArg menuExec}
          EOF

            sed -i \
                -e '/^MENU=/,/--fn .*/d' \
                -e "/# Configure menu tool/r menu" \
                $out/bin/menupass

            wrapProgram $out/bin/menupass \
                        --set GNUPGHOME "${config.ordenada.features.gnupg.storeDir}" \
                        --set PASSWORD_STORE_DIR "${config.ordenada.features.password-store.storeDir}" \
                        --set WAYLAND_DISPLAY "${
                          if (config.ordenada.globals.wayland == true) then "wayland-1" else ""
                        }" \
                        --prefix PATH : ${pkgs.lib.makeBinPath runtimeDeps}

            runHook postInstall
        '';
      meta = {
        description = "A simple pass menu using bemenu";
        homepage = "https://github.com/aehabdelouadoud/menupass";
        license = pkgs.lib.licenses.mit;
        platforms = pkgs.lib.platforms.linux;
      };
    };
in
mkFeature {
  name = "bemenu";
  options =
    { config, pkgs, ... }:
    let
      inherit (lib) mkOption mkPackageOption types;
      enabled = config.ordenada.features.bemenu.enable;
    in
    {
      package = mkPackageOption pkgs "bemenu" { };
      height = mkOption {
        type = types.int;
        description = "The height of the bemenu prompt.";
        default = 34;
      };
      enableLauncher = mkOption {
        type = types.bool;
        description = "Whether to enable this feature as the global launcher.";
        default = enabled;
      };
      enablePinentry = mkOption {
        type = types.bool;
        description = "Whether to enable this feature as the global pinentry.";
        default = enabled;
      };
      enablePasswordManager = mkOption {
        type = types.bool;
        description = "Whether to enable this feature as the global password manager.";
        default = enabled;
      };
    };
  globals =
    { config, pkgs, ... }:
    let
      cfg = config.ordenada.features.bemenu;
    in
    {
      apps.launcher = lib.mkIf cfg.enableLauncher (
        lib.mkForce ''
          ${pkgs.j4-dmenu-desktop}/bin/j4-dmenu-desktop --dmenu="${cfg.package}/bin/bemenu ${mkOpts (mkSettings config)}";
        ''
      );
      apps.passwordManager = lib.mkIf cfg.enablePasswordManager (
        lib.mkForce "${
          menuPass config pkgs
            "${cfg.package}/bin/bemenu ${
              mkOpts (
                removeAttrs (mkSettings config) [
                  "cw"
                  "hp"
                  "ch"
                ]
              )
            }"
        }/bin/menupass"
      );
    };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.bemenu = {
        enable = true;
        package = config.ordenada.features.bemenu.package;
        settings = mkSettings config;
      };
      services.gpg-agent.pinentry.package = lib.mkIf config.ordenada.features.bemenu.enablePinentry (
        lib.mkForce (
          pkgs.writeShellScriptBin "pinentry-bemenu" (
            with config.ordenada.features.bemenu;
            ''
              PATH="$PATH:${pkgs.coreutils}/bin:${package}/bin"
              unset BEMENU_OPTS
              "${pkgs.pinentry-bemenu}/bin/pinentry-bemenu" ${
                mkOpts (
                  removeAttrs (mkSettings config) [
                    "cw"
                    "hp"
                    "ch"
                  ]
                )
              }
            ''
          )
        )
      );
    };
}
