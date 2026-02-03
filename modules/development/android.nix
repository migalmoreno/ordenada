{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:
let
  inherit (lib)
    mkIf
    mkOption
    mkMerge
    types
    ;
in

mkFeature {
  name = "android";
  options =
    { config, ... }:
    {
      fdroidKey = mkOption {
        type = types.str;
        description = "Keybinding for fdroid.el map operations.";
        default = "f";
      };

      allowUnfreeAndAcceptLicenses = mkOption {
        type = types.bool;
        description = "Whether to allow unfree packages and accept the various android licenses.";
        default = false;
      };

      androidUserHome = mkOption {
        type = types.str;
        description = "Directory for storing android related data like AVDs, SDKs, etc.";
        default = "${config.ordenada.features.xdg.baseDirs.dataHome}/android";
      };

      sdks = mkOption {
        type = types.listOf (
          types.submodule {
            options = {
              platformVersion = mkOption {
                type = types.str;
                description = "The Android platform level to use.";
                default = "35";
              };
              platformToolsVersion = mkOption {
                type = types.nullOr types.str;
                description = "The Android platform tools version to use.";
                default = null;
              };
              toolsVersion = mkOption {
                type = types.nullOr types.str;
                description = "The Android tools version to use.";
                default = null;
              };
              buildToolsVersion = mkOption {
                type = types.nullOr types.str;
                description = "The build tools version to use.";
                default = null;
              };
              cmdLineToolsVersion = mkOption {
                type = types.nullOr types.str;
                description = "The command line tools version to use.";
                default = null;
              };
            };
          }
        );
        description = "List of SDKs to install. Possible option per SDK are `platformVersion`, `cmdLineToolsVersion`, `toolsVersion`, `platformToolsVersion` and `buildToolsVersion`.";
        default = [ ];
        example = [
          { platformVersion = "35"; }
          { platformVersion = "36"; }
        ];
      };

      activeSdkVersion = mkOption {
        type = types.nullOr types.str;
        description = "Version of actively used SDK.";
        default = null;
        example = "36";
      };

      extraSdkConfig = mkOption {
        type = types.attrs;
        description = "Extra configration for each SDK.";
        default = { };
        example = {
          useGoogleAPIs = true;
        };
      };

      emulators = mkOption {
        type = types.listOf (
          types.submodule {
            options = {
              name = mkOption {
                type = types.nullOr types.str;
                description = "Name of the emulator (e.g. Small phone)";
                default = null;
              };
              persistentData = lib.mkOption {
                type = lib.types.bool;
                description = ''
                  Whether the emulator's AVD data should be saved persistently.

                  If this option is set to `false`, the emulator will be isolated from
                  the rest of the environment. Tools like `avdmanager` won't be able to
                  see it. However it can be started through the terminal or the launcher.
                  The emulator will be set up from scratch every time it's started. No
                  data will be persisted between boots.
                  Use this if you need an isolated Android environment (e.g. for tests).

                  If this option is set to `true`, the emulator will store its data in
                  `ANDROID_AVD_HOME`. It can be seen and interacted with by other tools.
                  Use this if you need a day-to-day emulator for application development.
                '';
                default = false;
              };
              platformVersion = mkOption {
                type = types.str;
                description = "Android platform version (e.g. 35)";
              };
              abiVersion = mkOption {
                type = types.str;
                description = "ABI platform version (e.g. arm64-v8a)";
                default = if (config.ordenada.globals.platform == "darwin") then "arm64-v8a" else "x86_64";
              };
              systemImageType = mkOption {
                type = types.nullOr types.str;
                description = "System Image type (e.g. google_apis_playstore)";
                default = "google_apis_playstore";
              };
              width = mkOption {
                type = types.str;
                description = "Width of the AVD in px (e.g. \"500\")";
                default = "1280";
              };
              height = mkOption {
                type = types.str;
                description = "height of the AVD in px (e.g. \"1000\")";
                default = "2856";
              };
              density = mkOption {
                type = types.str;
                description = "density of the AVD in PPI (e.g. \"350\")";
                default = "495";
              };
              extraConfig = mkOption {
                type = types.attrs;
                description = ''
                  Extra options to add to the AVD config. Note that the following options:
                    "hw.gpu.enabled" = "true";
                    "hw.gpu.mode" = "host";
                    "hw.ramSize" = "2048";
                  are set by default, but can be overwritten using this option.
                '';
                default = { };
              };
            };
          }
        );
        default = [ ];
        description = "List of emulators to create.";
      };

    };
  nixos =
    { pkgs, ... }:
    {
      programs.adb.enable = true;
      virtualisation.waydroid.enable = true;
      environment.systemPackages = [ pkgs.wl-clipboard ];
      ordenada.features.userInfo.extraGroups = [ "adbusers" ];
    };
  homeManager =
    { config, pkgs, ... }:
    let
      features = config.ordenada.features;
      cfg = features.android;
      emacs-fdroid = pkgs.emacsPackages.melpaBuild {
        pname = "fdroid";
        version = "0.1.1";
        src = pkgs.fetchFromGitHub {
          owner = "migalmoreno";
          repo = "fdroid.el";
          rev = "94c2011630886011e15d37c163f7f59f2a045db0";
          hash = "sha256-6bSxjZOAZ8PpiaqbKdE2elobGgwmt1B8KZ7I9f+mhh0=";
        };
        packageRequires = with pkgs.emacsPackages; [ embark ];
      };

      androidPkgs = import pkgs.path {
        system = pkgs.system;
        config = {
          allowUnfree = true;
          android_sdk.accept_license = true;
        };
      };

      sdkRoot = "${cfg.androidUserHome}/sdk";
      avdRoot = "${cfg.androidUserHome}/avd";
      activeSdkRoot =
        if (cfg.activeSdkVersion != null) then
          "${sdkRoot}/${cfg.activeSdkVersion}/libexec/android-sdk"
        else
          null;
      activeNdkRoot = if (activeSdkRoot != null) then "${activeSdkRoot}/ndk-bundle" else null;

      latestOr = x: s: if (x ? s) then x else "latest";

      mkAndroidEnv =
        sdkConfig:
        let
          androidEnv = androidPkgs.androidenv;
          baseConfig = {
            includeSystemImages = true;
            includeEmulator = true;
            includeNDK = true;

            platformVersions = [ sdkConfig.platformVersion ];
            cmdLineToolsVersion = latestOr sdkConfig "cmdLineToolsVersion";
            toolsVersion = latestOr sdkConfig "toolsVersion";
            platformToolsVersion = latestOr sdkConfig "platformToolsVersion";
            buildToolsVersions =
              if (sdkConfig.buildToolsVersion != null) then [ sdkConfig.buildToolsVersion ] else [ "${sdkConfig.platformVersion}.0.0" ];

            extraLicenses = [
              "android-sdk-license"
              "android-sdk-preview-license"
              "android-googletv-license"
              "android-sdk-arm-dbt-license"
              "google-gdk-license"
              "intel-android-extra-license"
              "intel-android-sysimage-license"
              "mips-android-sysimage-license"
            ];
          };

          mergedConfig = lib.recursiveUpdate baseConfig features.android.extraSdkConfig;
          composition = androidEnv.composeAndroidPackages mergedConfig;
        in
        composition;

      sanitizeName =
        s:
        let
          regexSplit = builtins.split "[[:space:]]+" s;
          filterParts = x: if (builtins.isString x && x != "") then [ x ] else [ ];

        in
        builtins.concatStringsSep "-" (builtins.concatMap filterParts regexSplit);

      mkEmulator =
        emu:
        let
          emuSpec = if emu.name != null then sanitizeName emu.name else "${emu.platformVersion}";
          name = "android-emulator-${emuSpec}";

          emulator = androidPkgs.androidenv.emulateApp {
            name = name;
            deviceName = name;

            configOptions = {
              "hw.gpu.enabled" = "true";
              "hw.gpu.mode" = "host";
              "hw.ramSize" = "2048";
              "hw.keyboard" = "true";

              "hw.lcd.height" = emu.height;
              "hw.lcd.width" = emu.width;
              "hw.lcd.density" = emu.density;
            }
            // emu.extraConfig;

            androidUserHome = if (emu.persistentData == true) then cfg.androidUserHome else null;
            androidAvdHome = if (emu.persistentData == true) then avdRoot else null;
            avdHomeDir = if (emu.persistentData == true) then avdRoot else null;

            platformVersion = emu.platformVersion;
            abiVersion = emu.abiVersion;
            systemImageType = emu.systemImageType;
          };
        in
        pkgs.writeShellScriptBin name ''
          unset ANDROID_HOME
          unset ANDROID_SDK_ROOT
          exec ${emulator}/bin/run-test-emulator "$@"
        '';

      activeAndroidEnv = mkAndroidEnv (
        lib.findFirst (sdk: sdk.platformVersion == cfg.activeSdkVersion) null cfg.sdks
      );
      activePlatformTools = builtins.trace activeAndroidEnv (
        if (cfg.activeSdkVersion != null) then activeAndroidEnv.platform-tools else null
      );

      mkEmulatorDesktopEntry =
        emu: emuPkg:
        let
          emuSpec = if emu.name != null then "${emu.name}" else emu.platformVersion;
          emuName = "Android Emulator \(${emuSpec}\)";
          emuExe = builtins.baseNameOf (lib.getExe emuPkg);
          emuAndroidEnv = mkAndroidEnv ({ platformVersion = emu.platformVersion; buildToolsVersion = null; });
        in
        {
          name = "android-emulator-${emuSpec}";
          value = {
            name = emuName;
            genericName = "Emulator";
            icon = "${emuAndroidEnv.androidsdk}/libexec/android-sdk/platforms/android-${emu.platformVersion}/templates/ic_launcher_xhdpi.png";
            exec = "${emuPkg}/bin/${emuExe} %U";
            terminal = false;
            categories = [
              "Development"
              "Emulator"
            ];
            mimeType = [ "application/vnd.android.package-archive" ];
          };
        };

      mkEmulatorAppBundle =
        emu: emuPkg:
        let
          emuSpec = if emu.name != null then emu.name else emu.platformVersion;
          name = "Android Emulator \(${emuSpec}\)";
          emuExe = builtins.baseNameOf (lib.getExe emuPkg);
          emuAndroidEnv = mkAndroidEnv ({ platformVersion = emu.platformVersion; buildToolsVersion = null; });
          pngIcon = "${emuAndroidEnv.androidsdk}/libexec/android-sdk/platforms/android-${emu.platformVersion}/templates/ic_launcher_xhdpi.png";
        in
        pkgs.runCommand name
          {
            nativeBuildInputs = [
              pkgs.libicns
              pkgs.imagemagick
            ];
          }
          ''
            APP_DIR="$out/Applications/${name}.app/Contents"
            mkdir -p "$APP_DIR/MacOS"
            mkdir -p "$APP_DIR/Resources"

            convert "${pngIcon}" -resize 512x512 -background none -gravity center -extent 512x512 icon.png
            png2icns "$APP_DIR/Resources/icon.icns" icon.png

            ln -s "${emuPkg}/bin/${emuExe}" "$out/Applications/${name}.app/Contents/MacOS/${name}"

            cat <<EOF > "$APP_DIR/Info.plist"
                <?xml version="1.0" encoding="UTF-8"?>
                <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
                <plist version="1.0">
                <dict>
                    <key>CFBundleExecutable</key>
                    <string>${name}</string>
                    <key>CFBundleIconFile</key>
                    <string>icon.icns</string>
                    <key>CFBundleIdentifier</key>
                    <string>org.ordenada.${sanitizeName name}</string>
                    <key>CFBundleName</key>
                    <string>${name}</string>
                    <key>CFBundlePackageType</key>
                    <string>APPL</string>
                </dict>
                </plist>
                EOF
          '';

      emulators = lib.map mkEmulator cfg.emulators;
      emulatorAppBundles = lib.lists.imap0 (
        i: emu: mkEmulatorAppBundle emu (builtins.elemAt emulators i)
      ) cfg.emulators;
      emulatorDesktopEntries = builtins.listToAttrs (
        lib.lists.imap0 (i: emu: mkEmulatorDesktopEntry emu (builtins.elemAt emulators i)) cfg.emulators
      );

      mkSdkLink = sdkConfig: {
        name = "android/sdk/${toString sdkConfig.platformVersion}";
        value = {
          source = (mkAndroidEnv sdkConfig).androidsdk;
        };
      };
    in
    {
      assertions = [
        (mkIf ((builtins.length cfg.sdks > 0) && (cfg.activeSdkVersion != null)) {
          assertion = lib.any (sdk: sdk.platformVersion == cfg.activeSdkVersion) cfg.sdks;
          message = "The list of SDK versions must include the active sdk version.";
        })
        (mkIf (builtins.length cfg.sdks > 0 || builtins.length cfg.emulators > 0) {
          assertion = cfg.allowUnfreeAndAcceptLicenses == true;
          message = "The `allowUnfreeAndAcceptLicenses` option needs to be set to `true` in order to build an android environment";
        })
      ];

      home = mkMerge [
        (mkIf (cfg.activeSdkVersion != null) {
          sessionVariables.ANDROID_HOME = activeSdkRoot;
          sessionVariables.ANDROID_SDK_ROOT = activeSdkRoot;
          sessionVariables.ANDROID_NDK_ROOT = activeNdkRoot;
          sessionVariables.ANDROID_USER_HOME = cfg.androidUserHome;
          sessionVariables.ANDROID_AVD_HOME = avdRoot;
          sessionVariables.AVD_HOME_DIR = avdRoot;
        })
        {
          packages =
            with pkgs;
            mkMerge [
              (if cfg.activeSdkVersion != null then [ activePlatformTools ] else [ android-tools ])
              (
                [
                  payload-dumper-go
                  fdroidcl
                ]
                ++ emulators
              )
              (mkIf (config.ordenada.globals.platform == "darwin") emulatorAppBundles)
            ];
        }
      ];

      wayland.windowManager.sway.config.floating.criteria = [ { app_id = "Waydroid"; } ];

      xdg.dataFile = builtins.listToAttrs (map mkSdkLink features.android.sdks);
      xdg.desktopEntries = mkIf (config.ordenada.globals.platform == "nixos") emulatorDesktopEntries;

      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-android";
        config = # elisp
          ''
            (with-eval-after-load 'ordenada-keymaps
              (keymap-set ordenada-app-map "${features.android.fdroidKey}" 'fdroid-map))

            (defgroup ordenada-android nil
              "General Android programming utilities."
              :group 'ordenada)

            (defvar ordenada-android-mode-map (make-sparse-keymap))

            (defvar ordenada-android-mode-command-map nil
              "Map to bind `android-mode' commands under.")
            (define-prefix-command 'ordenada-android-mode-command-map)
            (defvar ordenada-android-mode-start-command-map nil
              "Map to bind `android-mode' commands that start something under.")
            (define-prefix-command 'ordenada-android-mode-start-command-map)
            (defvar ordenada-android-mode-build-command-map nil
              "Map to bind `android-mode' commands that build something under.")
            (define-prefix-command 'ordenada-android-mode-build-command-map)
            (defvar ordenada-android-mode-logcat-command-map nil
              "Map to bind `android-mode' commands that build something under.")
            (define-prefix-command 'ordenada-android-mode-logcat-command-map)

            (defcustom ordenada-android-project-files
              '("android"
                "AndroidManifest.xml"
                "src/main/AndroidManifest.xml"
                "app/src/main/AndroidManifest.xml"

                "build.gradle"
                "build.gradle.kts"
                "settings.gradle"
                "settings.gradle.kts"
                "gradlew"
                "gradlew.bat"

                "capacitor.config.ts"
                "react-native.config.js"
                "metro.config.js"

                "config.xml"
                "pubspec.yml")
              "List of files that each may denote an Android project."
              :group 'ordenada-android)

            (defun ordenada-android--is-android-project-p ()
              (cl-some #'file-exists-p
                       (mapcar (lambda (s) (concat (project-root (project-current)) s))
                               ordenada-android-project-files)))

            (define-minor-mode ordenada-android-mode
              "Set up convenient tweaks for Android development."
              :group 'ordenada-android :keymap ordenada-android-mode-map
              (when ordenada-android-mode
                (android-mode)))

            ;; Android projects don't have a designated major-mode we can hook into.
            ;; Instead, we check on every project switch whether the one we switch to
            ;; classifies as an Android project and if so, we activate our minor mode.
            (with-eval-after-load 'project
              (advice-add 'project-switch-project
                          :after (lambda (&rest _)
                                   (when (and
                                          (project-current)
                                          (ordenada-android--is-android-project-p))
                                     (ordenada-android-mode)))))

            (with-eval-after-load 'android-mode
              (setopt android-mode-sdk-dir ${ordenada-lib.elisp.toNilOr activeSdkRoot ''"${activeSdkRoot}"''})
              (setopt android-mode-key-prefix "") ;; Do not use the provided way of binding android-mode commands
              (setopt android-mode-map (make-sparse-keymap)) ;; Overriding default map to avoid collisions

              (advice-add
               'android-local-sdk-dir
               :override (lambda (&rest r)
                           android-mode-sdk-dir))

            (let ((map ordenada-android-mode-build-command-map))
              (keymap-set map "c" '("clean" . android-build-clean))
              (keymap-set map "d" '("debug" . android-build-debug))
              (keymap-set map "i" '("install" . android-build-install))
              (keymap-set map "r" '("reinstall" . android-build-reinstall))
              (keymap-set map "t" '("test" . android-build-test))
              (keymap-set map "u" '("uninstall" . android-build-uninstall)))

            (let ((map ordenada-android-mode-start-command-map))
              (keymap-set map "e" '("emulator" . android-start-emulator))
              (keymap-set map "a" '("app" . android-start-app))
              (keymap-set map "d" '("ddms" . android-start-ddms)))

            (let ((map ordenada-android-mode-logcat-command-map))
              (keymap-set map "e" '("erase" . android-logcat-erase-buffer))
              (keymap-set map "l" '("logcat" . android-logcat))
              (keymap-set map "c" '("clear filter" . android-logcat-clear-filter))
              (keymap-set map "s" '("set filter" . android-logcat-set-filter))
              (keymap-set map "m" '("find file mouse" . android-logcat-find-file-mouse))
              (keymap-set map "f" '("find file" . android-logcat-find-file)))

            (let ((map ordenada-android-mode-command-map))
              (keymap-set map "b"
                          '("build" . ordenada-android-mode-build-command-map))
              (keymap-set map "s"
                          '("start" . ordenada-android-mode-start-command-map))
              (keymap-set map "l"
                          '("logcat" . ordenada-android-mode-logcat-command-map)))
              (keymap-set ordenada-android-mode-map "C-c A"
                          '("android" . ordenada-android-mode-command-map)))
          '';
        elispPackages = with pkgs.emacsPackages; [
          emacs-fdroid
          android-mode
          groovy-mode
        ];
      };
    };
}
