YubiKey/PGP helper and indicator for i3bar
==========================================

`yubimonitor` displays a icon in the `i3bar` that reflects the status
of your YubiKey (or any other OpenPGP smart card), most importantly
visually notifies you when you have to press it.

To use `yubimonitor` simply `exec` it on startup:

```
exec --no-startup-id yubimonitor
```

And put the following two options in your `~/.gnupg/scdaemon.conf`:

```
log-file socket:///run/user/1000/gnupg/S.scdaemon.log
debug 4096
```

Replace `1000` with your user id. Or, you can use a different path and
specify it with `--log_socket` flag.

`yubimonitor` uses the standard system tray icon protocol, so it can
be used with any system tray-like application, not just `i3bar`. But
it was originally created to be used with `i3bar`.
