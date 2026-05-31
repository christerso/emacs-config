# Emacs server / client setup

This Emacs runs as a **[systemd](https://systemd.io/) user daemon**, not as a
per-window process. You start the daemon once at login; every window after that
is a lightweight [`emacsclient`](https://www.gnu.org/software/emacs/manual/html_node/emacs/emacsclient-Options.html)
frame talking to that one long-lived [Emacs](https://www.gnu.org/software/emacs/)
process. Buffers, LSP servers, and history stay warm between windows.

This directory holds the files that make that work, so the setup is reproducible
on a fresh machine. They are **copies for version control** — the live copies
live in the system locations listed below.

## Files

| File | Lives at (live copy) | Purpose |
|------|----------------------|---------|
| `emacs.service` | `/usr/lib/systemd/user/emacs.service` | Upstream unit shipped by the `emacs` package. Reference copy — do **not** edit; override it instead. |
| `emacs.service.override.conf` | `~/.config/systemd/user/emacs.service.d/override.conf` | Our drop-in. Makes the daemon respawn on any death (`Restart=always`). |
| `emacsclient.desktop` | `/usr/share/applications/emacsclient.desktop` | The taskbar/menu launcher. No-arg click runs `emacsclient --alternate-editor= --create-frame`. |
| `install.sh` | — | Installs the package, drops in the override, enables + starts the daemon. |

## How a launch flows

1. systemd starts `emacs --fg-daemon` at login (`WantedBy=default.target`).
2. Clicking the taskbar icon runs the launcher's no-arg branch:
   `emacsclient --alternate-editor= --create-frame`.
3. The daemon makes a new GUI frame on the current display and the client blocks
   until you close it (`C-x 5 0`).

`--alternate-editor=` (empty) means: if the daemon isn't up yet, the client
*starts* one instead of failing.

## Applying config changes

The daemon loads `~/.emacs.d/init.el` → `config.org` once at start. Editing
config files does **nothing** to the running daemon. To apply changes:

```sh
emacsclient -e '(save-some-buffers t)'          # save open file buffers
emacsclient -e '(desktop-save-in-desktop-dir)'  # persist the buffer list
systemctl --user restart emacs                  # re-reads init.el, re-tangles config.org
```

This is a **daemon restart**, not a cold app launch. Unsaved work is only safe
because of the save steps above. `desktop-save-mode` reopens the buffer list;
`lisp/session-layout.el` reopens the exact window splits.

## Useful commands

```sh
systemctl --user status emacs        # is the daemon up?
systemctl --user restart emacs       # apply config changes
systemctl --user stop emacs          # stop it (Restart=always does NOT fight a deliberate stop)
journalctl --user -u emacs -f        # live daemon log
emacsclient -e '(emacs-version)'     # poke the live daemon
emacsclient -c                       # new GUI frame
emacsclient -nw                      # new terminal (TTY) frame in the current shell
```

## Gotchas baked into the config

- **Frame geometry** — under Wayland→XWayland the compositor gives new frames no
  size, so they spawned as an 8×5 speck (looked like a dead black window).
  `default-frame-alist` now pins a real size (see `config.org`, Appearance).
- **Empty `*scratch*`** — with a dark theme an empty scratch buffer is a black
  rectangle; `initial-scratch-message` now carries a short banner so a fresh
  frame never looks dead.
