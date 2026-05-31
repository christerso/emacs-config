#!/usr/bin/env sh
# install.sh — reproduce the Emacs server/client setup on this machine.
# Arch host (pacman). Idempotent: safe to re-run.

set -eu

here="$(cd "$(dirname "$0")" && pwd)"

# 1. Emacs itself (X11/GTK build; emacs-wayland is the pgtk alternative).
if ! pacman -Q emacs >/dev/null 2>&1; then
    echo "installing emacs..."
    sudo pacman -S --needed emacs
fi

# 2. systemd user drop-in: respawn the daemon on any death.
override_dir="${HOME}/.config/systemd/user/emacs.service.d"
mkdir -p "$override_dir"
cp "$here/emacs.service.override.conf" "$override_dir/override.conf"

# 3. Enable + (re)start the daemon for this user.
systemctl --user daemon-reload
systemctl --user enable --now emacs

# 4. Put the restart helper on PATH (symlink, so repo edits take effect).
mkdir -p "${HOME}/.local/bin"
ln -sf "$here/restartemacsserver" "${HOME}/.local/bin/restartemacsserver"
chmod +x "$here/restartemacsserver"

echo
echo "done. The taskbar launcher is the package-provided"
echo "  /usr/share/applications/emacsclient.desktop"
echo "(a reference copy is in this directory). Open a window with:"
echo "  emacsclient -c        # GUI frame"
echo "  emacsclient -nw       # terminal frame"
echo "Restart the daemon after config changes with:  restartemacsserver"
