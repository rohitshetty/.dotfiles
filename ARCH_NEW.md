# Fresh Arch Linux Setup

Steps to go from a fresh Arch install (with root access) to a working daily-driver setup using configs from this repo.

## 1. Create your user

```bash
useradd -m -G wheel -s /bin/zsh rohit
passwd rohit
pacman -S sudo
EDITOR=nano visudo   # uncomment: %wheel ALL=(ALL:ALL) ALL
```

Log out and log back in as `rohit`.

## 2. Install an AUR helper

```bash
sudo pacman -S --needed git base-devel
git clone https://aur.archlinux.org/yay.git /tmp/yay
cd /tmp/yay && makepkg -si
```

## 3. Install packages

### Shell & terminal

```bash
sudo pacman -S zsh ghostty
```

### i3 window manager & utilities

```bash
sudo pacman -S i3-wm i3status i3lock rofi feh xorg-server xorg-xinit xorg-xdpyinfo xorg-xinput
```

### Fonts

```bash
sudo pacman -S ttf-fira-code ttf-dejavu
```

### Audio & media

```bash
sudo pacman -S pipewire pipewire-pulse pipewire-alsa wireplumber playerctl
```

### Dev tools

```bash
sudo pacman -S lua fzf
```

### AUR packages

```bash
yay -S n hyfetch powerline
```

### Other utilities

```bash
sudo pacman -S firefox ffmpeg at autorandr notify-osd libnotify
```

`at` is for the `remind()` shell function. Enable its service:

```bash
sudo systemctl enable --now atd
```

## 4. Clone and stow dotfiles

```bash
git clone https://github.com/<your-username>/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
```

Install stow:

```bash
sudo pacman -S stow
```

Stow each package you want:

```bash
stow zsh
# stow kitty   # if using kitty
stow i3
stow rofi
stow powerline
stow hyfetch
# stow emacs   # if using emacs
stow blurlock
```

**Tip**: Use `stow -n -v 2 <package>` to preview what will be symlinked before actually doing it.

## 5. Shell setup

The `stow zsh` command above creates `~/.zshrc`. The zshrc includes:
- Powerline prompt
- z.lua (directory jumper) — clone it first:
  ```bash
  mkdir -p ~/apps
  git clone https://github.com/skywind3000/z.lua.git ~/apps/z.lua
  ```
- hyfetch greeting on shell start
- n (Node.js version manager) with `N_PREFIX=~/.local`
- pnpm, bun, cargo, go PATH entries (activate as you install them)

## 6. Node.js (via n)

```bash
n lts
```

Then optionally:

```bash
npm i -g pnpm
```

## 7. i3 notes

The i3 config expects:
- **Wallpapers** in `~/screensavers/` — feh picks a random one on start
- **Blurlock** script at `~/apps/blurlock/lock.sh` — stow handles this, but you need `ffmpeg` and `i3lock` installed
- **Rofi** as app launcher (`Mod+d`) and window switcher (`Mod+q`)
- **Firefox** on `Mod+Shift+f`

## 8. Optional extras

| Tool | Install | What for |
|---|---|---|
| Rust/Cargo | `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \| sh` | Rust toolchain |
| Go | `sudo pacman -S go` | Go toolchain |
| Bun | `curl -fsSL https://bun.sh/install \| bash` | JS runtime |
| mpd | `sudo pacman -S mpd` | Music player daemon |

## Quick checklist

- [ ] User created with sudo access
- [ ] yay installed
- [ ] All packages installed
- [ ] Dotfiles cloned and stowed
- [ ] z.lua cloned to `~/apps/z.lua`
- [ ] `~/screensavers/` has wallpapers
- [ ] Reboot and `startx` or set up a display manager
