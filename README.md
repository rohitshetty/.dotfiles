## How to use this

Clone this repo in your home folder. So now you have `~/.dotfiles`.

Install `stow`[^1]. This used for managing the symlinking of the files to your `.config` folder.

To "install" a "package" type `stow <package>`. This will copy the package into the `.config/package` folder.
This is not magic, and happens because the `package` folder is structured as such.

Take the example of `kitty`

```
.
├── kitty
│   └── .config
│       └── kitty
│           └── kitty.conf
```

(The above file tree is relevant part from the output of `tree -a -I .git`)

When you run `stow kitty` inside the `.dotfiles` folder, assuming `~/.config` exists, stow will create `~/.config/kitty/kitty.conf`.

Stow uses the parent directory of `.dotfiles` (assumed to be `~/`) to create the necessary symlinks to reproduce the package contents (`kitty/`). Thus, `kitty/.config` becomes equivalent to `~/.config`.

As a result, `~/.config/kitty/kitty.conf` is created. Stow does not alter anything else in `~/.config`, only creating `~/.config/kitty`.

**Tip**: Use `stow -n -v 2 <packagename>` to see details on what stow will create. `-n` asks stow to simulate the symlinking without executing it. `-v 2` makes the command verbose; increasing the number makes it more verbose. This prints the source and target locations, and displays any errors or warnings.

## TODO

- [x] Install stow for management
- [x] Change the location of wallpapers hardcoded
- [ ] enable blurlock scripts
- [ ] Make a list of install apps

[^1]: https://www.gnu.org/software/stow/
