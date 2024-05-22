## How to use this

Install `stow`[^1]. This used for managing the symlinking of the files to your .config folder.

To "install" a "package" type `stow <package>`. This will copy the package into the `.config/package` folder.
This is done because the `package` folder here is structured as such.

Example take the example of `kitty`

```
.
├── kitty
│   └── .config
│       └── kitty
│           └── kitty.conf
```

(The above filetree is relevant part from the output of `tree -a -I .git`)

When you run `stow kitty` inside the `.dotfiles` folder, assuming `~/.config` exists, stow will try to create `.config/kitty/kitty.conf`.
stow tries to use the one directory above it (in this case,assuming `.dotfiles` is in `~/.dotfiles`) and creates symlinks required to reproduce contents of packages (which is `.config` inside the `~/.dotfiles/kitty`). Hence, it creates the `kitty` directory and `kitty.conf`.

**Tip**: Use `stow -n -v 2 <packagename>` to see details on what stow will create. `-n` asks stow to not really execute the symlinking and only simulate it. `-v 2` makes the command verbose, increase the number to make the command more verbose. This gives a clue by printing the source and target locations, any errors or warnings etc.

## TODO

- [x] Install stow for management
- [x] Change the location of wallpapers hardcoded
- [ ] enable blurlock scripts
- [ ] Make a list of install apps

[^1]: https://www.gnu.org/software/stow/
