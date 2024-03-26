My personal repository for bash, vim, emacs and other configuration files.
Many lines are taken from others' posted files, sometimes with attribution and often without.

Clone into a `configs` subdirectory, and link to files in
`~/configs`.  E.g.:

```bash
cd ~
git clone https://github.com/johannpeterson/configs.git
```

then

```bash
ln -s ~/configs/init.el ~/.emacs.d/init.el
ln -s ~/configs/.zshrc ~/.zshrc
ln -s ~/configs/.zshenv ~/.zshenv
ln -s ~/configs/.aliases ~/.aliases
```

Alternatively, to clone into home directory:

```bash
git init .
git remote add origin git@github.com:johannpeterson/configs.git
git pull origin master
```
