`pwsafe` is a command-line password manager written in Haskell.

It uses external programs for most of its tasks:

 * `gpg` for encryption of your password database (but you can easily adjust it
   to use `openssl` with AES and a master password)
 * `xdg-open` for interaction with your web browser
 * `xclip` for interaction with your X selection
 * `pwgen` for generation of user names and passwords
 * `vim` if you want to edit your password database with a text editor

Currently only Unix-like systems are supported.

Any questions/comments/patches are gladly welcome!
