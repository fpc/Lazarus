To link the icons of Lazarus

1 - For the current user only:
    Copy all the images in the directory ~/.local/share/icons/hicolor/48x48/mimetypes / (create it if does not exist)
    in terminal run: gtk-update-icon-cache -f -t ~/.local/share/icons/hicolor/

2 - For all users:
    Copy all the images in the directory usr/share/icons/hicolor/48x48/mimetypes / (create it if does not exist)
    in terminal run: sudo gtk-update-icon-cache -f usr/share/icons/hicolor/

Tested on Ubuntu 7.10
