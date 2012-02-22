#!/bin/bash
mkdir target/dist
VER=0.8.1
tar --directory target/products/final/linux/gtk/x86/ -cvzapf target/dist/zaluum.$VER-linux-gtk-x86.tar.gz zaluum
tar --directory target/products/final/linux/gtk/x86_64/ -cvzapf target/dist/zaluum.$VER-linux-gtk-x86_64.tar.gz zaluum

tar --directory target/products/final/win32/win32/x86/ -cvzapf target/dist/zaluum.$VER-win32-win32-x86.tar.gz zaluum
tar --directory target/products/final/win32/win32/x86_64/ -cvzapf target/dist/zaluum.$VER-win32-win32-x86_64.tar.gz zaluum

tar --directory target/products/final/macosx/cocoa/x86/ -cvzapf target/dist/zaluum.$VER-macosx-cocoa-x86.tar.gz zaluum
tar --directory target/products/final/macosx/cocoa/x86_64/ -cvzapf target/dist/zaluum.$VER-macosx-cocoa-x86_64.tar.gz zaluum

