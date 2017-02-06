# Gtk2Hs

[![Build Status](https://secure.travis-ci.org/gtk2hs/gtk2hs.png)](http://travis-ci.org/gtk2hs/gtk2hs)

Gtk2Hs is a GUI library for Haskell based on GTK+. GTK+ is an extensive and mature multi-platform toolkit
for creating graphical user interfaces.

For all new application development you should consider
using [haskell-gi](https://github.com/haskell-gi/haskell-gi) as it has much more complete bindings.
Cairo and WebKitGTK JavaScriptCore do not have GObject introspection data so you will still need to use
the Gtk2Hs packages for those (even when using haskell-gi for everything else).

## Installing C libraries

The following commands will install most of the C libraries used by the Gtk2Hs and haskell-gi. 

##### Fedora
`sudo dnf install gobject-introspection-devel webkitgtk3-devel  webkitgtk4-devel gtksourceview3-devel`

##### Ubuntu/Debian
`sudo apt-get install libgirepository1.0-dev libwebkitgtk-3.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev`

##### Arch Linux
`sudo pacman -S gobject-introspection gobject-introspection-runtime gtksourceview3 webkitgtk webkit2gtk`

##### OS X MacPorts
`sudo port install gobject-introspection webkit-gtk3 webkit2-gtk gtksourceview3 gtk-osx-application-gtk3 adwaita-icon-theme`

You will also need to build a MacPorts compatible of GHC.  First install GHC some other way then unpack the source for the GHC version you want to use and run:

    sudo port install libxslt gmp ncurses libiconv llvm-3.5 libffi
    ./configure --prefix=$HOME/ghc-8.0.1 --with-iconv-includes=/opt/local/include --with-iconv-libraries=/opt/local/lib --with-gmp-includes=/opt/local/include --with-gmp-libraries=/opt/local/lib --with-system-libffi --with-ffi-includes=/opt/local/lib/libffi-3.2.1/include --with-ffi-libraries=/opt/local/lib --with-nm=/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/nm-classic
    make
    make install

Then make sure the `$HOME/ghc-8.0.1/bin` is in your $PATH.

##### OS X Homebrew
Unlike MacPorts you will not need a custom build of GHC for homebrew since it uses the system `libiconv`.  However
some packaged (such as WebKitGTK) seem to be less well supported on Homebrew.

`brew install gtk+3`

And if you got an error about that pkg-config can not find libffi when installing haskell-gi or something like that, you need 
to find out where your brew installed libffi, and add the pkg_config(e.g. `/usr/local/Cellar/libffi/3.0.13/lib/pkgconfig`) directory to $PKG_CONFIG_PATH.

TIPS: If you installed GTK with homebrew, you do not need to install X11 on macOS. The one installed by brew was linked with GTK+'s Quartz backend, and you can use macOS's native display manager, keyboard, and pointing device.

##### Windows MSYS2
Install [MSYS2](https://msys2.github.io/) and [Chocolatey](https://chocolatey.org/).  Then in a shell with administrator privileges:

    choco install ghc
    pacman -S mingw64/mingw-w64-x86_64-pkg-config mingw64/mingw-w64-x86_64-gobject-introspection mingw64/mingw-w64-x86_64-gtksourceview3 mingw64/mingw-w64-x86_64-webkitgtk3

## Install Extra Tools

    cabal update
    cabal install alex happy
    cabal install haskell-gi

(make sure `~/.cabal/bin` is in PATH)

## Building with stack

While you currently cannot have the tools automatically installed with `stack` you should be able to install them
in the local stack.  For instance here are the build instructions for building Leksah using stack:

```
git clone --recursive https://github.com/leksah/leksah.git
cd leksah
stack install alex happy
stack install haskell-gi
stack install gtk2hs-buildtools
stack install
```
