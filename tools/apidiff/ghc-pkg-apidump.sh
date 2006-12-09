#/bin/bash

# tool for comparing the exported API of different version of the same GHC
# library package. It reports type changes and removals but not additions as
# thoses are what is important from an API compatability point of view.

# $ ./ghc-pkg-apidump.sh gtk-0.9.10 gtk-0.9.10.2 > apichanges

# You can override the version of GHC using:

# $ GHC=ghc-6.4.2 GHCPKG=ghc-pkg-6.4.2 ./ghc-pkg-apidump.sh ... etc

GHC=${GHC:-ghc}
GHCPKG=${GHCPKG:-ghc-pkg}

originalexposedmodules=$($GHCPKG field $1 exposed-modules | sed 's/exposed-modules://')
originalimportdirs=$($GHCPKG field $1 import-dirs | sed 's/import-dirs://')

modifiedimportdirs=$($GHCPKG field $2 import-dirs | sed 's/import-dirs://')

for pkg in ${originalexposedmodules}; do
  originalhifile=${originalimportdirs}/${pkg//./\/}.hi
  modifiedhifile=${modifiedimportdirs}/${pkg//./\/}.hi

  if test -f ${modifiedhifile}; then
    ./hidiff "${pkg}" <($GHC --show-iface ${originalhifile}) \
                      <($GHC --show-iface ${modifiedhifile})
  else
    echo "${pkg} missing from $2 (can't find ${modifiedhifile})"
  fi
done
