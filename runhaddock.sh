#!/bin/sh
chmod -R u+w /home/ger/uni/www/ghc/html
rm -rf /home/ger/uni/www/ghc/html
if test -n "/home/linux-bkb/ghc/ghc-6.2.1/share/ghc-6.2.1/html"
then
/bin/mkdir -p /home/ger/uni/www/ghc
/bin/cp -r /home/linux-bkb/ghc/ghc-6.2.1/share/ghc-6.2.1/html /home/ger/uni/www/ghc/html
else
/bin/mkdir -p /home/ger/uni/www/ghc/html
fi

# GHCDOCS should point to the HTML 
gmake haddock
gmake haddock # pick up forward references
gmake haddockgenindex 
   # generate the complete index (at least, it will, when Simon Marlow 
   # has fixed the relevant Haddock bug).