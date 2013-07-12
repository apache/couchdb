*WARNING:  this is a temporary solution for adding icons to the Icon Font. This will become a grunt task eventually.*


###Install Fontcustom

http://fontcustom.com/

###INSTALL OSX

`brew install fontforge ttfautohint
gem install fontcustom`

###INSTALL LINUX

`sudo apt-get install fontforge ttfautohint
wget http://people.mozilla.com/~jkew/woff/woff-code-latest.zip
unzip woff-code-latest.zip -d sfnt2woff && cd sfnt2woff && make && sudo mv sfnt2woff /usr/local/bin/
gem install fontcustom`


cd into the icons folder in Fauxton.

Run it once.  Doesn't seem like we need to set it on watch

`fontcustom compile /path/to/vectors`


You need to copy the fonts generated in the icons/fontcustom folder over to the style/assets/img folder in addons.
Then copy and paste the CSS generated into the icons.less file in the style/assets/less  folder

Update the @font-face paths to have this prefix ../img

###ICONS:
- SVG FORMAT
- NO COLORS
- NO OPACITIES
- ONE CONTINUAL PATH

open one of the existing icons, hide it, and paste your new icon in there. Resize it to fit that space and center it vertically and horizontally.This helps prevent sTUff LiKE thIS fROm hAppENiNg
