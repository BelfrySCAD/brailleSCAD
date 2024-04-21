# brailleSCAD

**Braille for OpenSCAD with Automatic Contraction**

A library for OpenSCAD to create braille text with automatic (English)
contractions.  Requires the BOSL2 library.  At the moment only English
Grade 2 (EBAE) is supported, but expansion to other languages should
be possible.  

A common misconception is that braille represents English, and other
languages, by a simple mapping between the letters of the Roman
alphabet.  In fact, the situation is much more complicated.
An extensive system of language dependent contractions make braille
writing more compact.  Braille is almost always written in this
contracted form.  

This library makes use of liblouis, a standard braille contraction
library, to provide contraction rules for representing English in
braille.  Contraction is fully automatic from English text in your
OpenSCAD code.

## Documentation

You can find the braileSCAD library documentation at
[https://github.com/BelfrySCAD/brailleSCAD/wiki/TOC](https://github.com/BelfrySCAD/brailleSCAD/wiki/TOC)

## Installation

1. Download the .zip or .tar.gz release file for this library.  Currently you should be able to find this at https://github.com/BelfrySCAD/brailleSCAD/archive/refs/heads/master.zip
2. Unpack it. Make sure that you unpack the whole file structure. Some zipfile unpackers call this option "Use folder names". It should create either a `braileSCAD-master` directory with the library files within it.  
3. Rename the unpacked main directory to `braileSCAD`.
4. Move the `braileSCAD` directory into the apropriate OpenSCAD library directory.  The library directory may be on the list below, but for SNAP or other prepackaged installations, it is probably somewhere else.  To find it, run OpenSCAD and select Help&rarr;Library Info, and look for the entry that says "User Library Path".  This is your default library directory.  You may choose to change it to something more convenient by setting the environment variable OPENSCADPATH.  Using this variable also means that all versions of OpenSCAD you install will look for libraries in the same location.  
    - Windows: `My Documents\OpenSCAD\libraries\`
    - Linux: `$HOME/.local/share/OpenSCAD/libraries/`
    - Mac OS X: `$HOME/Documents/OpenSCAD/libraries/`
5. If you don't have the BOSL2 library then download it and install it in the same way from https://github.com/BelfrySCAD/BOSL2/archive/refs/heads/master.zip
6. Restart OpenSCAD.


