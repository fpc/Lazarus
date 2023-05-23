# Delphi compiling script

This directory contains a script that will call the Delphi compiler using
wine. It will select the correct compiler based on the -t option.
The default compiler is the linux 64-bit compiler.

You must adapt this script to configure the location of the Delphi
compiler(s) and the Delphi sources, dcu files and so on: the variables at the
start of the script can be used for this.

The script will also change the file mode of the generated executable,
making it actually executable.

Run the script with -h to get a list of supported options.
