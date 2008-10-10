unit CommandLineConstants;

interface
{$I JcfGlobal.inc}
uses
  JcfUtils,
  VersionConsts;

const
  ABOUT_COMMANDLINE =
    'JEDI Code Format V' + PROGRAM_VERSION + AnsiLineBreak +
    ' ' + PROGRAM_DATE + AnsiLineBreak +
    ' A Delphi Object-Pascal Source code formatter' + AnsiLineBreak +
    ' A GUI version of this program is also available' + AnsiLineBreak +
    ' Latest version at ' + PROGRAM_HOME_PAGE + AnsiLineBreak + AnsiLineBreak +
    'Syntax: jcf [options] path/filename ' + AnsiLineBreak +
    ' Parameters to the command-line program: ' + AnsiLineBreak + AnsiLineBreak +

    ' Mode of operation: ' + AnsiLineBreak +
    ' -obfuscate Obfuscate mode or ' + AnsiLineBreak +
    ' -clarify Clarify mode' + AnsiLineBreak +
    '   When neither is specified, registry setting will be used.' + AnsiLineBreak +
    '   This normally means clarify.' + AnsiLineBreak + AnsiLineBreak +

    ' Mode of source: ' + AnsiLineBreak +
    ' -F Format a file. The file name must be specified.' + AnsiLineBreak +
    ' -D Format a directory. The directory name must be specified.' + AnsiLineBreak +
    ' -R Format a directory tree. The root directory name must be specified.' +
    AnsiLineBreak +
    '  When no file mode is specified, registry setting will be used.' +
    AnsiLineBreak + AnsiLineBreak +

    ' Mode of output: ' + AnsiLineBreak +
    ' -inplace change the source file without backup' + AnsiLineBreak +
    ' -out output to a new file' + AnsiLineBreak +
    ' -backup change the file and leave the original file as a backup' + AnsiLineBreak +
    '  If no output mode is specified, registry setting will be used.' +
    AnsiLineBreak + AnsiLineBreak +

    ' Other options: ' + AnsiLineBreak +
    ' -config=filename  To specify a named configuration file' + AnsiLineBreak +
    ' -y Overwrite files without confirmation.' + AnsiLineBreak +
    ' -? Display this help' + AnsiLineBreak;

implementation

end.
