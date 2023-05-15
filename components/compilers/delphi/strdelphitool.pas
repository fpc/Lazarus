unit strdelphitool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  PlusMinus : Array[Boolean] of char = ('-','+');


const
  // General
  DelphiToolsOptionsFile = 'delphitooloptions.xml';
  SDelphiToolName = 'DCC';
  SSubToolDelphi = 'Delphi';
  SDelphiParserName = 'Delphi Compiler';

  // globoal Settings
  KeyCompiler  = 'compiler/value';
  KeyConfigFileExt = 'configfileext/value';
  KeyConvertPaths = 'convertunixpath/value';
  KeyAdditionalOptions = 'additionaloptions/value';
  // Project settings
  pKeyAdditionalOptions = 'additionaloptions';
  pKeyGenConfigFile = 'genconfigfile';

resourcestring
  SDelphiLocalizedParserName = 'Delphi Compiler';
  SDelphiCompilerFileNameCaption = 'Delphi compiler executable';
  SConfigFileExtensionCaption = 'Configuration file extension';
  SGenerateConfigFileCaption = 'Generate Delphi config file based on FPC compiler options';
  SConvertDosToUnixCaption = 'Map filenames from Windows to Unix notation';
  SDelphiCompilerConfigFileName = 'Delphi compiler configuration filename for project';
  SDelphiCompileCommand  = 'Delphi compile command';
  SDelphiCompilerArgs = 'Additional compiler options';
  SSelectDelphiExecutable = 'Select Delphi compiler executable';

implementation

end.

