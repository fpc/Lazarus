{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IdeConsoleProviderExample;

{$warn 5023 off : no warning about unused units}
interface

uses
  IdeDebugTerminalOptionsExample, IdeDebugTerminalPanelExample, IdeDebugTerminalMenuExample, 
  IdeDebugTerminalSettingsFrameExamle, IdeDebugTerminalPluginExample, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('IdeDebugTerminalMenuExample', @IdeDebugTerminalMenuExample.Register);
  RegisterUnit('IdeDebugTerminalPluginExample', @IdeDebugTerminalPluginExample.Register);
end;

initialization
  RegisterPackage('IdeConsoleProviderExample', @Register);
end.
