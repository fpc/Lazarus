{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit idedebuggerpackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  IdeDebuggerBase, Debugger, ProcessDebugger, ProcessList, DebuggerTreeView, 
  IdeDebuggerUtils, IdeDebuggerWatchResult, IdeDebuggerWatchResPrinter, 
  IdeDebuggerWatchResUtils, ArrayNavigationFrame, IdeDebuggerStringConstants, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('IdeDebugger', @Register);
end.
