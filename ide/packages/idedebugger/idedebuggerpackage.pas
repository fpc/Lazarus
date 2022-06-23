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
  IdeDebuggerFpDbgValueConv, IdeFpDbgValueConverterSettingsFrame, 
  IdeDebugger_FpValConv_Options, IdeDebuggerOpts, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('IdeDebugger_FpValConv_Options', 
    @IdeDebugger_FpValConv_Options.Register);
end;

initialization
  RegisterPackage('IdeDebugger', @Register);
end.
