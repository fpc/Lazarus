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
  IdeDebuggerBackendValueConv, IdeDbgValueConverterSettingsFrame, 
  IdeDebugger_ValConv_Options, IdeDebuggerOpts, IdeDebuggerWatchResultJSon, 
  WatchInspectToolbar, BaseDebugManager, WatchPropertyDlg, DebuggerDlg, 
  WatchesDlg, CallStackDlg, LocalsDlg, ThreadDlg, BreakPropertyDlgGroups, 
  HistoryDlg, PseudoTerminalDlg, RegistersDlg, DebugOutputForm, ExceptionDlg, 
  FeedbackDlg, DebugAttachDialog, BreakPropertyDlg, EvaluateDlg, InspectDlg, 
  BreakPointsDlg, AssemblerDlg, DbgTreeViewWatchData, EnvDebuggerOptions, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('IdeDebugger_ValConv_Options', 
    @IdeDebugger_ValConv_Options.Register);
end;

initialization
  RegisterPackage('IdeDebugger', @Register);
end.
