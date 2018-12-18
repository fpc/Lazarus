{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazDebugTestBase;

{$warn 5023 off : no warning about unused units}
interface

uses
  TestDbgConfig, TestDbgTestSuites, TTestDbgExecuteables, 
  TestDbgCompilerProcess, TestDbgControlForm, TestDbgControl, 
  TestOutputLogger, TTestDebuggerClasses, TTestWatchUtilities, 
  TestCommonSources, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('LazDebugTestBase', @Register);
end.
