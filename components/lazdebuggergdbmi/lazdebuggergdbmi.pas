{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazDebuggerGdbmi;

{$warn 5023 off : no warning about unused units}
interface

uses
  CmdLineDebugger, DebugUtils, GDBTypeInfo, GDBMIMiscClasses, GDBMIDebugger, 
  GDBMIDebugInstructions, GDBMIServerDebugger, SSHGDBMIDebugger, 
  GdbmiStringConstants, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GDBMIDebugger', @GDBMIDebugger.Register);
  RegisterUnit('GDBMIServerDebugger', @GDBMIServerDebugger.Register);
  RegisterUnit('SSHGDBMIDebugger', @SSHGDBMIDebugger.Register);
end;

initialization
  RegisterPackage('LazDebuggerGdbmi', @Register);
end.
