{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ToDoListLaz;

{$warn 5023 off : no warning about unused units}
interface

uses
  ToDoDlg, ToDoList, ToDoListCore, ToDoListStrConsts, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ToDoDlg', @ToDoDlg.Register);
end;

initialization
  RegisterPackage('ToDoListLaz', @Register);
end.
