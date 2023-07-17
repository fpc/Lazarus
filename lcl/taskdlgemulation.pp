unit TaskDlgEmulation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  LazUTF8,
  LCLType, LCLStrConsts, LCLIntf, InterfaceBase, ImgList,
  LResources, Menus, Graphics, Forms, Controls, StdCtrls, ExtCtrls, Buttons, Dialogs, DialogRes;

function ExecuteLCLTaskDialog(const ADlg: TTaskDialog): Boolean;

implementation

function ExecuteLCLTaskDialog(const ADlg: TTaskDialog): Boolean;
begin
  Result := False;
end;

end.

