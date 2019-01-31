{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

 Author: Balázs Székely
}
unit onlinepackagemanagerintf;

{$mode objfpc}{$H+}

interface

{$R opkman.res}

uses
  Classes,
  // LCL
  LCLType,
  // IdeIntf
  MenuIntf, IDECommands, ToolBarIntf, PackageLinkIntf;

procedure Register;

implementation
uses opkman_const, opkman_mainfrm, opkman_maindm, opkman_intf;

procedure IDEMenuSectionClicked(Sender: TObject);
begin
  MainFrm := TMainFrm.Create(nil);
  try
    MainFrm.ShowModal;
  finally
    MainFrm.Free;
    MainFrm := nil;
  end;
end;

procedure Register;
var
  IDEShortCutX: TIDEShortCut;
  IDECommandCategory: TIDECommandCategory;
  IDECommand: TIDECommand;
begin
  IDEShortCutX := IDEShortCut(VK_O, [ssCtrl, ssAlt], VK_UNKNOWN, []);
  IDECommandCategory := IDECommandList.FindCategoryByName('Components');
  IDECommand := nil;
  if IDECommandCategory <> nil then
  begin
    IDECommand := RegisterIDECommand(IDECommandCategory, 'Online Package Manager', rsLazarusPackageManager, IDEShortCutX, nil, @IDEMenuSectionClicked);
    if IDECommand <> nil then
      RegisterIDEButtonCommand(IDECommand);
  end;
  RegisterIDEMenuCommand(itmPkgGraphSection, 'Online Package Manager',
    rsLazarusPackageManager, nil, @IDEMenuSectionClicked, IDECommand, 'pkg_opm');
end;

initialization
  MainDM := TMainDM.Create(nil);
  OPMInterface := TOPMInterfaceEx.Create;

finalization
  OPMInterface.Free;
  MainDM.Free;
end.

