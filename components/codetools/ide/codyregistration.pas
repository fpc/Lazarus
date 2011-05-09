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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    LCL controls for Cody.
}
unit CodyRegistration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  IDECommands, MenuIntf,
  CodyFrm, CodyStrConsts, CodyCtrls, PPUListDlg, AddAssignMethodDlg;

procedure Register;

implementation

procedure Register;
var
  CmdCategory: TIDECommandCategory;
  PPUListCommand: TIDECommand;
  AddAssignMethodCommand: TIDECommand;
  RemoveWithBlockCommand: TIDECommand;
begin
  CmdCategory:=IDECommandList.FindCategoryByName('ProjectMenu');
  if CmdCategory=nil then
    raise Exception.Create('cody: PPUListDlg.Register: command category ProjectMenu not found');
  // show ppu list of project
  PPUListCommand:=RegisterIDECommand(CmdCategory, 'ShowPPUList',
    crsShowUsedPpuFiles,
    CleanIDEShortCut,CleanIDEShortCut,nil,@ShowPPUList);
  RegisterIDEMenuCommand(itmProjectWindowSection,'PPUList',crsShowUsedPpuFiles,
    nil,nil,PPUListCommand);

  CmdCategory:=IDECommandList.FindCategoryByName('CodeTools');
  if CmdCategory=nil then
    raise Exception.Create('cody: AddAssignMethodDlg.Register: command category CodeTools not found');
  // add Assign method
  AddAssignMethodCommand:=RegisterIDECommand(CmdCategory, 'AddAssignMethod',
    crsAddAssignMethod,
    CleanIDEShortCut,CleanIDEShortCut,nil,@ShowAddAssignMethodDialog);
  RegisterIDEMenuCommand(SrcEditSubMenuSource, 'AddAssignMethod',
    crsAddAssignMethod2,nil,nil,AddAssignMethodCommand);
  // remove With block
  RemoveWithBlockCommand:=RegisterIDECommand(CmdCategory, 'RemoveWithBlock',
    crsRemoveWithBlock,
    CleanIDEShortCut,CleanIDEShortCut,nil,@RemoveWithBlockCmd);
  RegisterIDEMenuCommand(SrcEditSubMenuRefactor, 'RemoveWithBlock',
    crsRemoveWithBlock, nil, nil, RemoveWithBlockCommand);

  // components
  RegisterComponents('LazControls',[TCodyTreeView]);
end;

end.

