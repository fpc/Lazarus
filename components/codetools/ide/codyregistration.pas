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
    Registering menu items, shortcuts and components in the Lazarus IDE.
}
unit CodyRegistration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  IDECommands, MenuIntf,
  LResources, CodyStrConsts, CodyCtrls, PPUListDlg, AddAssignMethodDlg,
  CodyUtils, CodyNodeInfoDlg;

procedure Register;

implementation

procedure Register;
var
  CmdCatProjectMenu: TIDECommandCategory;
  CmdCatCodeTools: TIDECommandCategory;
  CmdCatFileMenu: TIDECommandCategory;
  PPUListCommand: TIDECommand;
  AddAssignMethodCommand: TIDECommand;
  RemoveWithBlockCommand: TIDECommand;
  InsertFileAtCursorCommand: TIDECommand;
  //DeclareVariableCommand: TIDECommand;
  TVIconRes: TLResource;
  AddCallInheritedCommand: TIDECommand;
  ShowCodeNodeInfoCommand: TIDECommand;
begin
  CmdCatFileMenu:=IDECommandList.FindCategoryByName('FileMenu');
  if CmdCatFileMenu=nil then
    raise Exception.Create('cody: command category FileMenu not found');
  CmdCatProjectMenu:=IDECommandList.FindCategoryByName('ProjectMenu');
  if CmdCatProjectMenu=nil then
    raise Exception.Create('cody: command category ProjectMenu not found');
  CmdCatCodeTools:=IDECommandList.FindCategoryByName('CodeTools');
  if CmdCatCodeTools=nil then
    raise Exception.Create('cody: command category CodeTools not found');

  // Source menu - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  // insert file at cursor
  InsertFileAtCursorCommand:=RegisterIDECommand(CmdCatFileMenu,
    'InsertFileAtCursor',crsInsertFileAtCursor,
    CleanIDEShortCut,CleanIDEShortCut,nil,@InsertFileAtCursor);
  RegisterIDEMenuCommand(SrcEditSubMenuSource,'InsertFileAtCursor',
    crsInsertFileAtCursor,nil,nil,InsertFileAtCursorCommand);

  // show ppu list of project
  PPUListCommand:=RegisterIDECommand(CmdCatProjectMenu, 'ShowPPUList',
    crsShowUsedPpuFiles,
    CleanIDEShortCut,CleanIDEShortCut,nil,@ShowPPUList);
  RegisterIDEMenuCommand(itmProjectWindowSection,'PPUList',crsShowUsedPpuFiles,
    nil,nil,PPUListCommand);

  // add call inherited
  AddCallInheritedCommand:=RegisterIDECommand(CmdCatCodeTools, 'AddCallInherited',
    crsAddCallInherited,
    CleanIDEShortCut,CleanIDEShortCut,nil,@AddCallInherited);
  RegisterIDEMenuCommand(SrcEditSubMenuSource, 'AddCallInherited',
    crsAddCallInherited, nil, nil, AddCallInheritedCommand);

  // declare variable
  {DeclareVariableCommand:=RegisterIDECommand(CmdCatCodeTools, 'DeclareVariable',
    crsDeclareVariable,
    CleanIDEShortCut,CleanIDEShortCut,nil,@ShowDeclareVariableDialog);
  RegisterIDEMenuCommand(SrcEditSubMenuRefactor, 'DeclareVariable',
    crsDeclareVariable2, nil, nil, DeclareVariableCommand);}

  // Show CodeTools node info
  ShowCodeNodeInfoCommand:=RegisterIDECommand(CmdCatCodeTools, 'ShowCodeNodeInfo',
    crsShowCodeToolsNodeInfo,
    CleanIDEShortCut,CleanIDEShortCut,nil,@ShowCodeNodeInfoDialog);
  RegisterIDEMenuCommand(SrcEditSubMenuSource, 'ShowCodeNodeInfo',
    crsShowCodeToolsNodeInfo, nil, nil, ShowCodeNodeInfoCommand);

  // Refactor menu - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  // add Assign method
  AddAssignMethodCommand:=RegisterIDECommand(CmdCatCodeTools, 'AddAssignMethod',
    crsAddAssignMethod,
    CleanIDEShortCut,CleanIDEShortCut,nil,@ShowAddAssignMethodDialog);
  RegisterIDEMenuCommand(SrcEditSubMenuRefactor, 'AddAssignMethod',
    crsAddAssignMethod2,nil,nil,AddAssignMethodCommand);

  // remove With block
  RemoveWithBlockCommand:=RegisterIDECommand(CmdCatCodeTools, 'RemoveWithBlock',
    crsRemoveWithBlock,
    CleanIDEShortCut,CleanIDEShortCut,nil,@RemoveWithBlockCmd);
  RegisterIDEMenuCommand(SrcEditSubMenuRefactor, 'RemoveWithBlock',
    crsRemoveWithBlock, nil, nil, RemoveWithBlockCommand);

  // components
  TVIconRes:=LazarusResources.Find('TTreeView');
  LazarusResources.Add(TCodyTreeView.ClassName,TVIconRes.ValueType,TVIconRes.Value);
  RegisterComponents('LazControls',[TCodyTreeView]);
end;

end.

