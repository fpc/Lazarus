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

  Author: Mattias Gaertner

  Abstract:
    Registering menu items, shortcuts and components in the Lazarus IDE.
}
unit CodyRegistration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazUTF8,
  // LCL
  LResources, LCLProc, Controls, Forms,
  // IdeIntf
  MenuIntf, IDEWindowIntf, SrcEditorIntf, IDEOptionsIntf, IDEOptEditorIntf,
  ProjectIntf, IDECommands,
  // CodeTools
  NewIDEWndDlg, CodeToolManager,
  CodyStrConsts, CodyUtils, CodyCtrls, CodyOpts,
  PPUListDlg, AddAssignMethodDlg, AddWithBlockDlg, CodyFindOverloads,
  {$IFDEF EnableCodyExperiments}
  CodyCopyDeclaration,
  {$ENDIF}
  CodyNodeInfoDlg, CodyFrm, DeclareVarDlg, CodyIdentifiersDlg, CodyMiscOptsFrame;

procedure Register;

function CreateRefactorCommand(CmdCatCodeTools: TIDECommandCategory;
  Name, Description: string;
  const OnExecuteMethod: TNotifyEvent; const OnExecuteProc: TNotifyProcedure;
  const MenuCaption: string = ''): TIDECommand;
function CreateSourceCommand(CmdCatCodeTools: TIDECommandCategory;
  Name, Description: string;
  const OnExecuteMethod: TNotifyEvent; const OnExecuteProc: TNotifyProcedure;
  const MenuCaption: string = ''): TIDECommand;

implementation

{$R cody.res}

function CreateRefactorCommand(CmdCatCodeTools: TIDECommandCategory;
  Name, Description: string;
  const OnExecuteMethod: TNotifyEvent; const OnExecuteProc: TNotifyProcedure;
  const MenuCaption: string): TIDECommand;
begin
  Result:=RegisterIDECommand(CmdCatCodeTools, Name, Description,
    CleanIDEShortCut,CleanIDEShortCut,OnExecuteMethod,OnExecuteProc);
  RegisterIDEMenuCommand(SrcEditSubMenuRefactor, 'SrcEditRefactor'+Name,
    MenuCaption, nil, nil, Result);
  RegisterIDEMenuCommand(itmRefactorAdvanced, 'BarRefactor'+Name,
    MenuCaption, nil, nil, Result);
end;

function CreateSourceCommand(CmdCatCodeTools: TIDECommandCategory; Name,
  Description: string; const OnExecuteMethod: TNotifyEvent;
  const OnExecuteProc: TNotifyProcedure; const MenuCaption: string
  ): TIDECommand;
begin
  Result:=RegisterIDECommand(CmdCatCodeTools, Name, Description,
    CleanIDEShortCut,CleanIDEShortCut,OnExecuteMethod,OnExecuteProc);
  RegisterIDEMenuCommand(SrcEditSubMenuSource, 'SrcEditSource'+Name,
    MenuCaption, nil, nil, Result);
  RegisterIDEMenuCommand(itmSourceInsertions, 'BarSource'+Name,
    MenuCaption, nil, nil, Result);
end;

procedure Register;
var
  CmdCatProjectMenu: TIDECommandCategory;
  CmdCatCodeTools: TIDECommandCategory;
  CmdCatFileMenu: TIDECommandCategory;
  PPUListCommand: TIDECommand;
  ShowCodeNodeInfoCommand: TIDECommand;
  CmdCatView: TIDECommandCategory;
  ViewCodyWindowCommand: TIDECommand;
  CmdCatSearchReplace: TIDECommandCategory;
begin
  CodyOptions:=TCodyMiscOptions.Create;
  CodyOptions.LoadSafe;

  CmdCatFileMenu:=IDECommandList.FindCategoryByName('FileMenu');
  if CmdCatFileMenu=nil then
    raise Exception.Create('cody: command category FileMenu not found');
  CmdCatSearchReplace:=IDECommandList.FindCategoryByName('SearchReplace');
  if CmdCatSearchReplace=nil then
    raise Exception.Create('cody: command category SearchReplace not found');
  CmdCatProjectMenu:=IDECommandList.FindCategoryByName('ProjectMenu');
  if CmdCatProjectMenu=nil then
    raise Exception.Create('cody: command category ProjectMenu not found');
  CmdCatCodeTools:=IDECommandList.FindCategoryByName(CommandCategoryCodeTools);
  if CmdCatCodeTools=nil then
    raise Exception.Create('cody: command category '+CommandCategoryCodeTools+' not found');
  CmdCatView:=IDECommandList.FindCategoryByName(CommandCategoryViewName);
  if CmdCatView=nil then
    raise Exception.Create('cody: command category '+CommandCategoryViewName+' not found');

  // Project menu - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  // show ppu list of project
  PPUListCommand:=RegisterIDECommand(CmdCatProjectMenu, 'ShowPPUList',
    crsShowUsedPpuFiles,
    CleanIDEShortCut,CleanIDEShortCut,nil,@ShowPPUList);
  RegisterIDEMenuCommand(itmProjectWindowSection,'PPUList',crsShowUsedPpuFiles,
    nil,nil,PPUListCommand);

  // Source menu - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  // insert file at cursor
  CreateSourceCommand(CmdCatFileMenu,'InsertFileAtCursor',
    crsInsertFileAtCursor,nil,@InsertFileAtCursor);

  // add call inherited
  CreateSourceCommand(CmdCatCodeTools,'InsertCallInherited',
    crsInsertCallInherited,nil,@InsertCallInherited);

  // insert int64 ID
  CreateSourceCommand(CmdCatCodeTools,'InsertInt64ID',
    crsInsertInt64IdYYYYDDMMhhnnss, nil, @InsertInt64ID);

  // Show unit / identifier dictionary
  InitUnitDictionary;
  CreateSourceCommand(CmdCatCodeTools,'ShowUnitDictionary',
    crsShowUnitIdentifierDictionary,nil,@ShowUnitDictionaryDialog);

  // Find overloads
  CreateSourceCommand(CmdCatCodeTools,'FindProcOverloads',
    crsFindProcedureMethodOverloads, nil, @ShowFindOverloadsClicked);

  // Refactor menu - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  // declare variable
  CreateRefactorCommand(CmdCatCodeTools,'DeclareVariable',
    crsDeclareVariable,nil,@ShowDeclareVariableDialog,crsDeclareVariable2);

  // add Assign method
  CreateRefactorCommand(CmdCatCodeTools,'AddAssignMethod',
    crsAddAssignMethod,nil,@ShowAddAssignMethodDialog,crsAddAssignMethod2);

  // Copy declaration to clipboard
  {$IFDEF EnableCodyExperiments}
  CreateRefactorCommand(CmdCatCodeTools,'CopyDeclarationToClipboard',
    crsCopyDeclarationToClipboard,nil,@CopyDeclarationToClipboard);
  {$ENDIF}

  // Cut declaration to clipboard
  {$IFDEF EnableCodyExperiments}
  CreateRefactorCommand(CmdCatCodeTools,'CutDeclarationToClipboard',
    crsCutDeclarationToClipboard,nil,@CutDeclarationToClipboard);
  {$ENDIF}

  // explode a With block
  CreateRefactorCommand(CmdCatCodeTools,'ExplodeAWithBlock',
    crsExplodeAWithBlock,nil,@ExplodeAWithBlockCmd);
  // add a With block
  CreateRefactorCommand(CmdCatCodeTools,'AddAWithBlock',
    crsAddAWithBlock,nil,@ShowAddWithBlockDialog);


  // IDE internals menu - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  // Show CodeTools node info
  ShowCodeNodeInfoCommand:=RegisterIDECommand(CmdCatCodeTools, 'ShowCodeNodeInfo',
    crsShowCodeToolsNodeInfo,
    CleanIDEShortCut,CleanIDEShortCut,nil,@ShowCodeNodeInfoDialog);
  RegisterIDEMenuCommand(itmViewIDEInternalsWindows, 'ShowCodeNodeInfo',
    crsShowCodeToolsNodeInfo, nil, nil, ShowCodeNodeInfoCommand);

  // View menu - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Cody tool window
  ViewCodyWindowCommand:=RegisterIDECommand(CmdCatView, 'Cody',
    'Cody', CleanIDEShortCut, CleanIDEShortCut, nil, @ShowCodyWindow);
  RegisterIDEMenuCommand(itmViewMainWindows, 'ViewCody',
    'Cody', nil, nil, ViewCodyWindowCommand)
  {$IFNDEF EnableCodyExperiments}
   .Visible:=false
  {$ENDIF};

  // Components - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  RegisterComponents('LazControls',[TCodyTreeView]);

  // Windows - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  CodyWindowCreator:=IDEWindowCreators.Add(CodyWindowName,@CreateCodyWindow,nil,
    '80%','50%','+18%','+25%','CodeExplorer',alBottom);

  // File types - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  RegisterProjectFileDescriptor(TFileDescIDEDockableWindow.Create);

  // Options Frame - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  CodyMiscOptionID:=RegisterIDEOptionsEditor(GroupCodetools,
      TCodyMiscOptionsFrame,CodyMiscOptionID)^.Index;
  CodyOptions.Apply;

  // Global handlers - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  SourceEditorManagerIntf.RegisterCopyPasteEvent(@Cody.SrcEditCopyPaste);
end;

end.

