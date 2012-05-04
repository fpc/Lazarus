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
  Classes, SysUtils, LResources, LCLProc, Controls, Forms, IDECommands,
  MenuIntf, IDEWindowIntf, SrcEditorIntf, IDEOptionsIntf, ProjectIntf,
  MacroIntf, CodyStrConsts, CodyCtrls, PPUListDlg,
  AddAssignMethodDlg, AddWithBlockDlg, CodyUtils, CodyNodeInfoDlg, CodyFrm,
  DeclareVarDlg, CodyCopyDeclaration, CodyIdentifiersDlg, CodyMiscOptsFrame,
  CodyOpts, CodeToolManager, CodeCache, LazFileUtils, LazUTF8;

type

  { TFileDescIDEDockableWindow }

  TFileDescIDEDockableWindow = class(TFileDescPascalUnit)
  private
    FTemplateImplementation: string;
    FTemplateInterface: string;
    FTemplateLFM: string;
    FTemplateUsesSection: string;
    FTemplateLoaded: boolean;
    function ExtractTemplate(Src, StartMarker, EndMarker: string): string;
    procedure LoadTemplate;
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetInterfaceUsesSection: string; override;
    function GetInterfaceSource(const {%H-}Filename, {%H-}SourceName,
                                ResourceName: string): string; override;
    function GetImplementationSource(const {%H-}Filename, {%H-}SourceName,
                                     ResourceName: string): string; override;
    property TemplateUsesSection: string read FTemplateUsesSection write FTemplateUsesSection;
    property TemplateInterface: string read FTemplateInterface write FTemplateInterface;
    property TemplateImplementation: string read FTemplateImplementation write FTemplateImplementation;
    property TemplateLFM: string read FTemplateLFM write FTemplateLFM;
  end;

procedure Register;

implementation

procedure Register;
var
  CmdCatProjectMenu: TIDECommandCategory;
  CmdCatCodeTools: TIDECommandCategory;
  CmdCatFileMenu: TIDECommandCategory;
  PPUListCommand: TIDECommand;
  AddAssignMethodCommand: TIDECommand;
  ExplodeAWithBlockCommand: TIDECommand;
  AddAWithBlockCommand: TIDECommand;
  InsertFileAtCursorCommand: TIDECommand;
  DeclareVariableCommand: TIDECommand;
  TVIconRes: TLResource;
  InsertCallInheritedCommand: TIDECommand;
  ShowCodeNodeInfoCommand: TIDECommand;
  CmdCatView: TIDECommandCategory;
  ViewCodyWindowCommand: TIDECommand;
  {$IFDEF EnableCodyExperiments}
  CopyDeclarationToClipboardCommand: TIDECommand;
  CutDeclarationToClipboardCommand: TIDECommand;
  {$ENDIF}
  ShowIdentifierDictionaryCommand: TIDECommand;
begin
  CodyOptions:=TCodyMiscOptions.Create;
  CodyOptions.LoadSafe;

  CmdCatFileMenu:=IDECommandList.FindCategoryByName('FileMenu');
  if CmdCatFileMenu=nil then
    raise Exception.Create('cody: command category FileMenu not found');
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
  InsertFileAtCursorCommand:=RegisterIDECommand(CmdCatFileMenu,
    'InsertFileAtCursor',crsInsertFileAtCursor,
    CleanIDEShortCut,CleanIDEShortCut,nil,@InsertFileAtCursor);
  RegisterIDEMenuCommand(SrcEditSubMenuSource,'SrcEditInsertFileAtCursor',
    crsInsertFileAtCursor,nil,nil,InsertFileAtCursorCommand);
  RegisterIDEMenuCommand(itmSourceInsertions,'InsertFileAtCursor',
    crsInsertFileAtCursor,nil,nil,InsertFileAtCursorCommand);

  // add call inherited
  InsertCallInheritedCommand:=RegisterIDECommand(CmdCatCodeTools, 'InsertCallInherited',
    crsInsertCallInherited,
    CleanIDEShortCut,CleanIDEShortCut,nil,@InsertCallInherited);
  RegisterIDEMenuCommand(SrcEditSubMenuSource, 'SrcEditAddCallInherited',
    crsInsertCallInherited, nil, nil, InsertCallInheritedCommand);
  RegisterIDEMenuCommand(itmSourceInsertions, 'InsertCallInherited',
    crsInsertCallInherited, nil, nil, InsertCallInheritedCommand);

  // declare variable
  DeclareVariableCommand:=RegisterIDECommand(CmdCatCodeTools, 'DeclareVariable',
    crsDeclareVariable,
    CleanIDEShortCut,CleanIDEShortCut,nil,@ShowDeclareVariableDialog);
  RegisterIDEMenuCommand(SrcEditSubMenuRefactor, 'SrcEditDeclareVariable',
    crsDeclareVariable2, nil, nil, DeclareVariableCommand);
  RegisterIDEMenuCommand(itmRefactorCodeTools, 'DeclareVariable',
    crsDeclareVariable2, nil, nil, DeclareVariableCommand);

  // Show unit / identifier dictionary
  InitUnitDictionary;
  ShowIdentifierDictionaryCommand:=RegisterIDECommand(CmdCatCodeTools, 'ShowUnitDictionary',
    crsShowUnitIdentifierDictionary,
    CleanIDEShortCut,CleanIDEShortCut,nil,@ShowUnitDictionaryDialog);
  RegisterIDEMenuCommand(SrcEditSubMenuSource, 'SrcEditShowIdentifierDictionary',
    crsShowUnitIdentifierDictionary, nil, nil, ShowIdentifierDictionaryCommand);
  RegisterIDEMenuCommand(itmSourceInsertions, 'ShowIdentifierDictionary',
    crsShowUnitIdentifierDictionary, nil, nil, ShowIdentifierDictionaryCommand);


  // Refactor menu - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  // add Assign method
  AddAssignMethodCommand:=RegisterIDECommand(CmdCatCodeTools, 'AddAssignMethod',
    crsAddAssignMethod,
    CleanIDEShortCut,CleanIDEShortCut,nil,@ShowAddAssignMethodDialog);
  RegisterIDEMenuCommand(SrcEditSubMenuRefactor, 'SrcEditAddAssignMethod',
    crsAddAssignMethod2,nil,nil,AddAssignMethodCommand);
  RegisterIDEMenuCommand(itmRefactorAdvanced, 'AddAssignMethod',
    crsAddAssignMethod2,nil,nil,AddAssignMethodCommand);

  // Copy declaration to clipboard
  {$IFDEF EnableCodyExperiments}
  CopyDeclarationToClipboardCommand:=RegisterIDECommand(CmdCatCodeTools,
    'CopyDeclarationToClipboard', crsCopyDeclarationToClipboard,
    CleanIDEShortCut,CleanIDEShortCut,nil,@CopyDeclarationToClipboard);
  RegisterIDEMenuCommand(SrcEditSubMenuRefactor, 'SrcEditCopyDeclarationToClipboard',
    crsCopyDeclarationToClipboard,nil,nil,CopyDeclarationToClipboardCommand);
  RegisterIDEMenuCommand(itmRefactorCodeTools, 'CopyDeclarationToClipboard',
    crsCopyDeclarationToClipboard,nil,nil,CopyDeclarationToClipboardCommand);
  {$ENDIF}

  // Cut declaration to clipboard
  {$IFDEF EnableCodyExperiments}
  CutDeclarationToClipboardCommand:=RegisterIDECommand(CmdCatCodeTools,
    'CutDeclarationToClipboard', crsCutDeclarationToClipboard,
    CleanIDEShortCut,CleanIDEShortCut,nil,@CutDeclarationToClipboard);
  RegisterIDEMenuCommand(SrcEditSubMenuRefactor, 'SrcEditCutDeclarationToClipboard',
    crsCutDeclarationToClipboard,nil,nil,CutDeclarationToClipboardCommand);
  RegisterIDEMenuCommand(itmRefactorCodeTools, 'CutDeclarationToClipboard',
    crsCutDeclarationToClipboard,nil,nil,CutDeclarationToClipboardCommand);
  {$ENDIF}

  // explode a With block
  ExplodeAWithBlockCommand:=RegisterIDECommand(CmdCatCodeTools, 'ExplodeAWithBlock',
    crsExplodeAWithBlock,
    CleanIDEShortCut,CleanIDEShortCut,nil,@ExplodeAWithBlockCmd);
  RegisterIDEMenuCommand(SrcEditSubMenuRefactor, 'SrcEditExplodeAWithBlock',
    crsExplodeAWithBlock, nil, nil, ExplodeAWithBlockCommand);
  RegisterIDEMenuCommand(itmRefactorAdvanced, 'ExplodeAWithBlock',
    crsExplodeAWithBlock, nil, nil, ExplodeAWithBlockCommand);
  // add a With block
  AddAWithBlockCommand:=RegisterIDECommand(CmdCatCodeTools, 'AddAWithBlock',
    crsAddAWithBlock,
    CleanIDEShortCut,CleanIDEShortCut,nil,@ShowAddWithBlockDialog);
  RegisterIDEMenuCommand(SrcEditSubMenuRefactor, 'SrcEditAddAWithBlock',
    crsAddAWithBlock, nil, nil, AddAWithBlockCommand);
  RegisterIDEMenuCommand(itmRefactorAdvanced, 'AddAWithBlock',
    crsAddAWithBlock, nil, nil, AddAWithBlockCommand);

  // IDE internals menu - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  // Show CodeTools node info
  ShowCodeNodeInfoCommand:=RegisterIDECommand(CmdCatCodeTools, 'ShowCodeNodeInfo',
    crsShowCodeToolsNodeInfo,
    CleanIDEShortCut,CleanIDEShortCut,nil,@ShowCodeNodeInfoDialog);
  RegisterIDEMenuCommand(itmViewIDEInternalsWindows, 'ShowCodeNodeInfo',
    crsShowCodeToolsNodeInfo, nil, nil, ShowCodeNodeInfoCommand);

  // View menu - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ViewCodyWindowCommand:=RegisterIDECommand(CmdCatView, 'Cody',
    'Cody', CleanIDEShortCut, CleanIDEShortCut, nil, @ShowCodyWindow);
  RegisterIDEMenuCommand(itmViewMainWindows, 'ViewCody',
    'Cody', nil, nil, ViewCodyWindowCommand)
  {$IFNDEF EnableCodyExperiments}
   .Visible:=false
  {$ENDIF};

  // Components - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  TVIconRes:=LazarusResources.Find('TTreeView');
  LazarusResources.Add(TCodyTreeView.ClassName,TVIconRes.ValueType,TVIconRes.Value);
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

{ TFileDescIDEDockableWindow }

function TFileDescIDEDockableWindow.ExtractTemplate(Src, StartMarker,
  EndMarker: string): string;
var
  StartPos: SizeInt;
  EndPos: SizeInt;
begin
  StartPos:=system.Pos(StartMarker,Src);
  EndPos:=system.Pos(EndMarker,Src);
  if StartPos<1 then begin
    debugln(['ERROR: TFileDescIDEDockableWindow.ExtractTemplate marker "'+StartMarker+'" not found']);
    debugln('Source: ==================================================');
    debugln(Src);
    debugln('==========================================================');
    exit;
  end;
  if EndPos<1 then begin
    debugln(['ERROR: TFileDescIDEDockableWindow.ExtractTemplate marker "'+EndMarker+'" not found']);
    exit;
  end;
  inc(StartPos,length(StartMarker));
  Result:=UTF8Trim(copy(Src,StartPos,EndPos-StartPos));
end;

procedure TFileDescIDEDockableWindow.LoadTemplate;
var
  BaseDir: String;
  Filename: String;
  Code: TCodeBuffer;
begin
  if FTemplateLoaded then exit;

  BaseDir:='$PkgDir(Cody)';
  IDEMacros.SubstituteMacros(BaseDir);
  if (BaseDir='') or (not DirectoryExistsUTF8(BaseDir)) then begin
    debugln(['ERROR: TFileDescIDEDockableWindow.Create cody directory not found']);
    exit;
  end;

  // load unit source
  Filename:=AppendPathDelim(BaseDir)+'templateidedockablewindow.pas';
  if not FileExistsUTF8(Filename) then begin
    debugln(['ERROR: TFileDescIDEDockableWindow.Create template file not found: '+Filename]);
    exit;
  end;
  Code:=CodeToolBoss.LoadFile(Filename,true,false);
  if Code=nil then begin
    debugln(['ERROR: TFileDescIDEDockableWindow.Create unable to read file ',Filename]);
    exit;
  end;

  TemplateUsesSection:=ExtractTemplate(Code.Source,'// UsesStart','// UsesEnd');
  TemplateInterface:=ExtractTemplate(Code.Source,'// InterfaceStart','// InterfaceEnd');
  TemplateImplementation:=ExtractTemplate(Code.Source,'// ImplementationStart','// ImplementationEnd');

  // load lfm
  Filename:=AppendPathDelim(BaseDir)+'templateidedockablewindow.lfm';
  if not FileExistsUTF8(Filename) then begin
    debugln(['ERROR: TFileDescIDEDockableWindow.Create template file not found: '+Filename]);
    exit;
  end;
  Code:=CodeToolBoss.LoadFile(Filename,true,false);
  if Code=nil then begin
    debugln(['ERROR: TFileDescIDEDockableWindow.Create unable to read file ',Filename]);
    exit;
  end;

  TemplateLFM:=Code.Source;
end;

constructor TFileDescIDEDockableWindow.Create;
begin
  inherited Create;
  Name:='IDE window, dockable';
  ResourceClass:=TForm;
  UseCreateFormStatements:=false;
  RequiredPackages:='IDEIntf';
  AddToProject:=false;
  TemplateInterface:='failed loading template';
end;

function TFileDescIDEDockableWindow.GetLocalizedName: string;
begin
  Result:='IDE window, dockable';
end;

function TFileDescIDEDockableWindow.GetLocalizedDescription: string;
begin
  Result:='A window for the Lazarus IDE. It can be docked like the Code Explorer or the FPDoc Editor. This also creates a menu item in the View menu and a short cut.';
end;

function TFileDescIDEDockableWindow.GetInterfaceUsesSection: string;
begin
  LoadTemplate;
  Result:=TemplateUsesSection;
end;

function TFileDescIDEDockableWindow.GetInterfaceSource(const Filename,
  SourceName, ResourceName: string): string;
begin
  LoadTemplate;
  Result:=StringReplace(TemplateInterface,'TemplateName',ResourceName,[rfReplaceAll]);
end;

function TFileDescIDEDockableWindow.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
begin
  LoadTemplate;
  Result:=StringReplace(TemplateImplementation,'TemplateName',ResourceName,[rfReplaceAll])
          +LineEnding+LineEnding;
end;

end.

