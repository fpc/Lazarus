{ /***************************************************************************
                 codetoolsdefines.pas  -  Lazarus IDE unit
                 -----------------------------------------

 ***************************************************************************/

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
    - TCodeToolsDefinesEditor is an editor for the CodeTools DefineTree used by
      the IDE. The DefineTree defines all values, that are not in the sources,
      but are provided by for example Makefiles, compiler command lines and
      compiler config files.
      
    There are three types of nodes:
      - auto generated: These are created by the IDE.
      - project specific: These nodes are saved in the project info file (.lpi)
      - the rest are global nodes, saved in the codetoolsoptions.xml file.

}
unit CodeToolsDefines;

{$mode objfpc}{$H+}

interface

uses
  //Classes, SysUtils, Math, LCLIntf, Forms, Controls, Buttons, StdCtrls,
  //ComCtrls,  LCLType, ExtCtrls, Menus, LCLProc, Graphics, Dialogs, ButtonPanel,
  Classes, SysUtils, Math,
  // LCL
  LCLType, LCLIntf, Forms, Controls, Buttons, StdCtrls,
  ComCtrls, ExtCtrls, Menus, Graphics, Dialogs, ButtonPanel,
  // LazUtils
  LazFileUtils, LazLogger, LazUtilities,
  // SynEdit
  SynEdit,
  // Codetools
  CodeToolManager, DefineTemplates,
  // IdeIntf
  IDEWindowIntf, IDEImagesIntf, IDEDialogs,
  // IDE
  LazarusIDEStrConsts, CodeToolsOptions, CodeToolsDefPreview, TransferMacros,
  EditorOptions, InputFileDialog, LazConf, IDEProcs, EditDefineTree, CompilerOptions;

type

  { TCodeToolsDefinesEditor }

  TCodeToolsDefinesEditor = class(TForm)
    ButtonPanel1: TButtonPanel;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    ValueAsTextPage: TTabSheet;
    ValueAsPathsPage: TTabSheet;
    MainSplitter: TSplitter;
    MainMenu: TMainMenu;


    // edit nodes
    EditMenuItem: TMenuItem;
    MoveNodeUpMenuItem: TMenuItem;
    MoveNodeDownMenuItem: TMenuItem;
    MoveNodeLvlUpMenuItem: TMenuItem;
    MoveNodeLvlDownMenuItem: TMenuItem;
    InsertBehindMenuItem: TMenuItem;
    InsertBehindDefineMenuItem: TMenuItem;
    InsertBehindDefineRecurseMenuItem: TMenuItem;
    InsertBehindUndefineMenuItem: TMenuItem;
    InsertBehindUndefineRecurseMenuItem: TMenuItem;
    InsertBehindUndefineAllMenuItem: TMenuItem;
    InsertBehindBlockMenuItem: TMenuItem;
    InsertBehindDirectoryMenuItem: TMenuItem;
    InsertBehindIfMenuItem: TMenuItem;
    InsertBehindIfDefMenuItem: TMenuItem;
    InsertBehindIfNotDefMenuItem: TMenuItem;
    InsertBehindElseIfMenuItem: TMenuItem;
    InsertBehindElseMenuItem: TMenuItem;
    InsertAsChildMenuItem: TMenuItem;
    InsertAsChildDefineMenuItem: TMenuItem;
    InsertAsChildDefineRecurseMenuItem: TMenuItem;
    InsertAsChildUndefineMenuItem: TMenuItem;
    InsertAsChildUndefineRecurseMenuItem: TMenuItem;
    InsertAsChildUndefineAllMenuItem: TMenuItem;
    InsertAsChildBlockMenuItem: TMenuItem;
    InsertAsChildDirectoryMenuItem: TMenuItem;
    InsertAsChildIfMenuItem: TMenuItem;
    InsertAsChildIfDefMenuItem: TMenuItem;
    InsertAsChildIfNotDefMenuItem: TMenuItem;
    InsertAsChildElseIfMenuItem: TMenuItem;
    InsertAsChildElseMenuItem: TMenuItem;
    DeleteNodeMenuItem: TMenuItem;
    ConvertActionMenuItem: TMenuItem;
    ConvertActionToDefineMenuItem: TMenuItem;
    ConvertActionToDefineRecurseMenuItem: TMenuItem;
    ConvertActionToUndefineMenuItem: TMenuItem;
    ConvertActionToUndefineRecurseMenuItem: TMenuItem;
    ConvertActionToUndefineAllMenuItem: TMenuItem;
    ConvertActionToBlockMenuItem: TMenuItem;
    ConvertActionToDirectoryMenuItem: TMenuItem;
    ConvertActionToIfMenuItem: TMenuItem;
    ConvertActionToIfDefMenuItem: TMenuItem;
    ConvertActionToIfNotDefMenuItem: TMenuItem;
    ConvertActionToElseIfMenuItem: TMenuItem;
    ConvertActionToElseMenuItem: TMenuItem;
    CopyToClipbrdMenuItem: TMenuItem;
    PasteFromClipbrdMenuItem: TMenuItem;

    // tools
    ToolsMenuItem: TMenuItem;
    OpenPreviewMenuItem: TMenuItem;
    ShowMacroListMenuItem: TMenuItem;

    // templates
    InsertTemplateMenuItem: TMenuItem;
    InsertFPCProjectDefinesTemplateMenuItem: TMenuItem;
    InsertFPCompilerDefinesTemplateMenuItem: TMenuItem;
    InsertFPCSourceDirTemplateMenuItem: TMenuItem;
    InsertDelphi5CompilerDefinesTemplateMenuItem: TMenuItem;
    InsertDelphi5DirectoryTemplateMenuItem: TMenuItem;
    InsertDelphi5ProjectTemplateMenuItem: TMenuItem;
    InsertDelphi6CompilerDefinesTemplateMenuItem: TMenuItem;
    InsertDelphi6DirectoryTemplateMenuItem: TMenuItem;
    InsertDelphi6ProjectTemplateMenuItem: TMenuItem;
    InsertDelphi7CompilerDefinesTemplateMenuItem: TMenuItem;
    InsertDelphi7DirectoryTemplateMenuItem: TMenuItem;
    InsertDelphi7ProjectTemplateMenuItem: TMenuItem;
    InsertKylix3CompilerDefinesTemplateMenuItem: TMenuItem;
    InsertKylix3DirectoryTemplateMenuItem: TMenuItem;
    InsertKylix3ProjectTemplateMenuItem: TMenuItem;

    // define tree
    DefineTreeView: TTreeView;

    // selected item
    SelectedItemGroupBox: TGroupBox;
    TypeLabel: TLabel;
    NameLabel: TLabel;
    NameEdit: TEdit;
    DescriptionLabel: TLabel;
    DescriptionEdit: TEdit;
    VariableLabel: TLabel;
    VariableEdit: TEdit;
    ValueNoteBook: TPageControl;
    ValueAsTextSynEdit: TSynEdit;
    ValueAsFilePathsSynEdit: TSynEdit;
    MoveFilePathUpBitBtn: TBitBtn;
    MoveFilePathDownBitBtn: TBitBtn;
    DeleteFilePathBitBtn: TBitBtn;
    InsertFilePathBitBtn: TBitBtn;
    
    // preview
    //DefinePreview: TCodeToolsDefinesPreview;

    // misc
    procedure CodeToolsDefinesEditorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CodeToolsDefinesEditorKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DefineTreeViewSelectionChanged(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);

    // value notebook
    procedure ValueNoteBookPageChanged(Sender: TObject);
    procedure MoveFilePathUpBitBtnClick(Sender: TObject);
    procedure MoveFilePathDownBitBtnClick(Sender: TObject);
    procedure DeleteFilePathBitBtnClick(Sender: TObject);
    procedure InsertFilePathBitBtnClick(Sender: TObject);
    
    // edit menu
    procedure InsertNodeMenuItemClick(Sender: TObject);
    procedure MoveNodeUpMenuItemClick(Sender: TObject);
    procedure MoveNodeDownMenuItemClick(Sender: TObject);
    procedure MoveNodeLvlUpMenuItemClick(Sender: TObject);
    procedure MoveNodeLvlDownMenuItemClick(Sender: TObject);
    procedure DeleteNodeMenuItemClick(Sender: TObject);
    procedure ConvertActionMenuItemClick(Sender: TObject);
    
    // tools menu
    procedure OpenPreviewMenuItemClick(Sender: TObject);
    
    // template menu
    procedure InsertFPCProjectDefinesTemplateMenuItemClick(Sender: TObject);
    procedure InsertFPCompilerDefinesTemplateMenuItemClick(Sender: TObject);
    procedure InsertFPCSourceDirDefinesTemplateMenuItemClick(Sender: TObject);
    procedure InsertDelphiCompilerDefinesTemplateMenuItemClick(Sender: TObject);
    procedure InsertDelphiDirectoryTemplateMenuItemClick(Sender: TObject);
    procedure InsertDelphiProjectTemplateMenuItemClick(Sender: TObject);
    procedure InsertKylixCompilerDefinesTemplateMenuItemClick(Sender: TObject);
    procedure InsertKylixDirectoryTemplateMenuItemClick(Sender: TObject);
    procedure InsertKylixProjectTemplateMenuItemClick(Sender: TObject);
  private
    FDefineTree: TDefineTree;
    FLastSelectedNode: TTreeNode;
    FBoss: TCodeToolManager;
    FTransferMacros: TTransferMacroList;
    procedure CreateComponents;
    function CreateSeperator : TMenuItem;
    procedure SetTransferMacros(const AValue: TTransferMacroList);
    procedure ValueAsPathToValueAsText;
    procedure SaveSelectedValues;
    procedure ShowSelectedValues;
    procedure SetTypeLabel;
    function ValueToFilePathText(const AValue: string): string;
    procedure InsertNewNode(Behind: boolean; DefAction: TDefineAction);
    procedure InsertTemplate(NewTemplate: TDefineTemplate);
    function FindUniqueName: string;
    function ConsistencyCheck: integer;
    procedure SetValuesEditable(AValue: boolean);
  public
    procedure SetOptions(ACodeToolBoss: TCodeToolManager);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property DefineTree: TDefineTree read FDefineTree;
    property Boss: TCodeToolManager read FBoss write FBoss;
    property Macros: TTransferMacroList
      read FTransferMacros write SetTransferMacros;
  end;

function ShowCodeToolsDefinesEditor(ACodeToolBoss: TCodeToolManager;
  Options: TCodeToolsOptions; Macros: TTransferMacroList): TModalResult;


implementation

{$R *.lfm}

function ShowCodeToolsDefinesEditor(ACodeToolBoss: TCodeToolManager;
  Options: TCodeToolsOptions; Macros: TTransferMacroList): TModalResult;
var CodeToolsDefinesEditor: TCodeToolsDefinesEditor;
begin
  CodeToolsDefinesEditor:=TCodeToolsDefinesEditor.Create(nil);
  CodeToolsDefinesEditor.SetOptions(ACodeToolBoss);
  CodeToolsDefinesEditor.Macros:=Macros;
  Result:=CodeToolsDefinesEditor.ShowModal;
  if Result=mrOk then begin
    if not CodeToolsDefinesEditor.DefineTree.IsEqual(ACodeToolBoss.DefineTree)
    then begin
      ACodeToolBoss.DefineTree.AssignNonAutoCreated(
        CodeToolsDefinesEditor.DefineTree);
      Options.ReadGlobalDefinesTemplatesFromTree(ACodeToolBoss.DefineTree);
      Options.Save;
    end;
  end;
  CodeToolsDefinesEditor.Free;
end;

{ TCodeToolsDefinesEditor }

procedure TCodeToolsDefinesEditor.CodeToolsDefinesEditorKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Shift=[]) and (Key=VK_ESCAPE) then ModalResult:=mrCancel;
end;

procedure TCodeToolsDefinesEditor.CodeToolsDefinesEditorKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_ESCAPE) and (Shift=[]) then
    ModalResult:=mrCancel;
end;

procedure TCodeToolsDefinesEditor.DefineTreeViewSelectionChanged(Sender: TObject);
begin
  ShowSelectedValues;
end;

procedure TCodeToolsDefinesEditor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if CloseAction=caNone then ;
  CodeToolsOpts.DefinesEditMainSplitterTop:=MainSplitter.Top;
  CodeToolsOpts.Save;
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TCodeToolsDefinesEditor.FormCreate(Sender: TObject);
begin
  ButtonPanel1.OKButton.Caption:= lisOk;
  ButtonPanel1.CancelButton.Caption:= lisCancel;
end;

procedure TCodeToolsDefinesEditor.OKButtonClick(Sender: TObject);
begin
  SaveSelectedValues;
  FLastSelectedNode:=nil;
  ModalResult:=mrOk;
end;

procedure TCodeToolsDefinesEditor.ValueNoteBookPageChanged(Sender: TObject);
begin
  if ValueNoteBook.PageIndex=0 then
    ValueAsPathToValueAsText
  else
    ValueAsFilePathsSynEdit.Text:=ValueToFilePathText(ValueAsTextSynEdit.Text);
end;

procedure TCodeToolsDefinesEditor.MoveFilePathUpBitBtnClick(Sender: TObject);
var y: integer;
begin
  if ValueAsFilePathsSynEdit.ReadOnly then exit;
  y:=ValueAsFilePathsSynEdit.CaretY-1;
  if (y>0) and (y<ValueAsFilePathsSynEdit.Lines.Count) then
    ValueAsFilePathsSynEdit.Lines.Move(y,y-1);
end;

procedure TCodeToolsDefinesEditor.MoveFilePathDownBitBtnClick(Sender: TObject);
var y: integer;
begin
  if ValueAsFilePathsSynEdit.ReadOnly then exit;
  y:=ValueAsFilePathsSynEdit.CaretY-1;
  if (y>=0) and (y<ValueAsFilePathsSynEdit.Lines.Count-1) then
    ValueAsFilePathsSynEdit.Lines.Move(y,y+1);
end;

procedure TCodeToolsDefinesEditor.DeleteFilePathBitBtnClick(Sender: TObject);
var y: integer;
begin
  if ValueAsFilePathsSynEdit.ReadOnly then exit;
  y:=ValueAsFilePathsSynEdit.CaretY-1;
  if (y>=0) and (y<ValueAsFilePathsSynEdit.Lines.Count) then
    ValueAsFilePathsSynEdit.Lines.Delete(y);
end;

procedure TCodeToolsDefinesEditor.InsertFilePathBitBtnClick(Sender: TObject);
var y: integer;
begin
  if ValueAsFilePathsSynEdit.ReadOnly then exit;
  y:=ValueAsFilePathsSynEdit.CaretY-1;
  if (y>=0) and (y<ValueAsFilePathsSynEdit.Lines.Count) then
    ValueAsFilePathsSynEdit.Lines.Insert(y,'');
end;

procedure TCodeToolsDefinesEditor.InsertNodeMenuItemClick(Sender: TObject);
var Behind: boolean;
  DefAction: TDefineAction;
begin
  Behind:=(TMenuItem(Sender).Parent=InsertBehindMenuItem);
  if Sender=InsertBehindDefineMenuItem then DefAction:=da_Define
  else if Sender=InsertBehindDefineRecurseMenuItem then DefAction:=da_DefineRecurse
  else if Sender=InsertBehindUndefineMenuItem then DefAction:=da_Undefine
  else if Sender=InsertBehindUndefineRecurseMenuItem then DefAction:=da_UndefineRecurse
  else if Sender=InsertBehindUndefineAllMenuItem then DefAction:=da_UndefineAll
  else if Sender=InsertBehindBlockMenuItem then DefAction:=da_Block
  else if Sender=InsertBehindDirectoryMenuItem then DefAction:=da_Directory
  else if Sender=InsertBehindIfMenuItem then DefAction:=da_If
  else if Sender=InsertBehindIfDefMenuItem then DefAction:=da_IfDef
  else if Sender=InsertBehindIfNotDefMenuItem then DefAction:=da_IfNDef
  else if Sender=InsertBehindElseIfMenuItem then DefAction:=da_ElseIf
  else if Sender=InsertBehindElseMenuItem then DefAction:=da_Else
  else if Sender=InsertAsChildDefineMenuItem then DefAction:=da_Define
  else if Sender=InsertAsChildDefineRecurseMenuItem then DefAction:=da_DefineRecurse
  else if Sender=InsertAsChildUndefineMenuItem then DefAction:=da_Undefine
  else if Sender=InsertAsChildUndefineRecurseMenuItem then DefAction:=da_UndefineRecurse
  else if Sender=InsertAsChildUndefineAllMenuItem then DefAction:=da_UndefineAll
  else if Sender=InsertAsChildBlockMenuItem then DefAction:=da_Block
  else if Sender=InsertAsChildDirectoryMenuItem then DefAction:=da_Directory
  else if Sender=InsertAsChildIfMenuItem then DefAction:=da_If
  else if Sender=InsertAsChildIfDefMenuItem then DefAction:=da_IfDef
  else if Sender=InsertAsChildIfNotDefMenuItem then DefAction:=da_IfNDef
  else if Sender=InsertAsChildElseIfMenuItem then DefAction:=da_ElseIf
  else if Sender=InsertAsChildElseMenuItem then DefAction:=da_Else
  else DefAction:=da_None;
  InsertNewNode(Behind,DefAction);
end;

procedure TCodeToolsDefinesEditor.MoveNodeUpMenuItemClick(Sender: TObject);
var
  SelTreeNode: TTreeNode;
  SelDefNode, PrevDefNode: TDefineTemplate;
begin
  SelTreeNode:=DefineTreeView.Selected;
  SaveSelectedValues;
  if (SelTreeNode=nil) or (SelTreeNode.GetPrevSibling=nil) then exit;
  SelDefNode:=TDefineTemplate(SelTreeNode.Data);
  PrevDefNode:=SelDefNode.Prior;
  // move node up in TreeView
  SelTreeNode.MoveTo(SelTreeNode.GetPrevSibling,naInsert);
  // move node up in DefineTree
  SelDefNode.Unbind;
  SelDefNode.InsertInFront(PrevDefNode);
  SelTreeNode.MakeVisible;
end;

procedure TCodeToolsDefinesEditor.MoveNodeDownMenuItemClick(Sender: TObject);
var
  SelTreeNode: TTreeNode;
  SelDefNode, NextDefNode: TDefineTemplate;
begin
  SelTreeNode:=DefineTreeView.Selected;
  SaveSelectedValues;
  if (SelTreeNode=nil) or (SelTreeNode.GetNextSibling=nil) then exit;
  SelDefNode:=TDefineTemplate(SelTreeNode.Data);
  NextDefNode:=SelDefNode.Next;
  // move node down in TreeView
  if SelTreeNode.GetNextSibling.GetNextSibling<>nil then
    SelTreeNode.MoveTo(SelTreeNode.GetNextSibling.GetNextSibling,naInsert)
  else
    SelTreeNode.MoveTo(SelTreeNode.GetNextSibling,naAdd);
  // move node down in DefineTree
  SelDefNode.Unbind;
  SelDefNode.InsertBehind(NextDefNode);
  SelTreeNode.MakeVisible;
end;

procedure TCodeToolsDefinesEditor.MoveNodeLvlUpMenuItemClick(Sender: TObject);
var
  SelTreeNode: TTreeNode;
  SelDefNode, PrevDefNode: TDefineTemplate;
begin
  SelTreeNode:=DefineTreeView.Selected;
  SaveSelectedValues;
  if (SelTreeNode=nil) or (SelTreeNode.Parent=nil) then exit;
  SelDefNode:=TDefineTemplate(SelTreeNode.Data);
  if SelDefNode.IsAutoGenerated then begin
    IDEMessageDialog(lisCodeToolsDefsNodeIsReadonly,
      lisCodeToolsDefsAutoGeneratedNodesCanNotBeEdited,
      mtInformation,[mbCancel]);
    exit;
  end;
  // move node one lvl up in TreeView
  if SelTreeNode.Parent.GetNextSibling<>nil then
    SelTreeNode.MoveTo(SelTreeNode.Parent.GetNextSibling,naInsert)
  else
    SelTreeNode.MoveTo(SelTreeNode.Parent,naAdd);
  // move node one lvl up in DefineTree
  PrevDefNode:=SelDefNode.Parent;
  SelDefNode.Unbind;
  SelDefNode.InsertBehind(PrevDefNode);
  SetNodeImages(SelTreeNode,true);
  SelTreeNode.MakeVisible;
end;

procedure TCodeToolsDefinesEditor.MoveNodeLvlDownMenuItemClick(Sender: TObject);
var
  SelTreeNode: TTreeNode;
  SelDefNode, PrevDefNode: TDefineTemplate;
begin
  SelTreeNode:=DefineTreeView.Selected;
  SaveSelectedValues;
  if (SelTreeNode=nil) or (SelTreeNode.GetPrevSibling=nil) then exit;
  SelDefNode:=TDefineTemplate(SelTreeNode.Data);
  PrevDefNode:=SelDefNode.Prior;
  if (SelDefNode.IsAutoGenerated) or (PrevDefNode.IsAutoGenerated) then begin
    IDEMessageDialog(lisCodeToolsDefsNodeIsReadonly,
      lisCodeToolsDefsAutoGeneratedNodesCanNotBeEdited,
      mtInformation,[mbCancel]);
    exit;
  end;
  if (not (PrevDefNode.Action in DefineActionBlocks)) then begin
    IDEMessageDialog(lisCodeToolsDefsInvalidPreviousNode,
      lisCodeToolsDefsPreviousNodeCanNotContainChildNodes,
      mtInformation,[mbCancel]);
    exit;
  end;
  // move node one lvl down in TreeView
  SelTreeNode.MoveTo(SelTreeNode.GetPrevSibling,naAddChild);
  // move node one lvl up in DefineTree
  SelDefNode.Unbind;
  PrevDefNode.AddChild(SelDefNode);
  SetNodeImages(SelTreeNode.Parent,true);
  SelTreeNode.MakeVisible;
end;

procedure TCodeToolsDefinesEditor.DeleteNodeMenuItemClick(Sender: TObject);
var
  SelTreeNode: TTreeNode;
  SelDefNode: TDefineTemplate;
begin
  SelTreeNode:=DefineTreeView.Selected;
  SaveSelectedValues;
  if (SelTreeNode=nil) then exit;
  SelDefNode:=TDefineTemplate(SelTreeNode.Data);
  if (SelDefNode.IsAutoGenerated) then begin
    IDEMessageDialog(lisCodeToolsDefsNodeIsReadonly,
      lisCodeToolsDefsAutoGeneratedNodesCanNotBeEdited,
      mtInformation,[mbCancel]);
    exit;
  end;
  if FLastSelectedNode=SelTreeNode then FLastSelectedNode:=nil;
  // delete node in TreeView
  SelTreeNode.Free;
  // delete node in DefineTree
  SelDefNode.Unbind;
  SelDefNode.Free;
end;

procedure TCodeToolsDefinesEditor.ConvertActionMenuItemClick(Sender: TObject);
var
  NewAction: TDefineAction;
  SelTreeNode: TTreeNode;
  SelDefNode: TDefineTemplate;
begin
  SelTreeNode:=DefineTreeView.Selected;
  SaveSelectedValues;
  if SelTreeNode=nil then exit;
  SelDefNode:=TDefineTemplate(SelTreeNode.Data);
  if (SelDefNode.IsAutoGenerated) then begin
    IDEMessageDialog(lisCodeToolsDefsNodeIsReadonly,
      lisCodeToolsDefsAutoGeneratedNodesCanNotBeEdited,
      mtInformation,[mbCancel]);
    exit;
  end;
  if Sender=ConvertActionToDefineMenuItem then NewAction:=da_Define
  else if Sender=ConvertActionToDefineRecurseMenuItem then NewAction:=da_DefineRecurse
  else if Sender=ConvertActionToUndefineMenuItem then NewAction:=da_Undefine
  else if Sender=ConvertActionToUndefineRecurseMenuItem then NewAction:=da_UndefineRecurse
  else if Sender=ConvertActionToUndefineAllMenuItem then NewAction:=da_UndefineAll
  else if Sender=ConvertActionToBlockMenuItem then NewAction:=da_Block
  else if Sender=ConvertActionToDirectoryMenuItem then NewAction:=da_Directory
  else if Sender=ConvertActionToIfMenuItem then NewAction:=da_If
  else if Sender=ConvertActionToIfDefMenuItem then NewAction:=da_IfDef
  else if Sender=ConvertActionToIfNotDefMenuItem then NewAction:=da_IfNDef
  else if Sender=ConvertActionToElseIfMenuItem then NewAction:=da_ElseIf
  else if Sender=ConvertActionToElseMenuItem then NewAction:=da_Else
  else NewAction:=da_None;
  SelDefNode.Action:=NewAction;
  SetNodeImages(SelTreeNode,false);
  SetTypeLabel;
end;

procedure TCodeToolsDefinesEditor.OpenPreviewMenuItemClick(Sender: TObject);
begin
  // apply changed tree
  CodeToolBoss.DefineTree.AssignNonAutoCreated(DefineTree);
  try
    // show preview
    ShowCodeToolsDefinesValuesDialog(DefineTree,'');
  finally
    // restore old tree
    CodeToolsOpts.AssignGlobalDefineTemplatesToTree(CodeToolBoss.DefineTree);
  end;
end;

procedure TCodeToolsDefinesEditor.InsertFPCProjectDefinesTemplateMenuItemClick(
  Sender: TObject);
var InputFileDlg: TInputFileDialog;
  DefaultFPCSrcDir, DefaultCompiler,
  CompilerPath, FPCSrcDir: string;
  DirTemplate, FPCTemplate: TDefineTemplate;
  TargetOS, TargetProcessor: string;
  UnitSetCache: TFPCUnitSetCache;
begin
  InputFileDlg:=GetInputFileDialog;
  InputFileDlg.Macros:=Macros;
  with InputFileDlg do begin
  
    DefaultFPCSrcDir:='$(FPCSrcDir)';
    DefaultCompiler:=DefaultCompilerPath;

    BeginUpdate;
    Caption:=lisCodeToolsDefsCreateFPCMacrosAndPathsForAFPCProjectDirectory;

    FileCount:=3;

    FileTitles[0]:=lisCodeToolsDefsProjectDirectory;
    FileDescs[0]:=lisCodeToolsDefsTheFreePascalProjectDirectory;
    FileNames[0]:='';
    FileFlags[0]:=[iftDirectory,iftNotEmpty,iftMustExist];

    FileTitles[1]:=lisCodeToolsDefscompilerPath;
    FileDescs[1]:=BreakString(
      lisCodeToolsDefsThePathToTheFreePascalCompilerForThisProject, 60, 0);
    FileNames[1]:=DefaultCompiler;
    FileFlags[1]:=[iftFilename];

    FileTitles[2]:=lisCodeToolsDefsFPCSVNSourceDirectory;
    FileDescs[2]:=BreakString(
      lisCodeToolsDefsTheFreePascalCVSSourceDirectory, 60, 0);
    FileNames[2]:=DefaultFPCSrcDir;
    FileFlags[2]:=[iftDirectory];

    EndUpdate;
    if ShowModal=mrCancel then exit;

    FPCSrcDir:=FileNames[2];
    if Macros<>nil then Macros.SubstituteStr(FPCSrcDir);
    if FPCSrcDir='' then FPCSrcDir:=DefaultFPCSrcDir;
    DebugLn('  FPCSrcDir="',FPCSrcDir,'"');

    // ask the compiler for Macros
    CompilerPath:=FileNames[1];
    if Macros<>nil then Macros.SubstituteStr(CompilerPath);
    DebugLn('  CompilerPath="',CompilerPath,'"');
    TargetOS:='';
    TargetProcessor:='';

    UnitSetCache:=Boss.CompilerDefinesCache.FindUnitSet(CompilerPath,
                                    TargetOS,TargetProcessor,'',FPCSrcDir,true);

    // create directory defines
    DirTemplate:=TDefineTemplate.Create('FPC Project ('+FileNames[0]+')',
       'Free Pascal Project Directory','',FileNames[0],da_Directory);
       
    if (DefaultFPCSrcDir=Filenames[2]) and (DefaultCompiler=Filenames[1]) then
    begin
      // a normal fpc project -> nothing special needed
    end else begin
      // a special fpc project -> create a world of its own
      DirTemplate.AddChild(TDefineTemplate.Create('Reset All',
         'Reset all values','','',da_UndefineAll));
      FPCTemplate:=CreateFPCTemplate(UnitSetCache,CodeToolsOpts);
      if FPCTemplate<>nil then
        DirTemplate.AddChild(FPCTemplate);
    end;

    DirTemplate.SetDefineOwner(CodeToolsOpts,true);
    InsertTemplate(DirTemplate);
  end;
end;

procedure TCodeToolsDefinesEditor.InsertFPCompilerDefinesTemplateMenuItemClick(
  Sender: TObject);
var InputFileDlg: TInputFileDialog;
  CompilerPath, DefaultCompiler: string;
  FPCTemplate: TDefineTemplate;
  UnitSearchPath, TargetOS, TargetProcessor: string;
begin
  InputFileDlg:=GetInputFileDialog;
  InputFileDlg.Macros:=Macros;
  with InputFileDlg do begin
    DefaultCompiler:=DefaultCompilerPath;
    
    BeginUpdate;
    Caption:=lisCodeToolsDefsCreateDefinesForFreePascalCompiler;
    FileCount:=1;

    FileTitles[0]:=lisCodeToolsDefscompilerPath;
    FileDescs[0]:=Format(
      lisCodeToolsDefsThePathToTheFreePascalCompilerForExample, [LineEnding,
       '"', GetDefaultCompilerFilename, '"', '"', '"']);
    FileNames[0]:=DefaultCompiler;
    FileFlags[0]:=[iftCmdLine,iftNotEmpty];

    EndUpdate;
    if ShowModal=mrCancel then exit;
    
    CompilerPath:=FileNames[0];
    if Macros<>nil then Macros.SubstituteStr(CompilerPath);
    DebugLn('  CompilerPath="',CompilerPath,'"');
    
    FPCTemplate:=Boss.DefinePool.CreateFPCTemplate(CompilerPath,'',
                                           CreateCompilerTestPascalFilename,
                                           UnitSearchPath,
                                           TargetOS,TargetProcessor,
                                           CodeToolsOpts);
    if (TargetOS='') or (TargetProcessor='') or (UnitSearchPath='') then
      DebugLn(['TCodeToolsDefinesEditor.InsertFPCompilerDefinesTemplateMenuItemClick TargetOS="',TargetOS,'" TargetProcessor="',TargetProcessor,'"']);
    if FPCTemplate=nil then exit;
    FPCTemplate.Name:='Free Pascal Compiler ('+CompilerPath+')';
    InsertTemplate(FPCTemplate);
  end;
end;

procedure TCodeToolsDefinesEditor.InsertFPCSourceDirDefinesTemplateMenuItemClick
  (Sender: TObject);
var InputFileDlg: TInputFileDialog;
  DefaultCompiler, CompilerPath, FPCSrcDir: string;
  TargetOS, TargetProcessor: string;
  FPCSrcTemplate: TDefineTemplate;
  UnitSetCache: TFPCUnitSetCache;
begin
  InputFileDlg:=GetInputFileDialog;
  InputFileDlg.Macros:=Macros;
  with InputFileDlg do begin
    DefaultCompiler:=DefaultCompilerPath;

    BeginUpdate;
    Caption:=lisCodeToolsDefsCreateDefinesForFreePascalSVNSources;
    FileCount:=2;

    FileTitles[0]:=lisCodeToolsDefsFPCSVNSourceDirectory;
    FileDescs[0]:=lisCodeToolsDefsTheFreePascalSVNSourceDir;
    FileNames[0]:='~/fpc_sources/2.4.1/fpc';
    FileFlags[0]:=[iftDirectory,iftNotEmpty,iftMustExist];

    FileTitles[1]:=lisCodeToolsDefsCompilerPath;
    FileDescs[1]:=Format(
      lisCodeToolsDefsThePathToTheFreePascalCompilerForThisSourceUsedToA, [LineEnding]);
    FileNames[1]:=DefaultCompiler;
    FileFlags[1]:=[iftFilename];

    EndUpdate;
    if ShowModal=mrCancel then exit;
    
    // ask the compiler for Macros
    CompilerPath:=FileNames[1];
    if Macros<>nil then Macros.SubstituteStr(CompilerPath);
    DebugLn('  CompilerPath="',CompilerPath,'"');

    TargetOS:='';
    TargetProcessor:='';
    FPCSrcDir:=FileNames[0];
    if Macros<>nil then Macros.SubstituteStr(FPCSrcDir);
    DebugLn('  FPCSrcDir="',FPCSrcDir,'"');

    UnitSetCache:=Boss.CompilerDefinesCache.FindUnitSet(CompilerPath,
                                    TargetOS,TargetProcessor,'',FPCSrcDir,true);
    // create FPC Source defines
    FPCSrcTemplate:=CreateFPCSourceTemplate(UnitSetCache,CodeToolsOpts);
    if FPCSrcTemplate=nil then begin
      DebugLn('ERROR: unable to create FPC CVS Src defines for "',FPCSrcDir,'"');
      exit;
    end;

    // create directory defines
    FPCSrcTemplate.Name:='FPC SVN Sources ('+FileNames[0]+')';

    FPCSrcTemplate.SetDefineOwner(CodeToolsOpts,true);
    InsertTemplate(FPCSrcTemplate);
  end;
end;

procedure TCodeToolsDefinesEditor.InsertDelphiCompilerDefinesTemplateMenuItemClick
  (Sender: TObject);
var DelphiVersion: integer;
begin
  if Sender=InsertDelphi7CompilerDefinesTemplateMenuItem then
    DelphiVersion:=7
  else if Sender=InsertDelphi6CompilerDefinesTemplateMenuItem then
    DelphiVersion:=6
  else
    DelphiVersion:=5;
  InsertTemplate(Boss.DefinePool.CreateDelphiCompilerDefinesTemplate(
                                                  DelphiVersion,CodeToolsOpts));
end;

procedure TCodeToolsDefinesEditor.InsertDelphiDirectoryTemplateMenuItemClick(
  Sender: TObject);
var InputFileDlg: TInputFileDialog;
  DirTemplate: TDefineTemplate;
  DelphiVersion: integer;
  DelphiName: string;
begin
  if Sender=InsertDelphi7DirectoryTemplateMenuItem then
    DelphiVersion:=7
  else if Sender=InsertDelphi6DirectoryTemplateMenuItem then
    DelphiVersion:=6
  else
    DelphiVersion:=5;
  DelphiName:='Delphi'+IntToStr(DelphiVersion);

  InputFileDlg:=GetInputFileDialog;
  InputFileDlg.Macros:=Macros;
  with InputFileDlg do begin
    BeginUpdate;
    Caption:=Format(lisCodeToolsDefsCreateDefinesForDirectory, [DelphiName]);
    FileCount:=1;
    
    FileTitles[0]:=Format(lisCodeToolsDefsdirectory, [DelphiName]);
    FileDescs[0]:=Format(lisCodeToolsDefsDelphiMainDirectoryDesc, [DelphiName,
      LineEnding, DelphiName, LineEnding, IntToStr(DelphiVersion)]);
    FileNames[0]:=GetForcedPathDelims(
                        'C:/Programme/Borland/Delphi'+IntToStr(DelphiVersion));
    FileFlags[0]:=[iftDirectory,iftNotEmpty,iftMustExist];
    
    EndUpdate;
    if ShowModal=mrCancel then exit;
    DirTemplate:=Boss.DefinePool.CreateDelphiDirectoryTemplate(FileNames[0],
                                                   DelphiVersion,CodeToolsOpts);
    if DirTemplate=nil then exit;
    DirTemplate.Name:=DelphiName+' ('+FileNames[0]+')';
    InsertTemplate(DirTemplate);
  end;
end;

procedure TCodeToolsDefinesEditor.InsertDelphiProjectTemplateMenuItemClick(
  Sender: TObject);
var
  InputFileDlg: TInputFileDialog;
  ProjTemplate: TDefineTemplate;
  DelphiVersion: integer;
  DelphiName: string;
begin
  if Sender=InsertDelphi7ProjectTemplateMenuItem then
    DelphiVersion:=7
  else if Sender=InsertDelphi6ProjectTemplateMenuItem then
    DelphiVersion:=6
  else
    DelphiVersion:=5;
  DelphiName:='Delphi'+IntToStr(DelphiVersion);

  InputFileDlg:=GetInputFileDialog;
  InputFileDlg.Macros:=Macros;
  with InputFileDlg do begin
    BeginUpdate;
    Caption:=Format(lisCodeToolsDefsCreateDefinesForProject, [DelphiName]);

    FileCount:=2;
    
    FileTitles[0]:=Format(lisCodeToolsDefsprojectDirectory2, [DelphiName]);
    FileDescs[0]:=Format(lisCodeToolsDefsTheProjectDirectory, [DelphiName, LineEnding]);
    FileNames[0]:=GetForcedPathDelims('C:/Programme/Borland/Delphi'
                   +IntToStr(DelphiVersion)+'/YourProject');
    FileFlags[0]:=[iftDirectory,iftNotEmpty,iftMustExist];
    
    FileTitles[1]:=Format(lisCodeToolsDefsdirectory, [DelphiName]);
    FileDescs[1]:=Format(lisCodeToolsDefsDelphiMainDirectoryForProject, [DelphiName,
      LineEnding, DelphiName, LineEnding, DelphiName, LineEnding, IntToStr(DelphiVersion)]);
    FileNames[1]:=GetForcedPathDelims('C:/Programme/Borland/Delphi'+IntToStr(DelphiVersion));
    FileFlags[1]:=[iftDirectory,iftNotEmpty,iftMustExist];

    EndUpdate;
    if ShowModal=mrCancel then exit;
    ProjTemplate:=Boss.DefinePool.CreateDelphiProjectTemplate(FileNames[0],
                                      FileNames[1],DelphiVersion,CodeToolsOpts);
    if ProjTemplate=nil then exit;
    ProjTemplate.Name:=DelphiName+' Project ('+FileNames[0]+')';
    InsertTemplate(ProjTemplate);
  end;
end;

procedure TCodeToolsDefinesEditor.InsertKylixCompilerDefinesTemplateMenuItemClick
  (Sender: TObject);
var KylixVersion: integer;
begin
  KylixVersion:=3;
  InsertTemplate(Boss.DefinePool.CreateKylixCompilerDefinesTemplate(
                                                   KylixVersion,CodeToolsOpts));
end;

procedure TCodeToolsDefinesEditor.InsertKylixDirectoryTemplateMenuItemClick(
  Sender: TObject);
var
  InputFileDlg: TInputFileDialog;
  DirTemplate: TDefineTemplate;
  KylixVersion: integer;
  KylixName: string;
  UserName: String;
begin
  KylixVersion:=3;
  KylixName:='Kylix'+IntToStr(KylixVersion);

  UserName:=GetCurrentUserName;
  if UserName='' then UserName:='user';
  InputFileDlg:=GetInputFileDialog;
  InputFileDlg.Macros:=Macros;
  with InputFileDlg do begin
    BeginUpdate;
    Caption:=Format(lisCodeToolsDefsCreateDefinesForDirectory, [KylixName]);
    FileCount:=1;

    FileTitles[0]:=Format(lisCodeToolsDefsdirectory, [KylixName]);
    FileDescs[0]:=Format(lisCodeToolsDefsKylixMainDirectoryDesc, [KylixName,
      LineEnding, KylixName, LineEnding, IntToStr(KylixVersion)]);
    FileNames[0]:=GetForcedPathDelims('/home/'+UserName+'/kylix'+IntToStr(KylixVersion));
    FileFlags[0]:=[iftDirectory,iftNotEmpty,iftMustExist];

    EndUpdate;
    if ShowModal=mrCancel then exit;
    DirTemplate:=Boss.DefinePool.CreateKylixDirectoryTemplate(FileNames[0],
                                                    KylixVersion,CodeToolsOpts);
    if DirTemplate=nil then exit;
    DirTemplate.Name:=KylixName+' ('+FileNames[0]+')';
    InsertTemplate(DirTemplate);
  end;
end;

procedure TCodeToolsDefinesEditor.InsertKylixProjectTemplateMenuItemClick(
  Sender: TObject);
var
  InputFileDlg: TInputFileDialog;
  ProjTemplate: TDefineTemplate;
  KylixVersion: integer;
  KylixName: string;
  UserName: String;
begin
  KylixVersion:=3;
  KylixName:='Kylix'+IntToStr(KylixVersion);

  UserName:=GetCurrentUserName;
  if UserName='' then UserName:='user';
  InputFileDlg:=GetInputFileDialog;
  InputFileDlg.Macros:=Macros;
  with InputFileDlg do begin
    BeginUpdate;
    Caption:=Format(lisCodeToolsDefsCreateDefinesForProject, [KylixName]);

    FileCount:=2;

    FileTitles[0]:=Format(lisCodeToolsDefsprojectDirectory2, [KylixName]);
    FileDescs[0]:=Format(lisCodeToolsDefsTheProjectDirectory, [KylixName, LineEnding]
      );
    FileNames[0]:=GetForcedPathDelims('/home/'+UserName+'/kylix'
                   +IntToStr(KylixVersion)+'/YourProject');
    FileFlags[0]:=[iftDirectory,iftNotEmpty,iftMustExist];

    FileTitles[1]:=Format(lisCodeToolsDefsdirectory, [KylixName]);
    FileDescs[1]:=Format(lisCodeToolsDefsKylixMainDirectoryForProject, [KylixName,
      LineEnding, KylixName, LineEnding, KylixName, LineEnding, IntToStr(KylixVersion)
      ]);
    FileNames[1]:=GetForcedPathDelims('/home/'+UserName+'/kylix'+IntToStr(KylixVersion));
    FileFlags[1]:=[iftDirectory,iftNotEmpty,iftMustExist];

    EndUpdate;
    if ShowModal=mrCancel then exit;
    ProjTemplate:=Boss.DefinePool.CreateDelphiProjectTemplate(FileNames[0],
                                       FileNames[1],KylixVersion,CodeToolsOpts);
    if ProjTemplate=nil then exit;
    ProjTemplate.Name:=KylixName+' Project ('+FileNames[0]+')';
    InsertTemplate(ProjTemplate);
  end;
end;

procedure TCodeToolsDefinesEditor.CreateComponents;
var
  DefAction: TDefineAction;
begin
  // edit nodes
  EditMenuItem.Caption := lisEdit;
  MoveNodeUpMenuItem.Caption:=lisCodeToolsDefsMoveNodeUp;
  MoveNodeDownMenuItem.Caption := lisCodeToolsDefsMoveNodeDown;
  MoveNodeLvlUpMenuItem.Caption := lisCodeToolsDefsMoveNodeOneLevelUp;
  MoveNodeLvlDownMenuItem.Caption := lisCodeToolsDefsMoveNodeOneLevelDown;
  InsertBehindMenuItem.Caption := lisCodeToolsDefsInsertNodeBelow;
  InsertAsChildMenuItem.Caption := lisCodeToolsDefsInsertNodeAsChild;
  DeleteNodeMenuItem.Caption := lisCodeToolsDefsDeleteNode;
  ConvertActionMenuItem.Caption := lisCodeToolsDefsConvertNode;

  // insert node behind submenu
  InsertBehindDefineMenuItem.Caption := lisCodeToolsDefsDefine;
  InsertBehindDefineRecurseMenuItem.Caption := lisCodeToolsDefsDefineRecurse;
  InsertBehindUndefineMenuItem.Caption := lisCodeToolsDefsUndefine;
  InsertBehindUndefineRecurseMenuItem.Caption := lisCodeToolsDefsUndefineRecurse;
  InsertBehindUndefineAllMenuItem.Caption := lisCodeToolsDefsUndefineAll;
  InsertBehindBlockMenuItem.Caption := lisCodeToolsDefsBlock;
  InsertBehindDirectoryMenuItem.Caption := lisCodeToolsDefsInsertBehindDirectory;
  InsertBehindIfMenuItem.Caption := lisCodeToolsDefsIf;
  InsertBehindIfDefMenuItem.Caption := lisCodeToolsDefsIfDef;
  InsertBehindIfNotDefMenuItem.Caption := lisCodeToolsDefsIfNDef;
  InsertBehindElseIfMenuItem.Caption := lisCodeToolsDefsElseIf;
  InsertBehindElseMenuItem.Caption := lisCodeToolsDefsElse;

  // insert node as child submenu
  InsertAsChildDefineMenuItem.Caption := lisCodeToolsDefsDefine;
  InsertAsChildDefineRecurseMenuItem.Caption := lisCodeToolsDefsDefineRecurse;
  InsertAsChildUndefineMenuItem.Caption := lisCodeToolsDefsUndefine;
  InsertAsChildUndefineRecurseMenuItem.Caption := lisCodeToolsDefsUndefineRecurse;
  InsertAsChildUndefineAllMenuItem.Caption := lisCodeToolsDefsUndefineAll;
  InsertAsChildBlockMenuItem.Caption := lisCodeToolsDefsBlock;
  InsertAsChildDirectoryMenuItem.Caption := lisCodeToolsDefsInsertBehindDirectory;
  InsertAsChildIfMenuItem.Caption := lisCodeToolsDefsIf;
  InsertAsChildIfDefMenuItem.Caption := lisCodeToolsDefsIfDef;
  InsertAsChildIfNotDefMenuItem.Caption := lisCodeToolsDefsIfNDef;
  InsertAsChildElseIfMenuItem.Caption := lisCodeToolsDefsElseIf;
  InsertAsChildElseMenuItem.Caption := lisCodeToolsDefsElse;

  // convert node sub menu
  ConvertActionToDefineMenuItem.Caption := lisCodeToolsDefsDefine;
  ConvertActionToDefineRecurseMenuItem.Caption := lisCodeToolsDefsDefineRecurse;
  ConvertActionToUndefineMenuItem.Caption := lisCodeToolsDefsUndefine;
  ConvertActionToUndefineRecurseMenuItem.Caption := lisCodeToolsDefsUndefineRecurse;
  ConvertActionToUndefineAllMenuItem.Caption := lisCodeToolsDefsUndefineAll;
  ConvertActionToBlockMenuItem.Caption := lisCodeToolsDefsBlock;
  ConvertActionToDirectoryMenuItem.Caption := lisCodeToolsDefsInsertBehindDirectory;
  ConvertActionToIfMenuItem.Caption := lisCodeToolsDefsIf;
  ConvertActionToIfDefMenuItem.Caption := lisCodeToolsDefsIfDef;
  ConvertActionToIfNotDefMenuItem.Caption := lisCodeToolsDefsIfNDef;
  ConvertActionToElseIfMenuItem.Caption := lisCodeToolsDefsElseIf;
  ConvertActionToElseMenuItem.Caption := lisCodeToolsDefsElse;

  // tools
  ToolsMenuItem.Caption := lisCTDefsTools;
  OpenPreviewMenuItem.Caption := lisCTDefsOpenPreview;

  // templates
  InsertTemplateMenuItem.Caption := lisCodeToolsDefsInsertTemplate;

  // FPC templates
  InsertFPCProjectDefinesTemplateMenuItem.Caption := lisCodeToolsDefsInsertFreePascalProjectTe;
  InsertFPCompilerDefinesTemplateMenuItem.Caption := lisCodeToolsDefsInsertFreePascalCompilerT;
  InsertFPCSourceDirTemplateMenuItem.Caption := lisCodeToolsDefsInsertFreePascalSVNSource;

  // Delphi 5 templates
  InsertDelphi5CompilerDefinesTemplateMenuItem.Caption := lisCodeToolsDefsInsertDelphi5CompilerTemp;
  InsertDelphi5DirectoryTemplateMenuItem.Caption := lisCodeToolsDefsInsertDelphi5DirectoryTem;
  InsertDelphi5ProjectTemplateMenuItem.Caption := lisCodeToolsDefsInsertDelphi5ProjectTempl;

  // Delphi 6 templates
  InsertDelphi6CompilerDefinesTemplateMenuItem.Caption := lisCodeToolsDefsInsertDelphi6CompilerTemp;
  InsertDelphi6DirectoryTemplateMenuItem.Caption := lisCodeToolsDefsInsertDelphi6DirectoryTem;
  InsertDelphi6ProjectTemplateMenuItem.Caption := lisCodeToolsDefsInsertDelphi6ProjectTempl;

  // Delphi 7 templates
  InsertDelphi7CompilerDefinesTemplateMenuItem.Caption := lisCodeToolsDefsInsertDelphi7CompilerTemp;
  InsertDelphi7DirectoryTemplateMenuItem.Caption := lisCodeToolsDefsInsertDelphi7DirectoryTem;
  InsertDelphi7ProjectTemplateMenuItem.Caption := lisCodeToolsDefsInsertDelphi7ProjectTempl;

  // Kylix 3 templates
  InsertKylix3CompilerDefinesTemplateMenuItem.Caption := lisCodeToolsDefsInsertKylix3CompilerTemp;
  InsertKylix3DirectoryTemplateMenuItem.Caption := lisCodeToolsDefsInsertKylix3DirectoryTem;
  InsertKylix3ProjectTemplateMenuItem.Caption := lisCodeToolsDefsInsertKylix3ProjectTempl;

  // selected item
  SelectedItemGroupBox.Caption:=lisCodeToolsDefsSelectedNode;

  NameLabel.Caption:=lisCodeToolsDefsName;
  DescriptionLabel.Caption:=lisCodeToolsDefsDescription;
  VariableLabel.Caption:=lisCodeToolsDefsVariable;

  ValueNotebook.Page[0].Caption := lisCodeToolsDefsValueAsText;
  ValueNotebook.Page[1].Caption := lisCodeToolsDefsValueAsFilePaths;
  ValueNotebook.PageIndex := 0;

  MoveFilePathUpBitBtn.Caption:=lisUp;
  MoveFilePathDownBitBtn.Caption:=lisDown;
  DeleteFilePathBitBtn.Caption:=lisDelete;
  InsertFilePathBitBtn.Caption:=lisInsert;

  DefineTreeView.Images := IDEImages.Images_24;
  DefineTreeView.StateImages := IDEImages.Images_16;

  DefineActionImages[Low(TDefineAction)] := -1;
  for DefAction := Succ(Low(TDefineAction)) to High(TDefineAction) do
    DefineActionImages[DefAction] := IDEImages.LoadImage('da_' + LowerCase(DefineActionNames[DefAction]), 24);

  AutogeneratedImage := IDEImages.LoadImage('laz_wand');
end;

function TCodeToolsDefinesEditor.CreateSeperator : TMenuItem;
begin
  Result := TMenuItem.Create(Self);
  Result.Caption := '-';
end;

procedure TCodeToolsDefinesEditor.SetTransferMacros(
  const AValue: TTransferMacroList);
begin
  FTransferMacros:=AValue;
end;

procedure TCodeToolsDefinesEditor.ValueAsPathToValueAsText;
var s: string;
  i, j, l: integer;
begin
  s:=ValueAsFilePathsSynEdit.Text;
  l:=length(s);
  // replace line ends with semicolon
  i:=1;
  j:=1;
  while i<=l do begin
    if s[i] in [#10,#13] then begin
      inc(i);
      if (i<l) and (s[i] in [#10,#13]) and (s[i-1]<>s[i]) then begin
        inc(i);
      end;
      s[j]:=';';
      inc(j);
    end else begin
      s[j]:=s[i];
      inc(i);
      inc(j);
    end;
  end;
  dec(j);
  while (j>=1) and (s[j]=';') do dec(j);
  SetLength(s,j);
  ValueAsTextSynEdit.Text:=s;
end;

procedure TCodeToolsDefinesEditor.SaveSelectedValues;
var
  ATreeNode: TTreeNode;
  ADefNode: TDefineTemplate;
  s: string;
  l: integer;
begin
  ATreeNode:=FLastSelectedNode;
  if (ATreeNode<>nil) then begin
    ADefNode:=TDefineTemplate(ATreeNode.Data);
    if (not ADefNode.IsAutoGenerated) then begin
      ADefNode.Name:=NameEdit.Text;
      ATreeNode.Text:=ADefNode.Name;
      ADefNode.Variable:=VariableEdit.Text;
      ADefNode.Description:=DescriptionEdit.Text;
      if ValueNoteBook.PageIndex=1 then
        ValueAsPathToValueAsText;
      s:=ValueAsTextSynEdit.Text;
      l:=length(s);
      if (l>0) and (s[l] in [#13,#10]) then begin
        // remove line end at end of Text, that was added automatically
        dec(l);
        if (l>0) and (s[l] in [#13,#10]) and (s[l]<>s[l+1]) then
          dec(l);
        SetLength(s,l);
      end;
      ADefNode.Value:=s;
    end;
    FLastSelectedNode:=nil;
  end;
end;

procedure TCodeToolsDefinesEditor.ShowSelectedValues;
var
  SelTreeNode: TTreeNode;
  SelDefNode: TDefineTemplate;
begin
  SelTreeNode:=DefineTreeView.Selected;
  if SelTreeNode<>FLastSelectedNode then begin
    SaveSelectedValues;
  end;
  if SelTreeNode<>nil then begin
    SelDefNode:=TDefineTemplate(SelTreeNode.Data);
    SetValuesEditable(not SelDefNode.IsAutoGenerated);
    NameEdit.Text:=SelDefNode.Name;
    DescriptionEdit.Text:=SelDefNode.Description;
    VariableEdit.Text:=SelDefNode.Variable;
    ValueAsTextSynEdit.Text:=SelDefNode.Value;
    ValueAsFilePathsSynEdit.Text:=ValueToFilePathText(SelDefNode.Value);
    if SelDefNode.IsAutoGenerated then begin
      //ValueAsTextSynEdit.Options:=ValueAsTextSynEdit.Options+[eoNoCaret];
      ValueAsTextSynEdit.ReadOnly:=true;
    end else begin
      //ValueAsTextSynEdit.Options:=ValueAsTextSynEdit.Options-[eoNoCaret];
      ValueAsTextSynEdit.ReadOnly:=false;
    end;
    ValueAsFilePathsSynEdit.Options:=ValueAsTextSynEdit.Options;
    ValueAsFilePathsSynEdit.ReadOnly:=ValueAsTextSynEdit.ReadOnly;
  end else begin
    SetValuesEditable(false);
    NameEdit.Text:='';
    DescriptionEdit.Text:='';
    VariableEdit.Text:='';
    ValueAsTextSynEdit.Text:='';
    ValueAsFilePathsSynEdit.Text:='';
  end;
  SetTypeLabel;
  FLastSelectedNode:=SelTreeNode;
end;

procedure TCodeToolsDefinesEditor.SetTypeLabel;
var
  SelTreeNode: TTreeNode;
  SelDefNode: TDefineTemplate;
  s: string;
begin
  SelTreeNode:=DefineTreeView.Selected;
  if SelTreeNode<>nil then begin
    SelDefNode:=TDefineTemplate(SelTreeNode.Data);
    s:=Format(lisCodeToolsDefsAction, [DefineActionNames[SelDefNode.Action]]);
    if SelDefNode.IsAutoGenerated then
      s:=Format(lisCodeToolsDefsautoGenerated, [s]);
  end else begin
    s:=lisCodeToolsDefsnoneSelected;
  end;
  TypeLabel.Caption:=s;
end;

function TCodeToolsDefinesEditor.ValueToFilePathText(const AValue: string
  ): string;
var i: integer;
begin
  Result:=AValue;
  for i:=1 to length(Result) do
    if Result[i]=';' then Result[i]:=#13;
end;

procedure TCodeToolsDefinesEditor.InsertNewNode(Behind: boolean;
  DefAction: TDefineAction);
var SelTreeNode, NodeInFront, ParentNode,
  NewTreeNode: TTreeNode;
  NewDefNode: TDefineTemplate;
  NewName, NewDescription, NewVariable, NewValue: string;
begin
  SelTreeNode:=DefineTreeView.Selected;
  SaveSelectedValues;
  NodeInFront:=nil;
  ParentNode:=nil;
  if SelTreeNode<>nil then begin
    // there is an selected node
    if Behind then begin
      // insert behind selected node
      NodeInFront:=SelTreeNode;
      ParentNode:=NodeInFront.Parent;
    end else begin
      // insert as last child of selected node
      ParentNode:=SelTreeNode;
      NodeInFront:=ParentNode.GetFirstChild;
      if NodeInFront<>nil then begin
        while NodeInFront.GetNextSibling<>nil do
          NodeInFront:=NodeInFront.GetNextSibling;
      end;
    end;
  end else begin
    // no node selected, add as last root node
    NodeInFront:=DefineTreeView.Items.GetLastNode;
  end;
  if (ParentNode<>nil) and (TDefineTemplate(ParentNode.Data).IsAutoGenerated)
  then begin
    IDEMessageDialog(lisCodeToolsDefsInvalidParent,
      Format(lisCodeToolsDefsAutoCreatedNodesReadOnly, [LineEnding]),
      mtInformation, [mbCancel]);
    exit;
  end;
  if (ParentNode<>nil)
  and (not (TDefineTemplate(ParentNode.Data).Action in DefineActionBlocks)) then
  begin
    IDEMessageDialog(lisCodeToolsDefsInvalidParentNode,
      lisCodeToolsDefsParentNodeCanNotContainCh,
      mtInformation,[mbCancel]);
    exit;
  end;
  NewName:=FindUniqueName;
  NewDescription:=NewName;
  NewVariable:='';
  NewValue:='';
  NewDefNode:=TDefineTemplate.Create(NewName,NewDescription,NewVariable,
                                     NewValue,DefAction);
  NewDefNode.Owner:=CodeToolsOpts;
  // add node to treeview
  if (NodeInFront<>nil) then
    // insert in front
    NewTreeNode:=DefineTreeView.Items.InsertObjectBehind(
                  NodeInFront,NewName,NewDefNode)
  else
    // add as last child
    NewTreeNode:=DefineTreeView.Items.AddChildObject(ParentNode,NewName,
                                                     NewDefNode);

  // add node to define tree
  if NodeInFront<>nil then
    NewDefNode.InsertBehind(TDefineTemplate(NodeInFront.Data))
  else if ParentNode<>nil then
    TDefineTemplate(ParentNode.Data).AddChild(NewDefNode)
  else
    FDefineTree.Add(NewDefNode);

  SetNodeImages(NewTreeNode,true);
  DefineTreeView.Selected:=NewTreeNode;
  ShowSelectedValues;
end;

procedure TCodeToolsDefinesEditor.InsertTemplate(NewTemplate: TDefineTemplate);

  procedure AddChilds(ATreeNode: TTreeNode);
  var ADefNode, ChildDefNode: TDefineTemplate;
    ChildTreeNode: TTreeNode;
  begin
    if ATreeNode=nil then exit;
    ADefNode:=TDefineTemplate(ATreeNode.Data);
    ChildDefNode:=ADefNode.FirstChild;
    while ChildDefNode<>nil do begin
      ChildTreeNode:=DefineTreeView.Items.AddChildObject(ATreeNode,
                                                ChildDefNode.Name,ChildDefNode);
      AddChilds(ChildTreeNode);
      ChildDefNode:=ChildDefNode.Next;
    end;
  end;

var
  SelTreeNode, NewTreeNode: TTreeNode;
  SelDefNode: TDefineTemplate;
begin
  SaveSelectedValues;
  if NewTemplate=nil then exit;
  NewTemplate.RemoveFlags([dtfAutoGenerated]);
  FLastSelectedNode:=nil;
  SelTreeNode:=DefineTreeView.Selected;
  if SelTreeNode<>nil then begin
    // insert behind selected node
    SelDefNode:=TDefineTemplate(SelTreeNode.Data);
    // insert in TreeView
    NewTreeNode:=DefineTreeView.Items.InsertObjectBehind(SelTreeNode,
                           NewTemplate.Name,NewTemplate);
    // insert in DefineTree
    NewTemplate.InsertBehind(SelDefNode);
  end else begin
    // add as last root node
    // add in TreeView
    NewTreeNode:=DefineTreeView.Items.AddObject(nil,NewTemplate.Name,NewTemplate);
    // add in DefineTree
    DefineTree.Add(NewTemplate);
  end;
  // add children to TreeView
  AddChilds(NewTreeNode);
  // show and select
  SetNodeImages(NewTreeNode,true);
  NewTreeNode.Selected:=true;
  ShowSelectedValues;
end;

function TCodeToolsDefinesEditor.FindUniqueName: string;
var i: integer;
begin
  i:=1;
  while (DefineTree.FindDefineTemplateByName(lisCodeToolsDefsNewNode+IntToStr(i
    ), false)<>nil)
  do inc(i);
  Result:=lisCodeToolsDefsNewNode+IntToStr(i);
end;

function TCodeToolsDefinesEditor.ConsistencyCheck: integer;

  function CheckNode(ATreeNode: TTreeNode): integer;
  var ADefNode, DummyDefNode: TDefineTemplate;
  begin
    if ATreeNode=nil then exit(0);
    ADefNode:=TDefineTemplate(ATreeNode.Data);
    //debugln(' CheckNode "',ATreeNode.Text,'" "',ADefNode.Name,'"');
    if ADefNode=nil then begin
      Result:=-1;  exit;
    end;
    if (ATreeNode.GetPrevSibling<>nil)
    and (TDefineTemplate(ATreeNode.GetPrevSibling.Data)<>ADefNode.Prior) then
    begin
      Result:=-2;  exit;
    end;
    if (ATreeNode.GetNextSibling<>nil)
    and (TDefineTemplate(ATreeNode.GetNextSibling.Data)<>ADefNode.Next) then
    begin
      DbgOut(' ERROR: ',ATreeNode.GetNextSibling.Text,' ');
      if ADefNode.Next<>nil then DbgOut('ADefNode.Next=',ADefNode.Next.Name,' ')
      else DbgOut('ADefNode.Next=nil ');
      DummyDefNode:=TDefineTemplate(ATreeNode.GetNextSibling.Data);
      if DummyDefNode<>nil then
        DebugLn('ATreeNode.GetNextSibling.Next=',DummyDefNode.Name)
      else
        DebugLn('ATreeNode.GetNextSibling.Next=nil');
      Result:=-3;  exit;
    end;
    if (ATreeNode.GetFirstChild<>nil)
    and (TDefineTemplate(ATreeNode.GetFirstChild.Data)<>ADefNode.FirstChild)
    then begin
      Result:=-4;  exit;
    end;
    Result:=CheckNode(ATreeNode.GetFirstChild);
    if Result<0 then exit;
    Result:=CheckNode(ATreeNode.GetNextSibling);
    if Result<0 then exit;
  end;

begin
  DefineTreeView.ConsistencyCheck;
  DefineTree.ConsistencyCheck;
  Result:=CheckNode(DefineTreeView.Items.GetFirstNode);
  if Result<0 then begin
    dec(Result,300000);
    exit;
  end;
  Result:=0;
end;

procedure TCodeToolsDefinesEditor.SetValuesEditable(AValue: boolean);
begin
  SelectedItemGroupBox.Enabled:=true;
  TypeLabel.Enabled:=true;
  NameLabel.Enabled:=AValue;
  NameEdit.Enabled:=AValue;
  DescriptionLabel.Enabled:=AValue;
  DescriptionEdit.Enabled:=AValue;
  VariableLabel.Enabled:=AValue;
  VariableEdit.Enabled:=AValue;
  ValueAsTextSynEdit.ReadOnly:=not AValue;
  ValueAsFilePathsSynEdit.ReadOnly:=not AValue;
  MoveFilePathUpBitBtn.Enabled:=AValue;
  MoveFilePathDownBitBtn.Enabled:=AValue;
  DeleteFilePathBitBtn.Enabled:=AValue;
  InsertFilePathBitBtn.Enabled:=AValue;
end;

procedure TCodeToolsDefinesEditor.SetOptions(ACodeToolBoss: TCodeToolManager);
begin
  FLastSelectedNode:=nil;
  FBoss:=ACodeToolBoss;
  FDefineTree.Assign(ACodeToolBoss.DefineTree);
  RebuildDefineTreeView(DefineTreeView,DefineTree.RootTemplate);
  ShowSelectedValues;
end;

constructor TCodeToolsDefinesEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  IDEDialogLayoutList.ApplyLayout(Self,500,460);

  Caption:=lisCodeToolsDefsCodeToolsDefinesEditor;

  CreateComponents;
  MainSplitter.SetSplitterPosition(
         Max(20,Min(ClientHeight-100,CodeToolsOpts.DefinesEditMainSplitterTop)));

  FDefineTree:=TDefineTree.Create;

  EditorOpts.GetSynEditSettings(ValueAsTextSynEdit);
  ValueAsTextSynEdit.Gutter.Visible:=false;
  EditorOpts.GetSynEditSettings(ValueAsFilePathsSynEdit);
  ValueAsFilePathsSynEdit.Gutter.Visible:=false;
end;

destructor TCodeToolsDefinesEditor.Destroy;
begin
  FDefineTree.Free;
  inherited Destroy;
end;

end.

