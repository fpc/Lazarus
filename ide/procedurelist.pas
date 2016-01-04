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

  Procedure List - Lazarus addon

  Author: Graeme Geldenhuys  (graemeg@gmail.com)
  Inspired by: GExperts  (www.gexperts.org)
  Last Modified:  2006-06-05

  Abstract:
  The procedure list enables you to view a list of Free Pascal / Lazarus
  procedures in the current unit and quickly jump to the implementation of a
  given procedure. Include files are also supported.
  
}

unit ProcedureList;

{$mode objfpc}{$H+}

interface

uses
  // FCL, LCL
  Classes, SysUtils,
  Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, StdCtrls, LCLType, Clipbrd,
  // Codetools
  CodeTree, CodeToolManager, CodeCache, PascalParserTool, KeywordFuncLists, FileProcs,
  // IdeIntf
  IDEImagesIntf, SrcEditorIntf, IDEWindowIntf, LazIDEIntf, IDECommands,
  ListViewFilterEdit,
  // IDE
  IDEOptionDefs, LazarusIDEStrConsts;

type
  { TProcedureListForm }
  TProcedureListForm = class(TForm)
    cbObjects: TComboBox;
    FilterMethods: TListViewFilterEdit;
    lblObjects: TLabel;
    LV: TListView;
    pnlHeader: TPanel;
    StatusBar: TStatusBar;
    TB: TToolBar;
    tbAbout: TToolButton;
    tbCopy: TToolButton;
    ToolButton2: TToolButton;
    tbJumpTo: TToolButton;
    ToolButton4: TToolButton;
    tbChangeFont: TToolButton;
    ToolButton9: TToolButton;
    procedure cbObjectsChange(Sender: TObject);
    procedure edMethodsKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LVDblClick(Sender: TObject);
    procedure LVSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure tbAboutClick(Sender: TObject);
    procedure tbChangeFontClick(Sender: TObject);
    procedure tbCopyClick(Sender: TObject);
  private
    FCaret: TCodeXYPosition;
    FMainFilename: string;
    FNewTopLine: integer;
    { Initialise GUI }
    procedure SetupGUI;
    { Move editors focus to selected method. }
    procedure JumpToSelection;
    { Populates Listview based on selected Class and user entered filter. }
    procedure AddToListView(pCodeTool: TCodeTool; pNode: TCodeTreeNode);
    procedure PopulateListview;
    { Populates only tho cbObjects combo with available classes. }
    procedure PopulateObjectsCombo;
  public
    property MainFilename: string read FMainFilename;
    property Caret: TCodeXYPosition read FCaret;
    property NewTopLine: integer read FNewTopLine;
  end; 


var
  ProcListView: TProcedureListForm = nil;

implementation

{$R *.lfm}

const
  cAbout =
    'Procedure List (Lazarus addon)' + #10#10 +
    'Author: Graeme Geldenhuys  (graemeg@gmail.com)' + #10 +
    'Inspired by: GExperts  (www.gexperts.org)';

// ToDo: set a callback notification for source editor page change.

{ TProcedureListForm }

procedure TProcedureListForm.FormCreate(Sender: TObject);
begin
  Name:=NonModalIDEWindowNames[nmiwProcedureList];
  SetupGUI;
  // Very weird: populating Combobox here shows only unique entries, no duplicates.
  // Calling the same method in FormShow shows duplicates. Makes no sense ...
  //PopulateObjectsCombo;
end;

procedure TProcedureListForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(Parent) then
  begin
    // Using a dock manager...
    CloseAction := caNone;
    // Copied from TComponentListForm.FormClose
    if Assigned(HostDockSite) and (HostDockSite.DockClientCount <= 1)
      and (HostDockSite is TCustomForm) and (HostDockSite.Parent = nil) then
    begin
      TCustomForm(HostDockSite).Close;
    end;
  end;
end;

procedure TProcedureListForm.FormShow(Sender: TObject);
begin
  if Assigned(SourceEditorManagerIntf.ActiveEditor) then
    FMainFilename := SourceEditorManagerIntf.ActiveEditor.Filename
  else
    FMainFilename := '';
  Caption := lisPListProcedureList + ' - ' + ExtractFileName(FMainFilename);
  PopulateObjectsCombo;
  PopulateListView;
  StatusBar.Panels[0].Text := self.MainFilename;
  FilterMethods.SetFocus;  // ActiveControl gets lost sometimes.
end;

procedure TProcedureListForm.FormResize(Sender: TObject);
begin
  StatusBar.Panels[0].Width := self.ClientWidth - 105;
end;

procedure TProcedureListForm.LVDblClick(Sender: TObject);
begin
  JumpToSelection;
end;

procedure TProcedureListForm.LVSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Item = nil then
    Exit; //==>
  if Item.SubItems.Count < 4 then
    Exit; //==>
  if Selected then
    StatusBar.Panels[0].Text := Item.SubItems[4];
end;

procedure TProcedureListForm.tbAboutClick(Sender: TObject);
begin
  ShowMessage(cAbout);
end;

procedure TProcedureListForm.tbChangeFontClick(Sender: TObject);
begin

end;

procedure TProcedureListForm.tbCopyClick(Sender: TObject);
begin
  if Assigned(LV.Selected) then
    Clipboard.AsText := LV.Selected.SubItems[0];
end;

procedure TProcedureListForm.SetupGUI;
begin
  self.KeyPreview     := True;
  self.Position       := poScreenCenter;

  // assign resource strings to Captions and Hints
  lblObjects.Caption    := lisPListObjects;
  tbAbout.Hint          := lisMenuTemplateAbout;
  tbJumpTo.Hint         := lisPListJumpToSelection;
  tbChangeFont.Hint     := lisPListChangeFont;
  tbCopy.Hint           := lisPListCopyMethodToClipboard;
  LV.Column[1].Caption  := lisProcedure;
  LV.Column[2].Caption  := lisPListType;
  LV.Column[3].Caption  := dlgAddHiAttrGroupLine;
  
  // assign resource images to toolbuttons
  TB.Images := IDEImages.Images_16;
  tbCopy.ImageIndex        := IDEImages.LoadImage(16, 'laz_copy');
  tbChangeFont.ImageIndex  := IDEImages.LoadImage(16, 'item_font');
  tbAbout.ImageIndex       := IDEImages.LoadImage(16, 'menu_information');
  tbJumpTo.ImageIndex      := IDEImages.LoadImage(16, 'menu_goto_line');

  LV.Column[0].Width  := 20;
  LV.Column[1].Width  := 300;
  LV.Column[2].Width  := 100;
  LV.Column[3].Width  := 60;
  
  LV.ReadOnly         := True;
  LV.RowSelect        := True;
  LV.SortColumn       := 1;
  LV.SortType         := stText;
  LV.HideSelection    := False;
  
  cbObjects.Style     := csDropDownList;
  cbObjects.Sorted    := True;
  cbObjects.DropDownCount := 8;
end;

procedure TProcedureListForm.JumpToSelection;
var
  lItem: TListItem;
  CodeBuffer: TCodeBuffer;
  ACodeTool: TCodeTool;
  lStartPos: integer;
begin
  lItem := LV.Selected;
  if lItem = nil then
    Exit; //==>
  if lItem.SubItems[3] = '' then
    Exit; //==>
    
  lStartPos := StrToInt(lItem.SubItems[3]);

  CodeBuffer := CodeToolBoss.LoadFile(MainFilename,false,false);
  if CodeBuffer = nil then
    Exit; //==>

  ACodeTool := nil;
  CodeToolBoss.Explore(CodeBuffer,ACodeTool,false);
  if ACodeTool = nil then
    Exit; //==>

  if not ACodeTool.CleanPosToCaretAndTopLine(lStartPos, FCaret, FNewTopLine) then
    Exit; //==>

  LazarusIDE.DoOpenFileAndJumpToPos(Caret.Code.Filename, Point(Caret.X, Caret.Y),
                                    NewTopLine, -1,-1, [ofRegularFile,ofUseCache]);
  Close;
end;

procedure TProcedureListForm.AddToListView(pCodeTool: TCodeTool; pNode: TCodeTreeNode);
var
  Data: TStringArray;
  lNodeText: string;
  lCaret: TCodeXYPosition;
begin
  SetLength(Data, 6);    // Data[0] remains empty

  { procedure name }
  Data[1] := pCodeTool.ExtractProcHead(pNode,
      [phpWithoutParamList, phpWithoutBrackets, phpWithoutSemicolon]);

  { type }
  lNodeText := pCodeTool.ExtractProcHead(pNode,
      [phpWithStart, phpWithoutParamList, phpWithoutBrackets, phpWithoutSemicolon]);
  if Pos('procedure', lNodeText) > 0 then
    Data[2] := 'Procedure'
  else
    Data[2] := 'Function';

  { line number }
  if pCodeTool.CleanPosToCaret(pNode.StartPos, lCaret) then
    Data[3] := IntToStr(lCaret.Y);

  { start pos - used by JumpToSelected() }
  Data[4] := IntToStr(pNode.StartPos);

  { full procedure name used in statusbar }
  Data[5] := pCodeTool.ExtractProcHead(pNode,
                  [phpWithStart,phpWithVarModifiers,
                   phpWithParameterNames,phpWithDefaultValues,phpWithResultType,
                   phpWithOfObject,phpWithCallingSpecs,phpWithProcModifiers]);

  FilterMethods.Items.Add(Data);
end;

procedure TProcedureListForm.PopulateListview;
var
  lSrcEditor: TSourceEditorInterface;
  lCodeBuffer: TCodeBuffer;
  lCodeTool: TCodeTool;
  lNode: TCodeTreeNode;
begin
  try
    FilterMethods.Items.Clear;
    { get active source editor }
    lSrcEditor := SourceEditorManagerIntf.ActiveEditor;
    if lSrcEditor = nil then
      Exit; //==>
    lCodeBuffer := lSrcEditor.CodeToolsBuffer as TCodeBuffer;

    { parse source }
    CodeToolBoss.Explore(lCodeBuffer,lCodeTool,False);

    { copy the tree }
    if (lCodeTool = nil) or (lCodeTool.Tree = nil) or (lCodeTool.Tree.Root = nil) then
      Exit; //==>

    if Assigned(lCodeTool.Tree) then
    begin
      { Find the starting point }
      lNode := lCodeTool.FindImplementationNode;
      if lNode = nil then
        { fall back - guess we are working with a program unit }
        lNode := lCodeTool.Tree.Root;

      { populate the listview here }
      lNode := lNode.FirstChild;
      while lNode <> nil do
      begin
        if lNode.Desc = ctnProcedure then
          AddToListView(lCodeTool, lNode);
        lNode := lNode.NextBrother;
      end;
    end;  { if }
  finally
    FilterMethods.InvalidateFilter;
    if LV.Items.Count > 0 then
    begin
      LV.Selected := LV.Items[0];
      LV.ItemFocused := LV.Items[0];
    end;
  end;
end;

procedure TProcedureListForm.PopulateObjectsCombo;
var
  lSrcEditor: TSourceEditorInterface;
  lCodeBuffer: TCodeBuffer;
  lCodeTool: TCodeTool;
  lNode: TCodeTreeNode;
  lNodeText: string;
begin
  cbObjects.Items.Clear;
  try
    { get active source editor }
    lSrcEditor := SourceEditorManagerIntf.ActiveEditor;
    if lSrcEditor = nil then
      Exit; //==>
    lCodeBuffer := lSrcEditor.CodeToolsBuffer as TCodeBuffer;

    { parse source }
    CodeToolBoss.Explore(lCodeBuffer,lCodeTool,False);

    if (lCodeTool = nil)
        or (lCodeTool.Tree = nil)
        or (lCodeTool.Tree.Root = nil) then
      Exit; //==>

    { copy the tree }
    { Find the starting point }
    lNode := lCodeTool.FindImplementationNode;
    if lNode = nil then
    begin
      { fall back - guess we are working with a program unit }
      lNode := lCodeTool.Tree.Root;
    end;
    { populate the Combobox here! }
    lNode := lNode.FirstChild;
    while lNode <> nil do
    begin
      if lNode.Desc = ctnProcedure then
      begin
        lNodeText := lCodeTool.ExtractClassNameOfProcNode(lNode);
        if lNodeText <> '' then
        begin
          DebugLn(['TProcedureListForm.PopulateObjectsCombo: Adding "', lNodeText, '" to combobox items.']);
          cbObjects.Items.Add(lNodeText);
        end;
      end;
      lNode := lNode.NextBrother;
    end;
    cbObjects.Sorted := true;
    cbObjects.Sorted := false;
    cbObjects.Items.Insert(0, lisPListAll);
    cbObjects.Items.Insert(1, lisPListNone);
  finally
    cbObjects.ItemIndex := 0;   // select <All> as the default
    if (cbObjects.Items.Count > 0) and (cbObjects.Text = '') then  // some widgetsets have issues here so we do this
      cbObjects.Text := cbObjects.Items[0];
  end;
end;

procedure TProcedureListForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then   // Escape key
  begin
    Close;
  end;
end;

procedure TProcedureListForm.cbObjectsChange(Sender: TObject);
begin
  // ToDo: populate based on the selected item
  PopulateListview;
end;

procedure TProcedureListForm.edMethodsKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
    #13:
      begin
        Key := #0;
        JumpToSelection;
      end;
    #27:
      begin
        Key := #0;
        Close;
      end;
  end;
end;

end.

