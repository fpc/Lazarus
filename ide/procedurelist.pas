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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls,
  CodeTree, CodeToolManager, CodeAtom, CodeCache,
  IDEImagesIntf;

type
  { TProcedureListForm }
  TProcedureListForm = class(TForm)
    cbObjects: TComboBox;
    edMethods: TEdit;
    lblObjects: TLabel;
    lblSearch: TLabel;
    LV: TListView;
    pnlHeader: TPanel;
    StatusBar: TStatusBar;
    TB: TToolBar;
    tbAbout: TToolButton;
    tbCopy: TToolButton;
    ToolButton2: TToolButton;
    tbJumpTo: TToolButton;
    ToolButton4: TToolButton;
    tbFilterAny: TToolButton;
    tbFilterStart: TToolButton;
    ToolButton7: TToolButton;
    tbChangeFont: TToolButton;
    ToolButton9: TToolButton;
    procedure cbObjectsChange(Sender: TObject);
    procedure edMethodsChange(Sender: TObject);
    procedure edMethodsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edMethodsKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LVDblClick(Sender: TObject);
    procedure LVSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure tbAboutClick(Sender: TObject);
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
    procedure PopulateListview;
    { Populates only tho cbObjects combo with available classes. }
    procedure PopulateObjectsCombo;
    procedure AddToListView(pCodeTool: TCodeTool; pNode: TCodeTreeNode);
    function  PassFilter(pSearchAll: boolean; pProcName, pSearchStr: string; pCodeTool: TCodeTool; pNode: TCodeTreeNode): boolean;
  public
    property MainFilename: string read FMainFilename;
    property Caret: TCodeXYPosition read FCaret;
    property NewTopLine: integer read FNewTopLine;
  end; 



procedure ExecuteProcedureList(Sender: TObject);

implementation

{$R *.lfm}

uses
  MenuIntf
  ,SrcEditorIntf
  ,PascalParserTool
  ,KeywordFuncLists
  ,LCLType
  ,LazIDEIntf
  ,IDECommands
  ,Clipbrd
  ,LazarusIDEStrConsts
  ;


const
  cAbout =
    'Procedure List (Lazarus addon)' + #10#10 +
    'Author: Graeme Geldenhuys  (graemeg@gmail.com)' + #10 +
    'Inspired by: GExperts  (www.gexperts.org)';


{ This is where it all starts. Gets called from Lazarus. }
procedure ExecuteProcedureList(Sender: TObject);
var
  frm: TProcedureListForm;
begin
  Assert(Sender<>nil);  // removes compiler warning
  
  frm := TProcedureListForm.Create(nil);
  try
    frm.ShowModal;
    if frm.ModalResult = mrOK then  // we need to jump
    begin
      LazarusIDE.DoOpenFileAndJumpToPos(frm.Caret.Code.Filename,
          Point(frm.Caret.X, frm.Caret.Y), frm.NewTopLine, -1,-1,
          [ofRegularFile,ofUseCache]);
    end;
  finally
    frm.Free;
  end;
end;


{ Check, if the given string starts with this substring. Check ignores case. }
function StrStartsWith(sStr, sSubstr: String): Boolean;
begin
  sStr := AnsiUpperCase(sStr);
  sSubstr := AnsiUpperCase(sSubstr);

  Result := Pos(sSubstr, sStr) = 1;
end;


function StrContains(const SubStr, Str: string; CaseSensitive: Boolean): Boolean;
begin
  if CaseSensitive then
    Result := Pos(SubStr, Str) > 0
  else
    Result := Pos(AnsiUpperCase(SubStr), AnsiUpperCase(Str)) > 0;
end;


function FilterFits(const SubStr, Str: string): boolean;
var
  Src: PChar;
  PFilter: PChar;
  c: Char;
  i: Integer;
begin
  if SubStr='' then
  begin
    Result := true;
  end
  else
  begin
    Src := PChar(Str);
    PFilter := PChar(SubStr);
    repeat
      c := Src^;
      if c <> #0 then
      begin
        if UpChars[Src^] = UpChars[PFilter^] then
        begin
          i := 1;
          while (UpChars[Src[i]] = UpChars[PFilter[i]]) and (PFilter[i] <> #0) do
            inc(i);
          if PFilter[i] = #0 then
          begin
            exit(true);
          end;
        end;
      end
      else
        exit(false);
      inc(Src);
    until false;
  end;
end;


{ TProcedureListForm }

procedure TProcedureListForm.FormResize(Sender: TObject);
begin
  StatusBar.Panels[0].Width := self.ClientWidth - 105;
end;


procedure TProcedureListForm.FormShow(Sender: TObject);
begin
  edMethods.SetFocus;
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
  self.Caption          := lisPListProcedureList;
  lblObjects.Caption    := lisPListObjects;
  lblSearch.Caption     := lisMenuSearch;
  tbAbout.Hint          := lisMenuTemplateAbout;
  tbJumpTo.Hint         := lisPListJumpToSelection;
  tbFilterAny.Hint      := lisPListFilterAny;
  tbFilterStart.Hint    := lisPListFilterStart;
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
  tbFilterAny.ImageIndex   := IDEImages.LoadImage(16, 'item_filter');
  tbFilterStart.ImageIndex := IDEImages.LoadImage(16, 'item_filter');

  LV.Column[0].Width  := 20;
  LV.Column[1].Width  := 300;
  LV.Column[2].Width  := 100;
  LV.Column[3].Width  := 60;
  
  LV.ReadOnly         := True;
  LV.RowSelect        := True;
  LV.SortColumn       := 1;
  LV.SortType         := stText;
  LV.HideSelection    := False;
  
  LV.Items.Clear;

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

  { This should close the form }
  self.ModalResult := mrOK;
end;


procedure TProcedureListForm.PopulateListview;
var
  lSrcEditor: TSourceEditorInterface;
  lCodeBuffer: TCodeBuffer;
  lCodeTool: TCodeTool;
  lNode: TCodeTreeNode;
begin
  LV.BeginUpdate;
  try
    LV.Items.Clear;
    { get active source editor }
    lSrcEditor := SourceEditorManagerIntf.ActiveEditor;
    if lSrcEditor = nil then
      Exit; //==>
    lCodeBuffer := lSrcEditor.CodeToolsBuffer as TCodeBuffer;

    { parse source }
    CodeToolBoss.Explore(lCodeBuffer,lCodeTool,False);

    { copy the tree }
    if (lCodeTool = nil)
        or (lCodeTool.Tree = nil)
        or (lCodeTool.Tree.Root = nil) then
      Exit; //==>

    if Assigned(lCodeTool.Tree) then
    begin
      { Find the starting point }
      lNode := lCodeTool.FindImplementationNode;
      if lNode = nil then
      begin
        { fall back - guess we are working with a program unit }
        lNode := lCodeTool.Tree.Root;
      end;

      { populate the listview here }
      lNode := lNode.FirstChild;
      while lNode <> nil do
      begin
        if lNode.Desc = ctnProcedure then
        begin
          AddToListView(lCodeTool, lNode);
        end;
        lNode := lNode.NextBrother;
      end;
    end;  { if }
  finally
    if LV.Items.Count > 0 then
    begin
      LV.Selected := LV.Items[0];
      LV.ItemFocused := LV.Items[0];
    end;
    LV.EndUpdate;
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
    if Assigned(lCodeTool.Tree) then
    begin
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
          cbObjects.Items.Add(lNodeText);
        end;
        lNode := lNode.NextBrother;
      end;
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


procedure TProcedureListForm.AddToListView(pCodeTool: TCodeTool; pNode: TCodeTreeNode);
var
  lItem: TListItem;
  lNodeText: string;
  lType: string;
  lCaret: TCodeXYPosition;
  FSearchAll: boolean;
begin
  FSearchAll := cbObjects.Text = lisPListAll;
  lNodeText := pCodeTool.ExtractProcHead(pNode,
      [phpWithoutClassKeyword, phpWithoutParamList, phpWithoutBrackets,
       phpWithoutSemicolon, phpWithoutClassName]);

  { Must we add this pNode or not? }
  if not PassFilter(FSearchAll, lNodeText, edMethods.Text, pCodeTool, pNode) then
    Exit; //==>
    
  { Add new list item }
  lItem := LV.Items.Add;

  { procedure name }
  lNodeText := pCodeTool.ExtractProcHead(pNode,
      [phpWithoutParamList, phpWithoutBrackets, phpWithoutSemicolon]);
  lItem.SubItems.Add(lNodeText);

  { type }
  lNodeText := pCodeTool.ExtractProcHead(pNode,
      [phpWithStart, phpWithoutParamList, phpWithoutBrackets, phpWithoutSemicolon]);
  if Pos('procedure', lNodeText) > 0 then
    lType := 'Procedure'
  else
    lType := 'Function';
  lItem.SubItems.Add(lType);
  
  { line number }
  if pCodeTool.CleanPosToCaret(pNode.StartPos, lCaret) then
    lItem.SubItems.Add(IntToStr(lCaret.Y));
    
  { start pos - used by JumpToSelected() }
  lItem.SubItems.Add(IntToStr(pNode.StartPos));
  
  { full procedure name used in statusbar }
  lNodeText := pCodeTool.ExtractProcHead(pNode,
                  [phpWithStart,phpWithVarModifiers,
                   phpWithParameterNames,phpWithDefaultValues,phpWithResultType,
                   phpWithOfObject,phpWithCallingSpecs,phpWithProcModifiers]);
  lItem.SubItems.Add(lNodeText);
end;


{ Do we pass all the filter tests to continue? }
function TProcedureListForm.PassFilter(pSearchAll: boolean;
  pProcName, pSearchStr: string; pCodeTool: TCodeTool; pNode: TCodeTreeNode
  ): boolean;
var
  lClass: string;
  
  function ClassMatches: boolean;
  begin
    { lets filter by class selection. }
    lClass := pCodeTool.ExtractClassNameOfProcNode(pNode);
    if cbObjects.Text = lisPListNone then
      Result := lClass = ''
    else
      Result := lClass = cbObjects.Text;

  end;
  
begin
  Result := False;
  if (Length(pSearchStr) = 0) then    // seach string is empty
  begin
    if pSearchAll then
      Result := True
    else
      Result := ClassMatches;
  end
  else if not pSearchAll and tbFilterStart.Down
  and SameText(pSearchStr, Copy(pProcName, 1, Length(pSearchStr))) then
    Result := True
  else if not pSearchAll and tbFilterAny.Down and ClassMatches
  and FilterFits(pSearchStr, pProcName) then
    Result := True
  else if pSearchAll and FilterFits(pSearchStr, pProcName) then
    Result := True;
end;


procedure TProcedureListForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then   // Escape key
  begin
    self.ModalResult := mrCancel;
  end;
end;


procedure TProcedureListForm.FormCreate(Sender: TObject);
begin
  if SourceEditorManagerIntf.ActiveEditor = nil then
    Exit; //==>
    
  FMainFilename := SourceEditorManagerIntf.ActiveEditor.Filename;
  Caption := Caption + ExtractFileName(FMainFilename);
  SetupGUI;
  PopulateObjectsCombo;
  PopulateListView;
  StatusBar.Panels[0].Text := self.MainFilename;
end;


procedure TProcedureListForm.edMethodsKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
    #13:
      begin
        JumpToSelection;
        Key := #0;
      end;
    #27:
      begin
        self.ModalResult := mrCancel;
        Key := #0;
      end;
  end;
end;


procedure TProcedureListForm.edMethodsChange(Sender: TObject);
begin
  PopulateListview;
end;


procedure TProcedureListForm.cbObjectsChange(Sender: TObject);
begin
  PopulateListview;
end;


procedure TProcedureListForm.edMethodsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if LV.Items.Count = 0 then
    Exit;

  if Key = VK_Down then
  begin
    if (LV.Items.IndexOf(LV.ItemFocused) + 1) < LV.Items.Count then
      LV.ItemFocused := LV.Items[(LV.Items.IndexOf(LV.ItemFocused) + 1)];
  end
  else if Key = VK_Up then
  begin
    if (LV.Items.IndexOf(LV.ItemFocused) - 1) >= 0 then
      LV.ItemFocused := LV.Items[(LV.Items.IndexOf(LV.ItemFocused) - 1)];
  end
  else if Key = VK_Home then
  begin
    LV.ItemFocused := LV.Items[0];
  end
  else if Key = VK_End then
  begin
    LV.ItemFocused := LV.Items[LV.Items.Count - 1];
  end;

  if LV.ItemFocused<>nil then
  begin
    LV.Selected := LV.ItemFocused;
    if Assigned(LV.Selected) then
      LV.Selected.MakeVisible(True);
  end;
end;

end.
