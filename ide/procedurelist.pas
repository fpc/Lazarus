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
  Classes, SysUtils,
  // LCL
  LCLType, Forms, Controls, Dialogs, ComCtrls, ExtCtrls, StdCtrls, Clipbrd,
  Graphics, Grids,
  // Codetools
  CodeTree, CodeToolManager, CodeCache, PascalParserTool, KeywordFuncLists,
  // IDEIntf
  LazIDEIntf, IDEImagesIntf, SrcEditorIntf, IDEWindowIntf,
  // IDE
  EnvironmentOpts, LazarusIDEStrConsts;

type

  { TGridRowObject }

  TGridRowObject = class
  public
    ImageIdx: Integer;
    NodeStartPos: Integer;
    FullProcedureName: string;
    constructor Create;
  end;

  { TProcedureListForm }
  TProcedureListForm = class(TForm)
    cbObjects: TComboBox;
    edMethods: TEdit;
    lblObjects: TLabel;
    lblSearch: TLabel;
    pnlHeader: TPanel;
    StatusBar: TStatusBar;
    SG: TStringGrid;
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
    procedure edMethodsKeyDown(Sender: TObject; var Key: Word;
      {%H-}Shift: TShiftState);
    procedure edMethodsKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SGDblClick(Sender: TObject);
    procedure SGDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      {%H-}aState: TGridDrawState);
    procedure SGSelectCell(Sender: TObject; {%H-}aCol, aRow: Integer;
      var {%H-}CanSelect: Boolean);
    procedure SomethingChange(Sender: TObject);
    procedure tbAboutClick(Sender: TObject);
    procedure tbCopyClick(Sender: TObject);
  private
    FCaret: TCodeXYPosition;
    FMainFilename: string;
    FNewTopLine: integer;
    FImageIdxProcedure: Integer;
    FImageIdxFunction: Integer;
    { Initialise GUI }
    procedure SetupGUI;
    { Move editors focus to selected method. }
    procedure JumpToSelection;
    { Populates Listview based on selected Class and user entered filter. }
    procedure PopulateGrid;
    { Populates only tho cbObjects combo with available classes. }
    procedure PopulateObjectsCombo;
    procedure AddToGrid(pCodeTool: TCodeTool; pNode: TCodeTreeNode);
    function  PassFilter(pSearchAll: boolean; pProcName, pSearchStr: string; pCodeTool: TCodeTool; pNode: TCodeTreeNode): boolean;
    procedure ClearGrid;
  public
    property MainFilename: string read FMainFilename;
    property Caret: TCodeXYPosition read FCaret;
    property NewTopLine: integer read FNewTopLine;
  end; 


procedure ExecuteProcedureList(Sender: TObject);

implementation

{$R *.lfm}

const
  SG_COLIDX_IMAGE = 0;
  SG_COLIDX_PROCEDURE = 1;
  SG_COLIDX_TYPE = 2;
  SG_COLIDX_LINE = 3;
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


function FilterFits(const SubStr, Str: string): boolean;
var
  Src: PChar;
  PFilter: PChar;
  c: Char;
  i: Integer;
begin
  Result := SubStr='';
  if not Result then
  begin
    Src := PChar(Str);
    PFilter := PChar(SubStr);
    repeat
      c := Src^;

      if PFilter^ = '.' then
      begin
        Inc(PFilter);
        if PFilter^ = #0 then
          exit(true);
        repeat
          inc(Src);
          c := Src^;
          if c = '.' then
          begin
            Inc(Src);
            break;
          end;
        until c = #0;
      end;

      if c <> #0 then
      begin
        if UpChars[Src^] = UpChars[PFilter^] then
        begin
          i := 1;
          while (UpChars[Src[i]] = UpChars[PFilter[i]]) and not ((PFilter[i] = #0) or (PFilter[i] = '.')) do
            inc(i);
          if PFilter[i] = #0 then
          begin
            exit(true);
          end
          else
          if PFilter[i] = '.' then
          begin
            PFilter := PChar(Copy(SubStr, i+2, Length(SubStr)-(i+1)));
            if PFilter^ = #0 then
              exit(true);
            while true do
            begin
              inc(Src);
              c := Src^;
              if (c = #0) or (c = '.') then
                break;
            end;
          end;
        end;
      end
      else
        exit(false);
      inc(Src);
    until false;
  end;
end;

{ TGridRowObject }

constructor TGridRowObject.Create;
begin
  ImageIdx := -1;
  NodeStartPos := -1;
  FullProcedureName := '';
end;


{ TProcedureListForm }

procedure TProcedureListForm.FormCreate(Sender: TObject);
begin
  if SourceEditorManagerIntf.ActiveEditor = nil then
  begin
    //SetupGUI makes the dialog look as it should, and is clears the listview
    //thus preventing a crash when clicking on the LV
    SetupGUI;
    Exit; //==>
  end;

  FMainFilename := SourceEditorManagerIntf.ActiveEditor.Filename;
  Caption := Caption + ExtractFileName(FMainFilename);
  SetupGUI;
  PopulateObjectsCombo;
  PopulateGrid;
  StatusBar.Panels[0].Text := self.MainFilename;
  tbFilterStart.Down := EnvironmentOptions.ProcedureListFilterStart;
  IDEDialogLayoutList.ApplyLayout(Self, 950, 680);
end;

procedure TProcedureListForm.FormDestroy(Sender: TObject);
begin
  EnvironmentOptions.ProcedureListFilterStart := tbFilterStart.Down;
  ClearGrid;
  IDEDialogLayoutList.SaveLayout(self);
end;

procedure TProcedureListForm.FormResize(Sender: TObject);
begin
  StatusBar.Panels[0].Width := self.ClientWidth - 105;
end;

procedure TProcedureListForm.FormShow(Sender: TObject);
begin
  edMethods.SetFocus;
end;

procedure TProcedureListForm.SGDblClick(Sender: TObject);
begin
  JumpToSelection;
end;

procedure TProcedureListForm.SGDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  bmp: TBitmap;
  grid: TStringGrid;
  iconTop, imageIdx: Integer;
  rowObj: TGridRowObject;
begin
  grid := TStringGrid(Sender);

  if (aCol = 0) and (aRow >= grid.FixedRows) then
  begin
    rowObj := TGridRowObject(grid.Rows[aRow].Objects[0]);
    if Assigned(rowObj) then
    begin
      imageIdx := rowObj.ImageIdx;

      bmp := TBitmap.Create;
      try
        IDEImages.Images_16.GetBitmap(imageIdx, bmp);
        iconTop := ((aRect.Bottom - aRect.Top) - bmp.Height) div 2 + aRect.Top;
        grid.Canvas.Draw(aRect.Left,iconTop, bmp);
      finally
        bmp.Free;
      end;
    end;
  end;
end;

procedure TProcedureListForm.SGSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
var
  rowObject: TGridRowObject;
begin
  rowObject := TGridRowObject(TStringGrid(Sender).Rows[aRow].Objects[0]);

  if Assigned(rowObject) then
  begin
    StatusBar.Panels[0].Text := rowObject.FullProcedureName;
  end;
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
  SG.Columns[SG_COLIDX_PROCEDURE].Title.Caption   := lisProcedure;
  SG.Columns[SG_COLIDX_TYPE].Title.Caption   := lisPListType;
  SG.Columns[SG_COLIDX_LINE].Title.Caption   := dlgAddHiAttrGroupLine;
  
  // assign resource images to toolbuttons
  TB.Images := IDEImages.Images_16;
  tbCopy.ImageIndex        := IDEImages.LoadImage('laz_copy');
  tbChangeFont.ImageIndex  := IDEImages.LoadImage('item_font');
  tbAbout.ImageIndex       := IDEImages.LoadImage('menu_information');
  tbJumpTo.ImageIndex      := IDEImages.LoadImage('menu_goto_line');
  tbFilterAny.ImageIndex   := IDEImages.LoadImage('item_filter');
  tbFilterStart.ImageIndex := IDEImages.LoadImage('item_filter');

  SG.Columns[SG_COLIDX_IMAGE].Width  := 20;
  SG.Columns[SG_COLIDX_PROCEDURE].Width  := 300;
  SG.Columns[SG_COLIDX_TYPE].Width  := 110;
  SG.Columns[SG_COLIDX_LINE].Width  := 60;

  FImageIdxProcedure  := IDEImages.LoadImage('cc_procedure');
  FImageIdxFunction   := IDEImages.LoadImage('cc_function');;

  cbObjects.Style     := csDropDownList;
  cbObjects.Sorted    := True;
  cbObjects.DropDownCount := 8;
end;


procedure TProcedureListForm.JumpToSelection;
var
  CodeBuffer: TCodeBuffer;
  ACodeTool: TCodeTool;
  lRowObject: TGridRowObject;
begin
  if SG.Row < SG.FixedRows then
    Exit;

  lRowObject := TGridRowObject(SG.Rows[SG.Row].Objects[0]);
  if not Assigned(lRowObject) then
    Exit;

  if lRowObject.NodeStartPos < 0 then
    Exit;

  CodeBuffer := CodeToolBoss.LoadFile(MainFilename,false,false);
  if CodeBuffer = nil then
    Exit; //==>

  ACodeTool := nil;
  CodeToolBoss.Explore(CodeBuffer,ACodeTool,false);
  if ACodeTool = nil then
    Exit; //==>

  if not ACodeTool.CleanPosToCaretAndTopLine(lRowObject.NodeStartPos, FCaret, FNewTopLine) then
    Exit; //==>

  { This should close the form }
  self.ModalResult := mrOK;
end;


procedure TProcedureListForm.PopulateGrid;
var
  lSrcEditor: TSourceEditorInterface;
  lCodeBuffer: TCodeBuffer;
  lCodeTool: TCodeTool;
  lNode: TCodeTreeNode;
begin
  SG.BeginUpdate;
  try
    ClearGrid;

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

    { Find the starting point }
    lNode := lCodeTool.FindImplementationNode;
    if lNode = nil then
    begin
      { fall back - guess we are working with a program or there is a syntax error }
      lNode := lCodeTool.Tree.Root;
    end;

    { populate the listview here }
    lNode := lNode.FirstChild;
    while lNode <> nil do
    begin
      if lNode.Desc = ctnProcedure then
      begin
        AddToGrid(lCodeTool, lNode);
      end;
      lNode := lNode.Next;
    end;
  finally
    if SG.RowCount > 0 then
    begin
      SG.Row := SG.FixedRows;
    end;
    SG.EndUpdate;
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


procedure TProcedureListForm.AddToGrid(pCodeTool: TCodeTool; pNode: TCodeTreeNode);
var
  lNodeText: string;
  lCaret: TCodeXYPosition;
  FSearchAll: boolean;
  lAttr: TProcHeadAttributes;
  lRowObject: TGridRowObject;
  lRowIdx: Integer;
begin
  FSearchAll := cbObjects.Text = lisPListAll;

  if FSearchAll and tbFilterAny.Down then
  begin
    lAttr := [phpWithoutClassKeyword, phpWithoutParamList, phpWithoutBrackets,
       phpWithoutSemicolon, phpAddClassName, phpAddParentProcs];
  end
  else
  begin
    lAttr := [phpWithoutClassKeyword, phpWithoutParamList, phpWithoutBrackets,
       phpWithoutSemicolon, phpWithoutClassName];
  end;

  lNodeText := pCodeTool.ExtractProcHead(pNode, lAttr);

  { Must we add this pNode or not? }
  if not PassFilter(FSearchAll, lNodeText, edMethods.Text, pCodeTool, pNode) then
    Exit; //==>
    
  { Add new row }
  lRowIdx := SG.RowCount;
  SG.RowCount := lRowIdx + 1;
  lRowObject := TGridRowObject.Create;
  SG.Rows[lRowIdx].Objects[0] := lRowObject;

  { procedure name }
  lNodeText := pCodeTool.ExtractProcHead(pNode,
      [phpAddParentProcs, phpWithoutParamList, phpWithoutBrackets, phpWithoutSemicolon]);
  SG.Cells[SG_COLIDX_PROCEDURE,lRowIdx] := lNodeText;

  { type }
  lNodeText := pCodeTool.ExtractProcHead(pNode,
      [phpWithStart, phpWithoutParamList, phpWithoutBrackets, phpWithoutSemicolon,
        phpWithoutClassName, phpWithoutName]);
  SG.Cells[SG_COLIDX_TYPE,lRowIdx] := lNodeText;
  
  { line number }
  if pCodeTool.CleanPosToCaret(pNode.StartPos, lCaret) then
    SG.Cells[SG_COLIDX_LINE,lRowIdx] := IntToStr(lCaret.Y);

    
  { start pos - used by JumpToSelected() }
  lRowObject.NodeStartPos := pNode.StartPos;
  
  { full procedure name used in statusbar }
  lNodeText := pCodeTool.ExtractProcHead(pNode,
                  [phpWithStart,phpAddParentProcs,phpWithVarModifiers,
                   phpWithParameterNames,phpWithDefaultValues,phpWithResultType,
                   phpWithOfObject,phpWithCallingSpecs,phpWithProcModifiers]);
  lRowObject.FullProcedureName := lNodeText;

  if Pos('procedure', LowerCase(lNodeText)) > 0 then
    lRowObject.ImageIdx := FImageIdxProcedure
  else
    lRowObject.ImageIdx := FImageIdxFunction;

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
  else
  if not pSearchAll and tbFilterStart.Down then
  begin
    Result := ClassMatches and SameText(pSearchStr, Copy(pProcName, 1, Length(pSearchStr)));
  end
  else
  if not pSearchAll and tbFilterAny.Down then
  begin
    Result := ClassMatches and FilterFits(pSearchStr, pProcName);
  end
  else
  if pSearchAll and tbFilterStart.Down then
  begin
    Result := SameText(pSearchStr, Copy(pProcName, 1, Length(pSearchStr)));
  end
  else
  if pSearchAll then
  begin
    Result := FilterFits(pSearchStr, pProcName);
  end;
end;

procedure TProcedureListForm.ClearGrid;
var
  i: Integer;
begin
  for i:=SG.FixedRows to SG.RowCount - 1 do
    SG.Rows[i].Objects[0].Free;

  SG.RowCount := SG.FixedRows;
end;


procedure TProcedureListForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then   // Escape key
  begin
    self.ModalResult := mrCancel;
  end;
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

procedure TProcedureListForm.edMethodsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if SG.RowCount <= SG.FixedRows then
    Exit;

  if Key = VK_DOWN then
  begin
    if SG.Row < (SG.RowCount - 1) then
      SG.Row := SG.Row + 1;
  end
  else if Key = VK_Up then
  begin
    if SG.Row > SG.FixedRows then
      SG.Row := SG.Row - 1;
  end
  else if Key = VK_Home then
  begin
    if SG.RowCount > SG.FixedRows then
      SG.Row := SG.FixedRows;
  end
  else if Key = VK_End then
  begin
    if SG.RowCount > SG.FixedRows then
      SG.Row := SG.RowCount - 1;
  end;

end;

procedure TProcedureListForm.SomethingChange(Sender: TObject);
begin
  PopulateGrid;
end;

procedure TProcedureListForm.tbAboutClick(Sender: TObject);
begin
  ShowMessage(cAbout);
end;

procedure TProcedureListForm.tbCopyClick(Sender: TObject);
begin
  if SG.Row > 0 then
    Clipboard.AsText := SG.Cells[SG_COLIDX_PROCEDURE,SG.Row];
end;

end.
