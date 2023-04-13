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
  Classes, SysUtils, Math,
  // LCL
  LCLType, Forms, Controls, Dialogs, ComCtrls, ExtCtrls, StdCtrls, Clipbrd,
  Graphics, Grids,
  // LazUtils
  LazStringUtils,
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
    procedure edMethodsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
    FImageIdxConstructor: Integer;
    FImageIdxDestructor: Integer;
    iconBmp: TBitmap;
    function GetCodeTreeNode(out lCodeTool: TCodeTool): TCodeTreeNode;
    { Move editors focus to selected method. }
    procedure JumpToSelection;
    { Populates grid based on selected Class and user entered filter. }
    procedure PopulateGrid;
    { Populates only tho cbObjects combo with available classes. }
    procedure PopulateObjectsCombo;
    function AddToGrid(pCodeTool: TCodeTool; pNode: TCodeTreeNode): boolean;
    function PassFilter(pSearchAll: boolean; pProcName, pSearchStr: string; pCodeTool: TCodeTool; pNode: TCodeTreeNode): boolean;
    procedure ClearGrid;
  public
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
procedure ExecuteProcedureList({%H-}Sender: TObject);
begin
  with TProcedureListForm.Create(nil) do
    try
      if ShowModal = mrOK then
      begin
        LazarusIDE.DoOpenFileAndJumpToPos(
          Caret.Code.Filename,
          Point(Caret.X, Caret.Y),
          NewTopLine,
          -1, -1,
          [ofRegularFile,ofUseCache]
        );
      end;
    finally
      Free;
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
  // assign resource strings to Captions and Hints
  Caption            := lisPListProcedureList;
  lblObjects.Caption := lisPListObjects;
  lblSearch.Caption  := lisMenuSearch;
  tbAbout.Hint       := lisMenuTemplateAbout;
  tbJumpTo.Hint      := lisPListJumpToSelection;
  tbFilterAny.Hint   := lisPListFilterAny;
  tbFilterStart.Hint := lisPListFilterStart;
  tbChangeFont.Hint  := lisPListChangeFont;
  tbCopy.Hint        := lisPListCopyMethodToClipboard;
  SG.Columns[SG_COLIDX_PROCEDURE].Title.Caption := lisProcedure;
  SG.Columns[SG_COLIDX_TYPE     ].Title.Caption := lisPListType;
  SG.Columns[SG_COLIDX_LINE     ].Title.Caption := dlgAddHiAttrGroupLine;

  // assign resource images to toolbuttons
  TB.Images := IDEImages.Images_16;
  tbCopy.ImageIndex        := IDEImages.LoadImage('laz_copy');
  tbChangeFont.ImageIndex  := IDEImages.LoadImage('item_font');
  tbAbout.ImageIndex       := IDEImages.LoadImage('menu_information');
  tbJumpTo.ImageIndex      := IDEImages.LoadImage('menu_goto_line');
  tbFilterAny.ImageIndex   := IDEImages.LoadImage('filter_any_place');
  tbFilterStart.ImageIndex := IDEImages.LoadImage('filter_from_begin');

  // assign resource images to procedures
  iconBmp := TBitmap.Create;
  FImageIdxProcedure       := IDEImages.LoadImage('cc_procedure');
  FImageIdxFunction        := IDEImages.LoadImage('cc_function');
  FImageIdxConstructor     := IDEImages.LoadImage('cc_constructor');
  FImageIdxDestructor      := IDEImages.LoadImage('cc_destructor');

  SG.FocusRectVisible := false;

  if SourceEditorManagerIntf.ActiveEditor = nil then Exit; //==>

  FMainFilename := SourceEditorManagerIntf.ActiveEditor.Filename;
  Caption := Caption + ' - ' + ExtractFileName(FMainFilename);
  PopulateObjectsCombo;
  PopulateGrid;
  StatusBar.Panels[0].Text := FMainFilename;
  tbFilterStart.Down := EnvironmentOptions.ProcedureListFilterStart;
  IDEDialogLayoutList.ApplyLayout(Self, 950, 680);
end;

procedure TProcedureListForm.edMethodsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

  if (Shift = []) and (Key in [VK_A..VK_Z]) then
  begin
    edMethods.SelText := chr(Key + $20); // VK-codes matches ASCII chars
    Key := 0;
  end;

  if (Shift = [ssShift]) and (Key in [VK_A..VK_Z]) then
  begin
    edMethods.SelText := chr(Key); // VK-codes matches ASCII chars
    Key := 0;
  end;

  if (Shift = []) and (Key = VK_OEM_PERIOD) then
  begin
    edMethods.SelText := '.';
    Key := 0;
  end;

end;

procedure TProcedureListForm.FormDestroy(Sender: TObject);
begin
  EnvironmentOptions.ProcedureListFilterStart := tbFilterStart.Down;
  ClearGrid;
  IDEDialogLayoutList.SaveLayout(Self);
  FreeAndNil(iconBmp);
end;

procedure TProcedureListForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of

    { Form }
    VK_RETURN : begin
        JumpToSelection;
        Key := 0;
      end;
    VK_ESCAPE : begin
        Key := 0;
        ModalResult := mrCancel;
      end;

    { Arrows }
    VK_DOWN : begin
        if cbObjects.Focused then exit;

        if SG.Row < SG.FixedRows then // if (Row = -1) or (Row < FixedRows)
        begin
          if SG.RowCount > SG.FixedRows then
            SG.Row := SG.FixedRows;
        end else begin
          if (SG.Row + 1) < SG.RowCount then
            SG.Row := SG.Row + 1;
        end;
        Key := 0;
      end;
    VK_UP : begin
        if cbObjects.Focused then exit;

        if SG.Row < SG.FixedRows then // if (Row = -1) or (Row < FixedRows)
        begin
          if SG.RowCount > SG.FixedRows then
            SG.Row := SG.RowCount - 1;
        end else begin
          if SG.Row > SG.FixedRows then
            SG.Row := SG.Row - 1;
        end;
        Key := 0;
      end;

    { PageUp and PageDown }
    VK_NEXT : begin
        if SG.Row < SG.FixedRows then // if (Row = -1) or (Row < FixedRows)
        begin
          if SG.RowCount > SG.FixedRows then
            SG.Row := SG.FixedRows;
        end else begin
          SG.Row := Min(SG.RowCount - 1, SG.Row + (SG.VisibleRowCount - 1));
        end;
        Key := 0;
      end;
    VK_PRIOR : begin
        if SG.Row < SG.FixedRows then // if (Row = -1) or (Row < FixedRows)
        begin
          if SG.Row > SG.FixedRows then
            SG.Row := SG.Row - 1;
        end else begin
          SG.Row := Max(SG.FixedRows, SG.Row - (SG.VisibleRowCount - 1));
        end;
        Key := 0;
      end;

    end;  // case
  end;  // if Shift = []

  if Shift = [ssCtrl] then
  begin
    case Key of

    { Home and End }
    VK_HOME : begin
        if SG.RowCount > SG.FixedRows then
          SG.Row := SG.FixedRows;
        Key := 0;
      end;
    VK_END : begin
        if SG.RowCount > SG.FixedRows then
          SG.Row := SG.RowCount - 1;
        Key := 0;
      end;

    { Scroll one line }
    VK_DOWN : begin
        if SG.RowCount > SG.FixedRows then
          SG.TopRow := Max(SG.FixedRows, Min(SG.RowCount - 1, SG.TopRow + 1));
        Key := 0;
      end;
    VK_UP : begin
        if SG.RowCount > SG.FixedRows then
          SG.TopRow := Max(SG.FixedRows, Min(SG.RowCount - 1, SG.TopRow - 1));
        Key := 0;
      end;

    { Copy }
    VK_C : begin
        // copy the selected procedure only if no text is selected in edMethods
        if edMethods.SelLength = 0 then
        begin
          tbCopyClick(Sender);
          Key := 0;
        end;
      end;

    end;  // case
  end;  // if Shift = [ssCtrl]
end;

procedure TProcedureListForm.FormResize(Sender: TObject);
begin
  StatusBar.Panels[0].Width := ClientWidth - 105;
end;

procedure TProcedureListForm.FormShow(Sender: TObject);
begin
  edMethods.SetFocus;
  cbObjects.DropDownCount := EnvironmentOptions.DropDownCount;
end;

procedure TProcedureListForm.SGDblClick(Sender: TObject);
begin
  JumpToSelection;
end;

procedure TProcedureListForm.SGDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  iconLeft, iconTop: Integer;
  rowObj: TGridRowObject;
begin
  if (aCol = SG_COLIDX_IMAGE) and (aRow >= SG.FixedRows) then
  begin
    rowObj := TGridRowObject(SG.Rows[aRow].Objects[0]);
    if Assigned(rowObj) then
    begin
      if rowObj.ImageIdx >= 0 then
      begin
        IDEImages.Images_16.GetBitmap(rowObj.ImageIdx, iconBmp);
        iconLeft := aRect.Width - iconBmp.Width; // right align
        iconTop  := aRect.Top  + (aRect.Height - iconBmp.Height) div 2; // center align
        SG.Canvas.Draw(iconLeft, iconTop, iconBmp);
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

  CodeBuffer := CodeToolBoss.LoadFile(FMainFilename,false,false);
  if CodeBuffer = nil then
    Exit; //==>

  ACodeTool := nil;
  CodeToolBoss.Explore(CodeBuffer,ACodeTool,false);
  if ACodeTool = nil then
    Exit; //==>

  if not ACodeTool.CleanPosToCaretAndTopLine(lRowObject.NodeStartPos, FCaret, FNewTopLine) then
    Exit; //==>

  { This should close the form }
  ModalResult := mrOK;
end;

procedure TProcedureListForm.PopulateGrid;
var
  lCodeTool: TCodeTool;
  lNode: TCodeTreeNode;
  lShown, lTotal: integer;
begin
  lShown := 0;
  lTotal := 0;
  SG.BeginUpdate;
  try
    ClearGrid;
    lNode := GetCodeTreeNode(lCodeTool);
    if lCodeTool = nil then exit;;
    while lNode <> nil do
    begin
      if lNode.Desc = ctnProcedure then
      begin
        inc(lTotal);
        if AddToGrid(lCodeTool, lNode) then
          inc(lShown);
      end;
      lNode := lNode.Next;
    end;
  finally
    if SG.RowCount > SG.FixedRows then
      SG.Row := SG.FixedRows;
    SG.EndUpdate;
    StatusBar.Panels[1].Text := inttostr(lShown) + ' / ' + inttostr(lTotal);
  end;
end;

procedure TProcedureListForm.PopulateObjectsCombo;
var
  lCodeTool: TCodeTool;
  lNode: TCodeTreeNode;
  lNodeText: string;
begin
  cbObjects.Items.Clear;
  try
    lNode := GetCodeTreeNode(lCodeTool);
    if lCodeTool = nil then exit;;
    while lNode <> nil do
    begin
      if lNode.Desc = ctnProcedure then
      begin
        lNodeText := lCodeTool.ExtractClassNameOfProcNode(lNode);
        if lNodeText <> '' then
          cbObjects.Items.Add(lNodeText);
      end;
      lNode := lNode.NextBrother;
    end;
  finally
    cbObjects.Sorted := false;
    cbObjects.Items.Insert(0, lisPListAll);
    cbObjects.Items.Insert(1, lisPListNone);
    cbObjects.ItemIndex := 0;   // select <All> as the default
    if (cbObjects.Items.Count > 0) and (cbObjects.Text = '') then
      cbObjects.Text := cbObjects.Items[0]; // some widgetsets have issues so we do this
  end;
end;

function TProcedureListForm.AddToGrid(pCodeTool: TCodeTool; pNode: TCodeTreeNode): boolean;
var
  lNodeText: string;
  lCaret: TCodeXYPosition;
  FSearchAll: boolean;
  lAttr: TProcHeadAttributes;
  lRowObject: TGridRowObject;
  lRowIdx: Integer;
begin
  result := false;

  FSearchAll := cbObjects.ItemIndex = 0; // lisPListAll

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

  if PosI('procedure ', lNodeText) > 0 then
    lRowObject.ImageIdx := FImageIdxProcedure
  else if PosI('function ', lNodeText) > 0 then
    lRowObject.ImageIdx := FImageIdxFunction
  else if PosI('constructor ', lNodeText) > 0 then
    lRowObject.ImageIdx := FImageIdxConstructor
  else if PosI('destructor ', lNodeText) > 0 then
    lRowObject.ImageIdx := FImageIdxDestructor
  else
    lRowObject.ImageIdx := -1;

  result := true;
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
    if cbObjects.ItemIndex = 1 then // lisPListNone
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
    Result := ClassMatches and LazStartsText(pSearchStr, pProcName)
  else
  if not pSearchAll and tbFilterAny.Down then
    Result := ClassMatches and FilterFits(pSearchStr, pProcName)
  else
  if pSearchAll and tbFilterStart.Down then
    Result := LazStartsText(pSearchStr, pProcName)
  else
  if pSearchAll then
    Result := FilterFits(pSearchStr, pProcName);
end;

procedure TProcedureListForm.ClearGrid;
var
  i: Integer;
begin
  for i:=SG.FixedRows to SG.RowCount - 1 do
    SG.Rows[i].Objects[0].Free;

  SG.RowCount := SG.FixedRows;
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
  if SG.Row >= SG.FixedRows then
    Clipboard.AsText := SG.Cells[SG_COLIDX_PROCEDURE, SG.Row];
end;

function TProcedureListForm.GetCodeTreeNode(out lCodeTool: TCodeTool): TCodeTreeNode;
var
  lCodeBuffer: TCodeBuffer;
begin
  result := nil;

  { get active source editor }
  if SourceEditorManagerIntf.ActiveEditor = nil then exit;

  lCodeBuffer := SourceEditorManagerIntf.ActiveEditor.CodeToolsBuffer as TCodeBuffer;
  if lCodeBuffer = nil then exit;

  { parse source }
  CodeToolBoss.Explore(lCodeBuffer, lCodeTool, False);

  if lCodeTool = nil then exit;
  if lCodeTool.Tree = nil then exit;
  if lCodeTool.Tree.Root = nil then exit;

  { Find the starting point }
  result := lCodeTool.FindImplementationNode;
  if result = nil then
  begin
    { fall back - guess we are working with a program or there is a syntax error }
    result := lCodeTool.Tree.Root;
  end;

  result := result.FirstChild;
end;

end.
