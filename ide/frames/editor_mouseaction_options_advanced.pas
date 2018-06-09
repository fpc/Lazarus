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
}
unit editor_mouseaction_options_advanced;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, math,
  // LCL
  StdCtrls, ExtCtrls, Controls, Grids, ComCtrls, Dialogs,
  // LazUtils
  Laz2_XMLCfg, LazUTF8, FileUtil,
  // SynEdit
  SynEditMouseCmds,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEImagesIntf, IDEDialogs,
  // IDE
  EditorOptions, LazarusIDEStrConsts, MouseActionDialog, KeyMapping;

type

  { TEditorMouseOptionsAdvFrame }

  TEditorMouseOptionsAdvFrame = class(TAbstractIDEOptionsEditor)
    OtherActionLabel: TLabel;
    OpenDialog1: TOpenDialog;
    OtherActionPanel: TPanel;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    OtherActionGrid: TStringGrid;
    OtherActToggleBox: TToggleBox;
    ToolBar1: TToolBar;
    BtnImport: TToolButton;
    BtnExport: TToolButton;
    ToolButton3: TToolButton;
    UpdateButton: TToolButton;
    AddNewButton: TToolButton;
    DelButton: TToolButton;
    ActionGrid: TStringGrid;
    ContextTree: TTreeView;
    procedure ActionGridHeaderClick(Sender: TObject; {%H-}IsColumn: Boolean; Index: Integer);
    procedure ActionGridCompareCells(Sender: TObject; {%H-}ACol, ARow, {%H-}BCol, BRow: Integer;
      var Result: integer);
    procedure ActionGridMouseDown(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState;
      {%H-}X, Y: Integer);
    procedure ActionGridMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure ActionGridMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X,
      {%H-}Y: Integer);
    procedure ActionGridSelection(Sender: TObject; {%H-}aCol, {%H-}aRow: Integer);
    procedure ContextTreeChange(Sender: TObject; Node: TTreeNode);
    procedure OtherActionGridHeaderClick(Sender: TObject; {%H-}IsColumn: Boolean; Index: Integer);
    procedure OtherActionGridHeaderSized(Sender: TObject; {%H-}IsColumn: Boolean; {%H-}Index: Integer);
    procedure OtherActionGridMouseDown(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState;
      {%H-}X, Y: Integer);
    procedure OtherActionGridMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure OtherActionGridMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X,
      {%H-}Y: Integer);
    procedure OtherActionGridResize(Sender: TObject);
    procedure AddNewButtonClick(Sender: TObject);
    procedure OtherActionGridSelectCell(Sender: TObject; {%H-}aCol, {%H-}aRow: Integer;
      var CanSelect: Boolean);
    procedure OtherActToggleBoxChange(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
    procedure DelButtonClick(Sender: TObject);
    procedure ActionGridHeaderSized(Sender: TObject; {%H-}IsColumn: Boolean; {%H-}Index: Integer);
    procedure ActionGridResize(Sender: TObject);
    procedure BtnExportClick(Sender: TObject);
    procedure BtnImportClick(Sender: TObject);
  private
    FTempMouseSettings: TEditorMouseOptions;
    FKeyMap: TKeyCommandRelationList;

    FGlobalNode: TTreeNode;
    FTextNode, FSelNode: TTreeNode;
    FGutterNode: TTreeNode;
    FGutterFoldNode, FGutterFoldExpNode, FGutterFoldColNode: TTreeNode;
    FGutterLinesNode: TTreeNode;
    FGutterChangeNode: TTreeNode;
    FGutterLineOverviewNode, FGutterLineOverviewMarksNode: TTreeNode;
    FCurNode: TTreeNode;

    FCurActions: TSynEditMouseActions;

    FSort, FOtherSort: Array [1..4] of Integer;
    ChangeDlg: TMouseaActionDialog;

    FColWidths: Array of Integer;
    FLastWidth: Integer;
    FIsHeaderSizing: Boolean;

    FOtherActColWidths: Array of Integer;
    FOtherActLastWidth: Integer;
    FIsOtherActHeaderSizing: Boolean;
    FAllowOtherActSel: Boolean;
    procedure SortGrid;
    procedure SelectRow(AAct: TSynEditMouseAction);
    procedure FillRow(aGrid: TStringGrid; aRow, aFirstCol: Integer; aAct: TSynEditMouseAction);
    procedure UpdateOthers;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    procedure RefreshSettings;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

var
  MMoveName: Array [Boolean] of String;

implementation

{$R *.lfm}

const
  MinGridColSize = 25;
{ TEditorMouseOptionsAdvFrame }

(* Sort Action Grid *)
procedure TEditorMouseOptionsAdvFrame.ActionGridHeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
begin
  If Index <> FSort[1] then begin
    if FSort[3] <> index then
      FSort[4] := FSort[3];
    if FSort[2] <> index then
      FSort[3] := FSort[2];
    FSort[2] := FSort[1];
    FSort[1] := Index;
  end;
  SortGrid;
end;

procedure TEditorMouseOptionsAdvFrame.ActionGridCompareCells(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);
  function CompareCol(i : Integer) : Integer;
  var
    j: Integer;
  begin
    j := i;
    if Sender = OtherActionGrid then
      dec(j, 2);
    case j of
     -2: // Order
        Result := Integer(PtrInt(TStringGrid(Sender).Objects[1, ARow]))
                - Integer(PtrInt(TStringGrid(Sender).Objects[1, BRow]));
      2: // ClickCount
        Result := ord(TSynEditMouseAction(TStringGrid(Sender).Objects[0, ARow]).ClickCount)
                - ord(TSynEditMouseAction(TStringGrid(Sender).Objects[0, BRow]).ClickCount);
      3: // ClickDir (down first)
        Result := ord(TSynEditMouseAction(TStringGrid(Sender).Objects[0, BRow]).ClickDir)
                - ord(TSynEditMouseAction(TStringGrid(Sender).Objects[0, ARow]).ClickDir);
      else
        Result := UTF8CompareText(TStringGrid(Sender).Cells[i, ARow], TStringGrid(Sender).Cells[i, BRow]);
    end;
  end;
var
  i: Integer;
begin
  Result := 0;
  if Sender = nil then exit;
  if Sender = OtherActionGrid then begin
    for i := 1 to 4 do if result = 0 then
      Result := CompareCol(FOtherSort[i]);
    if Result = 0 then
      Result := CompareCol(9); // Priority
  end else begin
    for i := 1 to 4 do if result = 0 then
      Result := CompareCol(FSort[i]);
    if Result = 0 then
      Result := CompareCol(7); // Priority
  end;
  if Result = 0 then
    Result := TSynEditMouseAction(TStringGrid(Sender).Objects[0, ARow]).ID
            - TSynEditMouseAction(TStringGrid(Sender).Objects[0, BRow]).ID;
end;

procedure TEditorMouseOptionsAdvFrame.SortGrid;
begin
  ActionGrid.SortColRow(True, 0);
end;

(* Resize Action Grid *)
procedure TEditorMouseOptionsAdvFrame.ActionGridHeaderSized(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
var
  i: Integer;
begin
  SetLength(FColWidths, ActionGrid.ColCount);
  for i := 0 to ActionGrid.ColCount - 1 do
    FColWidths[i] := Min(Max(MinGridColSize, ActionGrid.ColWidths[i]),
                             ActionGrid.ClientWidth);
  FLastWidth := -2;
  ActionGridResize(nil);
end;

procedure TEditorMouseOptionsAdvFrame.ActionGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FIsHeaderSizing := y <= ActionGrid.RowHeights[0];
end;

procedure TEditorMouseOptionsAdvFrame.ActionGridMouseMove(Sender: TObject;
  Shift: TShiftState; X,
  Y: Integer);
begin
  if not FIsHeaderSizing then exit;
  ActionGridHeaderSized(nil, true, 0);
end;

procedure TEditorMouseOptionsAdvFrame.ActionGridMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FIsHeaderSizing := False;
end;

procedure TEditorMouseOptionsAdvFrame.ActionGridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  UpdateOthers;
end;

procedure TEditorMouseOptionsAdvFrame.ActionGridResize(Sender: TObject);
var
  i, Oldwidth, NewWidth: Integer;
begin
  if ActionGrid.Width = FLastWidth then Exit;
  FLastWidth := ActionGrid.Width;
  if Length(FColWidths) < ActionGrid.ColCount then exit;
  Oldwidth := 0;
  for i := 0 to ActionGrid.ColCount-1 do Oldwidth := Oldwidth + FColWidths[i];
  NewWidth := ActionGrid.ClientWidth - 1;
  for i := 0 to ActionGrid.ColCount-1 do
    NewWidth := NewWidth - (MinGridColSize -
              Min(MinGridColSize, FColWidths[i] * NewWidth div Oldwidth));
  for i := 0 to ActionGrid.ColCount-1 do
    ActionGrid.ColWidths[i] := Max(MinGridColSize, FColWidths[i] * NewWidth div Oldwidth);
end;

(* Resize OtherAction Grid *)
procedure TEditorMouseOptionsAdvFrame.OtherActionGridHeaderSized(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
var
  i: Integer;
begin
  SetLength(FOtherActColWidths, OtherActionGrid.ColCount);
  for i := 0 to OtherActionGrid.ColCount - 1 do
    FOtherActColWidths[i] := Min(Max(MinGridColSize, OtherActionGrid.ColWidths[i]),
                             OtherActionGrid.ClientWidth);
  FOtherActLastWidth := -2;
  OtherActionGridResize(nil);
end;

procedure TEditorMouseOptionsAdvFrame.OtherActionGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FIsOtherActHeaderSizing := y <= OtherActionGrid.RowHeights[0];
end;

procedure TEditorMouseOptionsAdvFrame.OtherActionGridMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if not FIsOtherActHeaderSizing then exit;
  OtherActionGridHeaderSized(nil, true, 0);
end;

procedure TEditorMouseOptionsAdvFrame.OtherActionGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FIsOtherActHeaderSizing := False;
end;

procedure TEditorMouseOptionsAdvFrame.OtherActionGridResize(Sender: TObject);
var
  i, Oldwidth, NewWidth: Integer;
begin
  if OtherActionGrid.Width = FOtherActLastWidth then Exit;
  FOtherActLastWidth := OtherActionGrid.Width;
  if Length(FOtherActColWidths) < OtherActionGrid.ColCount then exit;
  Oldwidth := 0;
  for i := 0 to OtherActionGrid.ColCount-1 do Oldwidth := Oldwidth + FOtherActColWidths[i];
  NewWidth := OtherActionGrid.ClientWidth - 1;
  for i := 0 to OtherActionGrid.ColCount-1 do
    NewWidth := NewWidth - (MinGridColSize -
              Min(MinGridColSize, FOtherActColWidths[i] * NewWidth div Oldwidth));
  for i := 0 to OtherActionGrid.ColCount-1 do
    OtherActionGrid.ColWidths[i] := Max(MinGridColSize, FOtherActColWidths[i] * NewWidth div Oldwidth);
end;

(* Selection *)
procedure TEditorMouseOptionsAdvFrame.FillRow(aGrid: TStringGrid; aRow, aFirstCol: Integer;
  aAct: TSynEditMouseAction);

  function ShiftName(ss: TShiftStateEnum): String;
  begin
    if not(ss in aAct.ShiftMask) then exit(dlgMouseOptModKeyIgnore);
    if ss in aAct.Shift then exit(dlgMouseOptModKeyTrue);
    exit(dlgMouseOptModKeyFalse);
  end;

var
  j: Integer;
  optlist: TStringList;
begin
  aGrid.Objects[0, aRow] := aAct;
  aGrid.Cells[aFirstCol + 0, aRow] := aAct.DisplayName;
  aGrid.Cells[aFirstCol + 1, aRow] := ButtonName[aAct.Button];
  aGrid.Cells[aFirstCol + 2, aRow] := ClickName[aAct.ClickCount];
  aGrid.Cells[aFirstCol + 3, aRow] := ButtonDirName[aAct.ClickDir];;
  aGrid.Cells[aFirstCol + 4, aRow] := ShiftName(ssShift);
  aGrid.Cells[aFirstCol + 5, aRow] := ShiftName(ssAlt);
  aGrid.Cells[aFirstCol + 6, aRow] := ShiftName(ssCtrl);
  aGrid.Cells[aFirstCol + 7, aRow] := IntToStr(aAct.Priority);
  aGrid.Cells[aFirstCol + 8, aRow] := MMoveName[aAct.MoveCaret];
  aGrid.Cells[aFirstCol + 9, aRow] := '';
  if aAct.Command =  emcSynEditCommand then begin
    j := KeyMapIndexOfCommand(FKeyMap, AAct.Option);
    if (j >= 0) and (j < FKeyMap.RelationCount) then
      aGrid.Cells[aFirstCol + 9, aRow] := FKeyMap.Relations[j].GetLocalizedName;
  end
  else begin
    optlist := TStringlist.Create;
    optlist.CommaText := MouseCommandConfigName(aAct.Command);
    if aAct.Option < optlist.Count-1 then
      aGrid.Cells[aFirstCol + 9, aRow] := optlist[aAct.Option+1] +' ('+optlist[0]+')';
    optlist.Free;
  end;
end;

procedure TEditorMouseOptionsAdvFrame.ContextTreeChange(Sender: TObject; Node: TTreeNode);
var
  i: Integer;
begin
  if Node = nil then exit;
  FCurNode := Node;
  FCurActions := TSynEditMouseActions(Node.Data);
  ActionGrid.RowCount := FCurActions.Count + 1;
  for i := 1 to FCurActions.Count do
    FillRow(ActionGrid, i, 0, FCurActions[i-1]);
  ActionGrid.Row := 1;
  SortGrid;
  UpdateOthers;
end;

procedure TEditorMouseOptionsAdvFrame.OtherActionGridHeaderClick(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
begin
  If Index <> FOtherSort[1] then begin
    if FOtherSort[3] <> index then
      FOtherSort[4] := FOtherSort[3];
    if FOtherSort[2] <> index then
      FOtherSort[3] := FOtherSort[2];
    FOtherSort[2] := FOtherSort[1];
    FOtherSort[1] := Index;
  end;
  OtherActionGrid.SortColRow(True, 0);
end;

procedure TEditorMouseOptionsAdvFrame.SelectRow(AAct: TSynEditMouseAction);
var
  i: Integer;
begin
  For i := 1 to ActionGrid.RowCount -1 do
    if ActionGrid.Objects[0, i] = AAct then begin
      ActionGrid.Row := i;
      break;
    end;
end;

procedure TEditorMouseOptionsAdvFrame.UpdateOthers;
const
  DirList: Array [0..1] of TSynMAClickDir = (cdDown, cdUp);
  FallBackList: Array [0..1] of Boolean = (False, True);
var
  MAct: TSynEditMouseAction;
  ActList: TSynEditMouseActions;
  Node: TTreeNode;
  i, Row: Integer;
  Order, FoundOrder: Integer;
  FindDir, FindFallBack, FindPrior: Integer;
begin
  OtherActionGrid.RowCount := 1;
  if FCurActions = nil then exit;
  if (ActionGrid.Row-1 >= FCurActions.Count) or (ActionGrid.Row < 1) then exit;
  MAct := TSynEditMouseAction(ActionGrid.Objects[0, ActionGrid.Row]);
  if MAct = nil then exit;

  Row := 1;
  Order := 1;
  FoundOrder := 0;
  // Search up/Down
  for FindDir := low(DirList) to high(DirList) do begin
    // search context
    ActList := FCurActions;
    Node := ContextTree.Items.FindNodeWithData(FCurActions);
    while (ActList <> nil) and (Node <> nil) do begin
      // Search Mod-Key/Default
      for FindPrior := 0 to 3 do begin
        for FindFallBack := low(FallBackList) to high(FallBackList) do begin
          for i := 0 to ActList.Count - 1 do
            if (ActList[i].Button = MAct.Button) and
               (ActList[i].ClickDir = DirList[FindDir]) and
               (ActList[i].IsFallback = FallBackList[FindFallBack]) and
               (ActList[i].Priority = FindPrior) and
               ( (not OtherActToggleBox.Checked) or
                 (ActList[i].Shift * ActList[i].ShiftMask * MAct.ShiftMask =
                  MAct.Shift * ActList[i].ShiftMask * MAct.ShiftMask)  )
            then begin
              OtherActionGrid.RowCount := Row + 1;
              FillRow(OtherActionGrid, Row, 2, ActList[i]);
              OtherActionGrid.Cells[1,Row] := Node.Text;
              OtherActionGrid.Cells[0,Row] := IntToStr(Order);
              OtherActionGrid.Objects[1,Row] := TObject({%H-}Pointer(PtrInt(Order)));
              FoundOrder := Order;
              FAllowOtherActSel := True;
              if ActList[i].Equals(MAct) and (ActList = FCurActions) then
                OtherActionGrid.Row := Row;
              FAllowOtherActSel := False;
              inc(Row);
            end;
          if Order = FoundOrder then
            inc(Order);
        end; // FindFallBack
        if Order = FoundOrder then
          inc(Order);
      end; // FindPrior
      if Order = FoundOrder then
        inc(Order);
      Node := Node.Parent;
      if (Node <> nil) then
        ActList := TSynEditMouseActions(Node.Data);
    end; // Context
    if Order = FoundOrder then
      inc(Order);
  end; // FindDir
  OtherActionGrid.SortColRow(True, 0);
end;

(* ----- *)
procedure TEditorMouseOptionsAdvFrame.AddNewButtonClick(Sender: TObject);
var
  MAct: TSynEditMouseAction;
begin
  if FCurActions = nil then exit;

  ChangeDlg.KeyMap := FKeyMap;
  ChangeDlg.ResetInputs;
  if ChangeDlg.ShowModal = mrOK then begin
    try
      FCurActions.IncAssertLock;
      MAct := FCurActions.Add;
      ChangeDlg.WriteToAction(MAct);
    finally
      FCurActions.DecAssertLock;
    end;
    try
      FCurActions.AssertNoConflict(MAct);
    except
      FCurActions.Delete(FCurActions.Count - 1);
      IDEMessageDialog(dlgMouseOptErrorDup, dlgMouseOptErrorDupText, mtError, [mbOk]);
    end;
    ContextTreeChange(nil, FCurNode);
    SelectRow(MAct);
  end;
end;

procedure TEditorMouseOptionsAdvFrame.OtherActionGridSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  CanSelect := FAllowOtherActSel;
end;

procedure TEditorMouseOptionsAdvFrame.OtherActToggleBoxChange(Sender: TObject);
begin
  UpdateOthers;
end;

procedure TEditorMouseOptionsAdvFrame.UpdateButtonClick(Sender: TObject);
var
  MAct, MOld: TSynEditMouseAction;
begin
  if FCurActions = nil then exit;
  if (ActionGrid.Row-1 >= FCurActions.Count) or (ActionGrid.Row < 1) then exit;

  MAct := TSynEditMouseAction(ActionGrid.Objects[0, ActionGrid.Row]);
  ChangeDlg.KeyMap := FKeyMap;
  ChangeDlg.ReadFromAction(MAct);
  if ChangeDlg.ShowModal = mrOK then begin
    try
      FCurActions.IncAssertLock;
      MOld := TSynEditMouseAction.Create(nil);
      MOld.Assign(MAct);
      ChangeDlg.WriteToAction(MAct);
    finally
      FCurActions.DecAssertLock;
    end;
    try
      FCurActions.AssertNoConflict(MAct);
    except
      IDEMessageDialog(dlgMouseOptErrorDup, dlgMouseOptErrorDupText, mtError, [mbOk]);
      MAct.Assign(MOld);
    end;
    MOld.Free;
    ContextTreeChange(nil, FCurNode);
    SelectRow(MAct);
  end;
end;

procedure TEditorMouseOptionsAdvFrame.DelButtonClick(Sender: TObject);
begin
  if FCurActions = nil then exit;
  if (ActionGrid.Row-1 >= FCurActions.Count) or (ActionGrid.Row < 1) then exit;
  FCurActions.Delete(FCurActions.IndexOf(TSynEditMouseAction
                                      (ActionGrid.Objects[0, ActionGrid.row])));
  ActionGrid.Row := 1;
  ContextTreeChange(nil, FCurNode);
end;

procedure TEditorMouseOptionsAdvFrame.BtnExportClick(Sender: TObject);
var
  xml: TRttiXMLConfig;
  NewName: String;
  l: Integer;
begin
  if SaveDialog1.Execute then begin
    xml := TRttiXMLConfig.CreateClean(SaveDialog1.FileName);

    NewName := ExtractFileName(SaveDialog1.FileName);
    l := length(ExtractFileExt(NewName));
    if (l > 0) and (l+1 < Length(NewName)) then
      NewName := Copy(NewName, 1, Length(NewName) - l);
    l := UTF8CodepointSize(PChar(NewName));
    if l > 0 then
      NewName := UTF8UpperCase(copy(NewName, 1, l)) + copy(NewName, 1+l, length(NewName));

    xml.SetValue('Lazarus/MouseSchemes/Names/Count', 1);
    xml.SetValue('Lazarus/MouseSchemes/Names/Item1/Value', NewName);

    FTempMouseSettings.ExportToXml(xml, 'Lazarus/MouseSchemes/Scheme' + NewName + '/' );
    xml.Flush;
    xml.Free;
  end;
end;

procedure TEditorMouseOptionsAdvFrame.BtnImportClick(Sender: TObject);
var
  xml: TRttiXMLConfig;
  c: longint;
  n: String;
begin
  if OpenDialog1.Execute then begin
    xml := TRttiXMLConfig.Create(OpenDialog1.FileName);
    if xml.HasChildPaths('Mouse/') then begin
      // Load old export
      FTempMouseSettings.ImportFromXml(xml, 'Mouse/')
    end else begin
      c := xml.GetValue('Lazarus/MouseSchemes/Names/Count', 0);
      n := '';
      if c > 0 then   // Only reading First Scheme
        n := xml.GetValue('Lazarus/MouseSchemes/Names/Item1/Value', '');
      if n <> '' then
        FTempMouseSettings.ImportFromXml(xml, 'Lazarus/MouseSchemes/Scheme' + n+ '/');
    end;
    xml.Free;
    ContextTree.Selected := FTextNode;
    ContextTreeChange(nil, FTextNode);
  end;
end;

constructor TEditorMouseOptionsAdvFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ChangeDlg := TMouseaActionDialog.Create(self);
end;

destructor TEditorMouseOptionsAdvFrame.Destroy;
begin
  ChangeDlg.Free;
  inherited Destroy;
end;

function TEditorMouseOptionsAdvFrame.GetTitle: String;
begin
  Result := dlgMouseOptionsAdv;
end;

procedure TEditorMouseOptionsAdvFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  FTempMouseSettings := EditorOpts.TempMouseSettings;
  ContextTree.Items.Clear;
  FGlobalNode := ContextTree.Items.Add(nil, dlgMouseOptNodeAll);
  FGlobalNode.Data := FTempMouseSettings.MainActions;

  // Text
  FTextNode := ContextTree.Items.AddChild(FGlobalNode, dlgMouseOptNodeMain);
  FTextNode.Data := FTempMouseSettings.TextActions;
  // Selection
  FSelNode := ContextTree.Items.AddChild(FTextNode, dlgMouseOptNodeSelect);
  FSelNode.Data := FTempMouseSettings.SelActions;
  // Gutter
  FGutterNode := ContextTree.Items.AddChild(FGlobalNode, dlgMouseOptNodeGutter);
  FGutterNode.Data := FTempMouseSettings.GutterActions;
  // Gutter Fold
  FGutterFoldNode := ContextTree.Items.AddChild(FGutterNode, dlgMouseOptNodeGutterFold);
  FGutterFoldNode.Data := FTempMouseSettings.GutterActionsFold;
  FGutterFoldColNode := ContextTree.Items.AddChild(FGutterFoldNode, dlgMouseOptNodeGutterFoldCol);
  FGutterFoldColNode.Data := FTempMouseSettings.GutterActionsFoldCol;
  FGutterFoldExpNode := ContextTree.Items.AddChild(FGutterFoldNode, dlgMouseOptNodeGutterFoldExp);
  FGutterFoldExpNode.Data := FTempMouseSettings.GutterActionsFoldExp;
  // LineNum
  FGutterLinesNode := ContextTree.Items.AddChild(FGutterNode, dlgMouseOptNodeGutterLines);
  FGutterLinesNode.Data := FTempMouseSettings.GutterActionsLines;
  // Changes
  FGutterChangeNode := ContextTree.Items.AddChild(FGutterNode, dlgMouseOptNodeGutterChanges);
  FGutterChangeNode.Data := FTempMouseSettings.GutterActionsChanges;
  // Overview
  FGutterLineOverviewNode := ContextTree.Items.AddChild(FGutterNode, dlgMouseOptNodeGutterLineOverview);
  FGutterLineOverviewNode.Data := FTempMouseSettings.GutterActionsOverView;
  FGutterLineOverviewMarksNode := ContextTree.Items.AddChild(FGutterLineOverviewNode, dlgMouseOptNodeGutterLineOverviewMarks);
  FGutterLineOverviewMarksNode.Data := FTempMouseSettings.GutterActionsOverViewMarks;

  ActionGrid.Constraints.MinWidth := ActionGrid.ColCount * MinGridColSize;
  ActionGrid.Cells[0,0] := dlgMouseOptHeadDesc;
  ActionGrid.Cells[1,0] := dlgMouseOptHeadBtn;
  ActionGrid.Cells[2,0] := dlgMouseOptHeadCount;
  ActionGrid.Cells[3,0] := dlgMouseOptHeadDir;
  ActionGrid.Cells[4,0] := dlgMouseOptHeadShift;
  ActionGrid.Cells[5,0] := dlgMouseOptHeadAlt;
  ActionGrid.Cells[6,0] := dlgMouseOptHeadCtrl;
  ActionGrid.Cells[7,0] := dlgMouseOptHeadPriority;
  ActionGrid.Cells[8,0] := dlgMouseOptHeadCaret;
  ActionGrid.Cells[9,0] := dlgMouseOptHeadOpt;

  SetLength(FColWidths, ActionGrid.ColCount);
  FColWidths[0] := 40;
  FColWidths[1] := 15;
  FColWidths[2] := 15;
  FColWidths[3] := 13;
  FColWidths[4] :=  7;
  FColWidths[5] :=  7;
  FColWidths[6] :=  7;
  FColWidths[7] :=  7;
  FColWidths[8] :=  7;
  FColWidths[9] := 30;
  ActionGridResize(nil);

  ActionGridHeaderSized(nil, true, 0);

  OtherActionGrid.Constraints.MinWidth := OtherActionGrid.ColCount * MinGridColSize;
  OtherActionGrid.Cells[0,0] := dlgMouseOptHeadOrder;
  OtherActionGrid.Cells[1,0] := dlgMouseOptHeadContext;
  OtherActionGrid.Cells[2,0] := dlgMouseOptHeadDesc;
  OtherActionGrid.Cells[3,0] := dlgMouseOptHeadBtn;
  OtherActionGrid.Cells[4,0] := dlgMouseOptHeadCount;
  OtherActionGrid.Cells[5,0] := dlgMouseOptHeadDir;
  OtherActionGrid.Cells[6,0] := dlgMouseOptHeadShift;
  OtherActionGrid.Cells[7,0] := dlgMouseOptHeadAlt;
  OtherActionGrid.Cells[8,0] := dlgMouseOptHeadCtrl;
  OtherActionGrid.Cells[9,0] := dlgMouseOptHeadPriority;
  OtherActionGrid.Cells[10,0] := dlgMouseOptHeadCaret;
  OtherActionGrid.Cells[11,0] := dlgMouseOptHeadOpt;
  SetLength(FOtherActColWidths, OtherActionGrid.ColCount);
  FOtherActColWidths[0] :=  7;
  FOtherActColWidths[1] := 20;
  FOtherActColWidths[2] := 40;
  FOtherActColWidths[3] := 15;
  FOtherActColWidths[4] := 15;
  FOtherActColWidths[5] := 13;
  FOtherActColWidths[6] :=  7;
  FOtherActColWidths[7] :=  7;
  FOtherActColWidths[8] :=  7;
  FOtherActColWidths[9] :=  7;
  FOtherActColWidths[10] :=  7;
  FOtherActColWidths[11] := 30;
  OtherActionGridResize(nil);

  MMoveName[false] := dlgMouseOptMoveMouseFalse;
  MMoveName[true] := dlgMouseOptMoveMouseTrue;

  FSort[1] := 1; // Button
  FSort[2] := 2; // CCount
  FSort[3] := 3; // Cdir
  FSort[4] := 8; // Priority

  FOtherSort[1] := 0; // SearchOrder
  FOtherSort[2] := 1; // Context
  FOtherSort[3] := 4; // CCount
  FOtherSort[4] := 5; // CDir

  BtnImport.Caption := lisImport;
  BtnExport.Caption := lisExport;
  UpdateButton.Caption := lisChange;
  AddNewButton.Caption := lisAdd;
  DelButton.Caption := lisDelete;
  OtherActionLabel.Caption := dlgMouseOptOtherAct;
  OtherActionLabel.Hint := dlgMouseOptOtherActHint;
  OtherActToggleBox.Caption := dlgMouseOptOtherActToggle;

  ToolBar1.Images := IDEImages.Images_16;
  BtnImport.ImageIndex := IDEImages.LoadImage('laz_open');
  BtnExport.ImageIndex := IDEImages.LoadImage('laz_save');
  UpdateButton.ImageIndex := IDEImages.LoadImage('laz_edit');
  AddNewButton.ImageIndex := IDEImages.LoadImage('laz_add');
  DelButton.ImageIndex := IDEImages.LoadImage('laz_delete');

  OpenDialog1.Title := lisImport;
  SaveDialog1.Title := lisExport;

  OpenDialog1.Filter := Format('%s|*.xml|%s|%s|', [dlgFilterXML, dlgFilterAll, GetAllFilesMask]);
  SaveDialog1.Filter := OpenDialog1.Filter;
end;

procedure TEditorMouseOptionsAdvFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  FTempMouseSettings := TEditorOptions(AOptions).TempMouseSettings;
  FTempMouseSettings.Assign(TEditorOptions(AOptions).UserMouseSettings);

  with AOptions as TEditorOptions do
  begin
    FKeyMap := KeyMap;
  end;
  ContextTree.Selected := FGlobalNode;
  ContextTreeChange(ContextTree, FGlobalNode);
end;

procedure TEditorMouseOptionsAdvFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  TEditorOptions(AOptions).UserMouseSettings.Assign(FTempMouseSettings);
end;

procedure TEditorMouseOptionsAdvFrame.RefreshSettings;
begin
  if (FGlobalNode = nil) or (FKeyMap = nil) then exit;
  ContextTree.Selected := FGlobalNode;
  ContextTreeChange(ContextTree, FGlobalNode);
end;

class function TEditorMouseOptionsAdvFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorMouseOptionsAdvFrame,
                           EdtOptionsMouseAdv, EdtOptionsMouse);
end.

