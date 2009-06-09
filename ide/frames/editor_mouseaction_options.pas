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
}
unit editor_mouseaction_options;

{$mode objfpc}{$H+}

interface

uses
  LResources, EditorOptions, LazarusIDEStrConsts, IDEOptionsIntf, sysutils,
  StdCtrls, ExtCtrls, Classes, Controls, LCLProc, Grids, ComCtrls, Dialogs,
  SynEditMouseCmds, MouseActionDialog, math, KeyMapping;

type

  { TEditorMouseOptionsFrame }

  TEditorMouseOptionsFrame = class(TAbstractIDEOptionsEditor)
    DelButton: TButton;
    Splitter1: TSplitter;
    UpdateButton: TButton;
    AddNewButton: TButton;
    p2: TPanel;
    ActionGrid: TStringGrid;
    ContextTree: TTreeView;
    p3: TPanel;
    procedure ActionGridCompareCells(Sender: TObject; ACol, ARow, BCol, BRow: Integer;
      var Result: integer);
    procedure ActionGridHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure ActionGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure ActionGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ActionGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure ContextTreeChange(Sender: TObject; Node: TTreeNode);
    procedure AddNewButtonClick(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
    procedure DelButtonClick(Sender: TObject);
    procedure ActionGridHeaderSized(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure ActionGridResize(Sender: TObject);
  private
    FKeyMap: TKeyCommandRelationList;

    FMainNode, FSelNode: TTreeNode;
    FGutterNode: TTreeNode;
    FGutterFoldNode, FGutterFoldExpNode, FGutterFoldColNode: TTreeNode;
    FGutterLinesNode: TTreeNode;
    FCurNode: TTreeNode;

    FMainActions, FSelActions: TSynEditMouseActions;
    FGutterActions: TSynEditMouseActions;
    FGutterActionsFold, FGutterActionsFoldExp, FGutterActionsFoldCol: TSynEditMouseActions;
    FGutterActionsLines: TSynEditMouseActions;
    FCurActions: TSynEditMouseActions;

    FSort1, FSort2, FSort3, FSort4: Integer;
    ChangeDlg: TMouseaActionDialog;
    FColWidths: Array of Integer;
    FLastWidth: Integer;
    FIsHeaderSizing: Boolean;
    procedure SortGrid;
    procedure SelectRow(AAct: TSynEditMouseAction);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

const
  MinGridColSize = 25;
{ TEditorMouseOptionsFrame }

procedure TEditorMouseOptionsFrame.ContextTreeChange(Sender: TObject; Node: TTreeNode);
const
  MMoveName: Array [Boolean] of String = (dlgMouseOptMoveMouseFalse, dlgMouseOptMoveMouseTrue);
var
  act: TSynEditMouseAction;
  i, j: Integer;
  optlist: TStringList;
  function ShiftName(ss: TShiftStateEnum): String;
  begin
    if not(ss in act.ShiftMask) then exit(dlgMouseOptModKeyIgnore);
    if ss in act.Shift then exit(dlgMouseOptModKeyTrue);
    exit(dlgMouseOptModKeyFalse);
  end;

begin
  if Node = nil then exit;
  FCurNode := Node;
  FCurActions := TSynEditMouseActions(Node.Data);
  ActionGrid.RowCount := FCurActions.Count + 1;
  optlist := TStringlist.Create;
  for i := 1 to FCurActions.Count do begin
    act := FCurActions[i-1];
    ActionGrid.Objects[0, i] := act;
    ActionGrid.Cells[0, i] := act.DisplayName;
    ActionGrid.Cells[1, i] := ButtonName[act.Button];
    ActionGrid.Cells[2, i] := ClickName[act.ClickCount];
    ActionGrid.Cells[3, i] := ButtonDirName[act.ClickDir];;
    ActionGrid.Cells[4, i] := ShiftName(ssShift);
    ActionGrid.Cells[5, i] := ShiftName(ssAlt);
    ActionGrid.Cells[6, i] := ShiftName(ssCtrl);
    ActionGrid.Cells[7, i] := IntToStr(act.Priority);
    ActionGrid.Cells[8, i] := MMoveName[act.MoveCaret];
    ActionGrid.Cells[9, i] := '';
    if act.Command =  emcSynEditCommand then begin
      j := KeyMapIndexOfCommand(FKeyMap, Act.Option);
      if (j >= 0) and (j < FKeyMap.RelationCount) then
        ActionGrid.Cells[9, i] := FKeyMap.Relations[j].GetLocalizedName;
    end
    else begin
      optlist.CommaText := MouseCommandConfigName(act.Command);
      if act.Option < optlist.Count-1 then
        ActionGrid.Cells[9, i] := optlist[act.Option+1] +' ('+optlist[0]+')';
    end;
  end;
  optlist.Free;
  ActionGrid.Row := 1;
  SortGrid;
end;

procedure TEditorMouseOptionsFrame.ActionGridHeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
begin
  If Index <> FSort1 then begin
    if FSort3 <> index then
      Fsort4 := FSort3;
    if FSort2 <> index then
      Fsort3 := FSort2;
    Fsort2 := FSort1;
    Fsort1 := Index;
  end;
  SortGrid;
end;

procedure TEditorMouseOptionsFrame.ActionGridCompareCells(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);
  function CompareCol(i : Integer) : Integer;
  begin
    case i of
      2: // ClickCount
        Result := ord(TSynEditMouseAction(ActionGrid.Objects[0, ARow]).ClickCount)
                - ord(TSynEditMouseAction(ActionGrid.Objects[0, BRow]).ClickCount);
      3: // ClickDir (down first)
        Result := ord(TSynEditMouseAction(ActionGrid.Objects[0, BRow]).ClickDir)
                - ord(TSynEditMouseAction(ActionGrid.Objects[0, ARow]).ClickDir);
      else
        Result := AnsiCompareText(ActionGrid.Cells[i, ARow], ActionGrid.Cells[i, BRow]);
    end;
  end;
begin
  Result := CompareCol(FSort1);
  if Result = 0 then
    Result := CompareCol(FSort2);
  if Result = 0 then
    Result := CompareCol(FSort3);
  if Result = 0 then
    Result := CompareCol(FSort4);
  if Result = 0 then
    Result := CompareCol(7); // Priority
  if Result = 0 then
    Result := TSynEditMouseAction(ActionGrid.Objects[0, ARow]).ID
            - TSynEditMouseAction(ActionGrid.Objects[0, BRow]).ID;
end;

procedure TEditorMouseOptionsFrame.SortGrid;
begin
  ActionGrid.SortColRow(True, 0);
end;

procedure TEditorMouseOptionsFrame.SelectRow(AAct: TSynEditMouseAction);
var
  i: Integer;
begin
  For i := 1 to ActionGrid.RowCount -1 do
    if ActionGrid.Objects[0, i] = AAct then begin
      ActionGrid.Row := i;
      break;
    end;
end;


procedure TEditorMouseOptionsFrame.AddNewButtonClick(Sender: TObject);
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
      MessageDlg(dlgMouseOptErrorDup, dlgMouseOptErrorDupText, mtError, [mbOk], 0);
    end;
    ContextTreeChange(nil, FCurNode);
    SelectRow(MAct);
  end;
end;

procedure TEditorMouseOptionsFrame.UpdateButtonClick(Sender: TObject);
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
      MessageDlg(dlgMouseOptErrorDup, dlgMouseOptErrorDupText, mtError, [mbOk], 0);
      MAct.Assign(MOld);
    end;
    MOld.Free;
    ContextTreeChange(nil, FCurNode);
    SelectRow(MAct);
  end;
end;

procedure TEditorMouseOptionsFrame.DelButtonClick(Sender: TObject);
begin
  if FCurActions = nil then exit;
  if (ActionGrid.Row-1 >= FCurActions.Count) or (ActionGrid.Row < 1) then exit;
  FCurActions.Delete(FCurActions.IndexOf(TSynEditMouseAction
                                      (ActionGrid.Objects[0, ActionGrid.row])));
  ActionGrid.Row := 1;
  ContextTreeChange(nil, FCurNode);
end;

procedure TEditorMouseOptionsFrame.ActionGridResize(Sender: TObject);
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

procedure TEditorMouseOptionsFrame.ActionGridHeaderSized(Sender: TObject;
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

procedure TEditorMouseOptionsFrame.ActionGridMouseMove(Sender: TObject;
  Shift: TShiftState; X,
  Y: Integer);
begin
  if not FIsHeaderSizing then exit;
  ActionGridHeaderSized(nil, true, 0);
end;

procedure TEditorMouseOptionsFrame.ActionGridMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FIsHeaderSizing := False;
end;

procedure TEditorMouseOptionsFrame.ActionGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FIsHeaderSizing := y <= ActionGrid.RowHeights[0];
end;

constructor TEditorMouseOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMainActions := TSynEditMouseActions.Create(nil);
  FSelActions := TSynEditMouseActions.Create(nil);
  FGutterActions := TSynEditMouseActions.Create(nil);
  FGutterActionsFold := TSynEditMouseActions.Create(nil);
  FGutterActionsFoldExp := TSynEditMouseActions.Create(nil);
  FGutterActionsFoldCol := TSynEditMouseActions.Create(nil);
  FGutterActionsLines := TSynEditMouseActions.Create(nil);
  ChangeDlg := TMouseaActionDialog.Create(self);
end;

destructor TEditorMouseOptionsFrame.Destroy;
begin
  FMainActions.Free;
  FSelActions.Free;
  FGutterActions.Free;
  FGutterActionsFold.Free;
  FGutterActionsFoldExp.Free;
  FGutterActionsFoldCol.Free;
  FGutterActionsLines.Free;
  ChangeDlg.Free;
  inherited Destroy;
end;

function TEditorMouseOptionsFrame.GetTitle: String;
begin
  Result := dlgMouseOptions;
end;

procedure TEditorMouseOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  ContextTree.Items.Clear;
  FMainNode := ContextTree.Items.Add(nil, dlgMouseOptNodeMain);
  FMainNode.Data := FMainActions;
  // Selection
  FSelNode := ContextTree.Items.AddChild(FMainNode, dlgMouseOptNodeSelect);
  FSelNode.Data := FSelActions;
  // Gutter
  FGutterNode := ContextTree.Items.AddChild(nil, dlgMouseOptNodeGutter);
  FGutterNode.Data := FGutterActions;
  // Gutter Fold
  FGutterFoldNode := ContextTree.Items.AddChild(FGutterNode, dlgMouseOptNodeGutterFold);
  FGutterFoldNode.Data := FGutterActionsFold;
  FGutterFoldColNode := ContextTree.Items.AddChild(FGutterFoldNode, dlgMouseOptNodeGutterFoldCol);
  FGutterFoldColNode.Data := FGutterActionsFoldCol;
  FGutterFoldExpNode := ContextTree.Items.AddChild(FGutterFoldNode, dlgMouseOptNodeGutterFoldExp);
  FGutterFoldExpNode.Data := FGutterActionsFoldExp;
  // LineNum
  FGutterLinesNode := ContextTree.Items.AddChild(FGutterNode, dlgMouseOptNodeGutterLines);
  FGutterLinesNode.Data := FGutterActionsLines;

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
  ActionGrid.ColWidths[0] := ActionGrid.ColWidths[0] * 3;
  ActionGrid.ColWidths[9] := ActionGrid.ColWidths[8] * 3;
  ActionGridHeaderSized(nil, true, 0);

  FSort1 := 1; // Button
  FSort2 := 2; // CCount
  FSort3 := 3; // Cdir
  FSort4 := 8; // Priority

  DelButton.Caption := dlgMouseOptBtnDel;
  UpdateButton.Caption := dlgMouseOptBtnUdp;
  AddNewButton.Caption := dlgMouseOptBtnAdd;
end;

procedure TEditorMouseOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEditorOptions do
  begin
    FMainActions.Assign(MouseMap);
    FSelActions.Assign(MouseSelMap);
    FGutterActions.Assign(MouseGutterActions);
    FGutterActionsFold.Assign(MouseGutterActionsFold);
    FGutterActionsFoldExp.Assign(MouseGutterActionsFoldExp);
    FGutterActionsFoldCol.Assign(MouseGutterActionsFoldCol);
    FGutterActionsLines.Assign(MouseGutterActionsLines);

    FKeyMap := KeyMap;
  end;
  ContextTree.Selected := FMainNode;
end;

procedure TEditorMouseOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEditorOptions do
  begin
    MouseMap.Assign(FMainActions);
    MouseSelMap.Assign(FSelActions);
    MouseGutterActions.Assign(FGutterActions);
    MouseGutterActionsFold.Assign(FGutterActionsFold);
    MouseGutterActionsFoldExp.Assign(FGutterActionsFoldExp);
    MouseGutterActionsFoldCol.Assign(FGutterActionsFoldCol);
    MouseGutterActionsLines.Assign(FGutterActionsLines);
  end;
end;

class function TEditorMouseOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

initialization
  {$I editor_mouseaction_options.lrs}
  RegisterIDEOptionsEditor(GroupEditor, TEditorMouseOptionsFrame, EdtOptionsMouse);
end.

