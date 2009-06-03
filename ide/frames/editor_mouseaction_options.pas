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
  SynEditMouseCmds, MouseActionDialog, math;

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
    procedure ActionGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure ActionGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ActionGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure ContextTreeChange(Sender: TObject; Node: TTreeNode);
    procedure AddNewButtonClick(Sender: TObject);
    procedure Splitter1CanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure UpdateButtonClick(Sender: TObject);
    procedure DelButtonClick(Sender: TObject);
    procedure ActionGridHeaderSized(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure ActionGridResize(Sender: TObject);
  private
    FMainNode, FSelNode: TTreeNode;
    FCurNode: TTreeNode;
    FMainActions, FSelActions: TSynEditMouseActions;
    FCurActions: TSynEditMouseActions;
    ChangeDlg: TMouseaActionDialog;
    FColWidths: Array of Integer;
    FLastWidth: Integer;
    FIsHeaderSizing: Boolean;
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
  i: Integer;
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
  for i := 1 to FCurActions.Count do begin
    act := FCurActions[i-1];
    ActionGrid.Cells[0, i] := act.DisplayName;
    ActionGrid.Cells[1, i] := ButtonName[act.Button];
    ActionGrid.Cells[2, i] := ClickName[act.ClickCount];
    ActionGrid.Cells[3, i] := ButtonDirName[act.ClickDir];;
    ActionGrid.Cells[4, i] := ShiftName(ssShift);
    ActionGrid.Cells[5, i] := ShiftName(ssAlt);
    ActionGrid.Cells[6, i] := ShiftName(ssCtrl);
    ActionGrid.Cells[7, i] := MMoveName[act.MoveCaret];
  end;
  ActionGrid.Row := 1;
end;

procedure TEditorMouseOptionsFrame.AddNewButtonClick(Sender: TObject);
var
  MAct: TSynEditMouseAction;
begin
  if FCurActions = nil then exit;

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
      FCurActions.Delete(FCurActions.Count);
      MessageDlg(dlgMouseOptErrorDup, dlgMouseOptErrorDupText, mtError, [mbOk], 0);
    end;
    ContextTreeChange(nil, FCurNode);
    ActionGrid.Row := FCurActions.Count;
  end;
end;

procedure TEditorMouseOptionsFrame.Splitter1CanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
begin

end;

procedure TEditorMouseOptionsFrame.UpdateButtonClick(Sender: TObject);
var
  MAct, MOld: TSynEditMouseAction;
  r: LongInt;
begin
  if FCurActions = nil then exit;
  if (ActionGrid.Row-1 >= FCurActions.Count) or (ActionGrid.Row < 1) then exit;
  r := ActionGrid.Row;

  MAct := FCurActions[r-1];
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
    ActionGrid.Row := r;
  end;
end;

procedure TEditorMouseOptionsFrame.DelButtonClick(Sender: TObject);
begin
  if FCurActions = nil then exit;
  if (ActionGrid.Row-1 >= FCurActions.Count) or (ActionGrid.Row < 1) then exit;
  FCurActions.Delete(ActionGrid.row-1);
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
  ChangeDlg := TMouseaActionDialog.Create(self);
end;

destructor TEditorMouseOptionsFrame.Destroy;
begin
  FMainActions.Free;
  FSelActions.Free;
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
  FSelNode := ContextTree.Items.AddChild(FMainNode, dlgMouseOptNodeSelect);
  FSelNode.Data := FSelActions;
  ActionGrid.Constraints.MinWidth := ActionGrid.ColCount * MinGridColSize;
  Splitter1.MinSize := ActionGrid.ColCount * MinGridColSize;
  ActionGrid.Cells[0,0] := dlgMouseOptHeadDesc;
  ActionGrid.Cells[1,0] := dlgMouseOptHeadBtn;
  ActionGrid.Cells[2,0] := dlgMouseOptHeadCount;
  ActionGrid.Cells[3,0] := dlgMouseOptHeadDir;
  ActionGrid.Cells[4,0] := dlgMouseOptHeadShift;
  ActionGrid.Cells[5,0] := dlgMouseOptHeadAlt;
  ActionGrid.Cells[6,0] := dlgMouseOptHeadCtrl;
  ActionGrid.Cells[7,0] := dlgMouseOptHeadCaret;
  ActionGrid.ColWidths[0] := 100;
  ActionGridHeaderSized(nil, true, 0);

  DelButton.Caption := dlgMouseOptBtnDel;
  UpdateButton.Caption := dlgMouseOptBtnUdp;
  AddNewButton.Caption := dlgMouseOptBtnAdd;
end;

procedure TEditorMouseOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEditorOptions do
  begin
    TSynEditMouseActions(FMainNode.Data).Assign(MouseMap);
    TSynEditMouseActions(FSelNode.Data).Assign(MouseSelMap);
  end;
  ContextTree.Selected := FMainNode;
end;

procedure TEditorMouseOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEditorOptions do
  begin
    MouseMap.Assign(TSynEditMouseActions(FMainNode.Data));
    MouseSelMap.Assign(TSynEditMouseActions(FSelNode.Data));
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

