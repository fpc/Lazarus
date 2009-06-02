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
  SynEditMouseCmds, editor_mouseaction_options_dlg;
//  SynGutterCodeFolding;

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
    procedure ContextTreeChange(Sender: TObject; Node: TTreeNode);
    procedure AddNewButtonClick(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
    procedure DelButtonClick(Sender: TObject);
    procedure ActionGridHeaderSized(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure ActionGridResize(Sender: TObject);
  private
    FMainNode, FSelNode: TTreeNode;
    FCurNode: TTreeNode;
    FMainActions, FSelActions: TSynEditMouseActions;
    FCurActions: TSynEditMouseActions;
    ChangeDlg: TEditorMouseOptionsChangeDialog;
  protected
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

{ TEditorMouseOptionsFrame }

procedure TEditorMouseOptionsFrame.ContextTreeChange(Sender: TObject; Node: TTreeNode);
const
  Boolname: Array [Boolean] of String = ('', 'Y');
var
  act: TSynEditMouseAction;
  i: Integer;
  function ShiftName(ss: TShiftStateEnum): String;
  begin
    if not(ss in act.ShiftMask) then exit('-');
    if ss in act.Shift then exit('Y');
    exit('n');
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
    ActionGrid.Cells[7, i] := Boolname[act.MoveCaret];
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
  i, j, k: Integer;
begin
  j := 0;
  for i := 0 to ActionGrid.ColCount-1 do j := j + ActionGrid.ColWidths[i];
  k := ActionGrid.ClientWidth;
  for i := 0 to ActionGrid.ColCount-1 do
    ActionGrid.ColWidths[i] := ActionGrid.ColWidths[i] * k div j;
end;

procedure TEditorMouseOptionsFrame.ActionGridHeaderSized(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
begin
  ActionGridResize(nil);
end;

constructor TEditorMouseOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMainActions := TSynEditMouseActions.Create(nil);
  FSelActions := TSynEditMouseActions.Create(nil);
  ChangeDlg := TEditorMouseOptionsChangeDialog.Create(self);
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
var
  i: Integer;
  CName: String;
  mb: TMouseButton;
  cc: TSynMAClickCount;
begin
  ContextTree.Items.Clear;
  FMainNode := ContextTree.Items.Add(nil, dlgMouseOptNodeMain);
  FMainNode.Data := FMainActions;
  FSelNode := ContextTree.Items.AddChild(FMainNode, dlgMouseOptNodeSelect);
  FSelNode.Data := FSelActions;
  ActionGrid.Cells[0,0] := dlgMouseOptHeadDesc;
  ActionGrid.Cells[1,0] := dlgMouseOptHeadBtn;
  ActionGrid.Cells[2,0] := dlgMouseOptHeadCount;
  ActionGrid.Cells[3,0] := dlgMouseOptHeadDir;
  ActionGrid.Cells[4,0] := dlgMouseOptHeadShift;
  ActionGrid.Cells[5,0] := dlgMouseOptHeadAlt;
  ActionGrid.Cells[6,0] := dlgMouseOptHeadCtrl;
  ActionGrid.Cells[7,0] := dlgMouseOptHeadCaret;
  ActionGrid.ColWidths[0] := 100;
  ActionGridResize(nil);

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

