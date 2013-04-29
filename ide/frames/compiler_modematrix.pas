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
unit Compiler_ModeMatrix;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazLogger, LResources, Forms, Controls,
  Graphics, ComCtrls, ModeMatrixCtrl;

type

  { TFrame1 }

  TFrame1 = class(TFrame)
    BMMatrixToolBar: TToolBar;
    BMMMoveUpToolButton: TToolButton;
    BMMMoveDownToolButton: TToolButton;
    BMMUndoToolButton: TToolButton;
    BMMRedoToolButton: TToolButton;
    BMMNewTargetToolButton: TToolButton;
    BMMNewOptionToolButton: TToolButton;
    BMMDeleteToolButton: TToolButton;
    procedure BMMDeleteToolButtonClick(Sender: TObject);
    procedure BMMMoveDownToolButtonClick(Sender: TObject);
    procedure BMMMoveUpToolButtonClick(Sender: TObject);
    procedure BMMNewOptionToolButtonClick(Sender: TObject);
    procedure BMMNewTargetToolButtonClick(Sender: TObject);
    procedure BMMRedoToolButtonClick(Sender: TObject);
    procedure BMMUndoToolButtonClick(Sender: TObject);
    procedure GridEditingDone(Sender: TObject);
    procedure GridSelection(Sender: TObject; {%H-}aCol, {%H-}aRow: Integer);
  private
    FGrid: TGroupedMatrixControl;
    procedure UpdateButtons;
  public
    constructor Create(TheOwner: TComponent); override;
    property Grid: TGroupedMatrixControl read FGrid;
  end;

implementation

{$R *.lfm}

{ TFrame1 }

procedure TFrame1.GridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  UpdateButtons;
end;

procedure TFrame1.BMMUndoToolButtonClick(Sender: TObject);
begin
  Grid.Undo;
  UpdateButtons;
end;

procedure TFrame1.GridEditingDone(Sender: TObject);
begin
  DebugLn(['TFrame1.GridEditingDone ']);
  UpdateButtons;
end;

procedure TFrame1.BMMRedoToolButtonClick(Sender: TObject);
begin
  Grid.Redo;
  UpdateButtons;
end;

procedure TFrame1.BMMMoveUpToolButtonClick(Sender: TObject);
begin

end;

procedure TFrame1.BMMNewOptionToolButtonClick(Sender: TObject);
begin

end;

procedure TFrame1.BMMNewTargetToolButtonClick(Sender: TObject);
var
  aRow: Integer;
  MatRow: TGroupedMatrixRow;
  Group: TGroupedMatrixGroup;
begin
  aRow:=Grid.Row;
  if aRow<Grid.FixedRows then aRow:=Grid.FixedRows;
  Grid.MatrixChanging;
  try
    Grid.StoreUndo;
    MatRow:=Grid.Matrix[aRow-1];
    if MatRow is TGroupedMatrixGroup then
      Group:=TGroupedMatrixGroup(MatRow)
    else
      Group:=MatRow.Group;
    if Group.Group=nil then begin
      // Group is a storage group
      // => add as first target of storage group
      Grid.Matrix.AddGroup(Group,'Target: *');
      Group.Move(Group.Count-1,0);
    end else begin
      // Group is a target
      // => add target behind current target
      Grid.Matrix.AddGroup(Group.Group,'Target: *');
      Group.Group.Move(Group.Group.Count-1,Group.GetGroupIndex+1);
    end;
    Grid.Matrix.RebuildRows;
  finally
    Grid.MatrixChanged;
  end;
  UpdateButtons;
end;

procedure TFrame1.BMMMoveDownToolButtonClick(Sender: TObject);
begin

end;

procedure TFrame1.BMMDeleteToolButtonClick(Sender: TObject);
var
  aRow: Integer;
  MatRow: TGroupedMatrixRow;
begin
  aRow:=Grid.Row;
  if aRow<1 then exit;
  MatRow:=Grid.Matrix[aRow-1];
  if MatRow.Group=nil then begin
    // storage groups can not be deleted
    exit;
  end;
  Grid.DeleteMatrixRow(aRow);
  UpdateButtons;
end;

procedure TFrame1.UpdateButtons;
var
  aRow: Integer;
  MatRow: TGroupedMatrixRow;
begin
  aRow:=Grid.Row;
  if (aRow>0) and (aRow<=Grid.Matrix.RowCount) then begin
    MatRow:=Grid.Matrix[aRow-1];
  end else
    MatRow:=nil;

  // allow to delete targets and value rows
  BMMDeleteToolButton.Enabled:=(MatRow<>nil) and (MatRow.Group<>nil);
  //
  BMMUndoToolButton.Enabled:=Grid.CanUndo;
  BMMRedoToolButton.Enabled:=Grid.CanRedo;
  // move up/down
  BMMMoveUpToolButton.Enabled:=(MatRow<>nil) and (MatRow.Group<>nil)
                        and ((MatRow.GetPreviousSibling<>nil)
                           or (MatRow.GetTopLvlItem.GetPreviousSibling<>nil));
  BMMMoveDownToolButton.Enabled:=(MatRow<>nil) and (MatRow.Group<>nil)
                        and  (MatRow.GetNextSkipChildren<>nil);
end;

constructor TFrame1.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FGrid:=TGroupedMatrixControl.Create(Self);
  with Grid do begin
    Name:='TModeMatrixControl';
    Align:=alClient;
    Parent:=Self;
    OnSelection:=@GridSelection;
    OnEditingDone:=@GridEditingDone;
  end;

  UpdateButtons;
end;

end.

