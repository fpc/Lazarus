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
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
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
    FIDEColor: TColor;
    FProjectColor: TColor;
    FSessionColor: TColor;
    procedure MoveRow(Direction: integer);
    procedure UpdateButtons;
    function AddTarget(StorageGroup: TGroupedMatrixGroup): TGroupedMatrixGroup;
  public
    constructor Create(TheOwner: TComponent); override;
    property Grid: TGroupedMatrixControl read FGrid;
    property IDEColor: TColor read FIDEColor write FIDEColor;
    property ProjectColor: TColor read FProjectColor write FProjectColor;
    property SessionColor: TColor read FSessionColor write FSessionColor;
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
  //DebugLn(['TFrame1.GridEditingDone ']);
  UpdateButtons;
end;

procedure TFrame1.BMMRedoToolButtonClick(Sender: TObject);
begin
  Grid.Redo;
  UpdateButtons;
end;

procedure TFrame1.BMMMoveUpToolButtonClick(Sender: TObject);
begin
  MoveRow(-1);
end;

procedure TFrame1.BMMNewOptionToolButtonClick(Sender: TObject);
var
  aRow: Integer;
  MatRow: TGroupedMatrixRow;
  Group: TGroupedMatrixGroup;
  NewRow: TGroupedMatrixValue;

  procedure CreateOption;
  begin
    NewRow:=Grid.Matrix.AddValue(Group,Grid.Modes[Grid.ActiveMode].Caption,Grid.TypeColumn.PickList.Names[0],'');
  end;

begin
  aRow:=Grid.Row;
  if aRow<Grid.FixedRows then aRow:=Grid.FixedRows;
  NewRow:=nil;
  Grid.MatrixChanging;
  try
    Grid.StoreUndo;
    MatRow:=Grid.Matrix[aRow-1];
    if MatRow is TGroupedMatrixGroup then begin
      Group:=TGroupedMatrixGroup(MatRow);
      if Group.Group=nil then begin
        if Group.Count=0 then begin
          // storage group without target => add a target
          Group:=AddTarget(Group);
        end;
      end;
      // add option as first item of Group
      CreateOption;
    end else begin
      // add behind current value
      Group:=MatRow.Group;
      CreateOption;
      Group.Move(Group.Count-1,MatRow.GetGroupIndex+1);
    end;
    Grid.Matrix.RebuildRows;
  finally
    Grid.MatrixChanged;
  end;
  if NewRow<>nil then
    Grid.Row:=Grid.Matrix.IndexOfRow(NewRow)+1;
  UpdateButtons;
end;

procedure TFrame1.BMMNewTargetToolButtonClick(Sender: TObject);
var
  aRow: Integer;
  MatRow: TGroupedMatrixRow;
  Group: TGroupedMatrixGroup;
  NewRow: TGroupedMatrixGroup;
begin
  aRow:=Grid.Row;
  if aRow<Grid.FixedRows then aRow:=Grid.FixedRows;
  NewRow:=nil;
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
      NewRow:=AddTarget(Group);
      Group.Move(Group.Count-1,0);
    end else begin
      // Group is a target
      // => add target behind current target
      NewRow:=AddTarget(Group.Group);
      Group.Group.Move(Group.Group.Count-1,Group.GetGroupIndex+1);
    end;
    Grid.Matrix.RebuildRows;
  finally
    Grid.MatrixChanged;
  end;
  if NewRow<>nil then
    Grid.Row:=Grid.Matrix.IndexOfRow(NewRow)+1;
  UpdateButtons;
end;

procedure TFrame1.BMMMoveDownToolButtonClick(Sender: TObject);
begin
  MoveRow(1);
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

function TFrame1.AddTarget(StorageGroup: TGroupedMatrixGroup
  ): TGroupedMatrixGroup;
begin
  Result:=Grid.Matrix.AddGroup(StorageGroup,'Targets: ');
  Result.Value:='*';
  Result.Writable:=true;
end;

procedure TFrame1.MoveRow(Direction: integer);
var
  MatRow: TGroupedMatrixRow;
  aRow: Integer;
  TargetGroup: TGroupedMatrixGroup;
  i: Integer;
  TargetStorage: TGroupedMatrixGroup;
begin
  aRow:=Grid.Row;
  if aRow<1 then exit;
  MatRow:=Grid.Matrix[aRow-1];
  if MatRow.Group=nil then begin
    // storage groups can not be moved
    debugln(['TFrame1.MoveRow storage groups can not be moved']);
    exit;
  end;
  Grid.MatrixChanging;
  i:=MatRow.GetGroupIndex;
  if Direction<0 then begin
    if i>0 then begin
      // move up in group
      Grid.StoreUndo;
      MatRow.Group.Move(i,i-1);
    end else begin
      // move to previous group
      TargetGroup:=TGroupedMatrixGroup(MatRow.Group.GetPreviousSibling);
      if TargetGroup=nil then begin
        if MatRow is TGroupedMatrixValue then begin
          // move value to last target of previous storage
          if (MatRow.Group.Group=nil) then begin
            debugln(['TFrame1.MoveRow value has no storage+target']);
            exit;
          end;
          TargetStorage:=TGroupedMatrixGroup(MatRow.Group.Group.GetPreviousSibling);
          if TargetStorage=nil then begin
            debugln(['TFrame1.MoveRow no previous storage for value']);
            exit;
          end;
          if TargetStorage.Count>0 then begin
            TargetGroup:=TargetStorage[TargetStorage.Count-1] as TGroupedMatrixGroup;
          end else begin
            // add first target
            TargetGroup:=AddTarget(TargetStorage);
          end;
        end else begin
          // this is already the first target of the first storage
          debugln(['TFrame1.MoveRow no previous storage for target']);
          exit;
        end;
      end;
      // move MatRow to TargetGroup as last
      Grid.StoreUndo;
      MatRow.Group:=TargetGroup;
    end;
  end else begin
    if i+1<MatRow.Group.Count then begin
      // move down in group
      Grid.StoreUndo;
      MatRow.Group.Move(i,i+1);
    end else begin
      // move to next group
      TargetGroup:=TGroupedMatrixGroup(MatRow.Group.GetNextSibling);
      if TargetGroup=nil then begin
        if MatRow is TGroupedMatrixValue then begin
          // move value to first target of next storage
          if (MatRow.Group.Group=nil) then begin
            debugln(['TFrame1.MoveRow value has no storage+target']);
            exit;
          end;
          TargetStorage:=TGroupedMatrixGroup(MatRow.Group.Group.GetNextSibling);
          if TargetStorage=nil then begin
            debugln(['TFrame1.MoveRow no next storage for value']);
            exit;
          end;
          if TargetStorage.Count>0 then begin
            TargetGroup:=TargetStorage[0] as TGroupedMatrixGroup;
          end else begin
            // add first target
            TargetGroup:=AddTarget(TargetStorage);
          end;
        end else begin
          // this is already the last target of the last storage
          debugln(['TFrame1.MoveRow no next storage for target']);
          exit;
        end;
      end;
      // move MatRow to TargetGroup as first
      Grid.StoreUndo;
      MatRow.Group:=TargetGroup;
      TargetGroup.Move(TargetGroup.Count-1,0);
    end;
  end;
  Grid.Matrix.RebuildRows;
  Grid.MatrixChanged;
  Grid.Row:=Grid.Matrix.IndexOfRow(MatRow)+1;
  UpdateButtons;
end;

constructor TFrame1.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  IDEColor:=RGBToColor(200,255,255);
  ProjectColor:=RGBToColor(255,255,255);
  SessionColor:=RGBToColor(255,255,200);

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

