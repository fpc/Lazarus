{
 /***************************************************************************
                                   dbactns.pp
                                   ----------
 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
{$mode objfpc}{$h+}
unit DBActns;

interface

uses Classes, DB, ActnList;

type

{ ---------------------------------------------------------------------
    TDatasetAction - Parent for all other TDataset actions.
  ---------------------------------------------------------------------}

  TDataSetAction = Class(TAction)
  Private
    FDataSource: TDataSource;
    Procedure SetDataSource(Value: TDataSource);
  Protected
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Function GetDataSet(Target: TObject): TDataSet; virtual;
  Public
    Function HandlesTarget(Target: TObject): Boolean; override;
    property DataSource: TDataSource read FDataSource write SetDataSource;
  end;

{ ---------------------------------------------------------------------
    Navigation Actions 
  ---------------------------------------------------------------------}
  
  TDataSetFirst = Class(TDataSetAction)
  Public
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  Published
    property DataSource;
  end;

  TDataSetLast = Class(TDataSetAction)
  Public
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  Published
    property DataSource;
  end;

  TDataSetNext = Class(TDataSetAction)
  Public
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  Published
    property DataSource;
  end;

  TDataSetPrior = Class(TDataSetAction)
  Public
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  Published
    property DataSource;
  end;

  TDataSetRefresh = Class(TDataSetAction)
  Public
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  Published
    property DataSource;
  end;


{ ---------------------------------------------------------------------
    Data manipulation actions
  ---------------------------------------------------------------------}

  TDataSetCancel = Class(TDataSetAction)
  Public
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  Published
    property DataSource;
  end;

  TDataSetDelete = Class(TDataSetAction)
  Public
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  Published
    property DataSource;
  end;

  TDataSetEdit = Class(TDataSetAction)
  Public
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  Published
    property DataSource;
  end;

  TDataSetInsert = Class(TDataSetAction)
  Public
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  Published
    property DataSource;
  end;

  TDataSetPost = Class(TDataSetAction)
  Public
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  Published
    property DataSource;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterNoIcon([TDataSetFirst,TDataSetLast,TDataSetNext,
    TDataSetPrior,TDataSetRefresh,TDataSetCancel,TDataSetDelete,TDataSetEdit,
    TDataSetInsert,TDataSetPost]);
end;


{ ---------------------------------------------------------------------
    TDatasetAction
  ---------------------------------------------------------------------}

Function TDataSetAction.GetDataSet(Target: TObject): TDataSet;
begin
  Result:=(Target as TDataSource).DataSet;
end;

Function TDataSetAction.HandlesTarget(Target: TObject): Boolean;

begin
  Result:=(DataSource<>Nil);
  if Result then
    Result:=(DataSource.DataSet<>Nil)
  else
    Result:=(Target is TDataSource) and (TDataSource(Target).DataSet<>Nil);
end;


Procedure TDataSetAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent,Operation);
  if (Operation=opRemove) and 
     (AComponent=DataSource) then 
    FDataSource:=Nil;
end;


Procedure TDataSetAction.SetDataSource(Value: TDataSource);
begin
  if (Value<>FDataSource) then
    begin
    FDataSource:=Value;
    if (Value<>Nil) then 
      Value.FreeNotification(Self);
    end;
end;


{ ---------------------------------------------------------------------
    TDatasetFirst
  ---------------------------------------------------------------------}

Procedure TDataSetFirst.ExecuteTarget(Target: TObject);
begin
  GetDataSet(Target).First;
end;


Procedure TDataSetFirst.UpdateTarget(Target: TObject);
begin
  With GetDataSet(Target) do
    Enabled:=Active and not BOF;
end;


{ ---------------------------------------------------------------------
    TDataSetLast
  ---------------------------------------------------------------------}

Procedure TDataSetLast.ExecuteTarget(Target: TObject);
begin
  GetDataSet(Target).Last;
end;


Procedure TDataSetLast.UpdateTarget(Target: TObject);
begin
  with GetDataSet(Target) do
    Enabled:=Active and not EOF;
end;


{ ---------------------------------------------------------------------
    TDataSetNext
  ---------------------------------------------------------------------}

Procedure TDataSetNext.ExecuteTarget(Target: TObject);
begin
  GetDataSet(Target).Next;
end;


Procedure TDataSetNext.UpdateTarget(Target: TObject);
begin
  with GetDataSet(Target) do
    Enabled:=Active and not Eof;
end;


{ ---------------------------------------------------------------------
    TDataSetPrior
  ---------------------------------------------------------------------}

Procedure TDataSetPrior.ExecuteTarget(Target: TObject);
begin
  GetDataSet(Target).Prior;
end;


Procedure TDataSetPrior.UpdateTarget(Target: TObject);
begin
  with GetDataSet(Target) do
    Enabled:=Active and not BOF;
end;


{ ---------------------------------------------------------------------
    TDataSetRefresh
  ---------------------------------------------------------------------}

Procedure TDataSetRefresh.ExecuteTarget(Target: TObject);
begin
  GetDataSet(Target).Refresh;
end;


Procedure TDataSetRefresh.UpdateTarget(Target: TObject);
begin
  with GetDataSet(Target) do
    Enabled:=Active;
end;


{ ---------------------------------------------------------------------
    TDatasetInsert
  ---------------------------------------------------------------------}

Procedure TDataSetInsert.ExecuteTarget(Target: TObject);
begin
  GetDataSet(Target).Insert;
end;

Procedure TDataSetInsert.UpdateTarget(Target: TObject);
begin
  with GetDataSet(Target) do
    Enabled:=Active and CanModify and not (State in dsEditModes);
end;


{ ---------------------------------------------------------------------
    TDataSetPost
  ---------------------------------------------------------------------}

Procedure TDataSetPost.ExecuteTarget(Target: TObject);
begin
  GetDataSet(Target).Post;
end;

Procedure TDataSetPost.UpdateTarget(Target: TObject);
begin
  with GetDataSet(Target) do
    Enabled:=Active and (State in dsEditModes);
end;

{ ---------------------------------------------------------------------
    TDataSetCancel
  ---------------------------------------------------------------------}

Procedure TDataSetCancel.ExecuteTarget(Target: TObject);
begin
  GetDataSet(Target).Cancel;
end;


Procedure TDataSetCancel.UpdateTarget(Target: TObject);
begin
  with GetDataSet(Target) do
    Enabled:=Active and (State in dsEditModes);
end;


{ ---------------------------------------------------------------------
    TDataSetEdit
  ---------------------------------------------------------------------}

Procedure TDataSetEdit.ExecuteTarget(Target: TObject);
begin
  GetDataSet(Target).Edit;
end;


Procedure TDataSetEdit.UpdateTarget(Target: TObject);
begin
  with GetDataSet(Target) do
    Enabled:=Active and CanModify and not (State in dsEditModes);
end;


{ ---------------------------------------------------------------------
    TDataSetDelete
  ---------------------------------------------------------------------}

Procedure TDataSetDelete.ExecuteTarget(Target: TObject);
begin
  GetDataSet(Target).Delete;
end;


Procedure TDataSetDelete.UpdateTarget(Target: TObject);
begin
  with GetDataSet(Target) do
    Enabled:=Active and (not (BOF and EOF)) and CanModify;
end;


end.