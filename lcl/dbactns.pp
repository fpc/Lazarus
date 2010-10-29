{
 /***************************************************************************
                                   dbactns.pp
                                   ----------
 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
    procedure SetDataSource(Value: TDataSource);
  Protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetDataSet(Target: TObject): TDataSet; virtual;
  Public
    function HandlesTarget(Target: TObject): Boolean; override;
    property DataSource: TDataSource read FDataSource write SetDataSource;
  end;

{ ---------------------------------------------------------------------
    Navigation Actions 
  ---------------------------------------------------------------------}
  
  TDataSetFirst = Class(TDataSetAction)
  Public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  Published
    property DataSource;
  end;

  TDataSetLast = Class(TDataSetAction)
  Public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  Published
    property DataSource;
  end;

  TDataSetNext = Class(TDataSetAction)
  Public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  Published
    property DataSource;
  end;

  TDataSetPrior = Class(TDataSetAction)
  Public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  Published
    property DataSource;
  end;

  TDataSetRefresh = Class(TDataSetAction)
  Public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  Published
    property DataSource;
  end;


{ ---------------------------------------------------------------------
    Data manipulation actions
  ---------------------------------------------------------------------}

  TDataSetCancel = Class(TDataSetAction)
  Public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  Published
    property DataSource;
  end;

  TDataSetDelete = Class(TDataSetAction)
  Public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  Published
    property DataSource;
  end;

  TDataSetEdit = Class(TDataSetAction)
  Public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  Published
    property DataSource;
  end;

  TDataSetInsert = Class(TDataSetAction)
  Public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  Published
    property DataSource;
  end;

  TDataSetPost = Class(TDataSetAction)
  Public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
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

function TDataSetAction.GetDataSet(Target: TObject): TDataSet;
begin
  Result:=(Target as TDataSource).DataSet;
end;

function TDataSetAction.HandlesTarget(Target: TObject): Boolean;

begin
  Result:=(DataSource<>Nil);
  if Result and (DataSource=Target) then
    Result:=(DataSource.DataSet<>Nil)
  else
    Result:=(Target is TDataSource) and (TDataSource(Target).DataSet<>Nil);
end;


procedure TDataSetAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent,Operation);
  if (Operation=opRemove) and 
     (AComponent=DataSource) then 
    FDataSource:=Nil;
end;


procedure TDataSetAction.SetDataSource(Value: TDataSource);
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

procedure TDataSetFirst.ExecuteTarget(Target: TObject);
begin
  GetDataSet(Target).First;
end;


procedure TDataSetFirst.UpdateTarget(Target: TObject);
begin
  With GetDataSet(Target) do
    Enabled:=Active and not BOF;
end;


{ ---------------------------------------------------------------------
    TDataSetLast
  ---------------------------------------------------------------------}

procedure TDataSetLast.ExecuteTarget(Target: TObject);
begin
  GetDataSet(Target).Last;
end;


procedure TDataSetLast.UpdateTarget(Target: TObject);
begin
  with GetDataSet(Target) do
    Enabled:=Active and not EOF;
end;


{ ---------------------------------------------------------------------
    TDataSetNext
  ---------------------------------------------------------------------}

procedure TDataSetNext.ExecuteTarget(Target: TObject);
begin
  GetDataSet(Target).Next;
end;


procedure TDataSetNext.UpdateTarget(Target: TObject);
begin
  with GetDataSet(Target) do
    Enabled:=Active and not Eof;
end;


{ ---------------------------------------------------------------------
    TDataSetPrior
  ---------------------------------------------------------------------}

procedure TDataSetPrior.ExecuteTarget(Target: TObject);
begin
  GetDataSet(Target).Prior;
end;


procedure TDataSetPrior.UpdateTarget(Target: TObject);
begin
  with GetDataSet(Target) do
    Enabled:=Active and not BOF;
end;


{ ---------------------------------------------------------------------
    TDataSetRefresh
  ---------------------------------------------------------------------}

procedure TDataSetRefresh.ExecuteTarget(Target: TObject);
begin
  GetDataSet(Target).Refresh;
end;


procedure TDataSetRefresh.UpdateTarget(Target: TObject);
begin
  with GetDataSet(Target) do
    Enabled:=Active;
end;


{ ---------------------------------------------------------------------
    TDatasetInsert
  ---------------------------------------------------------------------}

procedure TDataSetInsert.ExecuteTarget(Target: TObject);
begin
  GetDataSet(Target).Insert;
end;

procedure TDataSetInsert.UpdateTarget(Target: TObject);
begin
  with GetDataSet(Target) do
    Enabled:=Active and CanModify and not (State in dsEditModes);
end;


{ ---------------------------------------------------------------------
    TDataSetPost
  ---------------------------------------------------------------------}

procedure TDataSetPost.ExecuteTarget(Target: TObject);
begin
  GetDataSet(Target).Post;
end;

procedure TDataSetPost.UpdateTarget(Target: TObject);
begin
  with GetDataSet(Target) do
    Enabled:=Active and (State in dsEditModes);
end;

{ ---------------------------------------------------------------------
    TDataSetCancel
  ---------------------------------------------------------------------}

procedure TDataSetCancel.ExecuteTarget(Target: TObject);
begin
  GetDataSet(Target).Cancel;
end;


procedure TDataSetCancel.UpdateTarget(Target: TObject);
begin
  with GetDataSet(Target) do
    Enabled:=Active and (State in dsEditModes);
end;


{ ---------------------------------------------------------------------
    TDataSetEdit
  ---------------------------------------------------------------------}

procedure TDataSetEdit.ExecuteTarget(Target: TObject);
begin
  GetDataSet(Target).Edit;
end;


procedure TDataSetEdit.UpdateTarget(Target: TObject);
begin
  with GetDataSet(Target) do
    Enabled:=Active and CanModify and not (State in dsEditModes);
end;


{ ---------------------------------------------------------------------
    TDataSetDelete
  ---------------------------------------------------------------------}

procedure TDataSetDelete.ExecuteTarget(Target: TObject);
begin
  GetDataSet(Target).Delete;
end;


procedure TDataSetDelete.UpdateTarget(Target: TObject);
begin
  with GetDataSet(Target) do
    Enabled:=Active and (not (BOF and EOF)) and CanModify;
end;


end.
