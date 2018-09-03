unit dbappform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, AppForm;

Type
  TDatasetActionEvent = Procedure(Dataset : TDataset; DoAction : Boolean) of Object;

  { TDBAppForm }

  TDBAppForm = Class(TAppForm)
  private
    FAfterOpen: TNotifyEvent;
    FBeforeOpen: TNotifyEvent;
    FOnOpenDataset: TDatasetActionEvent;
    FOpenDatasets: Boolean;
    Procedure OpenAllDatasets;
  Protected
    Procedure DoInitForm; override;
    Procedure DoOpenDatasets; virtual;
  Published
    Property OpenDatasets : Boolean Read FOpenDatasets Write FOpenDatasets;
    Property BeforeOpenDatasets : TNotifyEvent Read FBeforeOpen Write FBeforeOpen;
    Property AfterOpenDatasets : TNotifyEvent Read FAfterOpen Write FAfterOpen;
    Property OnOpenDataset : TDatasetActionEvent Read FOnOpenDataset Write FOnOpenDataset;
  end;

implementation

{ TDBAppForm }

procedure TDBAppForm.OpenAllDatasets;
begin
  If Assigned(BeforeOpenDatasets) then
    BeforeOpenDatasets(Self);
  DoOpenDatasets;
  If Assigned(AfterOpenDatasets) then
    AfterOpenDatasets(Self);
end;

procedure TDBAppForm.DoInitForm;
begin
  inherited DoInitForm;
  If OpenDatasets then
    OpenAllDatasets;
end;

procedure TDBAppForm.DoOpenDatasets;

Var
  I : Integer;
  D : TDataset;
  B : Boolean;
  
begin
  For I:=0 to ComponentCount-1 do
    begin
    If Components[i] is TDataset then
      begin
      D:=TDataset(Components[i]);
      B:=True;
      If Assigned(OnOpenDataset) then
        OnOpenDataset(D,B);
      If B then
        D.Open;
      end;
    end;
end;


end.

