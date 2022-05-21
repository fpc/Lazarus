unit frmRestData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, DBCtrls,
  ButtonPanel, DBGrids, stub.restdataset;

type

  { TRestDataForm }

  TRestDataForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    dsData: TDataSource;
    grdData: TDBGrid;
    navData: TDBNavigator;
  private
    FDataset: TDataset;
    procedure SetDataset(AValue: TDataset);
  public
    Procedure SetDatasetAndResource(aDataset : TDataset; const aResource : string);
    Property Dataset : TDataset Read FDataset Write SetDataset;
  end;

var
  RestDataForm: TRestDataForm;

implementation

{$R *.lfm}

{ TRestDataForm }

procedure TRestDataForm.SetDataset(AValue: TDataset);

var
  aRes : string;

begin
  aRes:='';
  if aValue is TSQLDBRestDataset then
    aRes:=(aValue as TSQLDBRestDataset).ResourceName;
  SetDatasetAndResource(aValue,aRes);
end;

procedure TRestDataForm.SetDatasetAndResource(aDataset: TDataset; const aResource : string);

Var
  Capt : String;

begin
  if FDataset=aDataset then Exit;
  FDataset:=aDataset;
  dsData.Dataset:=FDataset;
  Capt:=Caption+' : '+Dataset.Name;
  if aResource<>'' then
    Capt:=Capt+'['+aResource+']';
  Caption:=Capt;
end;

end.

