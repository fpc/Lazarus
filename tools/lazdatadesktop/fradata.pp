unit fradata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, LResources, Forms, Controls, ExtCtrls,
  DbCtrls, DBGrids, Buttons;

type

  { TDataFrame }

  TDataFrame = class(TFrame)
    FDBGrid: TDBGrid;
    FDataSource: TDataSource;
    FNavigator: TDBNavigator;
    FTopPanel: TPanel;
    FExportSB: TSpeedButton;
    FCodeSB: TSpeedButton;
    procedure DoExport(Sender : TObject);
    procedure DoCode(Sender : TObject);
  private
    { private declarations }
    FTableName : String;
    function GetDisplayMemoText: Boolean;
    procedure SetDisplayMemoText(AValue: Boolean);
  public
    { public declarations }
    procedure Checkbuttons;
    function GetDataset: TDataset;
    function GetExtra: Boolean;
    procedure SetExtra(const AValue: Boolean);
  Protected
    Property TopPanel : TPanel Read FTopPanel;
    Property DBGrid : TDBGrid Read FDBGrid;
    Property DataSource : TDatasource Read FDataSource;
    procedure SetDataset(const AValue: TDataset);virtual;
  Public
    Property Dataset : TDataset Read GetDataset Write SetDataset;
    Property TableName: String Read FTableName Write FTableName;
    Procedure ExportData;
    Procedure CreateCode;
    Property ShowExtraButtons : Boolean Read GetExtra Write SetExtra;
    Property DisplayMemoText : Boolean Read GetDisplayMemoText Write SetDisplayMemoText;
  end;

implementation

{$r *.lfm}
uses fpdataexporter,fpcodegenerator;

{ TDataFrame }

function TDataFrame.GetDataset: TDataset;
begin
  Result:=FDatasource.Dataset;
end;

procedure TDataFrame.DoExport(Sender: TObject);
begin
  ExportData;
end;

procedure TDataFrame.DoCode(Sender : TObject);

begin
  CreateCode;
end;


function TDataFrame.GetDisplayMemoText: Boolean;
begin
  result := (dgDisplayMemoText in FDBGrid.Options);
end;

procedure TDataFrame.SetDisplayMemoText(AValue: Boolean);
begin
  If AValue <> DisplayMemoText then
    begin
    If AValue then
      FDBGrid.Options := FDBGrid.Options + [dgDisplayMemoText]
    else
      FDBGrid.Options := FDBGrid.Options - [dgDisplayMemoText];
    end;
end;

function TDataFrame.GetExtra: Boolean;
begin
  Result:=FExportSB.Visible;
end;

procedure TDataFrame.SetExtra(const AValue: Boolean);
begin
  FExportSB.Visible:=AValue;
  FCodeSB.Visible:=AValue;
end;

procedure TDataFrame.SetDataset(const AValue: TDataset);
begin
  FDatasource.Dataset:=AValue;
  CheckButtons;
end;

procedure TDataFrame.ExportData;
begin
  With TFPDataExporter.Create(Dataset) do
    Try
      If Self.TableName<>'' then
        TableNameHint:=Self.TableName;
      Execute;
    Finally
      Free;
    end;
end;

procedure TDataFrame.CreateCode;
begin
  With TFPCodeGenerator.Create(Dataset) do
    try
      If Self.TableName<>'' then
        TableNameHint:=Self.TableName;
      Execute;
    Finally
      Free;
    end;
end;

procedure TDataFrame.Checkbuttons;

Const
  NavBtns  = [nbFirst,nbPrior,nbNext,nbLast,nbRefresh];
  EditBtns = [nbInsert,nbPost,nbDelete,nbCancel];

begin
  If Assigned(FNavigator) and Assigned(Dataset) then
    begin
    If Dataset.CanModify then
      begin
      FNavigator.VisibleButtons:=NavBtns;
      FNavigator.Width:=122;
      end
    else
      begin
      FNavigator.VisibleButtons:=NavBtns+EditBtns;
      FNavigator.Width:=244;
      end
    end;
end;

end.

