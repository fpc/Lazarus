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
unit datapanel;

{$mode objfpc}{$H+}

interface

uses
  Graphics, Classes, SysUtils, Controls, ExtCtrls, StdCtrls, DB, dbgrids, dbCtrls,
  buttons, fpdatadict;
  
Type

  { TDataPanel }
  TDataPanel = Class(TCustomPanel)
  private
    FDataSource: TDatasource;
    FTableName: String;
    FTopPanel: TPanel;
    FDBGrid : TDBGrid;
    FNavigator : TDBNavigator;
    FExportSB : TSpeedButton;
    FCodeSB : TSpeedButton;
    procedure Checkbuttons;
    procedure CreateControls;
    function GetDataset: TDataset;
    procedure DoExport(Sender : TObject);
    procedure DoCode(Sender : TObject);
    function GetExtra: Boolean;
    procedure SetExtra(const AValue: Boolean);
  Protected
    Property TopPanel : TPanel Read FTopPanel;
    Property DBGrid : TDBGrid Read FDBGrid;
    Property DataSource : TDatasource Read FDataSource;
    procedure SetDataset(const AValue: TDataset);virtual;
  Public
    Constructor Create(AOwner : TComponent); Override;
    Property Dataset : TDataset Read GetDataset Write SetDataset;
    Property TableName: String Read FTableName Write FTableName;
    Procedure ExportData;
    Procedure CreateCode;
    Property ShowExtraButtons : Boolean Read GetExtra Write SetExtra;
  end;

implementation

uses fpdataexporter,fpcodegenerator;

{ TDataPanel }

function TDataPanel.GetDataset: TDataset;
begin
  Result:=FDatasource.Dataset;
end;

procedure TDataPanel.DoExport(Sender: TObject);
begin
  ExportData;
end;

procedure TDataPanel.DoCode(Sender : TObject);

begin
  CreateCode;
end;

function TDataPanel.GetExtra: Boolean;
begin
  Result:=FExportSB.Visible;
end;

procedure TDataPanel.SetExtra(const AValue: Boolean);
begin
  FExportSB.Visible:=AValue;
  FCodeSB.Visible:=AValue;
end;

procedure TDataPanel.SetDataset(const AValue: TDataset);
begin
  FDatasource.Dataset:=AValue;
  CheckButtons;
end;

procedure TDataPanel.ExportData;
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

procedure TDataPanel.CreateCode;
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

constructor TDataPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataSource:=TDatasource.Create(Self);
  CreateControls;
end;

Procedure TDataPanel.CreateControls;

Var
  P : TPixMap;

begin
  P:=TPixMap.Create;
  try
  // Top panel;
  FTopPanel:=TPanel.Create(Self);
  FTopPanel.Parent:=Self;
  FTopPanel.Caption:='';
  FTopPanel.Align:=alTop;
  FTopPanel.height:=30;
  FTopPanel.BevelInner:=bvLowered;
  FTopPanel.BevelOuter:=bvNone;
  // Grid
  FDBgrid:=TDBGrid.Create(Self);
  FDBGrid.Parent:=Self;
  FDBGrid.Align:=alClient;
  FDBGrid.DataSource:=FDatasource;
  // Navigator;
  FNavigator:=TDBNavigator.Create(Self);
  FNavigator.Parent:=FTopPanel;
  FNavigator.Top:=4;
  FNavigator.Left:=4;
  FNavigator.Height:=22;
  FNavigator.DataSource:=FDatasource;
  //
  CheckButtons;
  FExportSB:=TSpeedButton.Create(Self);
  FExportSB.Parent:=FTopPanel;
  FExportSB.Left:=16+FNavigator.Width+FNavigator.Left;
  FExportSB.Top:=4;
  FExportSB.Height:=22;
  FExportSB.Width:=22;
  P.LoadFromLazarusResource('qrybtn_export');
  FExportSB.Glyph.Assign(P);
  FExportSB.Flat:=True;
  FExportSB.OnClick:=@DoExport;
  FCodeSB:=TSpeedButton.Create(Self);
  FCodeSB.Parent:=FTopPanel;
  FCodeSB.Left:=FExportSB.Width+FExportSB.Left;
  FCodeSB.Top:=4;
  FCodeSB.Height:=22;
  FCodeSB.Width:=22;
  P.LoadFromLazarusResource('qrybtn_code');
  FCodeSB.Glyph.Assign(P);
  FCodeSB.Flat:=True;
  FCodeSB.OnClick:=@DoCode;
  Finally
    FreeAndNil(P);
  end;
end;

procedure TDataPanel.Checkbuttons;

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

