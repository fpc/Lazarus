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
  Classes, SysUtils, Controls, ExtCtrls, StdCtrls, DB, dbgrids, dbCtrls;
  
Type

  { TDataPanel }
  TDataPanel = Class(TCustomPanel)
  private
    FDataSource: TDatasource;
    FTopPanel: TPanel;
    FDBGrid : TDBGrid;
    FNavigator : TDBNavigator;
    procedure Checkbuttons;
    procedure CreateControls;
    function GetDataset: TDataset;
  Protected
    Property TopPanel : TPanel Read FTopPanel;
    Property DBGrid : TDBGrid Read FDBGrid;
    Property DataSource : TDatasource Read FDataSource;
    procedure SetDataset(const AValue: TDataset);virtual;
  Public
    Constructor Create(AOwner : TComponent); Override;
    Property Dataset : TDataset Read GetDataset Write SetDataset;
  end;

implementation

{ TDataPanel }

function TDataPanel.GetDataset: TDataset;
begin
  Result:=FDatasource.Dataset;
end;

procedure TDataPanel.SetDataset(const AValue: TDataset);
begin
  FDatasource.Dataset:=AValue;
  CheckButtons;
end;

constructor TDataPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataSource:=TDatasource.Create(Self);
  CreateControls;
end;

Procedure TDataPanel.CreateControls;

begin
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
  FNavigator.Parent:=Self;
  FNavigator.Top:=4;
  FNavigator.Left:=4;
  FNavigator.Height:=22;
  FNavigator.DataSource:=FDatasource;
  CheckButtons;
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

