{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by Michael Van Canneyt, member of the
    Free Pascal development team

    TFPDataExporter dialog component.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpdataexporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, fpdbexport,
  forms, controls, dialogs, frmexportprogress, sdb_consts;

Type

  { TFPDataExporter }

  TFPDataExporter = Class(TComponent)
  Private
    FDataset : TDataset;
    FExportCount : Integer;
    FShowProgress: Boolean;
    FShowResult: Boolean;
    FExporter : TCustomDatasetExporter;
    FProgress : TExportProgressForm;
    FTableNameHint: String;
    procedure SetDataset(const AValue: TDataset);
    procedure DoCancel(Sender : TObject);
    Procedure Doprogress(Sender : TObject; Const ItemNo : Integer);
  public
    Constructor Create(AOWner : TComponent); override;
    Function Execute : Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Property ExportCount : Integer Read FExportCount;
    Property Exporter : TCustomDatasetExporter Read FExporter;
  Published
    Property Dataset : TDataset Read FDataset Write SetDataset;
    Property ShowProgress : Boolean Read FShowProgress Write FShowProgress default true;
    Property ShowResult : Boolean Read FShowResult Write FShowResult default true;
    Property TableNameHint : String Read FTableNameHint Write FTableNameHint;
  end;
  

implementation

uses typinfo, frmSelectExportFormat, frmBaseConfigExport;
  
procedure TFPDataExporter.SetDataset(const AValue: TDataset);
begin
  If (AValue<>FDataset) then
    begin
    If Assigned(FDataset) then
      FDataset.RemoveFreeNotification(Self);
    FDataset:=AValue;
    If Assigned(FDataset) then
      FDataset.FreeNotification(Self);
    end;
end;

procedure TFPDataExporter.DoCancel(Sender: TObject);
begin
  FExporter.Cancel;
end;

procedure TFPDataExporter.Doprogress(Sender: TObject; const ItemNo: Integer);
begin
  If Assigned(FProgress) then
    FProgress.StepIt;
end;

Constructor TFPDataExporter.Create(AOWner : TComponent);

begin
  Inherited;
  If (AOwner is TDataset) then
    Dataset:=AOwner as TDataset;
  FShowProgress:=True;
  FShowResult:=True;
end;

Function TFPDataExporter.Execute : Boolean;

Var
  FI : TExportFormatItem;

begin
  FI:=Nil;
  FProgress:=Nil;
  With TSelectExportFormatForm.Create(Self) do
    try
      Result:=(ShowModal=mrOK);
      If Result then
        begin
        FI:=SelectedFormat;
        Result:=FI<>Nil;
        end;
    finally
      Free;
    end;
  If Result then
    begin
    RegisterBaseExportConfigForm;
    FExporter:=FI.ExportClass.Create(Self);
    Try
      FExporter.Dataset:=Self.Dataset;
      If IsPublishedProp(FExporter.FormatSettings,'TableName') then
        SetStrProp(FExporter.FormatSettings,'TableName',TableNameHint);
      Result:=Exporter.ShowConfigDialog;
      if Result then
        begin
        Dataset.First;
        If ShowProgress then
          begin
          FProgress:=TExportProgressForm.Create(Self);
          FProgress.FreeNotification(Self);
          FProgress.OnCancel:=@self.DoCancel;
          FExporter.OnProgress:=@self.DoProgress;
          FProgress.Show;
          end;
        Try
          FExportCount:=FExporter.Execute;
        finally
          If Assigned(Fprogress) then
            FProgress.Free;
        end;
        If FShowResult then
          If FExporter.Canceled then
            ShowMessage(Format(SCancelRecordsExported,[FExportCount]))
          else
            ShowMessage(Format(SNRecordsExported,[FExportCount]));
        end;
    Finally
      FreeAndNil(FExporter);
    end;
    end;
end;

procedure TFPDataExporter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  If (Operation=opRemove) then
    begin
    If (AComponent=FDataset) then
      FDataset:=Nil
    else if (AComponent=FProgress) then
      FProgress:=Nil;
    end;
end;

end.

