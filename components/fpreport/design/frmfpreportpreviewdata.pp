{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Preview form for data in FPReport Data loop classes based on TDataset.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit frmfpreportpreviewdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel, DbCtrls, DBGrids, reportdesignbaseforms;

type
  TForm = TBaseReportDataPreviewForm;

  { TReportDataPreviewForm }

  TReportDataPreviewForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    DSPreview: TDataSource;
    GPreview: TDBGrid;
    NPreview: TDBNavigator;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  protected
    procedure SetDataSet(AValue: TDataset); override;
  end;

var
  ReportDataPreviewForm: TReportDataPreviewForm;

implementation

{$R *.lfm}

{ TReportDataPreviewForm }

procedure TReportDataPreviewForm.FormShow(Sender: TObject);

Var
  I : integer;

begin
  PreviewDataset.Open;
  For I:=0 To GPreview.Columns.Count-1 do
    if GPreview.Columns[i].Width>200 then
      GPreview.Columns[i].Width:=200;
end;

procedure TReportDataPreviewForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

procedure TReportDataPreviewForm.SetDataSet(AValue: TDataset);
begin
  Inherited;
  DSPreview.DataSet:=PreviewDataset;
end;

begin
  ReportDataPreviewClass:=TReportDataPreviewForm;
end.

