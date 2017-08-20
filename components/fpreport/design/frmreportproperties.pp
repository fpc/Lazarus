{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Report property editor.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit frmreportproperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, fpreport, reportdesignbaseforms;

type
  TForm = TBaseReportEditorForm;

  { TReportPropertiesForm }

  TReportPropertiesForm = class(TForm)
    BPReportProps: TButtonPanel;
    CBTwoPass: TCheckBox;
    ETitle: TEdit;
    EAuthor: TEdit;
    LLDateCreated: TLabel;
    LDateCreation: TLabel;
    Label2: TLabel;
    LCBTwoPass: TLabel;
    LTitle: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  protected
    procedure SetReport(AValue: TFPCustomReport); override;
  Public
    Procedure FormToReport; virtual;
    Procedure ReportToForm; virtual;
  end;

implementation

{$R *.lfm}

{ TReportPropertiesForm }

procedure TReportPropertiesForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=true;
  If ModalResult=mrOK then
    FormToReport;
end;

procedure TReportPropertiesForm.SetReport(AValue: TFPCustomReport);
begin
  Inherited;
  if Assigned(Report) then
    ReportToForm;
end;

procedure TReportPropertiesForm.FormToReport;
begin
  If Not Assigned(Report) then
    Raise Exception.Create('No report to save to');
  Report.Author  := EAuthor.Text;
  Report.Title   := ETitle.Text;
  Report.TwoPass := CBTWopass.Checked;
end;

procedure TReportPropertiesForm.ReportToForm;
begin
  If Not Assigned(Report) then
    Raise Exception.Create('No report to load');
  EAuthor.Text:=Report.Author;
  ETitle.Text:=Report.Title;
  CBTWopass.Checked:=Report.TwoPass;
  LDateCreation.Caption:=FormatDateTime('ddd yyyy-mm-dd hh:nn:ss',Report.DateCreated);
end;

initialization
  ReportPropertiesFormClass:=TReportPropertiesForm;
end.

