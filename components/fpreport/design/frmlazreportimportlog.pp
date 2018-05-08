unit frmlazreportimportlog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls, reportdesignbaseforms;

type

  { TReportImportLogForm }
  TForm = Class(TBaseImportReportForm);

  TReportImportLogForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    MLog: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public
     Procedure Log(Const Msg : String); override;
  end;

var
  ReportImportLogForm: TReportImportLogForm;

implementation

{$R *.lfm}

{ TReportImportLogForm }

procedure TReportImportLogForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

procedure TReportImportLogForm.Log(Const Msg : String);
begin
  MLog.Lines.Add(Msg);
  Application.ProcessMessages;
end;

initialization
  ReportImportFormClass:=TReportImportLogForm;
end.

