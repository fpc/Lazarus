{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Edit form to edit a report memo.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit frmfpreportmemoedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls, ExtCtrls, fpReport;

type

  { TfpReportMemoEditForm }

  TfpReportMemoEditForm = class(TForm)
    BPMemo: TButtonPanel;
    MMemo: TMemo;
    Pmemo: TPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    FReportMemo: TFPReportCustomMemo;
    procedure SetFPReportMemo(AValue: TFPReportCustomMemo);
  public
    Procedure MemoToForm;
    Procedure FormToMemo;
    Property ReportMemo : TFPReportCustomMemo Read FReportMemo Write SetFPReportMemo;
  end;

  { TDefaultReportMemoEditor }

  TDefaultReportMemoEditor = Class(TFPReportElementEditor)
    Class function DefaultClass: TFPReportElementClass; override;
    Function Execute : Boolean; override;
  end;

var
  fpReportMemoEditForm: TfpReportMemoEditForm;

implementation

{$R *.lfm}

{ TDefaultReportMemoEditor }

class function TDefaultReportMemoEditor.DefaultClass: TFPReportElementClass;
begin
  Result:=TFPReportMemo;
end;

Function TDefaultReportMemoEditor.Execute : Boolean;

Var
  F : TfpReportMemoEditForm;

begin
  F:=TfpReportMemoEditForm.Create(Self);
  try
    F.ReportMemo:=Self.Element as TFPReportCustomMemo;
    Result:=F.ShowModal=mrOK;
  finally
    F.Free;
  end;
end;

{ TfpReportMemoEditForm }

procedure TfpReportMemoEditForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult=mrOK then
    FormToMemo;
end;

procedure TfpReportMemoEditForm.SetFPReportMemo(AValue: TFPReportCustomMemo);
begin
  if FReportMemo=AValue then Exit;
  FReportMemo:=AValue;
  MemoToForm;
end;

procedure TfpReportMemoEditForm.MemoToForm;
begin
  MMemo.Lines.Text:=TFPReportMemo(FReportMemo).Text;
end;

procedure TfpReportMemoEditForm.FormToMemo;
begin
  TFPReportMemo(FReportMemo).Text:=MMemo.Lines.Text
end;

Initialization
  TDefaultReportMemoEditor.RegisterEditor;
end.

