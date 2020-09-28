{*
 * The MIT License (MIT)
 *
 * Copyright (c) 2020 Grupo SC10
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *}
unit lr_dotmatrix;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Forms,
  Controls,
  LResources,
  // lazreport
  LR_Class,
  LR_Prntr,
  LR_View,
  LR_Const,
  // lr_dotmatrix
  lr_dotmatrix_filter,
  lr_dotmatrix_dlg;

type

  { TlrDMReport }

  TlrDMReport = class(TfrReport)
  private
    fDotMatrixReport: boolean;
    function GetDotMatrixConfig: TlrDMConfig;
    procedure SetDotMatrixConfig(aValue: TlrDMConfig);
  public
    constructor Create(AOwner: TComponent); override;
    procedure PrintPreparedReport(const aPageNumbers: string; aCopies: integer);
    procedure ShowPreparedReport;
    procedure ShowReport;
  published
    property DotMatrixConfig: TlrDMConfig read GetDotMatrixConfig write SetDotMatrixConfig;
    property DotMatrixReport: boolean read fDotMatrixReport write fDotMatrixReport;
  end;

  { TlrDMPreview }

  TlrDMPreview = class(TfrPreviewForm)
    procedure PrintBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  public
    fDoc: Pointer; // => hack
    procedure Show_Modal(aDoc: Pointer);
  end;

procedure Register;

implementation

{$R lr_dotmatrix.res}

function IfThen(aCondition: boolean; const aFalseReturn: string; const aTrueReturn: string): string;
begin
  if (aCondition) then
  begin
    Result := aTrueReturn;
  end
  else
  begin
    Result := aFalseReturn;
  end;
end;

procedure Register;
begin
  RegisterComponents('LazReport', [TlrDMReport]);
end;

{ TlrDMReport }

function TlrDMReport.GetDotMatrixConfig: TlrDMConfig;
begin
  Result := vlrDMConfig;
end;

procedure TlrDMReport.SetDotMatrixConfig(aValue: TlrDMConfig);
begin
  vlrDMConfig.Assign(aValue);
end;

constructor TlrDMReport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDotMatrixReport := True;
  // Register dotmatix filter
  frRegisterExportFilter(TlrDMFilter, 'Draft file' + ' (*.drf)', '*.drf');
end;

procedure TlrDMReport.PrintPreparedReport(const aPageNumbers: string; aCopies: integer);
var
  vLoop: integer;
begin
  if (fDotMatrixReport) then
  begin
    CurReport := Self;
    MasterReport := Self;
    Terminated := False;
    PrepareReport;
    for vLoop := 0 to (aCopies - 1) do
    begin
      ExportTo(TlrDMFilter, './tmp.drf');
    end;
    Terminated := True;
  end
  else
  begin
    inherited PrintPreparedReport(aPageNumbers, aCopies);
  end;
end;

procedure TlrDMReport.ShowPreparedReport;
var
  vPreview: TlrDMPreview;
begin
  if (fDotMatrixReport) then
  begin
    CurReport := Self;
    MasterReport := Self;
    DocMode := dmPrinting;
    if (EMFPages.Count > 0) then
    begin
      vPreview := TlrDMPreview.Create(nil);
      vPreview.BorderIcons := vPreview.BorderIcons - [biMinimize];
      vPreview.Caption := IfThen((Title = ''), (SPreview), (SPreview + ' - ' + Title));
      vPreview.Show_Modal(Self);
    end;
  end
  else
  begin
    inherited ShowPreparedReport;
  end;
end;

procedure TlrDMReport.ShowReport;
begin
  if (PrepareReport) then
  begin
    ShowPreparedReport;
  end
  else
  begin
    inherited ShowReport;
  end;
end;

{ TlrDMPreview }

procedure TlrDMPreview.PrintBtnClick(Sender: TObject);
begin
  if (TlrDMReport(fDoc).DotMatrixReport) then
  begin
    lrDMDlg := TlrDMDlg.Create(nil);
    try
      // Auto new page
      lrDMDlg.chkOptionsAutoNewPage.Checked := vlrDMConfig.AutoNewPage;
      lrDMDlg.edtOptionsAutoNewPageLines.Value := vlrDMConfig.AutoNewPageLines;

      // Line spacing
      vlrDMConfig.AddingLineSpacingTo(lrDMDlg.cboOptionsLineSpacing.Items);
      lrDMDlg.cboOptionsLineSpacing.ItemIndex := Ord(vlrDMConfig.LineSpacing);
      lrDMDlg.edtOptionsLineSpacingCustom.Value := vlrDMConfig.LineSpacingCustomValue;

      // List printer
      lrDMDlg.cboPrinter.Items.Assign(Prn.Printers);
      lrDMDlg.cboPrinter.ItemIndex := Prn.PrinterIndex;

      // Show dialog
      if lrDMDlg.ShowModal = mrOk then
      begin
        // Auto new page
        vlrDMConfig.AutoNewPage := lrDMDlg.chkOptionsAutoNewPage.Checked;
        vlrDMConfig.AutoNewPageLines := lrDMDlg.edtOptionsAutoNewPageLines.Value;

        // Line spacing
        vlrDMConfig.LineSpacing := TlrDMLineSpacing(Ord(lrDMDlg.cboOptionsLineSpacing.ItemIndex));
        vlrDMConfig.LineSpacingCustomValue:= lrDMDlg.edtOptionsLineSpacingCustom.Value;

        // Printer Index
        Prn.PrinterIndex := lrDMDlg.cboPrinter.ItemIndex;
        // Print
        TlrDMReport(fDoc).PrintPreparedReport('', lrDMDlg.edtCopies.Value);
      end;
    finally
      lrDMDlg.Free;
    end;
  end
  else
  begin
    inherited PrintBtnClick(Sender);
  end;
end;

procedure TlrDMPreview.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Chr(Key) = 'P') and (PrintBtn.Visible) then
  begin
    PrintBtnClick(nil);
  end
  else
  begin
    inherited FormKeyDown(Sender, Key, Shift);
  end;
end;

procedure TlrDMPreview.Show_Modal(aDoc: Pointer);
begin
  fDoc := aDoc;
  inherited Show_Modal(aDoc);
end;

end.
