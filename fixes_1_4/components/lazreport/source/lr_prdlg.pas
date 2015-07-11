
{*****************************************}
{                                         }
{             FastReport v2.3             }
{              Print dialog               }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_PrDlg;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,
  Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls,LCLIntf,ExtCtrls, Spin,
  PrintersDlgs;

type

  { TfrPrintForm }

  TfrPrintForm = class(TForm)
    cbCollate: TCheckBox;
    Label1: TLabel;
    GroupBox2: TGroupBox;
    RB1: TRadioButton;
    RB2: TRadioButton;
    RB3: TRadioButton;
    E2: TEdit;
    Label2: TLabel;
    OkButton: TButton;
    CancelButton: TButton;
    GroupBox1: TGroupBox;
    CB1: TComboBox;
    PropButton: TButton;
    PrinterSetupDialog1: TPrinterSetupDialog;
    Image1: TImage;
    E1: TSpinEdit;
    procedure CB1DrawItem({%H-}Control: TWinControl; Index: Integer;
      ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure PropButtonClick(Sender: TObject);
    procedure CB1Click(Sender: TObject);
    procedure E2Click(Sender: TObject);
    procedure RB3Click(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    { Private declarations }
    OldIndex: Integer;
  public
    { Public declarations }
  end;

var
  frPrintForm: TfrPrintForm;

implementation

{$R *.lfm}

uses LR_Const, LR_Prntr,Printers;

procedure TfrPrintForm.FormCreate(Sender: TObject);
begin
  CB1.Items.Assign(Printer.Printers);
  CB1.ItemIndex := Printer.PrinterIndex;
  OldIndex := Printer.PrinterIndex;

  Caption := sPrintFormPrint;
  GroupBox1.Caption := sPrintFormPrinter;
  PropButton.Caption := sPrintFormProp;
  Label1.Caption := sPrintFormCopy;
  GroupBox2.Caption := sPrintFormPgRange;
  RB1.Caption := sPrintFormAll;
  RB2.Caption := sPrintFormCurPg;
  RB3.Caption := sPrintFormNumber;
  Label2.Caption := sPrintFormInfo;
  OkButton.Caption := sOk;
  CancelButton.Caption := sCancel;
  cbCollate.Caption:=sPrintFormCollate;
  {$IFDEF PRINTDIALOG_DISABLE_PRINTER_PROPERTIES}
  propButton.Enabled := false;
  {$ENDIF}
end;

procedure TfrPrintForm.FormDeactivate(Sender: TObject);
begin
  if ModalResult <> mrOk then
    Prn.PrinterIndex := OldIndex;
end;

procedure TfrPrintForm.CB1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  r: TRect;
begin
  r := ARect;
  r.Right := r.Left + 18;
  r.Bottom := r.Top + 16;

  OffsetRect(r, 2, (ARect.Bottom - ARect.Top) div 2 - 8);

  with CB1.Canvas do
  begin
    FillRect(ARect);
    BrushCopy(r, Image1.Picture.Bitmap, Rect(0, 0, 18, 16), clOlive);
    TextOut(ARect.Left + 24, ARect.Top + 1, CB1.Items[Index]);
  end;
end;

procedure TfrPrintForm.PropButtonClick(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
  CB1.ItemIndex := Printer.PrinterIndex;
end;

procedure TfrPrintForm.CB1Click(Sender: TObject);
begin
  Prn.PrinterIndex := CB1.ItemIndex;
end;

procedure TfrPrintForm.E2Click(Sender: TObject);
begin
  RB3.Checked := True;
end;

procedure TfrPrintForm.RB3Click(Sender: TObject);
begin
  E2.SetFocus;
end;

end.
