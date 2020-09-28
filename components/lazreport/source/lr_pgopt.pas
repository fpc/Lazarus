
{*****************************************}
{                                         }
{             FastReport v2.3             }
{              Page options               }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_pgopt;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,
  Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls,ComCtrls,LCLType, ButtonPanel, Spin, Printers;

type

{ TfrPgoptForm }

TfrPgoptForm = class(TForm)
  ButtonPanel1: TButtonPanel;
  CB5: TCheckBox;
  E3: TEdit;
  E4: TEdit;
  E5: TEdit;
  E6: TEdit;
  GroupBox4: TGroupBox;
  imgColumns: TImage;
  imgRows: TImage;
  Label3: TLabel;
  Label4: TLabel;
  Label5: TLabel;
  Label6: TLabel;
  lblLayout: TLabel;
    PageControl1: TPageControl;
    ecolCount: TSpinEdit;
    Panel1: TPanel;
    RBColumns: TRadioButton;
    RBRows: TRadioButton;
    RB1: TRadioButton;
    RB2: TRadioButton;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    GroupBox2: TGroupBox;
    imgLandScape: TImage;
    imgPortrait: TImage;
    GroupBox1: TGroupBox;
    CB1: TCheckBox;
    GroupBox3: TGroupBox;
    ComB1: TComboBox;
    E1: TEdit;
    E2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    GroupBox5: TGroupBox;
    Label7: TLabel;
    E7: TEdit;
    Label8: TLabel;
    procedure ComB1DrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure ComB1Select(Sender: TObject);
    procedure E1Change(Sender: TObject);
    procedure ecolCountChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure RB1Click(Sender: TObject);
    procedure RB2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure CB5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RBColumnsClick(Sender: TObject);
    procedure RBRowsClick(Sender: TObject);
  private
    { Private declarations }
    FCustomWidth, FCustomHeight: Integer;
    function  GetPgSize: Integer;
    procedure UpdateColumnsLayout;
    procedure UpdatePaperSize;
  public
    { Public declarations }
    property PgSize: Integer read GetPgSize;
    property PaperWidth: Integer read FCustomWidth write FCustomWidth;
    property PaperHeight: Integer read FCustomHeight write FCustomHeight;
  end;

var
  frPgoptForm: TfrPgoptForm;

implementation

{$R *.lfm}

uses LR_Prntr, LR_Class, LR_Const, LR_Utils, Math;

procedure TfrPgoptForm.RB1Click(Sender: TObject);
begin
  ImgPortrait.Show;
  ImgLandscape.Visible:=False;
end;

procedure TfrPgoptForm.ComB1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  S: String;
  i: Integer;
begin
  with TCombobox(Control) do begin
  
    if odSelected in State then
      Canvas.Brush.Color := clHighLight
    else
      Canvas.Brush.Color:=clWindow;
      
    Canvas.FillRect(aRect);
    Canvas.TextRect(aRect, aRect.Left + 17, aRect.Top+ 3, Items[Index]);
    
    aRect.Right := aRect.Left + 16;

    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(aRect);
    
    i := PtrInt(Items.Objects[Index]);
    if (i>=1)and(i<=MAX_TYP_KNOWN) then   S := 'W' else // Known Windows std paper size
    if (i>MAX_TYP_KNOWN) and (i<256) then S := 'w' else // Unknown Windows std paper size
    if (i=256) then                       S := 'U' else // User Defined paper size
    if (i>=2000) and (i<2050) then        S := 'I'      // Looks like an Input Slot
    else                                  S := 'C';     // Known Custom No-Std Paper Size

    Canvas.Font.Color := clWindowText;
    Canvas.TextRect(aRect, aRect.Left+1, aRect.Top+3, S);

  end;
end;

procedure TfrPgoptForm.ComB1Select(Sender: TObject);
begin
  UpdatePaperSize;
end;

procedure TfrPgoptForm.E1Change(Sender: TObject);
begin
  if pgSize=256 then begin
    if Sender=E1 then FCustomWidth := StrToIntDef(E1.Text, 0);
    if Sender=E2 then FCustomHeight := StrToIntDef(E2.Text, 0);
  end;
end;

procedure TfrPgoptForm.ecolCountChange(Sender: TObject);
begin
  UpdateColumnsLayout;
end;

procedure TfrPgoptForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (ModalResult=mrOk) and (PgSize=256) then
  begin
    canClose := (PaperWidth>1) and (PaperHeight>1);
    if not canClose then
      ShowMessage(sPgoptFormInvalidCustomPaperSize);
  end;
end;

procedure TfrPgoptForm.RB2Click(Sender: TObject);
begin
  ImgLandscape.Show;
  ImgPortrait.Visible:=False;
end;

procedure TfrPgoptForm.FormActivate(Sender: TObject);
begin
  OnActivate:=nil;
  if RB1.Checked then
    RB1Click(nil)
  else
    RB2Click(nil);
  UpdatePaperSize;
  CB5Click(nil);

  ComB1.Width:=E1.Width;
  ecolCount.Width:=E1.Width;
  
  Label3.Left:=Max(Label3.Left, Label4.Width + Label4.Left);
  Label3.Left:=Max(Label3.Left, Label5.Width + Label5.Left);
  
  UpdateColumnsLayout;
end;

procedure TfrPgoptForm.CB5Click(Sender: TObject);
begin
  frEnableControls([Label3, Label4, Label5, Label6, E3, E4, E5, E6],
    not CB5.Checked);
end;

procedure TfrPgoptForm.FormCreate(Sender: TObject);
begin
  Caption := sPgoptFormCapt + ' - ' + QuotedStr(prn.Printer.PrinterName);
  TabSheet1.Caption := sPgoptFormPaper;
  GroupBox2.Caption := sPgoptFormOr;
  RB1.Caption := sPgoptFormPort;
  RB2.Caption := sPgoptFormLand;
  GroupBox3.Caption :=sPgoptFormSize;
  Label1.Caption := sPgoptFormWidth;
  Label2.Caption := sPgoptFormHeight;
  GroupBox4.Caption := sPgoptFormPgMargins;
  Label3.Caption := sPgoptFormLeft;
  Label4.Caption := sPgoptFormTop;
  Label5.Caption := sPgoptFormRight;
  Label6.Caption := sPgoptFormBottom;
  CB5.Caption := sPgoptFormDontUse;
  TabSheet3.Caption := sPgoptFormOptions;
  GroupBox1.Caption := sPgoptFormOptions;
  CB1.Caption := sPgoptFormpRINT;
  GroupBox5.Caption := sPgoptFormColumn;
  Label7.Caption := sPgoptFormNumber;
  Label8.Caption := sPgoptFormColGap;
  lblLayout.Caption := sPgoptFormLayoutOrder;
  RBColumns.Caption := sPgoptFormByColumns;
  RBRows.Caption := sPgoptFormByRows;
end;

procedure TfrPgoptForm.RBColumnsClick(Sender: TObject);
begin
  ImgColumns.Visible:=true;
  ImgRows.Visible:=false;
end;

procedure TfrPgoptForm.RBRowsClick(Sender: TObject);
begin
  ImgColumns.Visible:=false;
  ImgRows.Visible:=true;
end;

function TfrPgoptForm.GetPgSize: Integer;
begin
  result := PtrInt(ComB1.Items.Objects[Comb1.ItemIndex]);
end;

procedure TfrPgoptForm.UpdateColumnsLayout;
begin
  if EColCount.Value<2 then begin
    RBColumns.Enabled:=false;
    RBRows.Enabled:=false;
  end else begin
    RBColumns.Enabled:=true;
    RBRows.Enabled:=true;
  end;
end;

procedure TfrPgoptForm.UpdatePaperSize;
var
  isCustom: Boolean;
  pgIndex: Integer;
  PaperRect: TPaperRect;
begin
  isCustom := (pgSize = $100);
  frEnableControls([Label1, Label2, E1, E2], isCustom);
  if isCustom then begin
    E1.Text := IntToStr(PaperWidth);
    E2.Text := IntToStr(PaperHeight);
  end else begin
    pgIndex := prn.GetArrayPos(pgSize);
    PaperRect := prn.Printer.PaperSize.PaperRectOf[prn.PaperNames[pgIndex]];
    E1.Text := IntToStr(round(PaperRect.PhysicalRect.Width*25.4/prn.Printer.XDPI));
    E2.Text := IntToStr(round(PaperRect.PhysicalRect.Height*25.4/prn.Printer.YDPI));
  end;
end;

end.

