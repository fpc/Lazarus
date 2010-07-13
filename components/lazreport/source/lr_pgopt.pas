
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
  ExtCtrls, Buttons, StdCtrls,ComCtrls,LCLType, ButtonPanel, Spin;

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
    procedure ecolCountChange(Sender: TObject);
    procedure RB1Click(Sender: TObject);
    procedure RB2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ComB1Click(Sender: TObject);
    procedure CB5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RBColumnsClick(Sender: TObject);
    procedure RBRowsClick(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateColumnsLayout;
  public
    { Public declarations }
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
    if (i>=1)and(i<256) then S:='W' else    // Windows std paper size
    if (i=256) then S:='U' else             // User Defined paper size
    if (i>=2000) then S:='I' else           // Looks like an Input Slot
    if (i>=1000) then S:='C'                // Known Custom No-Std Paper Size
    else
      S:='?';                               // unknown unclassified paper size
    Canvas.Font.Color := clWindowText;
    Canvas.TextRect(aRect, aRect.Left+1, aRect.Top+3, S);

  end;
end;

procedure TfrPgoptForm.ecolCountChange(Sender: TObject);
begin
  UpdateColumnsLayout;
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
  ComB1Click(nil);
  CB5Click(nil);

  ComB1.Width:=E1.Width;
  ecolCount.Width:=E1.Width;
  
  Label3.Left:=Max(Label3.Left, Label4.Width + Label4.Left);
  Label3.Left:=Max(Label3.Left, Label5.Width + Label5.Left);
  
  UpdateColumnsLayout;
end;

procedure TfrPgoptForm.ComB1Click(Sender: TObject);
begin
  frEnableControls([Label1, Label2, E1, E2],
    Prn.PaperSizes[ComB1.ItemIndex] = $100);
end;

procedure TfrPgoptForm.CB5Click(Sender: TObject);
begin
  frEnableControls([Label3, Label4, Label5, Label6, E3, E4, E5, E6],
    not CB5.Checked);
end;

procedure TfrPgoptForm.FormCreate(Sender: TObject);
begin
  Caption := sPgoptFormCapt;
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

end.

