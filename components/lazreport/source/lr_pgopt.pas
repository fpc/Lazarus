
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
  ExtCtrls, Buttons, StdCtrls,ComCtrls,LCLType;

type

{ TfrPgoptForm }

TfrPgoptForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    GroupBox2: TGroupBox;
    imgLandScape: TImage;
    imgPortrait: TImage;
    RB1: TRadioButton;
    RB2: TRadioButton;
    GroupBox1: TGroupBox;
    CB1: TCheckBox;
    GroupBox3: TGroupBox;
    ComB1: TComboBox;
    E1: TEdit;
    E2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    GroupBox4: TGroupBox;
    CB5: TCheckBox;
    E3: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    E4: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    E5: TEdit;
    E6: TEdit;
    GroupBox5: TGroupBox;
    Label7: TLabel;
    E7: TEdit;
    Label8: TLabel;
    Edit1: TEdit;
    Panel8: TPanel;
    SB1: TSpeedButton;
    SB2: TSpeedButton;
    procedure ComB1DrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure RB1Click(Sender: TObject);
    procedure RB2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ComB1Click(Sender: TObject);
    procedure CB5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SB1Click(Sender: TObject);
    procedure SB2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frPgoptForm: TfrPgoptForm;

implementation

uses LR_Prntr, LR_Class, LR_Const, LR_Utils;

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

procedure TfrPgoptForm.RB2Click(Sender: TObject);
begin
  ImgLandscape.Show;
  ImgPortrait.Visible:=False;
end;

procedure TfrPgoptForm.FormActivate(Sender: TObject);
begin
  if RB1.Checked then RB1Click(nil) else RB2Click(nil);
  ComB1Click(nil); CB5Click(nil);
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
  TabSheet2.Caption := sPgoptFormMargins;
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
  Button1.Caption := sOk;
  Button2.Caption := sCancel;
end;

procedure TfrPgoptForm.SB1Click(Sender: TObject);
var
  i: Integer;
begin
  i := StrToInt(Edit1.Text);
  Inc(i);
  Edit1.Text := IntToStr(i);
end;

procedure TfrPgoptForm.SB2Click(Sender: TObject);
var
  i: Integer;
begin
  i := StrToInt(Edit1.Text);
  Dec(i);
  if i < 0 then i := 0;
  Edit1.Text := IntToStr(i);
end;

initialization
  {$I lr_pgopt.lrs}

end.

