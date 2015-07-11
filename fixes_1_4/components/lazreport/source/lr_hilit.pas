
{*****************************************}
{                                         }
{             FastReport v2.3             }
{       Highlight attributes dialog       }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_Hilit;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,
  Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, ButtonPanel,
  LR_Const;

type

  { TfrHilightForm }

  TfrHilightForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    CB1: TCheckBox;
    CB2: TCheckBox;
    CB3: TCheckBox;
    ColorDialog1: TColorDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    RB1: TRadioButton;
    RB2: TRadioButton;
    GroupBox3: TGroupBox;
    Edit1: TEdit;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure RB1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FontColor, FillColor: TColor;
  end;

var
  frHilightForm: TfrHilightForm;

implementation

{$R *.lfm}

uses LR_Desgn, LR_Class;

procedure TfrHilightForm.SpeedButton1Click(Sender: TObject);
begin
  ColorDialog1.Color := FontColor;
  if ColorDialog1.Execute then
  begin
    FontColor := ColorDialog1.Color;
    frSetGlyph(FontColor, SpeedButton1, 0);
  end;
end;

procedure TfrHilightForm.SpeedButton2Click(Sender: TObject);
begin
  ColorDialog1.Color := FillColor;
  if ColorDialog1.Execute then
  begin
    FillColor := ColorDialog1.Color;
    frSetGlyph(FillColor, SpeedButton2, 1);
  end;
end;

procedure TfrHilightForm.FormActivate(Sender: TObject);
begin
  frSetGlyph(FontColor, SpeedButton1, 0);
  frSetGlyph(FillColor, SpeedButton2, 1);
  if FillColor = clNone then
    RB1.Checked := True else
    RB2.Checked := True;
  RB1Click(nil);
end;

procedure TfrHilightForm.RB1Click(Sender: TObject);
begin
  SpeedButton2.Enabled := RB2.Checked;
  if RB1.Checked then FillColor := clNone;
end;

procedure TfrHilightForm.FormCreate(Sender: TObject);
begin
  Caption := sHilightFormHilitAttr;
  GroupBox3.Caption :=sHilightFormCond;
  GroupBox1.Caption := sHilightFormFont;
  SpeedButton1.Caption := sHilightFormColor;
  CB1.Caption := sHilightFormBold;
  CB2.Caption := sHilightFormItalic;
  CB3.Caption := sHilightFormUnder;
  GroupBox2.Caption := sHilightFormBack;
  SpeedButton2.Caption := sHilightFormColor2;
  RB1.Caption := sHilightFormTransp;
  RB2.Caption := sHilightFormOther;
end;

end.

