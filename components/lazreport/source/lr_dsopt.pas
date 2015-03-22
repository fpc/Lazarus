
{*****************************************}
{                                         }
{             FastReport v2.3             }
{             Designer options            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_DsOpt;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,
  Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls,ComCtrls, ButtonPanel, Spin,
  LR_Const;

type

  { TfrDesOptionsForm }

  TfrDesOptionsForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PageControl1: TPageControl;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    Tab1: TTabSheet;
    GroupBox1: TGroupBox;
    CB1: TCheckBox;
    CB2: TCheckBox;
    GroupBox2: TGroupBox;
    RB4: TRadioButton;
    RB5: TRadioButton;
    GroupBox3: TGroupBox;
    RB6: TRadioButton;
    RB7: TRadioButton;
    RB8: TRadioButton;
    GroupBox4: TGroupBox;
    RB1: TRadioButton;
    RB2: TRadioButton;
    RB3: TRadioButton;
    GroupBox5: TGroupBox;
    CB3: TCheckBox;
    CB4: TCheckBox;
    CB5: TCheckBox;
    TabSheet1: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
  private
    procedure FillFonts;
  public
    { Public declarations }
  end;


implementation
uses LCLType, LCLIntf;

{$R *.lfm}

function EnumFontsProc( var LogFont: TEnumLogFontEx; var {%H-}Metric: TNewTextMetricEx;
  FontType: Longint; {%H-}Data: LParam):LongInt; stdcall;
var
  S: String;
  Lst: TStrings;
begin
  s := StrPas(LogFont.elfLogFont.lfFaceName);
  Lst := TStrings(PtrInt(Data));
  if Lst.IndexOf(S)<0 then
    Lst.AddObject(S, TObject(PtrInt(FontType)));
  Result := 1;
end;

procedure TfrDesOptionsForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex:=0;
  Caption := sDesOptionsFormOpt;
  Tab1.Caption := sDesOptionsFormDes;
  GroupBox1.Caption := sDesOptionsFormGrid;
  GroupBox2.Caption := sDesOptionsFormObj;
  GroupBox3.Caption := sDesOptionsFormUnits;
  GroupBox4.Caption := sDesOptionsFormGrdSize;
  GroupBox5.Caption := sDesOptionsFormOther;
  CB1.Caption := sDesOptionsFormShowGrd;
  CB2.Caption := sDesOptionsFormAlignGrd;
  CB3.Caption := sDesOptionsFormColoredButton;
  CB4.Caption := sDesOptionsFormEditing;
  CB5.Caption := sDesOptionsFormShowBand;
  RB1.Caption := sDesOptionsForm4Pix;
  RB2.Caption := sDesOptionsForm8Pix;
  RB3.Caption := sDesOptionsForm18pix;
  RB4.Caption := sDesOptionsFormShape;
  RB5.Caption := sDesOptionsFormContents;
  RB6.Caption := sDesOptionsFormPix;
  RB7.Caption := sDesOptionsFormmm;
  RB8.Caption := sDesOptionsFormInch;
  TabSheet1.Caption := sEditor;
  GroupBox6.Caption := sMemoEditor;
  RadioButton1.Caption := sUseMemoFontSettings;
  RadioButton2.Caption := sUseFixedFontSettings;
  Label1.Caption := sFRDesignerFormFontName;
  Label2.Caption := sFRDesignerFormFontSize;
  GroupBox7.Caption := sScriptEditor;
  Label3.Caption := sFRDesignerFormFontName;
  Label4.Caption := sFRDesignerFormFontSize;
  CheckBox1.Caption := sUseSyntaxHighlight;
  FillFonts;
  RadioButton1Change(nil);
end;

procedure TfrDesOptionsForm.RadioButton1Change(Sender: TObject);
begin
  Label1.Enabled:=RadioButton2.Checked;
  Label2.Enabled:=RadioButton2.Checked;
  ComboBox1.Enabled:=RadioButton2.Checked;
  SpinEdit1.Enabled:=RadioButton2.Checked;
end;

procedure TfrDesOptionsForm.FillFonts;
var
  DC: HDC;
  Lf: TLogFont;
begin
  ComboBox1.Clear;
  DC := GetDC(0);
  try
    Lf.lfFaceName := '';
    Lf.lfCharSet := DEFAULT_CHARSET;
    Lf.lfPitchAndFamily := 0;
    EnumFontFamiliesEx(DC, @Lf, @EnumFontsProc, PtrInt(ComboBox1.Items), 0);
  finally
    ReleaseDC(0, DC);
  end;
  ComboBox2.Items.Assign(ComboBox1.Items);
end;

end.

