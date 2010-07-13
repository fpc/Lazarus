
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
  Buttons, StdCtrls,ComCtrls, ButtonPanel,
  LR_Const;

type

  { TfrDesOptionsForm }

  TfrDesOptionsForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    PageControl1: TPageControl;
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
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.lfm}

procedure TfrDesOptionsForm.FormCreate(Sender: TObject);
begin
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
end;

end.

