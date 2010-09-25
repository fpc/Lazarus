
{*****************************************}
{                                         }
{             FastReport v2.3             }
{           Select Band dialog            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_BTyp;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,
  Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, ButtonPanel,

  LR_Class,LR_Const;

type

  { TfrBandTypesForm }

  TfrBandTypesForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    GB1: TGroupBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure bClick(Sender: TObject);
    procedure CreateOptions;
  public
    { Public declarations }
    SelectedTyp: TfrBandType;
  end;

var
  frBandTypesForm: TfrBandTypesForm;

implementation

{$R *.lfm}

uses LR_Desgn;


procedure TfrBandTypesForm.FormCreate(Sender: TObject);
begin
  CreateOptions;
end;

procedure TfrBandTypesForm.FormShow(Sender: TObject);
begin
  //CreateOptions;
end;

procedure TfrBandTypesForm.bClick(Sender: TObject);
begin
  SelectedTyp := TfrBandType((Sender as TComponent).Tag);
end;

procedure TfrBandTypesForm.CreateOptions;
var
  b    : TRadioButton;
  bt   : TfrBandType;
  First: Boolean;
begin
  First := True;
  for bt := btReportTitle to btCrossFooter do
  begin
    b := TRadioButton.Create(GB1);
    b.Parent := GB1;
    b.AutoSize:=True;
    b.Tag := Integer(bt);
    b.Caption := frBandNames[Bt];
    b.OnClick := @bClick;
    b.Enabled := (bt in [btMasterHeader..btSubDetailFooter,
      btGroupHeader, btGroupFooter]) or not frCheckBand(bt);
    if b.Enabled and First then
    begin
      b.Checked := True;
      SelectedTyp := bt;
      First := False;
    end;
  end;

  Caption := sBandTypesFormCapt;
  GB1.Caption := sBandTypesFormBType;
end;

end.

