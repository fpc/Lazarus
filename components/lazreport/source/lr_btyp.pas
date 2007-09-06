
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
  Buttons, StdCtrls,

  LR_Class,LR_Const;

type
  TfrBandTypesForm = class(TForm)
    Button1: TButton;
    GB1: TGroupBox;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure bClick(Sender: TObject);
  public
    { Public declarations }
    SelectedTyp: TfrBandType;
  end;

var
  frBandTypesForm: TfrBandTypesForm;

implementation

uses LR_Desgn;


procedure TfrBandTypesForm.FormCreate(Sender: TObject);
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

    if Integer(bt) > 10 then
    begin
      b.Left := 160;
      b.Top := (Integer(bt) - 11) * 20 + 20;
    end
    else
    begin
      b.Left := 8;
      b.Top := Integer(bt) * 20 + 20;
    end;

    b.Tag := Integer(bt);
    b.Caption := frBandNames[Bt];
    b.Width   := 140;
    b.OnClick := @bClick;
    b.Enabled := (bt in [btMasterHeader..btSubDetailFooter,
      btGroupHeader, btGroupFooter]) or not frCheckBand(bt);
    b.AdjustSize;
    if b.Enabled and First then
    begin
      b.Checked := True;
      SelectedTyp := bt;
      First := False;
    end;
  end;
  
  Caption := sBandTypesFormCapt;
  GB1.Caption := sBandTypesFormBType;
  Button1.Caption := sOk;
  Button2.Caption := sCancel;
end;

procedure TfrBandTypesForm.bClick(Sender: TObject);
begin
  SelectedTyp := TfrBandType((Sender as TComponent).Tag);
end;

INITIALIZATION
  {$I lr_btyp.lrs}

end.

