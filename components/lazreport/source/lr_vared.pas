
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            Variables editor             }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_Vared;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,
  Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, ButtonPanel,

  LR_Class,LR_Const;

type

  { TfrVaredForm }

  TfrVaredForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Memo1: TMemo;
    Label1: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Doc: TfrReport;
  end;

var
  frVaredForm: TfrVaredForm;

implementation

{$R *.lfm}

procedure TfrVaredForm.FormActivate(Sender: TObject);
begin
  Memo1.Lines.Assign(Doc.Variables);
end;

procedure TfrVaredForm.FormCreate(Sender: TObject);
begin
  Caption := sVaredFormCapt;
  Label1.Caption := sVaredFormCat;
end;

end.

