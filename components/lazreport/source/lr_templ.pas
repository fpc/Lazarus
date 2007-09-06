
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            New Template form            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_Templ;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,
  Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls,ClipBrd,ExtCtrls,

  LR_Const;

type
  TfrTemplNewForm = class(TForm)
    GroupBox2: TGroupBox;
    Panel1: TPanel;
    Image1: TImage;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    Memo1: TMemo;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frTemplNewForm: TfrTemplNewForm;

implementation

procedure TfrTemplNewForm.Button1Click(Sender: TObject);
begin
  OpenDialog1.Filter := sBMPFile + ' (*.bmp)|*.bmp';
  with OpenDialog1 do
  if Execute then
    Image1.Picture.LoadFromFile(FileName);
end;

procedure TfrTemplNewForm.FormActivate(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Image1.Picture.Assign(nil);
  Memo1.SetFocus;
end;

procedure TfrTemplNewForm.FormCreate(Sender: TObject);
begin
  Caption := LoadStr(frRes + 320);
  Label1.Caption := LoadStr(frRes + 321);
  GroupBox2.Caption := LoadStr(frRes + 322);
  Button1.Caption := LoadStr(frRes + 323);
  Button2.Caption := sOk;
  Button3.Caption := sCancel;
end;

initialization
  {$I lr_templ.lrs}

end.

