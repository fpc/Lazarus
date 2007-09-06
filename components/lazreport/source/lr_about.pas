
{*****************************************}
{                                         }
{             FastReport v2.3             }
{              About window               }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_About;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,
  Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls,ExtCtrls,

  LR_Const;

type
  TfrAboutForm = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Bevel1: TBevel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Image1: TImage;
    Bevel2: TBevel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frAboutForm: TfrAboutForm;

implementation

procedure TfrAboutForm.FormCreate(Sender: TObject);
begin
  Caption := sAboutFormCapt;
  Button1.Caption := sOk;
//  Label2.Caption := Label2.Caption + ' build 2';
end;

INITIALIZATION
  {$I lr_about.lrs}

end.

