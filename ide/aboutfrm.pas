unit aboutfrm; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources, StdCtrls,
Buttons, ExtCtrls;

type
  TAboutForm = class(TForm)
    Memo1: TMEMO;
    Button1: TBUTTON;
    Label1: TLABEL;
  private
    { private declarations }
    FPixmap : TPixmap;
  public
    { public declarations }
    procedure Paint; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end; 

var
  AboutForm: TAboutForm; 

implementation

constructor TAboutForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPixmap := TPixmap.Create;
  FPixmap.LoadFromLazarusResource('lazarus_about_logo');
  Label1.Caption := 'Version #: 0.8.3a';
end;


destructor TAboutForm.Destroy;
begin
  FPixmap.Free;
  FPixmap:=nil;

  inherited Destroy;
end;


procedure TAboutForm.Paint;
begin
  inherited Paint;
  if FPixmap <>nil
  then Canvas.Copyrect(Bounds(12, 36, Width, Height)
    ,FPixmap.Canvas, Rect(0,0, Width, Height));
end;



initialization
  {$I aboutfrm.lrs}
  {$I lazarus_about_logo.lrs}

end.

