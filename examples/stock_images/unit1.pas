unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, LCLType, Themes, StdCtrls, Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    ButtonImage: TImage;
    ButtonTrack: TTrackBar;
    DialogImage: TImage;
    DialogTrack: TTrackBar;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    DialogCaptionLbl: TLabel;
    ButtonCaptionLbl: TLabel;
    procedure ButtonTrackChange(Sender: TObject);
    procedure DialogTrackChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure UpdateDialogImage;
    procedure UpdateButtonImage;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  DialogTrack.Min := idDialogWarning - idDialogBase;
  DialogTrack.Max := idDialogShield - idDialogBase;
  DialogTrack.Position := DialogTrack.Min;
  UpdateDialogImage;

  ButtonTrack.Min := idButtonOk - idButtonBase;
  ButtonTrack.Max := idButtonShield - idButtonBase;
  ButtonTrack.Position := ButtonTrack.Min;
  UpdateButtonImage;
end;

procedure TForm1.DialogTrackChange(Sender: TObject);
begin
  UpdateDialogImage;
end;

procedure TForm1.ButtonTrackChange(Sender: TObject);
begin
  UpdateButtonImage;
end;

procedure TForm1.UpdateDialogImage;
var
  B: TCustomBitmap;
begin
  B := GetDialogIcon(DialogTrack.Position + idDialogBase);
  if B <> nil then
  begin
    DialogImage.Picture.Graphic := B;
    B.Free;
  end
  else
    DialogImage.Picture.Clear;
  DialogCaptionLbl.Caption := GetDialogCaption(DialogTrack.Position + idDialogBase);
end;

procedure TForm1.UpdateButtonImage;
var
  B: TCustomBitmap;
begin
  B := GetButtonIcon(ButtonTrack.Position + idButtonBase);
  if B <> nil then
  begin
    ButtonImage.Picture.Graphic := B;
    B.Free;
  end
  else
    ButtonImage.Picture.Clear;
  ButtonCaptionLbl.Caption := GetButtonCaption(ButtonTrack.Position + idButtonBase);
end;

initialization
  {$I unit1.lrs}

end.

