unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Forms, Graphics, Dialogs, ComCtrls, ExtCtrls, LCLType, StdCtrls, Buttons, DialogRes, ImgList;

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

  end; 

var
  Form1: TForm1; 

implementation

{$R unit1.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  DialogTrack.SetParams(Low(TDialogImage), Low(TDialogImage), High(TDialogImage));
  UpdateDialogImage;

  ButtonTrack.SetParams(Low(TButtonImage), Low(TButtonImage), High(TButtonImage));
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
begin
  DialogImage.ImageIndex := DialogGlyphs.DialogIcon[DialogTrack.Position];
  DialogImage.Images := DialogGlyphs;
  DialogCaptionLbl.Caption := GetDialogCaption(DialogTrack.Position);
end;

procedure TForm1.UpdateButtonImage;
begin
  ButtonImage.ImageIndex := GetButtonImageIndex(ButtonTrack.Position);
  ButtonImage.Images := LCLGlyphs;
  ButtonCaptionLbl.Caption := GetButtonCaption(ButtonTrack.Position);
end;

end.

