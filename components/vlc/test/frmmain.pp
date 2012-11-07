unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  EditBtn, StdCtrls, lclvlc, vlc, libvlc;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bplay: TButton;
    BStop: TButton;
    BPause: TButton;
    BResume: TButton;
    FEVideo: TFileNameEdit;
    Label1: TLabel;
    PVideo: TPanel;
    procedure BPauseClick(Sender: TObject);
    procedure BplayClick(Sender: TObject);
    procedure BResumeClick(Sender: TObject);
    procedure BStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FPlayer : TLCLVlcPlayer;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPlayer:=TLCLVLCPlayer.Create(Self);
  FPlayer.ParentWindow:=PVideo;
end;

procedure TForm1.BplayClick(Sender: TObject);
begin
  Fplayer.PlayFile(FEVideo.FileName);
end;

procedure TForm1.BResumeClick(Sender: TObject);
begin
  FPlayer.Resume;
end;

procedure TForm1.BPauseClick(Sender: TObject);
begin
  FPLayer.Pause;
end;

procedure TForm1.BStopClick(Sender: TObject);
begin
  FPlayer.Stop;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPlayer);
end;

end.

