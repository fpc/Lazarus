unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IntfGraphics, GraphType, FPImage, LCLType, ExtCtrls, ComCtrls, FileUtil;

type
  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Image1: TImage;
    TrackBar1: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    procedure UpdateTrackBar;
  public
  end;
  
var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Image1.Picture.LoadFromFile(ExtractFilePath(ParamStrUTF8(0)) + 'icons' + PathDelim + 'lazarus.ico');
  UpdateTrackbar;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Image1.Picture.LoadFromFile(ExtractFilePath(ParamStrUTF8(0)) + 'icons' + PathDelim + 'lazarus.icns');
  UpdateTrackbar;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Icon.Assign(Image1.Picture.Graphic);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Application.Icon.Assign(Image1.Picture.Graphic);
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  if TCustomIcon(Image1.Picture.Graphic).Count > TrackBar1.Position then
    TCustomIcon(Image1.Picture.Graphic).Current := TrackBar1.Position;
end;

procedure TForm1.UpdateTrackBar;
begin
  TrackBar1.Enabled := Image1.Picture.Graphic is TCustomIcon;
  if TrackBar1.Enabled then
  begin
    TrackBar1.Min := 0;
    TrackBar1.Max := TCustomIcon(Image1.Picture.Graphic).Count - 1;
    TrackBar1.Position := TCustomIcon(Image1.Picture.Graphic).Current;
    TrackBar1Change(TrackBar1);
  end;
end;

end.

