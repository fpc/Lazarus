unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, fpimage,

  IntfGraphics, GraphType,      //Intf basic routines

  EasyLazFreeType,  LazFreeTypeIntfDrawer;  //EasyFreeType with Intf

type

  { TForm1 }

  TForm1 = class(TForm)
    LFontSize: TLabel;
    Panel_Option: TPanel;
    TrackBar_Size: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure TrackBar_SizeChange(Sender: TObject);
  private
    procedure UpdateSizeLabel;
    { private declarations }
  public
    { public declarations }
    lazimg: TLazIntfImage;
    drawer: TIntfFreeTypeDrawer;
    ftFont: TFreeTypeFont;
    mx,my: integer; //mouse position
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  mx := clientwidth div 2;
  my := clientheight div 2;

  lazimg := TLazIntfImage.Create(0,0, [riqfRGB,riqfAlpha]);
  drawer := TIntfFreeTypeDrawer.Create(lazimg);
  ftFont := nil;

  try
    ftFont := TFreeTypeFont.Create; //only one font at once for now...
    ftFont.Name := 'arial.ttf';
  except
    on ex: Exception do
    begin
      FreeAndNil(drawer);
      FreeAndNil(lazimg);
      FreeAndNil(ftFont);
      MessageDlg('Font error',ex.Message,mtError,[mbOk],0);
    end;
  end;

  UpdateSizeLabel;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ftFont.Free;
  drawer.Free;
  lazimg.Free;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  mx := X;
  my := Y;
  invalidate;
end;

procedure TForm1.UpdateSizeLabel;
begin
  LFontSize.Caption := inttostr(TrackBar_Size.Position)+'pt';
  if ftFont <> nil then ftFont.SizeInPoints := TrackBar_Size.Position;
end;

procedure TForm1.FormPaint(Sender: TObject);
const zoom = 1;
      testtext = 'Hello world!';
var bmp: TBitmap;
    tx,ty: integer;
    w: array of single;
    x,y,y2: single;
    i: integer;
begin
  if lazimg = nil then exit;

  tx := ClientWidth div zoom;
  ty := Panel_Option.Top div zoom;
  if (lazimg.Width <> tx) or (lazimg.Height <> ty) then
    lazimg.SetSize(tx,ty);
  lazimg.FillPixels(TColorToFPColor(clWhite));

  ftFont.Hinted := true;
  ftFont.ClearType := true;
  ftFont.Quality := grqHighQuality;
  x := mx/zoom - ftFont.TextWidth(testtext)/2;
  y := round(my/zoom);
  drawer.DrawText(testtext, ftFont, x, y, colBlack, 255);
  w := ftFont.CharsWidth(testtext);
  y2 := y-ftFont.SizeInPixels;
  drawer.DrawVertLine(round(x),round(y),round(y2), TColorToFPColor(clBlue));
  for i := 0 to high(w) do
  begin
    x += w[i];
    drawer.DrawVertLine(round(x),round(y),round(y2), TColorToFPColor(clBlue));
  end;

  ftFont.Hinted := false;
  ftFont.ClearType := false;
  ftFont.Quality := grqLowQuality;
  drawer.DrawText(testtext, ftFont, mx/zoom - ftFont.TextWidth(testtext)/4, my/zoom + ftFont.SizeInPixels/2, colRed, 192);
  ftFont.Quality := grqMonochrome;
  drawer.DrawText(testtext, ftFont, mx/zoom, my/zoom + ftFont.SizeInPixels, colBlack, 128);

  bmp := TBitmap.Create;
  bmp.LoadFromIntfImage(lazimg);
  Canvas.StretchDraw(rect(0,0,lazimg.width*zoom,lazimg.height*zoom),bmp);
  bmp.Free;
end;

procedure TForm1.TrackBar_SizeChange(Sender: TObject);
begin
  UpdateSizeLabel;
  Invalidate;
end;

{$R *.lfm}

end.

