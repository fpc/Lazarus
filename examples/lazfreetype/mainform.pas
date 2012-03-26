unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Spin, fpimage,

  IntfGraphics, GraphType,      //Intf basic routines

  EasyLazFreeType,  LazFreeTypeIntfDrawer  //EasyFreeType with Intf

  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    LFontSize: TLabel;
    Panel_Option: TPanel;
    SpinEdit_Zoom: TSpinEdit;
    TrackBar_Size: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure SpinEdit_ZoomChange(Sender: TObject);
    procedure TrackBar_SizeChange(Sender: TObject);
  private
    procedure UpdateSizeLabel;
    { private declarations }
  public
    { public declarations }
    lazimg: TLazIntfImage;
    drawer: TIntfFreeTypeDrawer;
    ftFont1,ftFont2,ftFont3: TFreeTypeFont;
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

  lazimg := TLazIntfImage.Create(0,0, [riqfRGB]);
  drawer := TIntfFreeTypeDrawer.Create(lazimg);
  ftFont1 := nil;
  ftFont2 := nil;
  ftFont3 := nil;

  try
    ftFont1 := TFreeTypeFont.Create;
    ftFont1.Name := 'arial.ttf';

    ftFont2 := TFreeTypeFont.Create;
    ftFont2.Name := 'timesi.ttf';

    ftFont3 := TFreeTypeFont.Create;
    ftFont3.Name := 'verdana.ttf';
  except
    on ex: Exception do
    begin
      FreeAndNil(drawer);
      FreeAndNil(lazimg);
      FreeAndNil(ftFont1);
      FreeAndNil(ftFont2);
      FreeAndNil(ftFont3);
      MessageDlg('Font error',ex.Message,mtError,[mbOk],0);
    end;
  end;

  UpdateSizeLabel;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ftFont1.Free;
  ftFont2.Free;
  ftFont3.Free;
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
  if ftFont1 <> nil then ftFont1.SizeInPoints := TrackBar_Size.Position;
  if ftFont2 <> nil then ftFont2.SizeInPoints := TrackBar_Size.Position;
  if ftFont3 <> nil then ftFont3.SizeInPoints := TrackBar_Size.Position;
end;

procedure TForm1.FormPaint(Sender: TObject);
const testtext = 'Enjoy'+LineEnding+'and play!';
var bmp: TBitmap;
    tx,ty: integer;
    p: array of TCharPosition;
    x,y: single;
    i: integer;
    StartTime,EndTime,EndTime2: TDateTime;
    zoom: integer;
begin
  if lazimg = nil then exit;
  canvas.Font.Name := 'Comic Sans MS';

  zoom := SpinEdit_Zoom.Value;
  StartTime := Now;

  tx := ClientWidth div zoom;
  ty := Panel_Option.Top div zoom;
  if (lazimg.Width <> tx) or (lazimg.Height <> ty) then
    lazimg.SetSize(tx,ty);

  drawer.FillPixels(TColorToFPColor(clWhite));

  x := mx/zoom;
  y := my/zoom;

  ftFont1.Hinted := true;
  ftFont1.ClearType := true;
  ftFont1.Quality := grqHighQuality;
  ftFont1.SmallLinePadding := false;
  drawer.DrawText(ftFont1.Information[ftiFullName], ftFont1, x, y, colBlack, [ftaRight, ftaBottom]);

  ftFont2.Hinted := false;
  ftFont2.ClearType := false;
  ftFont2.Quality := grqHighQuality;
  drawer.DrawText(ftFont2.Information[ftiFullName], ftFont2, x, y, colRed, 192, [ftaCenter, ftaBaseline]);

  ftFont3.Hinted := false;
  ftFont3.ClearType := false;
  ftFont3.Quality := grqMonochrome;
  drawer.DrawText(ftFont3.Information[ftiFullName]+' '+ftFont3.VersionNumber, ftFont3, x, y, colBlack, 128, [ftaLeft, ftaTop]);

  p := ftFont1.CharsPosition(ftFont1.Information[ftiFullName],[ftaRight, ftaBottom]);
  for i := 0 to high(p) do
  begin
    drawer.DrawVertLine(round(x+p[i].x),round(y+p[i].yTop),round(y+p[i].yBottom), TColorToFPColor(clBlue));
    drawer.DrawHorizLine(round(x+p[i].x),round(y+p[i].yBase),round(x+p[i].x+p[i].width), TColorToFPColor(clBlue));
  end;

  EndTime := Now;

  bmp := TBitmap.Create;
  bmp.LoadFromIntfImage(lazimg);
  Canvas.StretchDraw(rect(0,0,lazimg.width*zoom,lazimg.height*zoom),bmp);
  bmp.Free;

  EndTime2 := Now;

  Canvas.TextOut(0,0, inttostr(round((EndTime-StartTime)*24*60*60*1000))+' ms + '+inttostr(round((EndTime2-EndTime)*24*60*60*1000))+' ms');

end;

procedure TForm1.SpinEdit_ZoomChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TForm1.TrackBar_SizeChange(Sender: TObject);
begin
  UpdateSizeLabel;
  Invalidate;
end;

{$R *.lfm}

end.

