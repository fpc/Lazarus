unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Spin, fpimage, LCLType,

  IntfGraphics, GraphType,      //Intf basic routines

  EasyLazFreeType,  LazFreeTypeIntfDrawer,  //EasyFreeType with Intf
  LazFreeTypeFontCollection
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox_Rect: TCheckBox;
    Label1: TLabel;
    LFontSize: TLabel;
    Panel_Option: TPanel;
    SpinEdit_Zoom: TSpinEdit;
    TrackBar_Size: TTrackBar;
    procedure CheckBox_RectChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_ZoomChange(Sender: TObject);
    procedure TrackBar_SizeChange(Sender: TObject);
  private
    procedure UpdateSizeLabel;
  public
    lazimg: TLazIntfImage;
    drawer: TIntfFreeTypeDrawer;
    ftFont1,ftFont2,ftFont3: TFreeTypeFont;
    mx,my: integer; //mouse position
    procedure EraseBackground(DC: HDC); override;
    procedure SetupFonts;
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.EraseBackground(DC: HDC);
begin
  // empty
end;

procedure TForm1.SetupFonts;
const
  defFonts:array[1..3] of string[13] = ('arial.ttf','timesi.ttf','verdana.ttf');
var
  n: Integer;
  LastFileName: string;

  function LoadFont: TFreeTypeFont;
  var
    FileName, FontFamilyName: string;
  begin
    result := nil;
    inc(n);
    FileName := defFonts[n];
    if not FileExists(FileName) then begin
      if (ParamCount>=n) then begin
        FileName := ParamStr(n);
        if not FileExists(FileName) then
          exit;
      end else
      if LastFileName<>'' then
        FileName := LastFileName
      else
        exit;
    end;
    FontFamilyName := FontCollection.AddFile(FileName).Family.FamilyName;
    result := TFreeTypeFont.Create;
    result.Name := FontFamilyName;
    LastFileName:= FileName;
  end;

begin

  try
    n := 0;
    LastFileName := '';
    ftFont1 := LoadFont;
    ftFont2 := LoadFont;
    ftFont3 := LoadFont;
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

  if (ftFont1=nil) and (ftFont2=nil) and (ftFont3=nil) then
    ShowMessage('This program needs up to 3 font filenames on the command line');

  UpdateSizeLabel;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  mx := clientwidth div 2;
  my := clientheight div 2;

  lazimg := TLazIntfImage.Create(0,0, [riqfRGB]);
  drawer := TIntfFreeTypeDrawer.Create(lazimg);
  ftFont1 := nil;
  ftFont2 := nil;
  ftFont3 := nil;
end;

procedure TForm1.CheckBox_RectChange(Sender: TObject);
begin
  invalidate;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ftFont1.Free;
  ftFont2.Free;
  ftFont3.Free;
  drawer.Free;
  lazimg.Free;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
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
const testtext = 'The'#13#10'quick brown fox jumps over the lazy dog';
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

  if ftFont1<>nil then
  begin
    ftFont1.Hinted := true;
    ftFont1.ClearType := true;
    ftFont1.Quality := grqHighQuality;
    ftFont1.SmallLinePadding := false;
    if CheckBox_Rect.Checked then
      drawer.DrawTextRect(testtext, ftFont1, 0,0, tx/3,ty, colBlack, [ftaLeft, ftaBottom])
    else
      drawer.DrawText(ftFont1.Information[ftiFullName], ftFont1, x, y, colBlack, [ftaRight, ftaBottom]);
  end;

  if ftFont2<>nil then
  begin
    ftFont2.Hinted := false;
    ftFont2.ClearType := false;
    ftFont2.Quality := grqHighQuality;
    if CheckBox_Rect.Checked then
      drawer.DrawTextRect(testtext, ftFont2, tx/3,0, 2*tx/3,ty, colRed, [ftaCenter, ftaVerticalCenter])
    else
      drawer.DrawText(ftFont2.Information[ftiFullName], ftFont2, x, y, colRed, 192, [ftaCenter, ftaBaseline]);
  end;

  if ftFont3<>nil then begin
    ftFont3.Hinted := false;
    ftFont3.ClearType := false;
    ftFont3.Quality := grqMonochrome;
    if CheckBox_Rect.Checked then
      drawer.DrawTextRect(testtext, ftFont3, 2*tx/3,0, tx,ty, colBlue, [ftaRight, ftaTop])
    else
      drawer.DrawText(ftFont3.Information[ftiFullName]+' '+ftFont3.VersionNumber, ftFont3, x, y, colBlack, 128, [ftaLeft, ftaTop]);
  end;

  if (ftFont1<>nil) and not CheckBox_Rect.Checked then
  begin
    p := ftFont1.CharsPosition(ftFont1.Information[ftiFullName],[ftaRight, ftaBottom]);
    for i := 0 to high(p) do
    begin
      drawer.DrawVertLine(round(x+p[i].x),round(y+p[i].yTop),round(y+p[i].yBottom), TColorToFPColor(clBlue));
      drawer.DrawHorizLine(round(x+p[i].x),round(y+p[i].yBase),round(x+p[i].x+p[i].width), TColorToFPColor(clBlue));
    end;
  end;

  EndTime := Now;

  bmp := TBitmap.Create;
  bmp.LoadFromIntfImage(lazimg);
  Canvas.StretchDraw(rect(0,0,lazimg.width*zoom,lazimg.height*zoom),bmp);
  bmp.Free;

  EndTime2 := Now;

  Canvas.TextOut(0,0, inttostr(round((EndTime-StartTime)*24*60*60*1000))+' ms + '+inttostr(round((EndTime2-EndTime)*24*60*60*1000))+' ms');

end;

procedure TForm1.FormShow(Sender: TObject);
begin
  SetupFonts;
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

