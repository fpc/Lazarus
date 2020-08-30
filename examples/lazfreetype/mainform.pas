unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Spin, fpimage, LCLType,

  IntfGraphics, GraphType,      //Intf basic routines

  EasyLazFreeType,  LazFreeTypeIntfDrawer,  //EasyFreeType with Intf
  LazFreeTypeFontCollection
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox_SingleLine: TRadioButton;
    CheckBox_Para: TRadioButton;
    CheckBox_Rot: TRadioButton;
    Label1: TLabel;
    LAngle: TLabel;
    LFontSize: TLabel;
    Panel_Option: TPanel;
    SpinEdit_Zoom: TSpinEdit;
    TrackBar_Size: TTrackBar;
    procedure CheckBox_SingleLineChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_ZoomChange(Sender: TObject);
    procedure TrackBar_SizeChange(Sender: TObject);
  private
    procedure UpdateSizeLabel;
    procedure UpdateAngleLabel;
  public
    lazimg: TLazIntfImage;
    drawer: TIntfFreeTypeDrawer;
    ftFont1,ftFont2,ftFont3: TFreeTypeFont;
    mx,my: integer; //mouse position
    fSin,fCos: double;
    fAngle: double;
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

procedure TForm1.CheckBox_SingleLineChange(Sender: TObject);
begin
  LAngle.Enabled := CheckBox_Rot.Checked;
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
  if CheckBox_Rot.Checked then
  begin
    fAngle := ArcTan2(Panel_Option.Top/2 - my, mx - ClientWidth/2);
    SinCos(fAngle, fSin, fCos);
    fAngle := RadToDeg(fAngle);
    if fAngle<0 then fAngle += 360;
    UpdateAngleLabel;
  end;
  invalidate;
end;

procedure TForm1.UpdateSizeLabel;
begin
  LFontSize.Caption := inttostr(TrackBar_Size.Position)+'pt';
  if ftFont1 <> nil then ftFont1.SizeInPoints := TrackBar_Size.Position;
  if ftFont2 <> nil then ftFont2.SizeInPoints := TrackBar_Size.Position;
  if ftFont3 <> nil then ftFont3.SizeInPoints := TrackBar_Size.Position;
end;

procedure TForm1.UpdateAngleLabel;
begin
  LAngle.Caption := format('%3.0fº',[fAngle]);
end;

procedure TForm1.FormPaint(Sender: TObject);
const testtext = 'The'#13#10'quick brown fox jumps over the lazy dog';
var bmp: TBitmap;
    tx,ty: integer;
    p: array of TCharPosition;
    x,y,h,rx,ry: single;
    i: integer;
    StartTime,EndTime,EndTime2: TDateTime;
    zoom: integer;
    Orientation: Integer;

    procedure Rotate(dx,dy:single);
    begin
      rx := tx/2 + (fCos*dx - fSin*dy);
      ry := ty/2 - (fSin*dx + fCos*dy);
    end;

begin
  if lazimg = nil then exit;
  canvas.Font.Name := 'Comic Sans MS';

  zoom := SpinEdit_Zoom.Value;
  StartTime := Now;

  if not CheckBox_Rot.Checked then
    Orientation := 0
  else
    Orientation := round(fAngle*10);

  tx := ClientWidth div zoom;
  ty := Panel_Option.Top div zoom;
  if (lazimg.Width <> tx) or (lazimg.Height <> ty) then
    lazimg.SetSize(tx,ty);

  drawer.FillPixels(TColorToFPColor(clWhite));

  x := mx/zoom;
  y := my/zoom;
  if CheckBox_Rot.Checked then
  begin
    h := 0.0;
    if ftFont1<>nil then h += ftFont1.LineFullHeight;
    if ftFont2<>nil then h += ftFont2.LineFullHeight;
    if ftFont3<>nil then h += ftFont3.LineFullHeight;
    h := h / 2;
  end;

  if ftFont1<>nil then
  begin
    ftFont1.Hinted := true;
    ftFont1.ClearType := not CheckBox_Rot.Checked; // ClearType and rotation is not working yet
    ftFont1.Quality := grqHighQuality;
    ftFont1.SmallLinePadding := false;
    ftFont1.Orientation := Orientation;
    if CheckBox_Para.Checked then
      drawer.DrawTextRect(testtext, ftFont1, 0,0, tx/3,ty, colBlack, [ftaLeft, ftaBottom])
    else if CheckBox_SingleLine.Checked then
      drawer.DrawText(ftFont1.Information[ftiFullName], ftFont1, x, y, colBlack, [ftaRight, ftaBottom])
    else begin
      Rotate( -ftFont1.TextWidth(ftFont1.Information[ftiFullName])/2,
              h - ftFont1.LineFullHeight);
      drawer.DrawText(ftFont1.Information[ftiFullName], ftFont1, rx, ry,   colBlack);
    end;
  end;

  if ftFont2<>nil then
  begin
    ftFont2.Hinted := false;
    ftFont2.ClearType := false;
    ftFont2.Quality := grqHighQuality;
    ftFont2.Orientation := Orientation;
    if CheckBox_Para.Checked then
      drawer.DrawTextRect(testtext, ftFont2, tx/3,0, 2*tx/3,ty, colRed, [ftaCenter, ftaVerticalCenter])
    else if CheckBox_SingleLine.Checked then
      drawer.DrawText(ftFont2.Information[ftiFullName], ftFont2, x, y, colRed, 192, [ftaCenter, ftaBaseline])
    else begin
      Rotate( -ftFont2.TextWidth(ftFont2.Information[ftiFullName])/2,
              -ftFont2.LineFullHeight/2);
      drawer.DrawText(ftFont2.Information[ftiFullName], ftFont2, rx, ry, colRed);
    end;
  end;

  if ftFont3<>nil then begin
    ftFont3.Hinted := false;
    ftFont3.ClearType := false;
    ftFont3.Quality := grqMonochrome;
    ftFont3.Orientation := Orientation;
    if CheckBox_Para.Checked then
      drawer.DrawTextRect(testtext, ftFont3, 2*tx/3,0, tx,ty, colBlue, [ftaRight, ftaTop])
    else if CheckBox_SingleLine.Checked then
      drawer.DrawText(ftFont3.Information[ftiFullName]+' '+ftFont3.VersionNumber, ftFont3, x, y, colBlack, 128, [ftaLeft, ftaTop])
    else begin
      Rotate( -ftFont3.TextWidth(ftFont3.Information[ftiFullName])/2, - h );
      drawer.DrawText(ftFont3.Information[ftiFullName], ftFont3, rx, ry, colBlue);
    end;
  end;

  if (ftFont1<>nil) and CheckBox_SingleLine.Checked then
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
  if CheckBox_Rot.Checked then
  begin
    Canvas.Pen.Color := clFuchsia;
    Canvas.Line(ClientWidth div 2, Panel_Option.Top div 2, mx, my);
  end;
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

