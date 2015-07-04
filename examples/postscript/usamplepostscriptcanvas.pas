unit uSamplePostScriptCanvas;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
  {$IFDEF UNIX}
    Unix,
  {$ENDIF}
  Classes, SysUtils, types, LazFileUtils, Forms, Graphics,
  Printers, PostScriptCanvas, StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    btnTestFonts: TButton;
    gpOrientation: TRadioGroup;
    txtResX: TEdit;
    txtResY: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListBox1: TListBox;
    procedure btnTestFontsClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    fPsCanvas: TPostscriptCanvas;
    function Sx(AX: Integer): Integer;
    function Sy(AY: Integer): Integer;
    function PointS(Ax,Ay:Integer): TPoint;
    function RectS(Ax1,Ay1,Ax2,AY2: Integer): TRect;
    function Pt2Pix(Pt: Integer): Integer;

    procedure ShowFile(aFile:string);
    procedure SelectPaper(res:Integer=0);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
    Pt : Array[0..2] of TPoint;
    Pt1: Array[0..3] of TPoint;
    Bmp : TBitMap;
    Xpm : TPixMap;
    png : TPortableNetworkGraphic;
    x,y,tmp : Integer;
    R   : TRect;

    procedure LineSample(txt:string);
    begin
      tmp := Pt2Pix(fPsCanvas.Font.Size) div 2;
      fPsCanvas.Line(x+Sx(170),y+tmp,x+Sx(170+100),y+tmp);
      fPsCanvas.TextOut(x,y,txt);
      inc(y,tmp*3);
    end;

    procedure BrushSample(txt:string);
    begin
      fPsCanvas.Rectangle(x,y,x+Sx(50),y+Sy(40));
      fPsCanvas.TextOut(x+Sx(55),y+SY(40)-Pt2Pix(fPsCanvas.Font.Size),txt);
      inc(y,Sy(50));
    end;

    procedure FontSample(txt:string);
    begin
      fPsCanvas.TextOut(x,y,txt);
      inc(y,Pt2Pix(fPsCanvas.Font.Size)+Sy(4));
    end;

    procedure LineSamples(AX,AY:Integer);
    begin
      x := AX;
      y := AY;
      fPsCanvas.Pen.Style:=psSolid;
      LineSample('Line style=psSolid');
      fPsCanvas.Pen.Style:=psDash;
      LineSample('Line style=psDash');
      fPsCanvas.Pen.Style:=psDot;
      LineSample('Line style=psDot');
      fPsCanvas.Pen.Style:=psDashDot;
      LineSample('Line style=psDashDot');
      fPsCanvas.Pen.Style:=psDashDotDot;
      LineSample('Line style=psDashDotDot');
    end;

    procedure BrushSamples(AX,AY:Integer);
    begin
      x := AX;
      y := AY;
      fPsCanvas.Pen.Style:=psSolid;
      fPsCanvas.Brush.Color:=clBlack;

      fPsCanvas.Brush.Style:=bsCross;
      BrushSample('Brush.Style:=bsCross');
      fPsCanvas.Brush.Style:=bsDiagCross;
      BrushSample('Brush.Style:=bsDiagCross');
      fPsCanvas.Brush.Style:=bsBDiagonal;
      BrushSample('Brush.Style:=bsBDiagonal');
      fPsCanvas.Brush.Style:=bsFDiagonal;
      BrushSample('Brush.Style:=bsFDiagonal');
      fPsCanvas.Brush.Style:=bsVertical;
      BrushSample('Brush.Style:=bsVertical');
      fPsCanvas.Brush.Style:=bsHorizontal;
      BrushSample('Brush.Style:=bsHorizontal');
    end;

    procedure FontSamples(AX,AY:Integer);
    begin
      x := AX;
      y := Ay;
      fPsCanvas.Font.Name:='Courier';
      tmp := fPsCanvas.Font.Size;
      fPsCanvas.Font.Style:=[fsUnderline];
      FontSample('Underline text '+#13#10+'sample (éàçè)');
      fPsCanvas.Font.Style:=[fsUnderline,fsBold];
      FontSample('Underline and bold text sample (éàçè)');
      fPsCanvas.Font.Style:=[fsItalic];
      FontSample('Italic Пример текста Random:ęðšćàÀâ¿€ÂáÁçÇñÑüÜ');
      fPsCanvas.Font.Style:=[];
      FontSample('Normal Пример текста Random:ęðšćàÀâ¿€ÂáÁçÇñÑüÜ');
      fPsCanvas.Font.Style:=[fsUnderline,fsBold,fsItalic];
      FontSample('all Пример текста Random:ęðšćàÀâ¿€ÂáÁçÇñÑüÜ');
      fPsCanvas.Font.Style:=[];
    end;
begin
  if Sender=nil then ;
  fPsCanvas := TPostscriptCanvas.Create;
  With fPsCanvas do
  try
    SelectPaper;

    BeginDoc;

    // title
    Font.Size:=24;
    Font.Style:=[fsBold,fsItalic,fsUnderline];
    TextOut(Sx(100),Sy(10),'PostScript Canvas Lazarus sample');

    Font.Size:=12;
    Brush.Color:=clRed;
    Pen.Width:=1;
    RoundRect(Sx(10),Sy(60),Sx(60),Sy(110),Sx(8),Sy(8));
    Brush.Color:=clMaroon;
    Rectangle(Sx(70),Sy(60),Sx(170),Sy(110));

    FontSamples(Sx(200), Sy(60));

    // green ellipse
    Pen.Style:=psSolid;
    Brush.Color:=clGreen;
    Ellipse(Sx(10),Sy(260),Sx(60),Sy(310));

    // pie
    Brush.Color:=clTeal;
    Brush.Style:=bsSolid;
    RadialPie(Sx(10),Sy(360),Sx(90),Sy(440),0,60*16);

    // polygon: triangle
    Pen.Style:=psSolid;
    Brush.Color:=clGray;
    Pt[0]:=PointS(10,140);
    Pt[1]:=PointS(10,240);
    Pt[2]:=PointS(140,140);
    Polygon(@Pt,3,True);

    // polyline: angle
    Pen.Style:=psDot;
    Pt1[0]:=PointS(10,400);
    Pt1[1]:=PointS(50,390);
    Pt1[2]:=PointS(120,410);
    Pt1[3]:=PointS(180,425);
    Polyline(@Pt1,4);

    // bezier
    Brush.Color:=clAqua;
    Pen.Style:=psSolid;
    Pt1[0]:=PointS(10,430);
    PolyBezier(@Pt1,4,true,True);

    // line samples

    LineSamples(Sx(200),Sy(165));

    BrushSamples(Sx(240),Sy(280));

    Bmp:=TBitMap.Create;
    try
      Bmp.LoadFromFile(ExpandFileNameUTF8('../../images/LazarusForm.bmp'));
      DRaw(Sx(10),SY(450),BMP);
    finally
      Bmp.Free;
    end;

    xpm:=TPixMap.Create;
    try
      xpm.LoadFromFile(ExpandFileNameUTF8('../../images/vase_trans.xpm'));
      StretchDraw(bounds(Sx(10), Sy(590), Sx(round(xpm.Width*0.60)),Sy(round(xpm.height*0.60))),xpm);
    finally
      xpm.Free;
    end;

    png := TPortableNetworkGraphic.Create;
    try
      png.LoadFromFile('../../images/splash_logo.png');
      StretchDraw(bounds(Sx(190), Sy(590), Sx(round(png.Width*0.60)),Sy(round(png.height*0.60))),png);
    finally
      png.Free;
    end;

    NewPage;

    Pen.Color:=clBlack;
    Brush.Color:=clTeal;
    Brush.Style:=bsSolid;
    Chord(Sx(10),Sy(360),Sx(90),Sy(440),0,60*16);

    BrushSamples(Sx(100),Sy(100));

    R := RectS(100,500,200,520);
    fPSCanvas.Brush.Style:=bsSolid;
    fPSCanvas.Brush.Color:=clWhite;
    fPSCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    fPSCanvas.ClipRect := R;
    fPSCanvas.TextRect(R, R.Left, R.Top, 'Testing clip rect on TextRect', fPSCanvas.TextStyle);
    fPSCanvas.ClipRect := Rect(0,0,0,0);

    EndDoc;
    SaveToFile('test1.ps');
  finally
    Free;
  end;
end;

procedure TForm1.btnTestFontsClick(Sender: TObject);
const
  arrNames:array[0..3] of string =
    ('Courier', 'Helvetica', 'Times-Roman', 'Symbol');

  function SampleFontName(ax, ay: Integer; aFontName: string): types.TSize;
  const
    SText = 'Tj1';
    ArrStyles: array[0..3] of TFontStyles =
      ([],[fsUnderline],[fsBold],[fsBold,fsUnderline]);
  var
    sz: types.TSize;
    i, fontSize, x, y: Integer;
  begin
    y := Sy(aY);
    x := Sx(ax);
    fPsCanvas.Font.Name:=aFontName;
    fontSize := 16;
    while fontSize<(12*3) do begin
      fPsCanvas.Font.Size := fontSize;
      for i:=0 to 3 do begin
        fPsCanvas.Font.Style:=arrStyles[i];
        fPsCanvas.TextOut(x, y, SText);
        sz := fPsCanvas.TextExtent(SText);
        inc(x, sz.cx);
      end;
      inc(fontSize, 8);
    end;
    inc(y, sz.cy);
    result.cx := x;
    result.cy := y;
    //x := ax;
  end;
var
  i: Integer;
  sz: Types.TSize;
begin
  fPsCanvas := TPostscriptCanvas.Create;
  try
    SelectPaper;

    fPsCanvas.BeginDoc;

    sz.cy := 10;
    for i:=0 to High(ArrNames) do
      sz := SampleFontName(10, sz.cy, ArrNames[i]);

    fPsCanvas.EndDoc;
    fPsCanvas.SaveToFile('fonts.ps');

  finally
    fPsCanvas.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ShowFile('test1.ps');
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  png : TPortableNetworkGraphic;
begin
  fPsCanvas := TPostscriptCanvas.Create;
  SelectPaper(72);

  fPsCanvas.BeginDoc;

  png := TPortableNetworkGraphic.Create;
  try
    png.LoadFromFile('small.png');
    fPsCanvas.StretchDraw(Bounds(50,500,50,50),png);
  finally
    png.Free;
  end;

  fPsCanvas.EndDoc;
  fPsCanvas.SaveToFile('small.ps');
  fPsCanvas.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ListBox1.ItemIndex:=0;
end;

function TForm1.Sx(AX: Integer): Integer;
begin
  // all values were based on 72 dpi
  result := round(fPsCanvas.XDPI/72*Ax);
end;

function TForm1.Sy(AY: Integer): Integer;
begin
  // all values were based on 72 dpi
  result := round(fPsCanvas.YDPI/72*Ay);
end;

function TForm1.PointS(Ax, Ay: Integer): TPoint;
begin
  Result.X:= Sx(Ax);
  Result.Y:= Sy(Ay);
end;

function TForm1.RectS(Ax1, Ay1, Ax2, AY2: Integer): TRect;
begin
  Result.TopLeft := PointS(AX1,AY1);
  Result.BottomRight := PointS(AX2,AY2);
end;

function TForm1.Pt2Pix(Pt: Integer): Integer;
begin
  result := round(fPSCanvas.YDPI/72*Pt);
end;

procedure TForm1.ShowFile(aFile: string);
begin
  aFile := ExpandFileNameUTF8('./'+aFile);
  if FileExistsUTF8(aFile) then
  begin
    {$IFDEF MSWINDOWS}
      ShellExecute(Handle, 'open', PChar(afile), nil, nil, SW_SHOWNORMAL);
    {$ENDIF}
    {$IFDEF UNIX}
      FpExecL('/usr/bin/evince', [aFile]);
      //Shell(format('gv %s',[aFile]));
    {$ENDIF}
  end;
end;

procedure TForm1.SelectPaper(res:Integer=0);
begin
  with fPsCanvas do begin
    if res=0 then begin
      XDPI := StrToIntDef(txtResX.Text,300);
      YDPI := StrToIntDef(txtResY.Text,300);
    end else begin
      XDPI := res;
      YDPI := res;
    end;
    if ListBox1.ItemIndex=1 then begin
      PaperHeight:=Sx(792);
      PaperWidth:=Sy(612);
    end else begin
      PaperHeight:=Sy(842);
      PaperWidth:=Sx(595);
    end;
    TopMargin:=Sy(40);
    LeftMargin:=Sx(20);

    case gpOrientation.ItemIndex of
      1: Orientation := poReversePortrait;
      2: Orientation := poLandscape;
      3: Orientation := poReverseLandscape;
      else Orientation := poPortrait;
    end;

  end;
end;

end.

