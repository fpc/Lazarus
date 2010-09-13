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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, PostScriptCanvas, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    txtResX: TEdit;
    txtResY: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
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
    PsCanvas: TPostscriptCanvas;
    R   : TRect;

    function Sx(AX: Integer): Integer;
    begin
      // all values were based on 72 dpi
      result := round(PsCanvas.XDPI/72*Ax);
    end;

    function Sy(AY: Integer): Integer;
    begin
      // all values were based on 72 dpi
      result := round(PsCanvas.YDPI/72*Ay);
    end;

    function PointS(Ax,Ay:Integer): TPoint;
    begin
      Result.X:= Sx(Ax);
      Result.Y:= Sy(Ay);
    end;

    function RectS(Ax1,Ay1,Ax2,AY2: Integer): TRect;
    begin
      Result.TopLeft := PointS(AX1,AY1);
      Result.BottomRight := PointS(AX2,AY2);
    end;

    function Pt2Pix(Pt: Integer): Integer;
    begin
      result := round(PSCanvas.YDPI/72*Pt);
    end;

    procedure LineSample(txt:string);
    begin
      tmp := Pt2Pix(PsCanvas.Font.Size) div 2;
      //WriteLn('LineSample: Y=',Y,' FontSize=', PsCanvas.Font.Size,' Pix=',tmp*2,' Spc= ',tmp,' Sy=',Sy(Tmp));
      PsCanvas.Line(x+Sx(170),y+tmp,x+Sx(170+100),y+tmp);
      PsCanvas.TextOut(x,y,txt);
      inc(y,tmp*3);
    end;

    procedure BrushSample(txt:string);
    begin
      PsCanvas.Rectangle(x,y,x+Sx(50),y+Sy(40));
      PsCanvas.TextOut(x+Sx(55),y+SY(40)-Pt2Pix(PsCanvas.Font.Size),txt);
      inc(y,Sy(50));
    end;

    procedure FontSample(txt:string);
    begin
      PsCanvas.TextOut(x,y,txt);
      inc(y,Pt2Pix(PsCanvas.Font.Size)+Sy(4));
    end;

    procedure LineSamples(AX,AY:Integer);
    begin
      x := AX;
      y := AY;
      PsCanvas.Pen.Style:=psSolid;
      LineSample('Line style=psSolid');
      PsCanvas.Pen.Style:=psDash;
      LineSample('Line style=psDash');
      PsCanvas.Pen.Style:=psDot;
      LineSample('Line style=psDot');
      PsCanvas.Pen.Style:=psDashDot;
      LineSample('Line style=psDashDot');
      PsCanvas.Pen.Style:=psDashDotDot;
      LineSample('Line style=psDashDotDot');
    end;

    procedure BrushSamples(AX,AY:Integer);
    begin
      x := AX;
      y := AY;
      PsCanvas.Pen.Style:=psSolid;
      PsCanvas.Brush.Color:=clBlack;

      PsCanvas.Brush.Style:=bsCross;
      BrushSample('Brush.Style:=bsCross');
      PsCanvas.Brush.Style:=bsDiagCross;
      BrushSample('Brush.Style:=bsDiagCross');
      PsCanvas.Brush.Style:=bsBDiagonal;
      BrushSample('Brush.Style:=bsBDiagonal');
      PsCanvas.Brush.Style:=bsFDiagonal;
      BrushSample('Brush.Style:=bsFDiagonal');
      PsCanvas.Brush.Style:=bsVertical;
      BrushSample('Brush.Style:=bsVertical');
      PsCanvas.Brush.Style:=bsHorizontal;
      BrushSample('Brush.Style:=bsHorizontal');
    end;

    procedure FontSamples(AX,AY:Integer);
    begin
      x := AX;
      y := Ay;
      PsCanvas.Font.Name:='Courier';
      tmp := PsCanvas.Font.Size;
      PsCanvas.Font.Style:=[fsUnderline];
      FontSample('Underline text '+#13#10+'sample (éàçè)');
      PsCanvas.Font.Style:=[fsUnderline,fsBold];
      FontSample('Underline and bold text sample (éàçè)');
      PsCanvas.Font.Style:=[fsItalic];
      FontSample('Italic Пример текста Random:ęðšćàÀâ¿€ÂáÁçÇñÑüÜ');
      PsCanvas.Font.Style:=[];
      FontSample('Normal Пример текста Random:ęðšćàÀâ¿€ÂáÁçÇñÑüÜ');
      PsCanvas.Font.Style:=[fsUnderline,fsBold,fsItalic];
      FontSample('all Пример текста Random:ęðšćàÀâ¿€ÂáÁçÇñÑüÜ');
      PsCanvas.Font.Style:=[];
    end;
begin
  if Sender=nil then ;
  PsCanvas := TPostscriptCanvas.Create;
  psCanvas.XDPI := StrToIntDef(txtResX.Text,300);
  psCanvas.YDPI := StrToIntDef(txtResY.Text,300);
  With PsCanvas do
  try
    if ListBox1.ItemIndex=1 then begin
      PaperHeight:=Sx(792);
      PaperWidth:=Sy(612);
    end else begin
      PaperHeight:=Sy(842);
      PaperWidth:=Sx(595);
    end;
    TopMargin:=Sy(40);
    LeftMargin:=Sx(20);

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
    PSCanvas.Brush.Style:=bsSolid;
    PSCanvas.Brush.Color:=clWhite;
    PSCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    PSCanvas.ClipRect := R;
    PSCanvas.TextRect(R, R.Left, R.Top, 'Testing clip rect on TextRect', PSCanvas.TextStyle);
    PSCanvas.ClipRect := Rect(0,0,0,0);

    EndDoc;
    SaveToFile('test1.ps');
  finally
    Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  FName: string;
begin
  if Sender=nil then ;
  if FileExistsUTF8(ExpandFileNameUTF8('./test1.ps')) then
  begin
    {$IFDEF MSWINDOWS}
      FName := '"C:\Program Files\Ghostgum\gsview\gsview32" '  + ExpandFileNameUTF8('./test1.ps');
      ShellExecute(Handle, 'open', PChar(FName), nil, nil, SW_SHOWNORMAL)
    {$ENDIF}
    {$IFDEF UNIX}
      Shell(format('gv %s',[ExpandFileNameUTF8('./test1.ps')]));
    {$ENDIF}
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
    png : TPortableNetworkGraphic;
    x,y,tmp : Integer;
    PsCanvas: TPostscriptCanvas;
begin
  psCanvas := TPostscriptCanvas.Create;
  psCanvas.XDPI := 72;
  psCanvas.YDPI := 72;
  psCanvas.PaperHeight:=842;
  psCanvas.PaperWidth:=595;
  psCanvas.TopMargin:=40;
  psCanvas.LeftMargin:=20;
  psCanvas.BeginDoc;

  png := TPortableNetworkGraphic.Create;
  try
    png.LoadFromFile('small.png');
    psCanvas.StretchDraw(Bounds(50,500,50,50),png);
  finally
    png.Free;
  end;

  psCanvas.EndDoc;
  psCanvas.SaveToFile('small.ps');
  psCanvas.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ListBox1.ItemIndex:=0;
end;

end.

