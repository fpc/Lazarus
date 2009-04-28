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
    Label1: TLabel;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

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

    procedure LineSample(txt:string);
    begin
      tmp := PsCanvas.Font.Size div 2;
      PsCanvas.Line(x+170,y+tmp,x+170+100,y+tmp);
      PsCanvas.TextOut(x,y,txt);
      inc(y,tmp*3);
    end;

    procedure BrushSample(txt:string);
    begin
      PsCanvas.Rectangle(x,y,x+50,y+40);
      PsCanvas.TextOut(x+55,y+40-PsCanvas.Font.Size,txt);
      inc(y,50);
    end;

    procedure FontSample(txt:string);
    begin
      PsCanvas.TextOut(x,y,txt);
      inc(y,PsCanvas.Font.Size+4);
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
  PsCanvas.XDPI:=72;
  PsCanvas.YDPI:=72;
  With PsCanvas do
  try
    if ListBox1.ItemIndex=1 then begin
      PageHeight:=792;
      PageWidth:=612;
    end else begin
      PageHeight:=842;
      PageWidth:=595;
    end;
    TopMargin:=40;
    LeftMargin:=20;

    BeginDoc;

    // title
    Font.Size:=24;
    Font.Style:=[fsBold,fsItalic,fsUnderline];
    TextOut(100,-10,'PostScript Canvas Lazarus sample');

    Font.Size:=12;
    Brush.Color:=clRed;
    Pen.Width:=1;
    RoundRect(10,60,60,110,8,8);
    Brush.Color:=clMaroon;
    Rectangle(70,60,170,110);

    FontSamples(200, 60);

    // green ellipse
    Pen.Style:=psSolid;
    Brush.Color:=clGreen;
    Ellipse(10,260,60,310);

    // pie
    Brush.Color:=clTeal;
    Brush.Style:=bsSolid;
    RadialPie(10,360,90,440,0,60*16);

    // polygon: triangle
    Pen.Style:=psSolid;
    Brush.Color:=clGray;
    Pt[0]:=Point(10,140);
    Pt[1]:=Point(10,240);
    Pt[2]:=Point(140,140);
    Polygon(@Pt,3,True);

    // polyline: angle
    Pen.Style:=psDot;
    Pt1[0]:=Point(10,400);
    Pt1[1]:=Point(50,390);
    Pt1[2]:=Point(120,410);
    Pt1[3]:=Point(180,425);
    Polyline(@Pt1,4);

    // bezier
    Brush.Color:=clAqua;
    Pen.Style:=psSolid;
    Pt1[0]:=Point(10,430);
    PolyBezier(@Pt1,4,true,True);

    // line samples

    LineSamples(200,165);

    BrushSamples(240,280);

    Bmp:=TBitMap.Create;
    try
      Bmp.LoadFromFile(ExpandFileNameUTF8('../../images/LazarusForm.bmp'));
      DRaw(10,450,BMP);
    finally
      Bmp.Free;
    end;

    xpm:=TPixMap.Create;
    try
      xpm.LoadFromFile(ExpandFileNameUTF8('../../images/vase_trans.xpm'));
      StretchDraw(bounds(10, 590, round(xpm.Width*0.60),round(xpm.height*0.60)),xpm);
    finally
      xpm.Free;
    end;

    png := TPortableNetworkGraphic.Create;
    try
      png.LoadFromFile('../../images/splash_logo.png');
      StretchDraw(bounds(190, 590, round(png.Width*0.60),round(png.height*0.60)),png);
    finally
      png.Free;
    end;

    NewPage;

    Pen.Color:=clBlack;
    Brush.Color:=clTeal;
    Brush.Style:=bsSolid;
    Chord(10,360,90,440,0,60*16);

    BrushSamples(100,100);

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
      Shell(format('kghostview %s',[ExpandFileNameUTF8('./test1.ps')]));
    {$ENDIF}
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ListBox1.ItemIndex:=0;
end;

initialization
  {$I usamplepostscriptcanvas.lrs}

end.

