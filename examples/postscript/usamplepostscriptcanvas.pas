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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  PostScriptCanvas;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
begin
  if Sender=nil then ;
  With TPostscriptCanvas.Create do
  try
    PageHeight:=842;
    PageWidth:=595;
    TopMarging :=40;
    LeftMarging:=20;

    BeginDoc;
    Font.Size:=24;
    Font.Style:=[fsBold,fsItalic,fsUnderline];
    TextOut(100,-10,'PostScript Canvas Lazarus sample');
    Font.Size:=12;
    Brush.Color:=clRed;
    Pen.Width:=1;
    RoundRect(10,10,100,100,8,8);
    Brush.Color:=clMaroon;
    Rectangle(130,10,220,100);
    Font.Name:='Courier';
    Font.Style:=[fsUnderline];
    TextOut(240,20,'Underline text '+#13#10+'sample (יאחט)');
    Font.Style:=[fsUnderline,fsBold];
    TextOut(240,35,'Underline and bold text sample (יאחט)');
    Font.Style:=[fsItalic];
    TextOut(240,50,'Italic text sample (יאחט)');
    Font.Style:=[];
    TextOut(240,65,'Normal text sample (אחטיש)');

    Pen.Style:=psSolid;
    Brush.Color:=clGreen;
    Ellipse(10,260,60,310);

    Brush.Color:=clTeal;
    Brush.Style:=bsSolid;
    RadialPie(10,380,50,50,0,60*16);

    Pen.Style:=psSolid;
    Brush.Color:=clGray;
    Pt[0]:=Point(10,140);
    Pt[1]:=Point(10,240);
    Pt[2]:=Point(140,140);
    Polygon(@Pt,3,True);

    Pen.Style:=psDot;
    Pt1[0]:=Point(10,400);
    Pt1[1]:=Point(50,390);
    Pt1[2]:=Point(120,410);
    Pt1[3]:=Point(180,425);
    Polyline(@Pt1,4);
    Brush.Color:=clAqua;
    Pen.Style:=psSolid;
    Pt1[0]:=Point(10,430);
    PolyBezier(@Pt1,4,true,True);

    TextOut(240,165,'Line style=psSolid');
    Pen.Style:=psSolid;
    Line(360,168,450,168);
    TextOut(240,185,'Line style=psDash');
    Pen.Style:=psDash;
    Line(360,188,450,188);
    TextOut(240,205,'Line style=psDot');
    Pen.Style:=psDot;
    Line(360,208,450,208);
    TextOut(240,225,'Line style=psDashDot');
    Pen.Style:=psDashDot;
    Line(360,228,450,228);
    TextOut(240,245,'Line style=psDashDotDot');
    Pen.Style:=psDashDotDot;
    Line(360,248,450,248);

    Pen.Style:=psSolid;
    Brush.Color:=clBlack;
    Brush.Style:=bsCross;
    Rectangle(240,260,290,300);
    TextOut(300,285,'Brush.Style:=bsCross');
    Brush.Style:=bsDiagCross;
    Rectangle(240,310,290,350);
    TextOut(300,335,'Brush.Style:=bsDiagCross');
    Brush.Style:=bsBDiagonal;
    Rectangle(240,360,290,400);
    TextOut(300,385,'Brush.Style:=bsBDiagonal');
    Brush.Style:=bsFDiagonal;
    Rectangle(240,410,290,450);
    TextOut(300,435,'Brush.Style:=bsFDiagonal');
    Brush.Style:=bsVertical;
    Rectangle(240,460,290,500);
    TextOut(300,485,'Brush.Style:=bsVertical');
    Brush.Style:=bsHorizontal;
    Rectangle(240,510,290,550);
    TextOut(300,535,'Brush.Style:=bsHorizontal');

    Bmp:=TBitMap.Create;
    try
      Bmp.LoadFromFile(ExpandFileName('../../images/LazarusForm.bmp'));
      DRaw(10,450,BMP);
    finally
      Bmp.Free;
    end;

    xpm:=TPixMap.Create;
    try
      xpm.LoadFromFile(ExpandFileName('../../images/lazarus.xpm'));
      StretchDraw(Rect(10,500,110,600),xpm);
    finally
      xpm.Free;
    end;

    NewPage;

    Pen.Color:=clBlack;
    Brush.Color:=clTeal;
    Brush.Style:=bsSolid;
    Chord(10,380,50,50,0,60*16);

    Pen.Style:=psSolid;
    Brush.Color:=clBlack;
    Brush.Style:=bsCross;
    Rectangle(240,260,290,300);
    TextOut(300,285,'Brush.Style:=bsCross');
    Brush.Style:=bsDiagCross;
    Rectangle(240,310,290,350);
    TextOut(300,335,'Brush.Style:=bsDiagCross');
    Brush.Style:=bsBDiagonal;
    Rectangle(240,360,290,400);
    TextOut(300,385,'Brush.Style:=bsBDiagonal');
    Brush.Style:=bsFDiagonal;
    Rectangle(240,410,290,450);
    TextOut(300,435,'Brush.Style:=bsFDiagonal');
    Brush.Style:=bsVertical;
    Rectangle(240,460,290,500);
    TextOut(300,485,'Brush.Style:=bsVertical');
    Brush.Style:=bsHorizontal;
    Rectangle(240,510,290,550);
    TextOut(300,535,'Brush.Style:=bsHorizontal');

    EndDoc;
    SaveToFile('./test1.ps');
  finally
    Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  FName: string;
begin
  if Sender=nil then ;
  if FileExists(ExpandFileName('./test1.ps')) then
  begin
    {$IFDEF MSWINDOWS}
      FName := '"C:\Program Files\Ghostgum\gsview\gsview32" '  + ExpandFileName('./test1.ps');
      ShellExecute(Handle, 'open', PChar(FName), nil, nil, SW_SHOWNORMAL)
    {$ENDIF}
    {$IFDEF UNIX}
      Shell(format('kghostview %s',[ExpandFileName('./test1.ps')]));
    {$ENDIF}
  end;
end;

initialization
  {$I usamplepostscriptcanvas.lrs}

end.

