
{*****************************************}
{                                         }
{             FastReport v2.3             }
{         Checkbox Add-In Object          }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_Shape;

interface

{$I lr_vers.inc}

uses
  Classes, SysUtils, LResources,
  Graphics,GraphType, Controls, Forms, Dialogs,Buttons,
  StdCtrls,

  LCLType,LCLIntf,LR_Class, ExtCtrls, ButtonPanel;


type

  { TfrShapeObject }

  TfrShapeObject = class(TComponent)  // fake component
  public
    Constructor Create(aOwner : TComponent); override;
  end;

  TfrShapeType=(frstRectangle,frstRoundRect,frstEllipse,frstTriangle,
                frstDiagonal1, frstDiagonal2);
  
  { TfrShapeView }

  TfrShapeView = class(TfrView)
  private
    fShapeType: TfrShapeType;

    procedure DrawShape(aCanvas : TCanvas);
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(aCanvas: TCanvas); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    function  GetClipRgn(rt: TfrRgnType): HRGN; override;

  published
    property FillColor;
    property FrameColor;
    property FrameStyle;
    property FrameWidth;
    property Restrictions;

    property ShapeType : TfrShapeType Read fShapeType write fShapeType;
  end;

  { TfrShapeForm }

  TfrShapeForm = class(TfrObjEditorForm)
    ButtonPanel1: TButtonPanel;
    GroupBox1: TGroupBox;
    CB1: TComboBox;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowEditor(t: TfrView); override;
  end;


var
  frShapeForm: TfrShapeForm;

implementation

{$R *.lfm}

uses LR_Const;

procedure DumpRgn(Msg:string;Rgn: HRGN);
var
  res: LongInt;
  R: TRect;
  y: LongInt;
  x: LongInt;
  ch: char;
  //Line: string;
begin
  res := GetRgnBox(Rgn, @R);
  //SetLength(Line, R.Right-R.Left+1;
  WriteLn(msg);
  Write('    ');
  for x := R.Left to R.Right do begin
    ch := chr(ord('0')+(x div 10));
    if ch='0' then
      ch := ' ';
    Write(ch);
  end; WriteLn;
  Write('    ');
  for x := R.Left to R.Right do
    Write(Chr(ord('0')+(x mod 10))); WriteLn;

  for y := R.Top to R.Bottom do begin
    Write(y:3,' ');
    for x :=R.Left to R.Right do begin
      if PtInRegion(Rgn, X, Y) then
        Write('1') //Line[x-R.Left+1] := '1'
      else
        Write('0'); //Line[x-R.Left+1] := '0';
    end; WriteLn;
  end;
end;

constructor TfrShapeView.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  Typ := gtAddIn;
  BaseName := 'Shape';
  fShapeType := frstRectangle;
end;

procedure TfrShapeView.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TfrShapeView then
    ShapeType := TfrShapeView(Source).ShapeType;
end;

procedure TfrShapeView.DrawShape(aCanvas : TCanvas);
var
  x1, y1, min, fw : Integer;
  Pts         : Array[0..3] of TPoint;
begin
  x1 := Round((SaveX + SaveDX) * ScaleX + OffsX);
  y1 := Round((SaveY + SaveDY) * ScaleY + OffsY);
  fw := Round(FrameWidth);
  min := dx;
  if dy < dx then
    min := dy;
    
  with aCanvas do
  begin
    Pen.Width := Round(FrameWidth);
    Pen.Color := FrameColor;
    if FrameStyle = frsDouble then
      Pen.Style := psSolid
    else
      Pen.Style := TPenStyle(FrameStyle);
    Brush.Style := bsSolid;
    Brush.Color := FillColor;
    case ShapeType of
      frstRectangle:
        if FrameStyle = frsDouble then
        begin
          Rectangle(x - fw, y - fw, x1 + 1 + fw, y1 + 1 + fw);
          Rectangle(x + fw, y + fw, x1 + 1 - fw, y1 + 1 - fw);
        end
        else
          Rectangle(x, y, x1 + 1, y1 + 1);
      frstRoundRect:
        if FrameStyle = frsDouble then
        begin
          RoundRect(x - fw, y - fw, x1 + 1 + fw, y1 + 1 + fw, (min + 2 * fw) div 4, min div 4);
          RoundRect(x + fw, y + fw, x1 + 1 - fw, y1 + 1 - fw, (min - 2 * fw) div 4, min div 4);
        end
        else
          RoundRect(x, y, x1 + 1, y1 + 1, min div 4, min div 4);
      frstEllipse:
        if FrameStyle = frsDouble then
        begin
          Ellipse(x - fw, y - fw, x1 + 1 + fw, y1 + 1 + fw);
          Ellipse(x + fw, y + fw, x1 + 1 - fw, y1 + 1 - fw);
        end
        else
          Ellipse(x, y, x1 + 1, y1 + 1);
      frstTriangle:
        if FrameStyle = frsDouble then
        begin
          Pts[0]:=Point(x1 + fw * 2, y1 + fw);
          Pts[1]:=Point(x - fw * 2, y1 + fw);
          Pts[2]:=Point(x + (x1 - x) div 2, y - fw * 2);
          Pts[3]:=Point(x1 + fw * 2, y1 + fw);
          Polygon(Pts);

          Pts[0]:=Point(x1 - fw * 2, y1 - fw);
          Pts[1]:=Point(x + fw * 2, y1 - fw);
          Pts[2]:=Point(x + (x1 - x) div 2, y + fw * 2);
          Pts[3]:=Point(x1 - fw * 2, y1 - fw);
          Polygon(Pts);
        end
        else
        begin
          Pts[0]:=Point(x1, y1);
          Pts[1]:=Point(x, y1);
          Pts[2]:=Point(x + (x1 - x) div 2, y);
          Pts[3]:=Point(x1, y1);
          Polygon(Pts);
        end;
      frstDiagonal1:
        if FrameStyle = frsDouble then
        begin
          Line(x,y-fw,x1,y1-fw);
          Line(x,y+fw,x1,y1+fw);
        end
        else
          Line(x,y,x1,y1);
      frstDiagonal2:
        if FrameStyle = frsDouble then
        begin
          Line(x,y1+fw,x1,y+fw);
          Line(x,y1-fw,x1,y-fw);
        end
        else
          Line(x,y1,x1,y);
    end;
  end;
end;

procedure TfrShapeView.Draw(aCanvas: TCanvas);
var
  FillC: Integer;
  OldPen: TPen;
  OldBrush: TBrush;
begin
  OldPen := TPen.Create;
  OldPen.Assign(aCanvas.Pen);
  OldBrush := TBrush.Create;
  OldBrush.Assign(aCanvas.Brush);
  BeginDraw(aCanvas);
  aCanvas.AntialiasingMode:=amOn;
  Memo1.Assign(Memo);
  BeginUpdate;
  try
    CalcGaps;
    FillC := FillColor;
    FillColor := clNone;
    Frames :=[];
    //ShowBackground;
    FillColor := FillC;
    DrawShape(aCanvas);
    RestoreCoord;
  finally
    EndUpdate;
    aCanvas.Brush.Assign(OldBrush);
    aCanvas.Pen.Assign(OldPen);
    OldBrush.Free;
    OldPen.Free;
  end;
end;

procedure TfrShapeView.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  Stream.Read(fShapeType, 1);
end;

procedure TfrShapeView.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(ShapeType, 1);
end;

procedure TfrShapeView.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  
  RestoreProperty('ShapeType',XML.GetValue(Path+'ShapeType/Value',''));
end;

procedure TfrShapeView.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);

  XML.SetValue(Path+'ShapeType/Value', GetSaveProperty('ShapeType'));
end;

function TfrShapeView.GetClipRgn(rt: TfrRgnType): HRGN;
const
  Delta = 10;
var
  xp : Integer;
  Pts         : Array[0..6] of TPoint;
  min, bx, by, bx1, by1, w1, w2, fw: Integer;
begin
  w1 := Round(FrameWidth / 2);
  w2 := Round((FrameWidth - 1) / 2);
  fw := Round(FrameWidth);
  bx:=x;
  by:=y;
  bx1:=x+dx+1;
  by1:=y+dy+1;

  case ShapeType of

    frstRoundRect:
      begin
        Inc(bx1, w2);
        Inc(by1, w2);
        Dec(bx, w1);
        Dec(by, w1);

        if FrameStyle = frsDouble then
        begin
          Dec(bx, fw);
          Dec(by, fw);
          Inc(bx1, fw);
          Inc(by1, fw);
        end;

        min := dx;
        if dy < dx then
          min := dy;

        if rt=rtExtended then begin
          min := min + 2 * delta;
          result := CreateRoundRectRgn(bx-delta, by-delta, bx1+delta, by1+delta, min div 4, min div 4)
        end
        else
          result := CreateRoundRectRgn(bx, by, bx1, by1, min div 4, min div 4);
      end;

    frstEllipse, frstRectangle:
      begin
        Inc(bx1, w2);
        Inc(by1, w2);
        Dec(bx, w1);
        Dec(by, w1);

        if FrameStyle = frsDouble then
        begin
          Dec(bx, fw);
          Dec(by, fw);
          Inc(bx1, fw);
          Inc(by1, fw);
        end;

        if rt=rtExtended then begin
          if ShapeType=frstRectangle then
            result := CreateRectRgn(bx-Delta, by-Delta, bx1 + Delta, by1 + Delta)
          else
            result := CreateEllipticRgn(bx-Delta, by-Delta, bx1 + Delta, by1 + Delta)
        end else begin
          if ShapeType=frstRectangle then
            result := CreateRectRgn(bx, by, bx1, by1)
          else
            result := CreateEllipticRgn(bx, by, bx1, by1);
        end;
      end;

    frstTriangle:
      begin
        Inc(bx1, w2);
        Inc(by1, w2);
        Dec(bx, w1);
        Dec(by, w1);

        if FrameStyle = frsDouble then
        begin
          Dec(bx, fw * 2);
          Dec(by, fw * 2);
          Inc(bx1, fw * 2);
          Inc(by1, fw * 2);
        end;

        xp := bx + (bx1 - bx) div 2;
        if rt=rtExtended then
        begin
          Pts[0]:=Point(bx1+Delta, by1+Delta);
          Pts[1]:=Point(bx-Delta, by1+Delta);
          Pts[2]:=Point(xp, by-Delta);
          Pts[3]:=Point(bx1+Delta, by1+Delta);
        end else begin
          Pts[0]:=Point(bx1, by1);
          Pts[1]:=Point(bx, by1);
          Pts[2]:=Point(xp, by);
          Pts[3]:=Point(bx1, by1);
        end;
        result := CreatePolygonRgn(@Pts, 4, 1);
      end;

    frstDiagonal1: //Line(x,y,x1,y1);
      begin
        if FrameStyle = frsDouble then
        begin
          Dec(by, fw);
          Inc(by1, fw);
        end;
        if w1=0 then
          w1 := 1; // avoid disappearing  line
        if rt=rtExtended then
        begin
          Pts[0]:=Point(bx-w1-Delta, by);
          Pts[1]:=Point(bx+w2+Delta, by);
          Pts[2]:=Point(bx1+w2+Delta, by1);
          Pts[3]:=Point(bx1-w1-Delta, by1);
          Pts[4]:=Point(bx-w1-Delta, by);
        end else begin
          Pts[0]:=Point(bx-w1, by);
          Pts[1]:=Point(bx+w2, by);
          Pts[2]:=Point(bx1+w2, by1);
          Pts[3]:=Point(bx1-w1, by1);
          Pts[4]:=Point(bx-w1, by);
        end;
        result := CreatePolygonRgn(@Pts, 5, 1);
      end;

    frstDiagonal2: //Line(x,y1,x1,y);
      begin
        if FrameStyle = frsDouble then
        begin
          Dec(by, fw);
          Inc(by1, fw);
        end;
        if w1=0 then
          w1 := 1; // avoid disappearing  line
        if rt=rtExtended then begin
          Pts[0]:=Point(bx-w1-Delta, by1);
          Pts[1]:=Point(bx+w2+Delta, by1);
          Pts[2]:=Point(bx1+w2+Delta, by);
          Pts[3]:=Point(bx1-w1-Delta, by);
          Pts[4]:=Point(bx-w1-Delta,by);
        end else begin
          Pts[0]:=Point(bx-w1, by1);
          Pts[1]:=Point(bx+w2, by1);
          Pts[2]:=Point(bx1+w2, by);
          Pts[3]:=Point(bx1-w1, by);
          Pts[4]:=Point(bx-w1,by1);
        end;
        result := CreatePolygonRgn(@Pts, 5, 1);
      end
  end;
end;


{------------------------------------------------------------------------}
procedure TfrShapeForm.ShowEditor(t: TfrView);
begin
  CB1.Items.Clear;
  CB1.Items.Add(sShape1);
  CB1.Items.Add(sShape2);
  CB1.Items.Add(sShape3);
  CB1.Items.Add(sShape4);
  CB1.Items.Add(sShape5);
  CB1.Items.Add(sShape6);
  CB1.ItemIndex:=0;
  
  with TfrShapeView(t) do
  begin
    CB1.ItemIndex :=Ord(ShapeType);
    if ShowModal = mrOk then
      ShapeType :=TfrShapeType(CB1.ItemIndex);
  end;
end;

procedure TfrShapeForm.FormCreate(Sender: TObject);
begin
  Caption := sShapeFormCaption;
  GroupBox1.Caption := sShapeFormKind;
end;

{ TfrShapeObject }
constructor TfrShapeObject.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  
  if not assigned(frShapeForm) {and not (csDesigning in ComponentState)} then
  begin
    frShapeForm:=TfrShapeForm.Create(nil);
    frRegisterObject(TfrShapeView, frShapeForm.Image1.Picture.Bitmap,
      sInsShape, frShapeForm);
  end;
end;

initialization

  frShapeForm:=nil;

finalization

  if Assigned(frShapeForm) then
    frShapeForm.Free;

end.

