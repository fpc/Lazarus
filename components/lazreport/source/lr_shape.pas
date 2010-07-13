
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

  LCLType,LR_Class, ExtCtrls, ButtonPanel;


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
    constructor Create; override;
    procedure Assign(From: TfrView); override;
    procedure Draw(aCanvas: TCanvas); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;

  published
    property FillColor;
    property FrameColor;
    property FrameStyle;
    property FrameWidth;

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

constructor TfrShapeView.Create;
begin
  inherited Create;
  Typ := gtAddIn;
  BaseName := 'Shape';
  fShapeType := frstRectangle;
end;

procedure TfrShapeView.Assign(From: TfrView);
begin
  inherited Assign(From);
  ShapeType := TfrShapeView(From).ShapeType;
end;

procedure TfrShapeView.DrawShape(aCanvas : TCanvas);
var
  x1, y1, min : Integer;
  Pts         : Array[0..3] of TPoint;
begin
  x1 := Round((SaveX + SaveDX) * ScaleX + OffsX);
  y1 := Round((SaveY + SaveDY) * ScaleY + OffsY);
  min := dx;
  if dy < dx then
    min := dy;
    
  with aCanvas do
  begin
    Pen.Width := Round(FrameWidth);
    Pen.Color := FrameColor;
    Pen.Style := psSolid;
    Brush.Style := bsSolid;
    Brush.Color := FillColor;
    case ShapeType of
      frstRectangle:
        Rectangle(x, y, x1 + 1, y1 + 1);
      frstRoundRect:
        RoundRect(x, y, x1 + 1, y1 + 1, min div 4, min div 4);
      frstEllipse:
        Ellipse(x, y, x1 + 1, y1 + 1);
      frstTriangle:
        begin
          Pts[0]:=Point(x1, y1);
          Pts[1]:=Point(x, y1);
          Pts[2]:=Point(x + (x1 - x) div 2, y);
          Pts[3]:=Point(x1, y1);
          Polygon(Pts);
        end;
      frstDiagonal1:
      	Line(x,y,x1,y1);
      frstDiagonal2:
      	Line(x,y1,x1,y);
    end;
  end;
end;

procedure TfrShapeView.Draw(aCanvas: TCanvas);
var
  FillC: Integer;
begin
  BeginDraw(aCanvas);
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

