unit lmf experimental;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, Types,
  Graphics, LCLType, LCLIntf, FPCanvas, FPImage, syncobjs;

type
  TlmfList = class;

  TlmfImage=class(TGraphic)
  private
    forgX,forgY,
    fWidth,fHeight:integer;
    kx,ky:double;
    fList:TlmfList;
    fCrs:TCriticalSection;
  protected
    procedure AssignTo(Dest:TPersistent);override;
    function GetWidth:integer;override;
    procedure SetWidth(AVal:integer);override;
    function GetHeight:integer;override;
    procedure SetHeight(AVal:integer);override;
    function GetEmpty:boolean;override;
    function GetTransparent: Boolean; override;
    procedure SetTransparent(Value: Boolean); override;
      //procedure Erase;override;

  public
    constructor Create;override;
    destructor Destroy;override;
    procedure Clear;override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;

    function ScaleSizeX(ax: Integer): Integer;
    function ScaleSizeY(ay: Integer): Integer;
    function ScaleX(ax: Integer): Integer;
    function ScaleY(ay:Integer): Integer;

    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;

    property List:TlmfList read fList;
  end;

  TlmfList = class(TComponent)
  private
    fWidth,fHeight:integer;
  public
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetChildOwner: TComponent; override;
  published
    property Width:integer read fWidth write fWidth;
    property Height:integer read fHeight write fHeight;
  end;

  TlmfCanvas = class(TCanvas)
  private
    fClipRect:TRect;
    fState:TCanvasState;
    fImage:TlmfImage;
  protected
    procedure CreateFont;override;
    procedure CreateBrush;override;
    procedure CreatePen;override;
    function DoCreateDefaultFont : TFPCustomFont; override;
    function DoCreateDefaultPen : TFPCustomPen; override;
    function DoCreateDefaultBrush : TFPCustomBrush; override;
    procedure DoGetTextSize (text:string; var w,h:integer);override;
    function  DoAllowBrush (ABrush : TFPCustomBrush) : boolean; override;
    procedure DoMoveTo(x, y: integer); override;
    procedure DoLineTo(x, y: integer); override;
    procedure DoLine(x1, y1, x2, y2: integer); override;
    //  procedure DoEllipseFill (const Bounds:TRect); override;
    procedure DoEllipse (const Bounds:TRect); override;
//    procedure DoRectangleFill (Const Bounds:TRect); override;
    procedure SetPixel(X,Y: Integer; Value: TColor); override;
    procedure SetColor (x,y:integer; const Value:TFPColor); override;
    function  GetColor (x,y:integer) : TFPColor; override;
    procedure SetClipRect(const AValue: TRect); override;
    function GetClipRect:TRect; override;
    procedure RequiredState(ReqState: TCanvasState); override;

  public
    constructor Create(Almf:TlmfImage);

    procedure StretchDraw(const DestRect: TRect; SrcGraphic: TGraphic); override;

    procedure Ellipse (x1,y1,x2,y2:integer); override;

    procedure FillRect(const ARect: TRect); override; overload;
    procedure FillRect(X1, Y1, X2, Y2: Integer); overload;

    procedure Rectangle(X1,Y1,X2,Y2: Integer); override; // already in fpcanvas

    procedure Polyline(Points: PPoint; NumPts: Integer);override;
    procedure Polygon(Points: PPoint; NumPts: Integer;  Winding: boolean = False);override;

    procedure TextOut (x,y:integer;const text:string); override; // already in fpcanvas
    function TextExtent(const Text: string): TSize;override;
    procedure TextRect(ARect: TRect; X, Y: integer; const Text: string;
      const Style: TTextStyle); override;

  end;


implementation

uses
  lmfObj;

constructor TlmfImage.Create;
begin
  inherited Create;
  fCrs:=syncobjs.TCriticalSection.Create;
  fList:=TlmfList.Create(nil);
end;

destructor TlmfImage.Destroy;
begin
  Clear;
  fList.Free;
  inherited Destroy;
  fCrs.Free;
end;

procedure TlmfImage.AssignTo(Dest:TPersistent);
var
  mf:TMemoryStream;
begin
  if Dest is TlmfImage then
  begin
    mf := TMemoryStream.Create;
    try
      mf.WriteComponent(fList);
      mf.Position := 0;
      mf.ReadComponent(TlmfImage(Dest).fList);
      TlmfImage(Dest).fWidth:=fWidth;
      TlmfImage(Dest).fHeight:=fHeight;
    finally
      mf.Free;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TlmfImage.Clear;
var
  i:integer;
  item:TObject;
begin
  fList.DestroyComponents;
 (* for i:=fList.Count-1 downto 0 do
  begin
    item:=TObject(fList[i]);
    fList.Delete(i);
    item.Free;
  end;*)
end;

function TlmfImage.GetWidth: integer;
begin
  Result := flist.fWidth;
end;

procedure TlmfImage.SetWidth(AVal: integer);
begin
  if (AVal=fWidth) then exit;
  fWidth:=AVal;
  fList.fWidth:=fWidth;
  Self.Modified:=true;
end;

function TlmfImage.GetHeight:integer;
begin
  Result:=fList.fHeight;
end;

function TlmfImage.ScaleSizeX(ax: Integer): Integer;
begin
  Result := round(ax * kx);
end;

function TlmfImage.ScaleSizeY(ay: Integer): Integer;
begin
  Result := round(ay * ky);
end;

function TlmfImage.ScaleX(ax: Integer):integer;
begin
  Result := fOrgX + trunc(ax * kx);
  //if Result>Width then Result:=width;
end;

function TlmfImage.ScaleY(ay: Integer):integer;
begin
  Result := fOrgY + trunc(ay * ky);
  //if Result>height then Result:=height;
end;

procedure TlmfImage.SetHeight(AVal:integer);
begin
  if (AVal=fHeight) then exit;
  fHeight:=AVal;
  fList.fHeight:=fHeight;
  Modified:=true;
end;

function TlmfImage.GetEmpty:boolean;
begin
  Result:=Assigned(fList) and (fList.ComponentCount>0);
end;

procedure TlmfImage.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  i:integer;
begin
  fCrs.Acquire;
  try
    fOrgX:=Rect.Left;
    fOrgY:=Rect.Top;
    kx:=(Rect.Right-Rect.Left)/Width;
    ky:=(Rect.Bottom-Rect.Top)/Height;
    ACanvas.MoveTo(ScaleX(Rect.Left), ScaleY(Rect.Top));
    for i:=0 to fList.ComponentCount-1 do
      TlmfObject(fList.Components[i]).Action(Self, ACanvas);
  finally
    kx:=1;
    ky:=1;
    fCrs.Release;
  end;
end;

function TlmfImage.GetTransparent: Boolean;
begin
  Result:=true; // assume it is always
end;

procedure TlmfImage.SetTransparent(Value: Boolean);
begin
  // nothing to do
end;

procedure TlmfImage.SaveToStream(Stream: TStream);
begin
  Stream.WriteComponent(fList);
end;

procedure TlmfImage.LoadFromStream(Stream: TStream);
begin
  Stream.ReadComponent(fList);
end;


{  TlmfCanvas }

constructor TlmfCanvas.Create(Almf:TlmfImage);
begin
  fImage:=Almf;
  inherited Create;
end;

procedure TlmfCanvas.RequiredState(ReqState: TCanvasState);
var
  Needed: TCanvasState;
begin
  Needed := ReqState - fState;
  if Needed <> [] then
  begin
    if csHandleValid in Needed then
    begin
      RealizeAntialiasing;
      Include(FState, csHandleValid);
    end;
    if csFontValid in Needed then
      CreateFont;
    if csPenValid in Needed then
    begin
      CreatePen;
      if Pen.Style in [psDash, psDot, psDashDot, psDashDotDot]
        then Include(Needed, csBrushValid);
    end;
    if csBrushValid in Needed then
      CreateBrush;
  end;
end;


// workaround

function TlmfCanvas.DoCreateDefaultFont: TFPCustomFont;
begin
  Result := TFont.Create;
  Result.Name := 'Sans';
  Result.Size := 10;
  TFont(Result).Orientation := 0;
end;

function TlmfCanvas.DoCreateDefaultPen: TFPCustomPen;
begin
  Result:=TPen.Create;
  TPen(Result).Color:=clBlack;
  Tpen(Result).Style:=psSolid;
end;

function TlmfCanvas.DoCreateDefaultBrush: TFPCustomBrush;
begin
  Result:=TBrush.Create;
  Result.Style:=bsClear;
  Tbrush(Result).Color:=clNone;
end;

procedure TlmfCanvas.DoMoveTo(x, y: integer);
var
  item:TlmfMoveTo;
begin
  item := TlmfMoveTo.Create(x,y);
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.DoLineTo(x, y: integer);
var
  item: TlmfAnchor;
begin
  RequiredState([csPenValid]);
  item := TlmfLineTo.Create(x,y);
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.DoLine(x1, y1, x2, y2: integer);
var
  item:TlmfAnchor;
begin
  RequiredState([csPenValid]);
  item := TlmfLine.Create(x1,y1,x2,y2);
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.DoEllipse(const Bounds:TRect);
var
  item:TlmfEllipse;
begin
  RequiredState([csPenValid,csBrushValid]);
  item := TlmfEllipse.Create(Bounds);
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.TextOut(x,y:integer;const text:string);
var
  item:TlmfText;
begin
  RequiredState([csFontValid,csBrushValid]);
  item := TlmfText.Create(x,y,text);
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.TextRect(ARect: TRect; X, Y: integer; const Text: string;
  const Style: TTextStyle);
var
  item:TlmfText;
begin
  RequiredState([csFontValid,csBrushValid]);
  item:=TlmfText.Create(x,y,text);
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.StretchDraw(const DestRect: TRect; SrcGraphic: TGraphic);
var
  item:TlmfGraph;
begin
  //RequiredState([csFontValid,csBrushValid]);
  item:=TlmfGraph.Create(nil);
  fImage.fList.InsertComponent(item);
  item.Graph.Assign(SrcGraphic);
  item.Clip := DestRect;
end;

procedure TlmfCanvas.SetColor(x,y:integer; const Value:TFPColor);
var
  item:TlmfAnchor;
begin
  item:=TlmfColor.Create(x,y,Value);
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.SetPixel(X,Y: Integer; Value: TColor);
begin
  SetColor(x,y,TColorToFPColor(Value));
end;

function TlmfCanvas.GetColor(x,y:integer): TFPColor;
begin
  Result.alpha:=0;
  Result.red:=0;
  Result.green:=0;
  Result.blue:=0;
end;

procedure TlmfCanvas.SetClipRect(const AValue: TRect);
var
  item:TlmfObject;
begin
  inherited SetClipRect(AValue);
  fClipRect:=AValue;
  item:=TlmfClip.Create(AValue);
  fImage.fList.InsertComponent(item);
end;

function TlmfCanvas.GetClipRect:TRect;
begin
  Result:=fClipRect;
end;

procedure TlmfCanvas.Rectangle(X1,Y1,X2,Y2: Integer);
var
  item:TlmfObject;
begin
  RequiredState([csPenValid, csBrushValid]); // this adds TlmfPen and TlmfBrush
  item:=TlmfRect.Create(Rect(x1,y1,x2,y2));
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.FillRect(const ARect: TRect);
var
  item: TlmfObject;
begin
  RequiredState([csBrushValid]);
  item := TlmfFillRect.Create(ARect);
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.FillRect(X1, Y1, X2, Y2: Integer);
begin
  FillRect(Rect(X1, Y1, X2, Y2));
end;
  (*
procedure TlmfCanvas.DoRectangleFill(const Bounds:TRect);
var
  item:TlmfObject;
begin
  RequiredState([csBrushValid, csPenValid]); // this adds TlmfBrush and TlmfPen
  item := TlmfRect.Create(nil);
  fImage.fList.InsertComponent(item);
end;
*)
function  TlmfCanvas.DoAllowBrush (ABrush: TFPCustomBrush): boolean;
begin
  Result:=true;
end;

procedure TlmfCanvas.CreateFont;
var
  item:TlmfFont;
begin
  item:=TlmfFont.Create(nil);
  item.Rotation:=TFont(Font).Orientation;
  item.Font.Assign(Font);
  item.Height:=Font.Height;
  item.Name:=Font.Name;
  item.Rotation:=TFont(item.Font).Orientation;

  //writems('Created font "%s" size=%d rot=%d',[item.Font.Name,item.Font.Size,TrotFont(item.Font).Rotation]);
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.CreateBrush;
var
  item:TlmfBrush;
begin
  item:=TlmfBrush.Create(nil);
  item.Brush.Assign(Brush);
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.CreatePen;
var
  item:TlmfPen;
begin
  item:=TlmfPen.Create(nil);
  item.Pen.Assign(Pen);
  fImage.fList.InsertComponent(item);
end;

type
  TSafeObject=class(TObject)
    Font:TFont;
    ext:TSize;
    str:string;
    procedure MeasureExtent;
  end;

  procedure TSafeObject.MeasureExtent;
  var
    dc:HDC;
    ofh:HFONT;
  begin
    dc:=CreateCompatibleDC(0);
    try
      try
        ofh:=SelectObject(dc,Font.Handle);
        GetTextExtentPoint(dc, PChar(str), Length(str), ext);
        SelectObject(dc,ofh);
      finally
        DeleteDC(dc);
      end
    except
    //  writeln('wrong string:',str);
    end;
  end;

function TlmfCanvas.TextExtent(const Text: string): TSize;
var
  so:TSafeObject;
begin
  Result.cX := 0;
  Result.cY := 0;
  if Text='' then exit;
  RequiredState([csHandleValid,csFontValid]);
  so:=TSafeObject.Create;
  try
    so.Font:=Self.Font;
    so.Str:=Text;
    TThread.Synchronize(nil,so.MeasureExtent);
    Result:=so.ext;
  finally
    so.Free;
  end;
end;

procedure TlmfCanvas.DoGetTextSize (text:string; var w,h:integer);
var
  sz:TSize;
begin
  sz:=TextExtent(Text);
  w:=sz.cx;
  h:=sz.cy;
end;

procedure TlmfCanvas.Polyline(Points: PPoint; NumPts: Integer);
var
  item:TlmfPolyLine;
begin
  Changing;
  RequiredState([csHandleValid, csPenValid]);
  item:=TlmfPolyline.Create(Points,NumPts);
  item.Clip := Self.ClipRect;
  fImage.fList.InsertComponent(item);
  Changed;
end;

procedure TlmfCanvas.Polygon(Points: PPoint; NumPts: Integer;
  Winding: boolean = False);
var
  item:TlmfPolygon;
begin
  if NumPts<=0 then exit;
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  item:=TlmfPolygon.Create(Points,NumPts);
  item.Clip := Self.ClipRect;
  fImage.fList.InsertComponent(item);
  Changed;
end;

procedure TlmfCanvas.Ellipse (x1,y1,x2,y2:integer);
begin
  DoEllipse(Rect(x1,y1,x2,y2));
end;


{ LMF list }

procedure TlmfList.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i:integer;
begin
  for i:=0 to ComponentCount-1 do
    Proc(Components[i]);
end;

function TlmfList.GetChildOwner: TComponent;
begin
  Result:=self;
end;


initialization
  RegisterClasses([TlmfList]);

end.

