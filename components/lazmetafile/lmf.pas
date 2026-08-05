unit lmf experimental;

{$mode delphi}{$H+}

interface

uses
  Types, Classes, SysUtils, Graphics,FPCanvas, FPImage, syncobjs;

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

    function ScaleX(ax:integer):integer;
    function ScaleY(ay:integer):integer;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure Clear;override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;

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

  TlmfObject = class(TComponent)
  public
    procedure Action(fImage: TlmfImage; ACanvas:TCanvas); virtual; abstract;
  end;

  TlmfAnchor = class(TlmfObject)
  private
    fPos:TPoint;
  public
    constructor Create(Ax,Ay:integer);virtual;
  published
    property px:integer read fPos.x write fpos.x;
    property py:integer read fPos.y write fpos.y;
  end;

  TlmfMoveTo = class(TlmfAnchor)
  public
    procedure Action(fImage:TlmfImage; ACanvas:TCanvas); override;
  end;

  TlmfLineTo = class(TlmfAnchor)
  public
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas); override;
  end;

  TlmfLine = class(TlmfAnchor)
  private
    fEndPos:TPoint;
  public
    constructor Create(x1,y1,x2,y2:integer);overload;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  published
    property px1:integer read fEndPos.x write fEndpos.x;
    property py1:integer read fEndPos.y write fEndpos.y;
  end;

  TlmfText=class(TlmfAnchor)
  private
    fText:string;
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(x,y:integer; const AText:string);overload;
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas);override;
  published
    property Text:string read fText write fText;
  end;

  TlmfColor=class(TlmfAnchor)
  private
    fColor:TfpColor;
  public
    constructor Create(x,y:integer; AColor:TfpColor);overload;
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas);override;
  published
    property r:word read fColor.red write fColor.red;
    property g:word read fColor.green write fColor.green;
    property b:word read fColor.blue write fColor.blue;
    property a:word read fColor.alpha write fColor.alpha;
  end;

  TlmfClip=class(TlmfObject)
  private
    fClip:TRect;
  public
    constructor Create(AClip:TRect);virtual;overload;
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas);override;
  published
    property Left:integer read fClip.Left write fClip.Left;
    property Top:integer read fClip.Top write fClip.Top;
    property Right:integer read fClip.Right write fClip.Right;
    property Bottom:integer read fClip.Bottom write fClip.Bottom;
  end;

  TlmfRect=class(TlmfClip)
  public
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas);override;
  end;

  TlmfFillRect=class(TlmfRect)
  public
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas);override;
  end;

  TlmfEllipse=class(TlmfClip)
  public
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas);override;
  end;

  TlmfFont=class(TlmfObject)
  private
    fFont:TFont;
    fHeight,fRotation:integer;
    fName:string;
  public
    constructor Create(AnOwner:TComponent);override;
    destructor Destroy;override;
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas);override;
  published
    property Font:TFont read fFont write fFont;
    property Height:integer read fHeight write fHeight;
    property Rotation:integer read fRotation write fRotation;
  end;

  TlmfBrush=class(TlmfObject)
  private
    fBrush:TBrush;
  public
    constructor Create(AnOwner:TComponent);override;
    destructor Destroy;override;
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas);override;
  published
    property Brush:TBrush read fBrush write fBrush;
  end;

  TlmfPen=class(TlmfObject)
  private
    fPen:TPen;
  public
    constructor Create(AnOwner:TComponent);override;
    destructor Destroy;override;
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas);override;
  published
    property Pen:TPen read fPen write fPen;
  end;

  TlmfGraph=class(TlmfClip)
  private
    fGraph:TPicture;
  public
    constructor Create(AnOwner:TComponent);override;
    destructor Destroy;override;
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas);override;
  published
    property Graph:TPicture read fGraph write fGraph;
  end;

  TlmfPolyline=class(TlmfRect)
  private
    pts:array of TPoint;
  protected
    procedure StorePoints(AStream:TStream);virtual;
    procedure LoadPoints(AStream:TStream);virtual;
    procedure DefineProperties(Afiler:TFiler);override;
  public
    constructor Create(Points:PPoint;NumPts:integer);overload;
    destructor Destroy;override;
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas);override;
  end;

  TlmfPolygon=class(TlmfPolyline)
  private
    fWinding:boolean;
  public
    constructor Create(Points:PPoint;NumPts:integer;Winding:boolean=false);overload;
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas);override;
  published
    property Winding:boolean read fWinding write fWinding;
  end;

  TlmfCanvas=class(TCanvas)
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
    procedure DoRectangleFill (Const Bounds:TRect); override;
    procedure SetPixel(X,Y: Integer; Value: TColor); override;
    procedure SetColor (x,y:integer; const Value:TFPColor); override;
    function  GetColor (x,y:integer) : TFPColor; override;
    procedure SetClipRect(const AValue: TRect); override;
    function GetClipRect:TRect; override;
    procedure RequiredState(ReqState: TCanvasState); override;

  public
    constructor Create(Almf:TlmfImage);
    procedure TextOut (x,y:integer;const text:string); override; // already in fpcanvas
    function TextExtent(const Text: string): TSize;override;
    procedure TextRect(ARect: TRect; X, Y: integer; const Text: string;
      const Style: TTextStyle); override;
    procedure StretchDraw(const DestRect: TRect; SrcGraphic: TGraphic); override;
    procedure Rectangle(X1,Y1,X2,Y2: Integer); override; // already in fpcanvas
    procedure Polyline(Points: PPoint; NumPts: Integer);override;
    procedure Polygon(Points: PPoint; NumPts: Integer;  Winding: boolean = False);override;
    procedure Ellipse (x1,y1,x2,y2:integer); override;
  end;


implementation

uses
  lcltype,lclintf;

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

function TlmfImage.ScaleX(ax:integer):integer;
begin
  Result:=fOrgX+trunc(ax*kx);
  //if Result>Width then Result:=width;
end;

function TlmfImage.ScaleY(ay:integer):integer;
begin
  Result:=fOrgY+trunc(ay*ky);
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
  item.fGraph.Assign(SrcGraphic);
  item.fClip := DestRect;
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
  RequiredState([csPenValid]); // this adds TlmfPen
  item:=TlmfRect.Create(Rect(x1,y1,x2,y2));
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.DoRectangleFill(const Bounds:TRect);
var
  item:TlmfFillRect;
begin
  RequiredState([csBrushValid,csPenvalid]); // this adds TlmfBrush, TlmfPen
  item:=TlmfFillRect.Create(nil);
  fImage.fList.InsertComponent(item);
end;

function  TlmfCanvas.DoAllowBrush (ABrush: TFPCustomBrush): boolean;
begin
  Result:=true;
end;

procedure TlmfCanvas.CreateFont;
var
  item:TlmfFont;
begin
  item:=TlmfFont.Create(nil);
  item.fRotation:=TFont(Font).Orientation;
  item.Font.Assign(Font);
  item.fHeight:=Font.Height;
  item.fName:=Font.Name;
  item.fRotation:=TFont(item.Font).Orientation;

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
  item.fClip:=Self.ClipRect;
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
  item.fClip:=Self.ClipRect;
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


{ LMF object }
constructor TlmfAnchor.Create(Ax,Ay:integer);
begin
  inherited Create(nil);
  fPos.X:=Ax;
  fPos.Y:=Ay;
end;


{ TlmfMoveTo}

procedure TlmfMoveTo.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.MoveTo(fImage.ScaleX(fPos.X), fImage.ScaleY(fPos.Y));
end;


{ TlmfLineTo }

procedure TlmfLineTo.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.LineTo(fImage.ScaleX(fPos.X), fImage.ScaleY(fPos.Y));
end;


{ TlmfLine }

constructor TlmfLine.Create(x1,y1,x2,y2:integer);
begin
  inherited Create(x1,y1);
  fEndPos.X:=x2;
  fEndPos.Y:=y2;
end;

procedure TlmfLine.Action(fImage:TlmfImage;ACanvas:TCanvas);
begin
  ACanvas.Line(
    fImage.ScaleX(fPos.X),
    fImage.ScaleY(fPos.Y),
    fImage.ScaleX(fEndPos.X),
    fImage.ScaleY(fEndPos.Y));
end;


constructor TlmfText.Create(x,y:integer; const AText:string);
begin
  inherited Create(x,y);
  fText:=AText;
end;

procedure TlmfText.Action(fImage:TlmfImage;ACanvas:TCanvas);
var
  fnt:TFont;
  ofh:Hfont;
begin
(*	if (fRotation<>0) then
  begin
  	fnt:=CreateOrtFont(round(fImage.ky*fHeight),fRotation div 10,ACanvas.Font.PixelsPerInch);
    Acanvas.Font.Assign(fnt);
    Acanvas.Font.Name:='Arial';
	  {$message 'This is font-selection workaround'}
  	ofh:=SelectObject(ACanvas.Handle,fnt.Handle);
    ACanvas.TextOut(fImage.ScaleX(fPos.X),fImage.ScaleY(fPos.Y),fText);
    ofh:=SelectObject(ACanvas.Handle,ofh);
    fnt.Free;
  end
  else
  begin
	  ACanvas.Font.Height:=round(fImage.ky*fHeight);
  	ACanvas.TextOut(fImage.ScaleX(fPos.X),fImage.ScaleY(fPos.Y),fText);
  end;*)
  ACanvas.TextOut(fImage.ScaleX(fPos.X),fImage.ScaleY(fPos.Y),fText);
end;

procedure TlmfText.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
end;

// pixel mode
constructor TlmfColor.Create(x,y:integer; AColor:TfpColor);
begin
  inherited Create(x,y);
  fColor:=AColor;
end;

procedure TlmfColor.Action(fImage:TlmfImage;ACanvas:TCanvas);
begin
  ACanvas.Colors[fImage.ScaleX(fpos.x), fImage.ScaleY(fpos.y)] := fColor;
end;

// cliprect
constructor TlmfClip.Create(AClip:TRect);
begin
  inherited Create(nil);
  fClip:=AClip;
end;

procedure TlmfClip.Action(fImage:TlmfImage;ACanvas:TCanvas);
var
  newClip:TRect;
begin
  // reset the clipping
  if (fClip.Left=0) and (fClip.Top=0) and (fClip.Right=MaxInt) and (fClip.Bottom=MaxInt) then
  begin
    // this clip rect have not to scale
    ACanvas.ClipRect:=fClip; // actually does clipping through virtualization
    SelectClipRgn(ACanvas.Handle,0)
  end
  else
  begin
    newClip:=Rect(
      fImage.ScaleX(fClip.Left),
      fImage.ScaleY(fClip.Top),
      fImage.Scalex(fClip.Right),
      fImage.ScaleY(fClip.Bottom)
    );

    ACanvas.ClipRect:=newClip; // actually does nothing

    // this is real clipping
    lclintf.IntersectClipRect(ACanvas.Handle,
    	newClip.Left,newClip.Top,newClip.Right,newClip.Bottom);
  end;
end;

// rectangle
procedure TlmfRect.Action(fImage:TlmfImage; ACanvas:TCanvas);
begin
 // ACanvas.Brush.Style:=bsClear;
  ACanvas.Rectangle(
    fImage.ScaleX(fClip.Left),
    fImage.ScaleY(fClip.Top),
    fImage.Scalex(fClip.Right),
    fImage.ScaleY(fClip.Bottom)
  );
end;

procedure TlmfFillRect.Action(fImage:TlmfImage;ACanvas:TCanvas);
begin
  ACanvas.FillRect(
    fImage.ScaleX(fClip.Left),
    fImage.ScaleY(fClip.Top),
    fImage.Scalex(fClip.Right),
    fImage.ScaleY(fClip.Bottom)
  );
end;

procedure TlmfEllipse.Action(fImage:TlmfImage;ACanvas:TCanvas);
begin
  ACanvas.Ellipse(
    fImage.ScaleX(fClip.Left),
    fImage.ScaleY(fClip.Top),
    fImage.Scalex(fClip.Right),
    fImage.ScaleY(fClip.Bottom)
  );
end;

constructor TlmfFont.Create(AnOwner:TComponent);
begin
  inherited Create(AnOwner);
  fFont:=TFont.Create;
end;

destructor TlmfFont.Destroy;
begin
  fFont.Free;
  inherited Destroy;
end;

procedure TlmfFont.Action(fImage: TlmfImage; ACanvas: TCanvas);
var
  AFont:TFont;
  rot,ht:integer;
  ofh:Hfont;
begin
  rot:=fRotation;//TRotFont(fFont).Rotation;

  Acanvas.Font.Assign(fFont);
  ht:=abs(round(fImage.ky*fHeight));
  if ht<=0 then ht:=1;
  ACanvas.Font.Height:=-ht;
  ACanvas.Font.Orientation:=rot;
end;


{ TlmfBrush }

constructor TlmfBrush.Create(AnOwner:TComponent);
begin
  inherited Create(AnOwner);
  fBrush := TBrush.Create;
end;

destructor TlmfBrush.Destroy;
begin
  fBrush.Free;
  inherited Destroy;
end;

procedure TlmfBrush.Action(fImage:TlmfImage;ACanvas:TCanvas);
begin
  ACanvas.Brush.Assign(fBrush);
end;


{ TlmfPen }

constructor TlmfPen.Create(AnOwner:TComponent);
begin
  inherited Create(AnOwner);
  fPen := TPen.Create;
end;

destructor TlmfPen.Destroy;
begin
  fPen.Free;
  inherited Destroy;
end;

procedure TlmfPen.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.Pen.Assign(fPen);
  ACanvas.Pen.Width := round(fImage.ky * fPen.Width);
end;


{ TlmfGraph }

constructor TlmfGraph.Create(AnOwner:TComponent);
begin
  inherited Create(AnOwner);
  fGraph := TPicture.Create;
end;

destructor TlmfGraph.Destroy;
begin
  fGraph.Free;
  inherited Destroy;
end;

procedure TlmfGraph.Action(fImage:TlmfImage;ACanvas:TCanvas);
begin
  ACanvas.StretchDraw(
    Rect(
      fImage.ScaleX(fClip.Left),
      fImage.ScaleY(fClip.Top),
      fImage.ScaleX(fClip.Right),
      fImage.ScaleY(fClip.Bottom)
    ),
    fGraph.Graphic
  );
end;


{ TlmfPolyLine }

constructor TlmfPolyLine.Create(Points:PPoint;NumPts:integer);
begin
  inherited Create(nil);
  Setlength(pts,numPts);
  System.Move(Points^,pts[0],NumPts*sizeof(pts[0]));
end;

destructor TlmfPolyLine.Destroy;
begin
  Setlength(pts,0);
  inherited Destroy;
end;

procedure TlmfPolyLine.StorePoints(AStream:TStream);
var
  len:longint;
begin
  len:=length(pts);
  AStream.Write(len,sizeof(len));
  if len>0 then
    AStream.Write(pts[0],len*sizeof(pts[0]));
end;

procedure TlmfPolyLine.LoadPoints(AStream:TStream);
var
  len:longint;
begin
  Setlength(pts,0);
  if AStream.Read(len,sizeof(len))=sizeof(len) then
    if len>0 then
    begin
      setlength(pts,len);
      AStream.Read(pts[0],len*sizeof(pts[0]));
    end;
end;

procedure TlmfPolyLine.DefineProperties(Afiler:TFiler);
begin
  inherited DefineProperties(AFiler);
  AFiler.DefineBinaryProperty('Points',LoadPoints,StorePoints,length(pts)>0);
end;

procedure TlmfPolyLine.Action(fImage:TlmfImage;ACanvas:TCanvas);
var
  i:longint;
  npts:array of TPoint;
begin
  setlength(npts,length(pts));
  for i:=0 to high(pts) do
  begin
    npts[i].x:=fImage.ScaleX(pts[i].x);
    npts[i].y:=fImage.ScaleY(pts[i].y);
  end;
  ACanvas.Polyline(npts);
end;


{ TlmfPolygon }

constructor TlmfPolygon.Create(Points:PPoint;NumPts:integer;Winding:boolean=false);
begin
  inherited Create(Points,NumPts);
  fWinding:=Winding;
end;

procedure TlmfPolygon.Action(fImage:TlmfImage;ACanvas:TCanvas);
var
  i:longint;
  npts:array of TPoint;
begin
  Setlength(npts,length(pts));
  for i:=0 to high(pts) do
  begin
    npts[i].x:=fImage.ScaleX(pts[i].x);
    npts[i].y:=fImage.ScaleY(pts[i].y);
  end;
  ACanvas.Polygon(npts,fWinding,0,length(npts));
end;


initialization
  RegisterClasses([TlmfList, TlmfAnchor, TlmfMoveTo, TlmfLineTo,
    TlmfLine, TlmfText, TlmfColor, TlmfClip, TlmfRect, TlmfFont, TlmfBrush,
    TlmfPen, TlmfGraph, TlmfPolyLine, TlmfPolygon, TlmfEllipse]);
end.

