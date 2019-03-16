unit lr_e_cairo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, LResources, LCLProc, Forms, Controls, Graphics, Dialogs,
  Barcode, lr_class, lr_barc, lr_rrect, lr_shape, Cairo, CairoCanvas, CairoPrinter,
  lr_chbox;

type
  TShapeData = record
    ShapeType: TfrShapeType;
    FillColor: TColor;
    FrameStyle: TfrFrameStyle;
    FrameWidth: Double;
    FrameColor: TColor;
    Radius: Single;
    Corners: TCornerSet;
  end;

  PImageItem = ^TImageItem;
  TImageItem = record
    surface: pcairo_surface_t;
    sharename: string;
  end;

  TClipState = record
    Enabled: boolean;
    Clipping: boolean;
    ClipRect: TRect;
  end;

  TlrCairoExport = class(TComponent)
  end;

  TlrCairoBackend = (cePDF, cePS);

  { TlrCairoExportFilter }

  TlrCairoExportFilter = class(TfrExportFilter)
  private
    fBackend: TlrCairoBackend;
    fCairoPrinter: TCairoFilePrinter;
    FPageNo : Integer;
    ScaleX,ScaleY: Double;
    DataRect: TRect;
    fImageList: TfpList;
    fClipState: TClipState;
    fBarc: TBarcode;
    procedure AddShape(Data: TShapeData; x, y, h, w: integer);
    procedure DefaultShowView(View: TfrView; nx, ny, ndy, ndx: Integer);
    procedure DbgPoint(x, y: Integer; color: TColor; delta:Integer=5);
    procedure ClearImageList;
    function  IndexOfImage(SharedName: string): Integer;
    procedure SaveClipping(NewClipRect: PRect);
    procedure RestoreClipping;
  protected
    function Setup:boolean; override;
  public
    constructor Create(AStream: TStream); override;
    destructor Destroy; override;
    procedure OnBeginPage; override;
    procedure OnEndPage; override;
    procedure OnBeginDoc; override;
    procedure OnEndDoc; override;
    procedure ShowBackGround(View: TfrView; x, y, h, w: integer);
    procedure Frame(View: TfrView; x, y, h, w: integer);
    procedure ShowFrame(View: TfrView; x, y, h, w: integer);
    procedure Line(View: TfrView; x1,y1, x2,y2: Integer);
    procedure ShowBarCode(View: TfrCustomBarCodeView; x, y, h, w: integer);
    procedure ShowPicture(View: TfrPictureView; x, y, h, w: integer);
    procedure ShowRoundRect(View: TfrRoundRectView; x, y, h, w: integer);
    procedure ShowShape(View: TfrShapeView; x, y, h, w: integer);
    procedure OnText(X, Y: Integer; const Text: string; View: TfrView); override;
    procedure OnData(x, y: Integer; View: TfrView); override;
    procedure OnExported(x, y: Integer; View: TfrView); override;
  public
    property Backend: TlrCairoBackend read fBackend write fBackend;
  end;

  { TlrCairoPDFExportFilter }

  TlrCairoPDFExportFilter = class(TlrCairoExportFilter)
    constructor Create(AStream: TStream); override;
  end;

  { TlrCairoPSExportFilter }

  TlrCairoPSExportFilter = class(TlrCairoExportFilter)
    constructor Create(AStream: TStream); override;
  end;

implementation
uses LR_Utils;

// missing cairo functions to make shared images posible
const
  CAIRO_MIME_TYPE_JPEG = 'image/jpeg';
{$IFDEF CAIRO_HAS_MIME_TYPE_UNIQUE}
  CAIRO_MIME_TYPE_UNIQUE = 'application/x-cairo.uuid'
{$ENDIF}
procedure cairo_surface_get_mime_data(surface:Pcairo_surface_t; mime_type:Pchar; data:PPbyte; length:Pdword); cdecl; external LIB_CAIRO;
function cairo_surface_set_mime_data(surface:Pcairo_surface_t; mime_type:Pchar; data:Pbyte; length:dword;
                                     destroy:cairo_destroy_func_t; closure:pointer):cairo_status_t; cdecl; external LIB_CAIRO;


function rtrunc(value: extended): Integer;
begin
  result := trunc(value + 0.5);
end;

{ TlrCairoPSExportFilter }

constructor TlrCairoPSExportFilter.Create(AStream: TStream);
begin
  inherited Create(AStream);
  Backend := cePS;
end;

{ TlrCairoPDFExportFilter }

constructor TlrCairoPDFExportFilter.Create(AStream: TStream);
begin
  inherited Create(AStream);
  Backend := cePDF;
end;

{ TlrCairoExportFilter }

procedure TlrCairoExportFilter.AddShape(Data: TShapeData; x, y, h, w: integer);
var
  points: array of tpoint;
begin
  fCairoPrinter.Canvas.Brush.Color := Data.FillColor;
  fCairoPrinter.Canvas.Pen.Color := Data.FrameColor;
  fCairoPrinter.Canvas.Pen.Style := TPenStyle(Data.FrameStyle);
  fCairoPrinter.Canvas.Pen.Width := rtrunc(Data.FrameWidth*ScaleX);

  with fCairoPrinter.Canvas do
  case Data.ShapeType of
    frstRectangle:
      Rectangle(x, y, x+w, y+h);

    frstEllipse:
      Ellipse(x, y, x+w, y+h);

    frstRoundRect:
      begin
        //RoundRect(x, y, x+w, y+h, round(data.Radius), round(data.Radius));
        TCairoPrinterCanvas(fCairoPrinter.Canvas).MixedRoundRect(x, y, x+w, y+h,
          round(data.Radius), round(data.Radius), TSquaredCorners(Data.Corners));
        // TODO: SquaredCorners
      end;

    frstTriangle:
      begin
        SetLength(Points, 3);
        Points[0] := point(x+w, y+h);
        Points[1] := point(x, y+h);
        Points[2] := point(round(x+w/2), y);
        Polygon(Points);
        SetLength(Points, 0);
      end;

    frstDiagonal1:
      begin
        SetLength(Points, 2);
        Points[0] := Point(x, y);
        Points[1] := Point(x+w, y+h);
        Polygon(Points);
        SetLength(Points, 0);
      end;

    frstDiagonal2:
      begin
        SetLength(Points, 2);
        Points[0] := Point(x, y+h);
        Points[1] := Point(x+w, y);
        Polygon(Points);
        SetLength(Points, 0);
      end;
  end;
end;

procedure TlrCairoExportFilter.DefaultShowView(View: TfrView; nx, ny, ndy,
  ndx: Integer);
begin
  if (View.FillColor <> clNone)
     and not (View is TfrCustomBarCodeView)
     and not ((View is TfrPictureView) and
              TfrPictureView(View).Stretched and not TfrPictureView(View).KeepAspect)
  then
    ShowBackGround(View, nx, ny, ndy, ndx);

  if View is TfrCustomBarCodeView then
      ShowBarCode(TfrCustomBarCodeView(View), nx, ny, ndy, ndx)
  else if View is TfrPictureView then
      ShowPicture(TfrPictureView(View), nx, ny, ndy, ndx);

  if (View.Frames<>[]) and not (View is TfrCustomBarCodeView) then
     ShowFrame(View, nx, ny, ndy, ndx);
end;

procedure TlrCairoExportFilter.DbgPoint(x, y: Integer; color: TColor; delta:Integer=5);
begin
  fCairoPrinter.Canvas.Brush.Color := color;
  fCairoPrinter.Canvas.Brush.Style := bsSolid;
  fCairoPrinter.Canvas.Ellipse(x-Delta, y-Delta, x+Delta, y+Delta);
end;

procedure TlrCairoExportFilter.ClearImageList;
var
  Item: PImageItem;
  i: Integer;
begin
  for i:=0 to fImageList.Count-1 do begin
    Item := fImagelist[i];
    cairo_surface_destroy(Item^.surface);
    Item^.sharename:='';
    Dispose(Item);
  end;
  fImageList.Clear;
end;

function TlrCairoExportFilter.IndexOfImage(SharedName: string): Integer;
var
  i: Integer;
begin
  result := -1;
  for i:=0 to fImageList.Count-1 do begin
    if SharedName=PImageItem(fImageList[i])^.sharename then begin
      result := i;
      break;
    end;
  end;
end;

procedure TlrCairoExportFilter.SaveClipping(NewClipRect: PRect);
begin

  // save current clipping state
  fClipState.Clipping := fCairoPrinter.Canvas.Clipping;
  fClipState.ClipRect := fCairoPrinter.Canvas.ClipRect;

  // if supplied, set new cliprect
  if NewClipRect<>nil then begin
    fCairoPrinter.Canvas.ClipRect := NewClipRect^;
    fCairoPrinter.Canvas.Clipping := true;
  end;

  fClipState.Enabled := true;
end;

procedure TlrCairoExportFilter.RestoreClipping;
begin
  if not fClipState.Enabled then
    exit;

  fClipState.Enabled := false;

  fCairoPrinter.Canvas.ClipRect := fClipState.ClipRect;
  fCairoPrinter.Canvas.Clipping := fClipState.Clipping;
end;

function TlrCairoExportFilter.Setup: boolean;
begin
  Result:=inherited Setup;

  case Backend of
    cePS:
      fCairoPrinter.CairoBackend := cbPS;
    else
      fCairoPrinter.CairoBackend := cbPDF;
  end;
end;

constructor TlrCairoExportFilter.Create(AStream: TStream);
var
  ext: string;
begin
  inherited Create(AStream);
  fCairoPrinter := TCairoFilePrinter.create;
  fCairoPrinter.Stream := AStream;
  fCairoPrinter.CairoBackend := cbPDF;
  if AStream is TFileStream then begin
    ext := ExtractFileExt(TFileStream(AStream).FileName);
    case lowercase(ext) of
      '.ps':
        Backend := cePS;
      else
        Backend := cePDF;
    end;
  end;
  fImageList := TfpList.Create;
  fBarc := TBarcode.Create(nil);
end;

destructor TlrCairoExportFilter.Destroy;
begin
  fBarc.free;
  ClearImageList;
  fImageList.Free;
  fCairoPrinter.Free;
  inherited Destroy;
end;

procedure TlrCairoExportFilter.OnBeginPage;
begin
  inc(fPageNo);

  if fPageNo>1 then
    fCairoPrinter.NewPage;

  with CurReport.EMFPages[FPageNo - 1]^ do begin
    fCairoPrinter.PaperHeight := PrnInfo.PPgh;
    fCairoPrinter.PaperWidth := PrnInfo.PPgw;
  end;

  // TODO: non paged backends ...
end;

procedure TlrCairoExportFilter.OnEndPage;
begin
  inherited OnEndPage;
end;

procedure TlrCairoExportFilter.OnBeginDoc;
begin

  fCairoPrinter.XDPI := CurReport.EMFPages[0]^.PrnInfo.ResX;
  fCairoPrinter.YDPI := CurReport.EMFPages[0]^.PrnInfo.ResY;

  ScaleX := fCairoPrinter.XDPI / (93 / 1.022);  // scaling factor X Printer DPI / Design Screen DPI
  ScaleY := fCairoPrinter.YDPI / (93 / 1.015);  // "              Y "

  fCairoPrinter.BeginDoc;
  fCairoPrinter.Canvas.Handle; // make sure handle is created
end;

procedure TlrCairoExportFilter.OnEndDoc;
begin
  fCairoPrinter.EndDoc;
end;

procedure TlrCairoExportFilter.ShowBackGround(View: TfrView; x, y, h, w: integer
  );
begin
  fCairoPrinter.Canvas.Brush.Style:=bsSolid;
  fCairoPrinter.Canvas.Brush.Color:=View.FillColor;
  fCairoPrinter.Canvas.FillRect(x, y, x+w, y+h);
end;

procedure TlrCairoExportFilter.Frame(View: TfrView; x, y, h, w: integer);
begin
  fCairoPrinter.Canvas.Pen.Style:=TPenStyle(View.FrameStyle);
  fCairoPrinter.Canvas.Pen.Color:=View.FrameColor;
  fCairoPrinter.Canvas.Pen.Width := rtrunc(View.FrameWidth*ScaleX);
  fCairoPrinter.Canvas.Brush.style := bsClear;
  fCairoPrinter.Canvas.Rectangle(x, y, x+w, y+h);
end;

procedure TlrCairoExportFilter.ShowFrame(View: TfrView; x, y, h, w: integer);
begin
  if ([frbLeft,frbTop,frbRight,frbBottom]-View.Frames=[]) then
    Frame(View, x, y, h, w)
  else
  begin
    if frbRight in View.Frames then
      Line(View, x+w, y, x+w, y+h);
    if frbLeft in View.Frames then
      Line(View, x, y+h, x, y);
    if frbBottom in View.Frames then
      Line(View, x+w, y+h, x, y+h);
    if frbTop in View.Frames then
      Line(View, x, y, x+w, y);
  end;
end;

procedure TlrCairoExportFilter.Line(View:TfrView; x1, y1, x2, y2: Integer);
begin
  fCairoPrinter.Canvas.Pen.Style:=TPenStyle(View.FrameStyle);
  fCairoPrinter.Canvas.Pen.Color:=View.FrameColor;
  fCairoPrinter.Canvas.Pen.Width := rtrunc(View.FrameWidth*ScaleX);
  fCairoPrinter.Canvas.MoveTo(x1,y1);
  fCairoPrinter.Canvas.LineTo(X2,Y2);
end;

{$PUSH}
{$HINTS OFF}
{$NOTES OFF}
function isNumeric(St: String): Boolean;
var
  {%H-}R: Double;
  E: Integer;
begin
  Val(St, R, E);
  Result := (E = 0);
end;
{$POP}

procedure TlrCairoExportFilter.ShowBarCode(View: TfrCustomBarCodeView; x, y, h,
  w: integer);
const
  cbDefaultText = '12345678';
  arrIsAlpha: array[TBarcodeType] of boolean = (
    false,  //bcCode_2_5_interleaved,
    false,  //bcCode_2_5_industrial,
    false,  //bcCode_2_5_matrix,
    true,   //bcCode39,
    true,   //bcCode39Extended,
    true,   //bcCode128A,
    true,   //bcCode128B,
    false,  //bcCode128C,
    true,   //bcCode93,
    true,   //bcCode93Extended,
    false,  //bcCodeMSI,
    false,  //bcCodePostNet,
    true,   //bcCodeCodabar,
    false,  //bcCodeEAN8,
    false   //bcCodeEAN13
  );

var
  Text: string;
  aLine: string;
  dx, dy, ox, oy, Angle, fh: Integer;
  r: TRect;
  ts: TTextStyle;
begin

  fBarC.Typ:= View.BarType;
  if Trim(View.Memo.Text)='' then
    exit;

  Text := View.Memo.Text;
  aLine := View.Memo.Strings[0];

  if (Text <> '') and (pos('[',aLine)=0) and
    ((arrIsAlpha[fBarC.typ] or IsNumeric(aLine) or
      fBarC.BarcodeTypeChecked(fBarC.Typ)))  then
  begin
    fBarC.Text := aLine;
    fBarC.Checksum := view.CheckSum;
  end else begin
    fBarC.Text := cbDefaultText;
    fBarC.Checksum := true;
  end;

  if fBarC.Text='0' then
    exit;

  if View.ShowText then
    with fCairoPrinter.Canvas do begin
      Font.Name := 'Arial';
      Font.Color := clBlack;
      Font.Size := 10;
      Font.Orientation:=0;
      fh := TextHeight('09');
    end
  else
    fh := 0;

  fBarC.Angle := View.Angle;
  fBarC.Ratio := 2;
  fBarC.Modul := rtrunc(ScaleX*View.Zoom);

  Angle := round(View.Angle);

  // barcode height
  dx := w;
  dy := h;
  if (Angle=90) or (Angle=270) then begin
    dy := fBarC.Width;
    fBarC.Height := dx - fh;
  end
  else begin
    dx := fBarC.Width;
    fBarC.Height := dy - fh;
  end;

  // barcode origin
  ox := x;
  oy := y;
  case angle of
    0:
      if fBarC.typ=bcCodePostNet then
      begin
        oy:=fBarC.Height;
        fBarC.Height:=-oy;
      end;
    90:
      oy := y+dy;
    180:
      begin
        oy := y+dy;
        ox := x+dx;
      end;
    270:
      ox := x+dx;
  end;
  fBarC.Left := ox;
  fBarC.Top := oy;

  fBarC.DrawBarcode(fCairoPrinter.Canvas);

  // barcode text
  if View.ShowText then
    with fCairoPrinter.Canvas do begin

      if fBarC.Checksum then
        aLine := fBarC.CodeText
      else
        aLine := fBarC.Text;

      r := rect(x, y, x+dx, y+dy);

      font.Orientation := angle * 10;

      case angle of
          0: begin r.top := r.bottom - fh; oy := r.Top; end;
        180: begin r.Bottom := r.Top + fh; oy := r.Bottom; ox := r.Right; end;
         90: begin r.left := r.right - fh; oy := r.Bottom; ox := r.Left;  end;
        270: begin r.right := r.left + fh; oy := r.Top;    ox := r.Right; end;
      end;

      brush.Color := clWhite;
      brush.style := bsSolid;
      Fillrect(r);
      ts := TextStyle;
      ts.Alignment:=taCenter;
      ts.Layout:=tlcenter;
      ts.SingleLine:=true;

      TextRect(r, ox, oy, aLine, ts);
    end;
end;

procedure destroymstream(data: pointer); cdecl;
begin
  TMemoryStream(data).Free;
end;

{$IFDEF CAIRO_HAS_MIME_TYPE_UNIQUE}
procedure destroybuf(data: pointer); cdecl;
begin
  freemem(data);
end;
{$ENDIF}

procedure TlrCairoExportFilter.ShowPicture(View: TfrPictureView; x, y, h,
  w: integer);
var
  isf: pcairo_surface_t;
  m: TMemoryStream;
  item: PImageItem;

  r: Double;
  L: Integer;
  pw, ph, picw, pich: Integer;
  Picture: TPicture;
  i: Integer;

  ImageShared, AddToList: boolean;
  imgbuf: pbyte;
begin

  picture := View.Picture;
  if Picture.Graphic=nil then
  begin
    DebugLn('WARNING: tried to export an empty image (%s)',[View.Name]);
    exit;
  end;
  picw := Picture.Graphic.Width;
  pich := Picture.Graphic.Height;

  ImageShared := (View.SharedName<>'') and (Backend=cePDF);

  AddToList := false;
  M := nil;
  imgbuf := nil;

  // check if image is shared and has already been used
  isf := nil;
  if ImageShared then begin
    i := IndexOfImage(View.SharedName);
    if i>=0 then
      isf := PImageItem(fImageList[i])^.surface;
  end;


  // at least in cairo 10.2 JPEG doesn't work ok for PS backend
  if (Picture.Graphic is TJPegImage) and (backend=cePDF) then
  begin

    if isf=nil then begin
      m := TMemoryStream.Create;
      TJPegImage(Picture.Graphic).SaveToStream(M);
      isf := cairo_image_surface_create(CAIRO_FORMAT_RGB24, picw, pich);
      cairo_surface_set_mime_data(isf, CAIRO_MIME_TYPE_JPEG, pbyte(m.Memory),
                                  m.Size,  @destroymstream, pointer(m));
      AddToList := ImageShared;
    end;

  end else
  begin

    if isf=nil then begin
      imgbuf := GetMem(picw * pich * 4);
      GraphicToARGB32(Picture.Graphic, imgbuf);
      isf := cairo_image_surface_create_for_data(imgbuf, CAIRO_FORMAT_ARGB32, picw, pich, picw*4);

      // for sharing non JPeg images, we need CAIRO_MIME_TYPE_UNIQUE_ID
      // but this is only implemented in cairo 1.12, which is atm too new
      {$IFDEF CAIRO_HAS_MIME_TYPE_UNIQUE}
      cairo_surface_set_mime_data(isf, CAIRO_MIME_TYPE_UNIQUE_ID, imgbuf,
                                  picw * pich * 4, @destroybuf, imgbuf);
      AddToList := ImageShared;
      {$ENDIF}
    end;

  end;

  if AddToList then begin
    New(Item);
    Item^.surface:=isf;
    Item^.sharename:=View.SharedName;
    fImageList.Add(Item);
  end;

  // clipping is only meanful if image is not stretched
  if not View.Stretched then
    SaveClipping(@DataRect);

  // calc the image scale
  ph := h;
  pw := w;

  if view.Stretched then
  begin
    if (View.Flags and flPictRatio<>0) and
       (picw>0) and (pich>0) then
    begin
      r  := picw/pich;
      if (w/h) < r then
      begin
        L := h;
        ph := rtrunc(w/r);
        if (View.Flags and flPictCenter<>0) then
          y := y + (L-ph) div 2;
      end
      else
      begin
        L := w;
        pw := rtrunc(h*r);
        if (View.Flags and flPictCenter<>0) then
          x := x + (L-pw) div 2;
      end;
    end;
  end
  else begin
    pw := rtrunc(picw * ScaleX) + 1;
    ph := rtrunc(pich * ScaleY) + 1;
    if (View.Flags and flPictCenter<>0) then begin
       x := x + (w - pw) div 2 - 1;
       y := y + (h - ph) div 2 - 1;
    end;
  end;

  // draw the image
  TCairoPrinterCanvas(fCairoPrinter.Canvas).DrawSurface(
    rect(0, 0, picw, pich),
    rect(x, y, x+pw, y+ph),
    isf);

  if not ImageShared then begin
    cairo_surface_destroy(isf);
    if imgbuf<>nil then
      freemem(imgbuf);
  end;

  if not View.Stretched then
    RestoreClipping;
end;

procedure TlrCairoExportFilter.ShowRoundRect(View: TfrRoundRectView; x, y, h,
  w: integer);
var
  Data: TShapeData;
  SWidth: Integer;
begin

  if view.ShowGradian then
    // not supported yet
    DefaultShowView(View, x, y, h, w)

  else
  begin

    SWidth := trunc((View.RoundRectCurve/2) * ScaleX + 0.5);
    if View.RoundRect then
      Data.Radius := SWidth
    else
      Data.Radius := 0.0;
    Data.Corners:=View.SquaredCorners;

    // draw shadow
    Data.ShapeType := frstRoundRect;
    Data.FillColor := ColorToRGB(View.ShadowColor);
    Data.FrameColor := Data.FillColor; //ColorToRGB(View.FrameColor);
    Data.FrameWidth := 0;
    Data.FrameStyle := frsSolid;
    SWidth := trunc(View.ShadowWidth * ScaleX + 0.5);
    if View.ShadowWidth>0 then
      AddShape(Data, x + SWidth, y + SWidth, h - SWidth, w - SWidth);

    // draw roundrect
    Data.ShapeType := frstRoundRect;
    if View.FillColor=clNone then
      Data.FillColor := clNone
    else
      Data.FillColor := ColorToRGB(View.FillColor);
    if View.Frames=[] then
      Data.FrameColor := Data.FillColor
    else
      Data.FrameColor := ColorToRGB(View.FrameColor);
    Data.FrameWidth := View.FrameWidth;
    Data.FrameStyle := View.FrameStyle;
    AddShape(Data, x, y, h - SWidth, w - SWidth);
  end;
end;

procedure TlrCairoExportFilter.ShowShape(View: TfrShapeView; x, y, h, w: integer
  );
var
  Data: TShapeData;
begin
  Data.ShapeType := View.ShapeType;
  Data.FillColor := View.FillColor;
  Data.FrameColor := View.FrameColor;
  Data.FrameStyle := View.FrameStyle;
  Data.FrameWidth := View.FrameWidth;
  Data.Radius := 10; // TODO: check correct value
  Data.Corners := [];
  AddShape(Data, x, y, h, w);
end;

type
  TfrMemoView_ = class(TfrMemoView);

procedure TlrCairoExportFilter.OnText(X, Y: Integer; const Text: string;
  View: TfrView);
var
  nx, ny, gapx, gapy, sgapx, sgapy: Integer;
  aStyle, oldStyle: TTextStyle;
  R: TRect;
begin

  // The text drawn in this function is enclosed in a view frame
  // given by DataRect (cached from OnData event)
  //
  // this function is called repeteadly for each line of text contained in view
  // For horizontal text, the y parameter has the y-position of current line
  //                      and x parameter is constant
  // For vertical text, the x parameter has the x-position of the curent line
  //                    and y parameter is constant
  //
  // however this function is resposible for calculating the x-position (for
  // horizontal text) or the y-position (for vertical text) corresponding to
  // the specified alignment.

  // This means LazReport is a mess. It gives exporter the task to calc the proper
  // text widths for "alignment" purposes but it internally calc the height of text
  // and so it assumes it properly calc the "layout" of text........

  // setup clipping
  //OldClipping := fCairoPrinter.Canvas.Clipping;
  //if OldClipping then
  //  OldClipRect := fCairoPrinter.Canvas.ClipRect;
  //fCairoPrinter.Canvas.ClipRect := DataRect;
  //fCairoPrinter.Canvas.Clipping := true;

  oldStyle := fCairoPrinter.Canvas.TextStyle;
  aStyle := oldStyle;
  aStyle.Clipping:=false;  // NOTE: there are some interaction between this and roundrect
  aStyle.Layout:=tlTop;    //       background painting, set to false for the moment
  if View is TfrMemoView then
  begin
    aStyle.Alignment:=TfrMemoView_(View).Alignment;
    aStyle.Wordbreak:= TfrMemoView_(View).WordWrap;
  end else
  if View is TfrCheckBoxView then
  begin
    aStyle.Alignment:=taCenter;
    aStyle.WordBreak:=false;
    aStyle.Layout:=tlCenter;
    fCairoPrinter.Canvas.Font.Size := 10;
    fCairoPrinter.Canvas.Font.Style := fCairoPrinter.Canvas.Font.Style + [fsBold];
  end else
  begin
    aStyle.Alignment:=taLeftJustify;
    aStyle.WordBreak:=false;
  end;

  gapx := trunc(View.FrameWidth / 2 + 0.5);
  gapy := trunc(View.FrameWidth / 4 + 0.5);
  sgapx := trunc( gapx * ScaleX + 0.5);
  sgapy := trunc( gapy * ScaleY + 0.5);
  nx := trunc((x+gapx) * ScaleX + 0.5);
  ny := trunc((y+gapy) * ScaleY + 0.5);
  R := DataRect;
  InflateRect(R, -sgapx, -sgapy);

  if View is TfrMemoView then
  begin
    fCairoPrinter.Canvas.Font := TfrMemoView_(View).Font;
    fCairoPrinter.Canvas.Font.Orientation := (View as TfrMemoView).Angle * 10;

    if fCairoPrinter.Canvas.Font.Orientation<>0 then
      fCairoPrinter.Canvas.TextRect(R, nx, R.Bottom, Text, aStyle)
    else
    begin
      if TfrMemoView_(View).Justify and not TfrMemoView_(View).LastLine then
        CanvasTextRectJustify(fCairoPrinter.Canvas, R, nx, R.Right, ny, Text, true)
      else
        fCairoPrinter.Canvas.TextRect(R, {R.Left} nx, ny, Text, aStyle);
    end;
  end else
    fCairoPrinter.Canvas.TextRect(R, {R.Left} nx, ny, Text, aStyle);

  // restore previous clipping
  //if OldClipping then
  //  fCairoPrinter.Canvas.ClipRect := OldClipRect
  //else
  //  fCairoPrinter.Canvas.Clipping := false;
  fCairoPrinter.Canvas.TextStyle := oldStyle;
end;

procedure TlrCairoExportFilter.OnData(x, y: Integer; View: TfrView);
var
  nx, ny, ndx, ndy: Integer;
begin

  nx := Trunc( x * ScaleX + 0.5 );
  ny := Trunc( y * ScaleY + 0.5 );
  ndx := Trunc( View.dx * ScaleX + 0.5 );
  ndy := Trunc( View.dy * ScaleY + 0.5 );

  DataRect := Rect(nx, ny, nx+ndx, ny+ndy);

  // enable global clipping only for this view classes
  // others classes might need clipping only in some cases or
  // may need another clipping shape.
  if
    (View is TfrMemoView)
  then
    SaveClipping(@DataRect);

  if View is TfrShapeView then begin

    ShowShape(TfrShapeView(View), nx, ny, ndy, ndx);

  end else
  if View is TfrRoundRectView then begin

    ShowRoundRect(TfrRoundRectView(View), nx, ny, ndy, ndx);

  end else
    DefaultShowView(View, nx, ny, ndy, ndx);
end;

procedure TlrCairoExportFilter.OnExported(x, y: Integer; View: TfrView);
begin
  RestoreClipping;
end;

initialization
    frRegisterExportFilter(TlrCairoPDFExportFilter, 'Cairo PDF (*.pdf)', '*.pdf');
    frRegisterExportFilter(TlrCairoPSExportFilter, 'Cairo Postscript (*.ps)', '*.ps');

end.
