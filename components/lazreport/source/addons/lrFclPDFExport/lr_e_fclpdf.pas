{
  LazReport PDF export

 Copyright (C) 2016 alexs alexs75.at.yandex.ru

 The module is designed to create an image of the report with the exact
 positioning of objects and subsequent binding to the worksheet

 This library is free software; you can redistribute it and/or modify it
 under the terms of the GNU Library General Public License as published by
 the Free Software Foundation; either version 2 of the License, or (at your
 option) any later version with the following modification:

 As a special exception, the copyright holders of this library give you
 permission to link this library with independent modules to produce an
 executable, regardless of the license terms of these independent modules,and
 to copy and distribute the resulting executable under terms of your choice,
 provided that you also meet, for each linked independent module, the terms
 and conditions of the license of that module. An independent module is a
 module which is not derived from or based on this library. If you modify
 this library, you may extend this exception to your version of the library,
 but you are not obligated to do so. If you do not wish to do so, delete this
 exception statement from your version.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
 for more details.

 You should have received a copy of the GNU Library General Public License
 along with this library; if not, write to the Free Software Foundation,
 Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}


unit lr_e_fclpdf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LR_Class, LR_ChBox, LR_BarC, LR_Shape, LR_RRect, fpPDF, Graphics, fpTTF;

type
  TExportFonts = class;
  TlrPdfExportFilter = class;

  { TExportFontItem }

  TExportFontItem = class
  private
    FFontColor: TColor;
    FFontName: string;
    FFontSize: Integer;
    FFontStyle: TFontStyles;
    FOwner:TExportFonts;
    FDefaultFont: boolean;
    //
    FPdfFont:integer;
    FTTFFontInfo: TFPFontCacheItem;
    function GetBold: boolean;
    function GetItalic: boolean;
    procedure SetFontSize(AValue: Integer);
    //
    function TextWidth(const AText: utf8string) : single;
    function TextHeight(const AText: utf8string) : single;
  public
    constructor Create(AOwner:TExportFonts; AFontName:string; AFontStyle: TFontStyles);
    destructor Destroy; override;
    procedure Activate;
    property FontStyle: TFontStyles read FFontStyle;
    property FontSize:Integer read FFontSize write SetFontSize;
    property FontColor:TColor read FFontColor write FFontColor;
    property Bold:boolean read GetBold;
    property Italic:boolean read GetItalic;
    property DefaultFont:boolean read FDefaultFont;
    property FontName:string read FFontName;
  end;

  { TExportFonts }

  TExportFonts = class
  private
    //FDefaultFontBold: TExportFontItem;
    FDefaultFontNormal: TExportFontItem;
    FOwner:TlrPdfExportFilter;
    FList:TFPList;
    function GetCount: integer;
    function GetItem(Index: integer): TExportFontItem;
  public
    constructor Create(AOwner:TlrPdfExportFilter);
    destructor Destroy; override;
    procedure Clear;
    function AddItem(AFontName: string; AFontStyle:TFontStyles = []): TExportFontItem;
    function FindItem(AFontName: string; AFontStyle:TFontStyles = []):TExportFontItem;
    property DefaultFontNormal:TExportFontItem read FDefaultFontNormal;
    //property DefaultFontBold:TExportFontItem read FDefaultFontBold;
    property Count:integer read GetCount;
    property Item[Index:integer]:TExportFontItem read GetItem;
  end;

  { TlrPdfExportFilter }

  TlrPdfExportFilter = class(TfrExportFilter)
  private
    FPDFDocument: TPDFDocument;
    FCurSection: TPDFSection;
    FCurPageNo : integer;
    FCurPage: TPDFPage;
    FFontItems:TExportFonts;
    FCurFont: TExportFontItem;
    procedure SetupFonts;
    procedure InitFonts;
  private
    InternalGapX:integer;
    InternalGapY:integer;
    procedure WriteTextRectJustify(AExportFont: TExportFontItem; X, Y, W, H: TPDFFloat; const Text: string; Trimmed: boolean);
    procedure WriteTextRect(AExportFont:TExportFontItem; X, Y, W{, H}:TPDFFloat; AText:string; AHAlign:TAlignment);
    procedure DrawRect(X, Y, W, H: TPDFFloat; ABorderColor, AFillColor: TColor;
      AFrames: TfrFrameBorders; ABorderWidth: TPDFFloat);
    procedure DrawRectView(AView: TfrView);
    procedure WriteURL(X, Y, W, H: TPDFFloat; AUrlText:string);
    procedure DrawLine(X1, Y1, X2, Y2: TPDFFloat; ABorderColor: TColor; ABorderWidth: TPDFFloat);
    procedure DrawEllipse(X, Y, W, H: TPDFFloat; ABorderColor, AFillColor: TColor;
      AFrames: TfrFrameBorders; ABorderWidth: TPDFFloat);
    procedure DrawImage(X, Y, W, H: integer; ABmp:TBitmap);
    procedure DrawLRObjectInternal(View:TfrView);
  private
    procedure DoMemoView(View:TfrMemoView);
    procedure DoImageView(View:TfrPictureView);
    procedure DoLineView(View:TfrLineView);
    procedure DoCheckBoxView(View:TfrCheckBoxView);
    procedure DoShapeView(View:TfrShapeView);
    procedure DoBarCodeView(View:TfrCustomBarCodeView);
    procedure DoRoundRectView(View:TfrRoundRectView);
  public
    constructor Create(AStream: TStream); override;
    destructor Destroy; override;
    procedure OnBeginDoc; override;
    procedure OnEndDoc; override;
    procedure OnBeginPage; override;
    procedure OnEndPage; override;
    procedure OnData(x, y: Integer; View: TfrView); override;
    procedure OnText({%H-}x, {%H-}y: Integer; const {%H-}Text: String; {%H-}View: TfrView); override;
    procedure OnExported({%H-}x, {%H-}y: Integer; {%H-}View: TfrView); override;
  end;

implementation
uses LazFileUtils, Forms, LR_Utils, LazUTF8, Printers, FPReadBMP, FPReadPNG, FPReadJPEG;

const
  cInchToMM = 25.4;

function ConvetUnits(AUnits:TPDFFloat):TPDFFloat; inline;
begin
  Result := (AUnits * cInchToMM) / gTTFontCache.DPI;
end;

function ConvetUnits1(AUnits:TPDFFloat):TPDFFloat; inline;
begin
  Result:= AUnits * gTTFontCache.DPI / cInchToMM;
end;

function ColorToPdfColor(C:Graphics.TColor):TARGBColor;
var
  A:array [1..4] of byte absolute C;
begin
  if C = clWindow then
    Result:=clWhite
  else
    Result:={A[1] shl 24 +} A[1] shl 16 + A[2] shl 8 + A[3];
end;

type
  TfrHackView = class(TfrView);

{ TExportFonts }

function TExportFonts.GetCount: integer;
begin
  Result:=FList.Count;
end;

function TExportFonts.GetItem(Index: integer): TExportFontItem;
begin
  Result:=TExportFontItem(FList[Index]);
end;

constructor TExportFonts.Create(AOwner: TlrPdfExportFilter);
begin
  inherited Create;
  FOwner:=AOwner;
  FList:=TFPList.Create;
end;

destructor TExportFonts.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TExportFonts.Clear;
var
  I: Integer;
begin
  for I:=0 to FList.Count-1 do
    TExportFontItem(FList[i]).Free;
  FList.Clear;
end;

function MakePSName(AFontName: string; AFontStyle: TFontStyles):string;
begin
  Result:=AFontName;

  if Graphics.fsBold in AFontStyle then
    Result:=Result + '-Bold';

  if Graphics.fsItalic in AFontStyle then
    Result:=Result + '-Oblique';

  if Graphics.fsUnderline in AFontStyle then
    Result:=Result + '-Underline';

  if Graphics.fsStrikeOut in AFontStyle then
    Result:=Result + '-StrikeOut';
end;

function TExportFonts.AddItem(AFontName: string; AFontStyle: TFontStyles
  ): TExportFontItem;
var
  S1, S2, S3, S: String;

begin
  Result:=FindItem(AFontName, AFontStyle);
  if Assigned(Result) then exit;

  if Assigned(gTTFontCache.Find(AFontName, Graphics.fsBold in AFontStyle, Graphics.fsItalic in AFontStyle)) then
  begin
    Result:=TExportFontItem.Create(Self, AFontName, AFontStyle);
    S1:=ExtractFileDir(Result.FTTFFontInfo.FileName);
    S2:=ExtractFileName(Result.FTTFFontInfo.FileName);
    S3:=AFontName;
    FOwner.FPDFDocument.FontDirectory:=S1;
    S:=MakePSName(AFontName, AFontStyle);
    Result.FPdfFont:=FOwner.FPDFDocument.AddFont(S2, S);
  end
  else
    Result:=FDefaultFontNormal;
end;

function TExportFonts.FindItem(AFontName: string; AFontStyle: TFontStyles
  ): TExportFontItem;
var
  K: TExportFontItem;
  i: Integer;
begin
  Result:=nil;

  if AFontName = 'default' then
  begin
{    if Graphics.fsBold in AFontStyle then
      Result:=FDefaultFontBold
    else}
      Result:=FDefaultFontNormal;
  end
  else
  begin
    for i:=0 to FList.Count-1 do
    begin
      K:=TExportFontItem(FList[i]);
      if (K.FontName = AFontName) and (K.FontStyle = AFontStyle) then
      begin
        Result:=K;
        exit;
      end
    end;
  end;
end;

{ TExportFontItem }

function TExportFontItem.GetBold: boolean;
begin
  Result:=Graphics.fsBold in FFontStyle;
end;

function TExportFontItem.GetItalic: boolean;
begin
  Result:=Graphics.fsItalic in FFontStyle;
end;

procedure TExportFontItem.SetFontSize(AValue: Integer);
begin
  if AValue = 0 then
    FFontSize:=10
  else
    FFontSize:=AValue;
end;

function TExportFontItem.TextWidth(const AText: utf8string): single;
begin
  Result:=ConvetUnits(FTTFFontInfo.TextWidth(AText, FFontSize));
end;

function TExportFontItem.TextHeight(const AText: utf8string): single;
var
  ADescender: single;
begin
  Result:=FTTFFontInfo.TextHeight(AText, FFontSize, ADescender);
  Result:=ConvetUnits(Result + ADescender);
{  FTH:=ConvetUnits(AExportFont.FTTFFontInfo.TextHeight(AText, AExportFont.FontSize, ADescender));
  FTH:=FTH + ConvetUnits(ADescender);}
end;

constructor TExportFontItem.Create(AOwner: TExportFonts; AFontName: string;
  AFontStyle: TFontStyles);
begin
  inherited Create;
  FOwner:=AOwner;
  FOwner.FList.Add(Self);
  FFontStyle:=AFontStyle;
  FFontName:=AFontName;
  FTTFFontInfo:=gTTFontCache.Find(AFontName, Graphics.fsBold in AFontStyle, Graphics.fsItalic in AFontStyle);
  if not Assigned(FTTFFontInfo) then
    raise Exception.CreateFmt('fpTTF:in gTTFontCache not found font "%s" info.', [AFontName]);
end;

destructor TExportFontItem.Destroy;
begin
  inherited Destroy;
end;

procedure TExportFontItem.Activate;
begin
  FOwner.FOwner.FCurPage.SetFont(FPdfFont, FontSize);
  FOwner.FOwner.FCurPage.SetColor(ColorToPdfColor(FontColor), false);
end;

{ TlrPdfExportFilter }

procedure TlrPdfExportFilter.SetupFonts;
//Find default font name
function DefFontName:string;
const
  DefFontNames : array [1..3] of string =
  // TODO: Check if Arial is better default choice in windows/linux/mac
  {$IFDEF MSWINDOWS}
     ('Arial', 'Liberation Sans', 'FreeSans');
  {$ELSE}
     ('Liberation Sans', 'Arial', 'FreeSans');
  {$ENDIF}
var
  i: Integer;
begin
  for i:=1 to 3 do
    if Assigned(gTTFontCache.Find(DefFontNames[i], false, false)) then
    begin
      Result:=DefFontNames[i];
      exit;
    end;
  raise Exception.Create('Not found Sans font');
end;

var
  i: Integer;
  sDefFontName:string;
begin
  sDefFontName:=DefFontName;
  FFontItems.FDefaultFontNormal:=FFontItems.AddItem(sDefFontName, []);
end;

procedure TlrPdfExportFilter.InitFonts;
procedure CreateFontDirList;
{$IFDEF WINDOWS}
var
  s: String;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  s := SHGetFolderPathUTF8(20); // CSIDL_FONTS = 20
  if s <> '' then
    gTTFontCache.SearchPath.Add(s);
  {$ENDIF}
  {$IFDEF linux}
  //tested on Fedora 24
  gTTFontCache.SearchPath.Add('/usr/share/cups/fonts/');
  gTTFontCache.SearchPath.Add('/usr/share/fonts/');
  gTTFontCache.SearchPath.Add('/usr/share/wine/fonts/');
  gTTFontCache.SearchPath.Add('/usr/local/lib/X11/fonts/');
  gTTFontCache.SearchPath.Add(GetUserDir + '.fonts/');
  {$ENDIF}

end;
begin
  if gTTFontCache.Count = 0 then
  begin
    {$IF (FPC_FULLVERSION >= 30101)}
    gTTFontCache.BuildFontCacheIgnoresErrors:=true;
    {$ENDIF}
    {$IFDEF WINDOWS}
    CreateFontDirList;
    {$ELSE}
    gTTFontCache.ReadStandardFonts;
    {$ENDIF}
    gTTFontCache.BuildFontCache;
  end;
end;

procedure TlrPdfExportFilter.DoMemoView(View: TfrMemoView);
var
  S: String;
begin
  DrawRectView(View);

  S:=UpperCase(TfrMemoView(View).URLInfo);
  if (S <> '') and ((Copy(S, 1,  7) = 'HTTP://') or (Copy(S, 1, 8) = 'HTTPS://')) then
    WriteURL(View.Left, View.Top, View.Width, View.Height, TfrMemoView(View).URLInfo);


  //prepare font
  FCurFont:=FFontItems.AddItem(View.Font.Name, View.Font.Style);
  if Assigned(FCurFont) then
  begin
    FCurFont.FontSize:=View.Font.Size;
    FCurFont.FontColor:=View.Font.Color;
    FCurFont.Activate;
  end;
end;

procedure TlrPdfExportFilter.DoImageView(View: TfrPictureView);
var
  IDX: Integer;

function LoadImage:boolean;
var
  S: TMemoryStream;
begin
  Result:=false;
  S:=TMemoryStream.Create;
  try
    View.Picture.SaveToStream(S);
    S.Position:=0;
    if View.Picture.Graphic is TJPegImage then
      IDX := FPDFDocument.Images.AddFromStream(S, TFPReaderJPEG, False)
    else
    if View.Picture.Graphic is TPortableNetworkGraphic then
      IDX := FPDFDocument.Images.AddFromStream(S, TFPReaderPNG, False)
    else
      IDX := FPDFDocument.Images.AddFromStream(S, TFPReaderBMP, False);
    Result:=true;
  finally
    S.Free;
  end;
end;

var
  fX, fY, fW, fH: TPDFFloat;
  X, Y, W, H, L: Double;
  R: Extended;
begin
  DrawRectView(View);

  if not ((View.Picture.Graphic = nil) or View.Picture.Graphic.Empty) then
  begin
    if not LoadImage then exit;

    if View.Stretched then
    begin
      X:=View.Left;
      Y:=View.Top;
      W:=View.Width;
      H:=View.Height;

      if View.KeepAspect then
      begin
        R:=FPDFDocument.Images[IDX].Width / FPDFDocument.Images[IDX].Height;
        if (W / H) < R then
        begin
          L := H;
          H := W/R;
          if View.Centered then
            Y := Y + (L - H) / 2;
        end
        else
        begin
          L := W;
          W := H * R;
          if View.Centered then
            X := X + (L - W) / 2;
        end;
      end;
    end
    else
    if View.Centered then
    begin
      if FPDFDocument.Images[IDX].Width < View.dx then
      begin
        X:=View.X + (View.dx - FPDFDocument.Images[IDX].Width) div 2;
        W:=FPDFDocument.Images[IDX].Width;
      end
      else
      begin
        X:=View.x;
        W:=View.dx;
      end;

        if FPDFDocument.Images[IDX].Height < View.dy then
        begin
          Y:=View.y + (View.dy - FPDFDocument.Images[IDX].Height) div 2;
          H:=FPDFDocument.Images[IDX].Height;
        end
        else
        begin
          Y:=View.y;
          H:=View.dy;
        end;
    end
    else
    begin
      X:=View.X;
      Y:=View.Y;
      W:=FPDFDocument.Images[IDX].Width;
      H:=FPDFDocument.Images[IDX].Height;
    end;

    fX:=ConvetUnits(X);
    fY:=ConvetUnits(Y);
    fW:=ConvetUnits(W);
    fH:=ConvetUnits(H);

    FCurPage.DrawImage(fX, fY + fH, fW, fH, IDX);  // left-bottom coordinate of image
  end;
end;

procedure TlrPdfExportFilter.DoLineView(View: TfrLineView);
begin
  DrawRect(View.Left, View.Top, View.Width, View.Height, View.FrameColor, clNone, View.Frames, View.FrameWidth);
end;

procedure TlrPdfExportFilter.DoCheckBoxView(View: TfrCheckBoxView);
var
  C: Boolean;
  GX, GY: Integer;
begin
  DrawRectView(View);
  C:=View.Checked;
  if View.Memo.Count > 0 then
    C:=View.Memo[0] = '1';

  View.CalcGaps;
  GX:= TfrHackView(View).InternalGapX * 2; //View.GapX + View.FrameWidth + 2;
  GY:= TfrHackView(View).InternalGapY * 2; //View.GapY + View.FrameWidth + 2;

  if C then
  begin
    DrawLine(
      View.X + GX,
      View.Y + GY,
      View.X + View.DX - GX * 2,
      View.Y + View.DY - GY * 2,
      View.FrameColor,
      ConvetUnits1(3)
    );

    DrawLine(
      View.X + View.DX - GX * 2,
      View.Y + GY,
      View.X + GX * 2,
      View.Y + View.DY - GY * 2,
      View.FrameColor,
      ConvetUnits1(3)
    );
  end;
end;

procedure TlrPdfExportFilter.DoShapeView(View: TfrShapeView);
begin
 case View.ShapeType of
    frstRectangle:
      DrawRect(View.X, View.Y, View.DX, View.DY, View.FrameColor, View.FillColor, [frbLeft, frbTop, frbRight, frbBottom], View.FrameWidth);
    frstRoundRect:
      DrawLRObjectInternal(View);
    frstEllipse:
      DrawEllipse(View.X, View.Y, View.DX, View.DY, View.FrameColor, View.FillColor, View.Frames, View.FrameWidth);
    frstTriangle:
      DrawLRObjectInternal(View);
    frstDiagonal1:
      DrawLine( View.X, View.Y, View.X + View.DX, View.Y + View.DY, View.FrameColor,View.FrameWidth);
    frstDiagonal2:
      DrawLine( View.X + View.DX, View.Y, View.X, View.Y + View.DY, View.FrameColor,View.FrameWidth);
 end;
end;

procedure TlrPdfExportFilter.DoBarCodeView(View: TfrCustomBarCodeView);
var
  FBmp: TBitmap;
  X, Y: Integer;
begin
  DrawRectView(View);
  FBmp:=View.GenerateBitmap;
  try
    DrawImage(View.X, View.Y, FBmp.Width, FBmp.Height, FBmp);
  finally
    FBmp.Free;
  end;
end;

procedure TlrPdfExportFilter.DoRoundRectView(View: TfrRoundRectView);
begin
//  DrawRectView(View);
  DrawLRObjectInternal(View);
end;

procedure TlrPdfExportFilter.WriteTextRectJustify(AExportFont: TExportFontItem;
  X, Y, W, H: TPDFFloat; const Text: string; Trimmed: boolean);
var
  S: String;
  Arr: TArrUTF8Item;
  AvailWidth, PxSpc, RxSpc, Extra: TPDFFloat;
  WordCount, SpcCount, SpcSize, Cini, CEnd, i: Integer;
  SpaceWidth, FTH: Single;
begin
  //Calc text height
  FTH:=AExportFont.TextHeight('Wg');

  X:=ConvetUnits(X);
  Y:=ConvetUnits(Y);
  W:=ConvetUnits(W);
  H:=ConvetUnits(H);

  AvailWidth:=W;
  // count words
  Arr := UTF8CountWords(Text, WordCount, SpcCount, SpcSize);
  // handle trimmed text
  S := Text;
  if (SpcCount>0) then
  begin
    Cini := 0;
    CEnd := Length(Arr)-1;
    if Trimmed then
    begin
      s := UTF8Trim(Text, [u8tKeepStart]);
      if Arr[CEnd].Space then
      begin
        Dec(CEnd);
        Dec(SpcCount);
      end;
    end;
    AvailWidth := AvailWidth - AExportFont.TextWidth(S);
  end;

  // check if long way is needed
  if (SpcCount>0) and (AvailWidth>0) then
  begin

    SpaceWidth := AExportFont.TextWidth(' ');
    PxSpc := AvailWidth / SpcCount;
    RxSpc := AvailWidth - PxSpc * SpcCount;
    if PxSPC=0 then
    begin
      PxSPC := 1;
      RxSpc := 0;
    end;

    for i:=CIni to CEnd do
      if Arr[i].Space then
      begin
        X := X + Arr[i].Count * SpaceWidth;
        if AvailWidth>0 then
        begin
          Extra := PxSpc;
          if RxSpc>0 then
          begin
            Extra:=Extra + ConvetUnits1(1);
            RxSpc:=RxSpc - ConvetUnits1(1);
          end;
          X := X + Extra;
          AvailWidth:=AvailWidth - Extra;
        end;
      end
      else
      begin
        s := Copy(Text, Arr[i].Index, Arr[i].Count);
        FCurPage.WriteText(X, Y + FTH, S);
        X := X + AExportFont.TextWidth(S)
      end;

  end
  else
    FCurPage.WriteText(X, Y + FTH, S);

  SetLength(Arr, 0);
end;

procedure TlrPdfExportFilter.WriteTextRect(AExportFont: TExportFontItem; X, Y,
  W: TPDFFloat; AText: string; AHAlign: TAlignment);
var
  FTW, FTH: Single;
  X1: TPDFFloat;
  Y1, fX, fY, fW: TPDFFloat;
begin
  fX := ConvetUnits(X);
  fY := ConvetUnits(Y);
  fW := ConvetUnits(W);

  //Calc text width
  FTW:=AExportFont.TextWidth(AText);
  //Calc text height
  FTH:=AExportFont.TextHeight(AText);

   case AHAlign of
    taLeftJustify:
      begin
        Y1:=fY + FTH;
        X1:=fX;
      end;
    taRightJustify:
      begin
        Y1:=fY + FTH;
        X1:=fX + fW - FTW;
        if X1 < fX then
          X1:=fX;
      end;
    taCenter:
      begin
        Y1:=fY + FTH;
        X1:=fX + fW / 2 - FTW / 2;
        if X1 < fX then
          X1:=fX;
      end;
  end;
  FCurPage.WriteText(X1, Y1, AText);
end;

procedure TlrPdfExportFilter.DrawRect(X, Y, W, H: TPDFFloat; ABorderColor,
  AFillColor: TColor; AFrames: TfrFrameBorders; ABorderWidth: TPDFFloat);
var
  fX, fY, fW, fH: Extended;
begin
  if (AFillColor = clNone) and (ABorderColor = clNone) then exit;

  if ABorderColor <> clNone then
    FCurPage.SetColor(ColorToPdfColor(ABorderColor), true);

  if (AFillColor <> clNone) then
    FCurPage.SetColor(ColorToPdfColor(AFillColor), false);

  fW:= ConvetUnits(W);
  fH:= ConvetUnits(H);
  fX:= ConvetUnits(X);
  fY:= ConvetUnits(Y);
  ABorderWidth:=ConvetUnits(ABorderWidth);

  if AFrames = [frbLeft, frbTop, frbRight, frbBottom] then
    FCurPage.DrawRect(fX, fY + fH, fW, fH, ABorderWidth, (AFillColor <> clNone), (ABorderColor <> clNone))
  else
  begin
    if frbLeft in AFrames then
      FCurPage.DrawLine(fX, fY, fX, fY + fH, ABorderWidth);

    if frbTop in AFrames then
      FCurPage.DrawLine(fX, fY, fX + fW, fY, ABorderWidth);

    if frbRight in AFrames then
      FCurPage.DrawLine(fX + fW, fY, fX + fW, fY + fH, ABorderWidth);

    if frbBottom in AFrames then
      FCurPage.DrawLine(fX, fY + fH, fX + fW, fY + fH, ABorderWidth);
  end;
end;

procedure TlrPdfExportFilter.DrawRectView(AView: TfrView);
begin
  if AView.Frames <> [] then
    DrawRect(AView.Left, AView.Top, AView.Width, AView.Height, AView.FrameColor, AView.FillColor, AView.Frames, AView.FrameWidth);
end;

procedure TlrPdfExportFilter.WriteURL(X, Y, W, H: TPDFFloat; AUrlText: string);
begin
  {$IF (FPC_FULLVERSION >= 30101)}
  X := ConvetUnits(X);
  Y := ConvetUnits(Y);
  W := ConvetUnits(W);
  H := ConvetUnits(H);
  FCurPage.AddExternalLink(X, Y + H, W, H, AUrlText, false);
  {$ENDIF}
end;

procedure TlrPdfExportFilter.DrawLine(X1, Y1, X2, Y2: TPDFFloat;
  ABorderColor: TColor; ABorderWidth: TPDFFloat);
begin
  if (ABorderColor = clNone) then exit;

  if ABorderColor <> clNone then
    FCurPage.SetColor(ColorToPdfColor(ABorderColor), true);


  FCurPage.DrawLine(
    ConvetUnits(X1),
    ConvetUnits(Y1),
    ConvetUnits(X2),
    ConvetUnits(Y2),
    ConvetUnits(ABorderWidth));
end;

procedure TlrPdfExportFilter.DrawEllipse(X, Y, W, H: TPDFFloat; ABorderColor,
  AFillColor: TColor; AFrames: TfrFrameBorders; ABorderWidth: TPDFFloat);
var
  fX, fY, fW, fH: Extended;
begin
  if (AFillColor = clNone) and (ABorderColor = clNone) then exit;

  if ABorderColor <> clNone then
    FCurPage.SetColor(ColorToPdfColor(ABorderColor), true);

  if (AFillColor <> clNone) then
    FCurPage.SetColor(ColorToPdfColor(AFillColor), false);

  fW:= ConvetUnits(W);
  fH:= ConvetUnits(H);
  fX:= ConvetUnits(X);
  fY:= ConvetUnits(Y);
  ABorderWidth:=ConvetUnits(ABorderWidth);

  FCurPage.DrawEllipse(fX, fY + fH, fW, fH, ABorderWidth, (AFillColor <> clNone), (ABorderColor <> clNone))
end;

procedure TlrPdfExportFilter.DrawImage(X, Y, W, H: integer; ABmp: TBitmap);
var
  X1, Y1, W1, H1: TPDFFloat;
  S: TMemoryStream;
  IDX: Integer;
begin
  begin
    S:=TMemoryStream.Create;
    try
      ABmp.SaveToStream(S);
      S.Position:=0;
      IDX := FPDFDocument.Images.AddFromStream(S, TFPReaderBMP, False);
      X1:=ConvetUnits(X);
      Y1:=ConvetUnits(Y);
      W1 := ConvetUnits(W); // FPDFDocument.Images[IDX].Width);
      H1 := ConvetUnits(H); // FPDFDocument.Images[IDX].Height);
      FCurPage.DrawImage(X1, Y1 + H1, W1, H1, IDX);  // left-bottom coordinate of image
    finally
      S.Free;
    end;
  end;
end;

procedure TlrPdfExportFilter.DrawLRObjectInternal(View: TfrView);
var
  FBmp: TBitmap;
  X, Y: Integer;
begin
  X:=View.X;
  Y:=View.Y;
  FBmp:=TBitmap.Create;
  try
    FBmp.Width:=View.DX + 1;
    FBmp.Height:=View.DY + 1;
    FBmp.Canvas.Brush.Color := clWhite;
    FBmp.Canvas.Brush.style := bsSolid;
    FBmp.Canvas.FillRect(0, 0, FBmp.Width, FBmp.Height);
    View.X:=0;
    View.Y:=0;
    View.Draw(FBmp.Canvas);
    DrawImage(X, Y, FBmp.Width, FBmp.Height, FBmp);
  finally
    FBmp.Free;
  end;
end;

constructor TlrPdfExportFilter.Create(AStream: TStream);
begin
  inherited Create(AStream);
  FPDFDocument:=TPDFDocument.Create(nil);
  FFontItems:=TExportFonts.Create(Self);
  InitFonts;
end;

destructor TlrPdfExportFilter.Destroy;
begin
  FreeAndNil(FFontItems);
  FreeAndNil(FPDFDocument);
  inherited Destroy;
end;

procedure TlrPdfExportFilter.OnBeginDoc;
begin
  inherited OnBeginDoc;
  FCurPageNo:=-1;
  FPDFDocument.Infos.Title := Application.Title;
{  FPDFDocument.Infos.Author := FAuthorPDF;
  FPDFDocument.Infos.Producer := FProducerPDF;}
  FPDFDocument.Infos.ApplicationName := ApplicationName;
  FPDFDocument.Infos.CreationDate := Now;

  FPDFDocument.Options:=FPDFDocument.Options + [poPageOriginAtTop, poUseRawJPEG, poSubsetFont];
  FPDFDocument.DefaultOrientation := ppoPortrait;

  FPDFDocument.StartDocument;
  FCurSection := FPDFDocument.Sections.AddSection;

  SetupFonts;
end;

procedure TlrPdfExportFilter.OnEndDoc;
begin
  inherited OnEndDoc;
  FPDFDocument.SaveToStream(Stream);
end;

procedure TlrPdfExportFilter.OnBeginPage;
var
  lrPg: PfrPageInfo;
begin
  inherited OnBeginPage;
  Inc(FCurPageNo);
  FCurPage := FPDFDocument.Pages.AddPage;
  FCurPage.UnitOfMeasure := uomMillimeters; //normal work only whis mm ??
  FCurSection.AddPage(FCurPage);

  //setup page size
  lrPg:=CurReport.EMFPages[FCurPageNo];
  case lrPg^.pgSize of
    9:FCurPage.PaperType := ptA4;
    11:FCurPage.PaperType := ptA5;
    1,2:FCurPage.PaperType := ptLetter;
    5:FCurPage.PaperType := ptLegal;
    7:FCurPage.PaperType := ptExecutive;
    //:FCurPage.PaperType := ptComm10;
    37:FCurPage.PaperType := ptMonarch;
    27:FCurPage.PaperType := ptDL;
    28:FCurPage.PaperType := ptC5;
    34:FCurPage.PaperType := ptB5;
  else
    FCurPage.PaperType := ptA4;
  end;

  if lrPg^.pgOr in [poPortrait, poReversePortrait] then
    FCurPage.Orientation:=ppoPortrait
  else //poReverseLandscape, poLandscape,
    FCurPage.Orientation:=ppoLandscape;
end;

procedure TlrPdfExportFilter.OnEndPage;
begin
  inherited OnEndPage;
end;

procedure TlrPdfExportFilter.OnData(x, y: Integer; View: TfrView);
begin
  InternalGapX:=2 + View.GapX;
  InternalGapY:=2 + View.GapY;

  if (View is TfrRoundRectView) then
    DoRoundRectView(TfrRoundRectView(View))
  else
  if (View is TfrMemoView) then
    DoMemoView(TfrMemoView(View))
  else
  if (View is TfrPictureView) then
    DoImageView(TfrPictureView(View))
  else
  if (View is TfrLineView) then
    DoLineView(TfrLineView(View))
  else
  if (View is TfrCheckBoxView) then
    DoCheckBoxView(TfrCheckBoxView(View))
  else
  if (View is TfrShapeView) then
    DoShapeView(TfrShapeView(View))
  else
  if (View is TfrCustomBarCodeView) then
    DoBarCodeView(TfrCustomBarCodeView(View))

    ;
end;

procedure TlrPdfExportFilter.OnText(x, y: Integer; const Text: String;
  View: TfrView);
var
  W: Double;
begin
  if (View is TfrMemoView) and Assigned(FCurFont) then
  begin
    if TfrMemoView(View).FirstLine then
      W:=TfrMemoView(View).Width - TfrMemoView(View).ParagraphGap - InternalGapX * 2
    else
      W:=TfrMemoView(View).Width - InternalGapX * 2;

    if TfrMemoView(View).Justify and not TfrMemoView(View).LastLine then
      WriteTextRectJustify(FCurFont, X + InternalGapX, Y, W, View.dy, Text, true)
    else
      WriteTextRect(FCurFont, X + InternalGapX, Y, W, Text, TfrMemoView(View).Alignment);
  end;
end;

procedure TlrPdfExportFilter.OnExported(x, y: Integer; View: TfrView);
begin
end;

initialization
  frRegisterExportFilter(TlrPdfExportFilter, 'PDF file (*.pdf)', '*.pdf');
end.

