unit CocoaTextLayout;

interface

{$mode objfpc}{$H+}

uses
  MacOSAll, CocoaAll,
  Types, SysUtils, LCLType, CocoaGDIObjects;

type
  { TASTUITextLayout }

  // legacy layout used for Mac OS X 10.4
  TASTUITextLayout = class(TCocoaTextLayout)
  private
    fBuffer : WideString;
    fUTF8   : String;
    FLayout : ATSUTextLayout;
    FStyle  : ATSUStyle;

    FTextBefore : ATSUTextMeasurement;
    FTextAfter  : ATSUTextMeasurement;
    FAscent     : ATSUTextMeasurement;
    FDescent    : ATSUTextMeasurement;

    FValidSize  : Boolean;
    procedure RecountSize;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetFont(AFont: TCocoaFont); override;
    procedure SetText(UTF8Text: PChar; ByteSize: Integer); override;
    function GetSize: TSize; override;
    procedure Draw(cg: CGContextRef; X, Y: Integer; DX: PInteger; DXCount: Integer); override;
  end;

  { TCoreTextLayout }

  //todo: use CoreText for newer OSes
  //TCoreTextLayout = class(TCocoaTextLayout);

implementation

{ TASTUITextLayout }

function IntToFix(i: integer): Integer; inline;
begin
  Result:=i shl 16;
end;

function FixToInt(f: Integer): Integer; inline;
begin
  Result:=Round(Fix2X(F));
end;

procedure TASTUITextLayout.RecountSize;
begin
  ATSUGetUnjustifiedBounds(FLayout, kATSUFromTextBeginning, kATSUToTextEnd,
    FTextBefore, FTextAfter, FAscent, FDescent);
end;

constructor TASTUITextLayout.Create;
begin
  // create text layout
  ATSUCreateTextLayout(FLayout);
  SetText(#0, 1);
  ATSUSetTextLayoutRefCon(FLayout, URefCon(Self));

  ATSUCreateStyle(FStyle);

  // allow font substitution for exotic glyphs
  ATSUSetTransientFontMatching(FLayout, True);
end;

destructor TASTUITextLayout.Destroy;
begin
  ATSUDisposeTextLayout(FLayout);
  ATSUDisposeStyle(FStyle);
  inherited Destroy;
end;

const
  DefaultFont = 'Lucida Grande';
  DefaultSize = 13;

function FindATSUFontID(const FontName: String): ATSUFontID;
var
  fn  : String;
begin
  Result := 0;
  if CompareText(FontName, 'default')=0 then fn:=DefaultFont else fn:=FontName;
  if (fn <> '') then
    ATSUFindFontFromName(@fn[1], Length(fn),
        kFontFullName, kFontMacintoshPlatform, kFontRomanScript,
        kFontEnglishLanguage, Result);
end;

procedure TASTUITextLayout.SetFont(AFont:TCocoaFont);
var
  Attr: ATSUAttributeTag;
  M: ATSUTextMeasurement;
  O: ATSStyleRenderingOptions;
  B: Boolean;
  S: ByteCount;
  A: ATSUAttributeValuePtr;
  ID: ATSUFontID;
const
  ATSStyleRenderingOption: array [Boolean] of ATSStyleRenderingOptions =
    (kATSStyleNoAntiAliasing, kATSStyleApplyAntiAliasing);
begin
  if not Assigned(AFont) then Exit;

  ID := FindATSUFontID(AFont.Name);

  if ID <> 0 then
  begin
    Attr := kATSUFontTag;
    A := @ID;
    S := SizeOf(ID);
    ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);
  end;

  Attr := kATSUSizeTag;
  M := IntToFix(Abs(AFont.Size));
  A := @M;
  S := SizeOf(M);
  ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);

  S := SizeOf(B);
  Attr := kATSUQDBoldfaceTag;
  B := cfs_Bold in AFont.Style;
  A := @B;
  ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);

  Attr := kATSUQDItalicTag;
  B := cfs_Italic in AFont.Style;
  A := @B;
  ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);

  Attr := kATSUQDUnderlineTag;
  B := cfs_Underline in AFont.Style;
  A := @B;
  ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);

  Attr := kATSUStyleStrikeThroughTag;
  B := cfs_Strikeout in AFont.Style;
  A := @B;
  ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);

  Attr := kATSUStyleRenderingOptionsTag;
  O := ATSStyleRenderingOption[AFont.Antialiased];
  A := @O;
  S := SizeOf(O);
  ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);

  FValidSize:=False;
end;

procedure TASTUITextLayout.SetText(UTF8Text: PChar; ByteSize: Integer);
begin
  if (ByteSize=length(fUTF8)) and (fUTF8<>'') and
    (CompareChar(UTF8Text^, fUTF8[1], ByteSize)=0) then Exit; // same buffer, nothing to change!

  SetLength(fUTF8, ByteSize);
  if ByteSize>0 then
    System.Move(UTF8Text^, fUTF8[1], ByteSize)
  else
    fUTF8:='';

  fBuffer:=UTF8Decode(fUTF8);
  if fBuffer='' then fBuffer:=#0;
  ATSUSetTextPointerLocation(FLayout, @fBuffer[1], 0, length(fBuffer), length(fBuffer));
  ATSUSetRunStyle(FLayout, FStyle, kATSUFromTextBeginning, kATSUToTextEnd);

  FValidSize:=False;
end;

function TASTUITextLayout.GetSize:TSize;
begin
  if not FValidSize then RecountSize;
  Result.cx := FixToInt(FTextAfter - FTextBefore);
  Result.cy := FixToInt(FDescent + FAscent);
end;

procedure TASTUITextLayout.Draw(cg:CGContextRef;X,Y:Integer;DX:PInteger;DXCount: Integer);
var
  MX, MY      : Integer;

  Tag       : ATSUAttributeTag;
  DataSize  : ByteCount;
  PValue    : ATSUAttributeValuePtr;
begin
  if not Assigned(cg) then Exit;
  if not FValidSize then RecountSize;

  MX:=0;
  MY:=0;
  Tag := kATSUCGContextTag;
  DataSize := sizeOf(CGContextRef);
  PValue := @cg;
  ATSUSetLayoutControls(FLayout, 1, @Tag, @DataSize, @PValue);

  ATSUDrawText(FLayout, kATSUFromTextBeginning, kATSUToTextEnd,
    IntToFix(X)- FTextBefore + MX, IntToFix(Y) - FAscent + MY);
end;


procedure InitTextLayout;
begin
  if not Assigned(TextLayoutClass) then
    TextLayoutClass:=TASTUITextLayout;
end;

initialization
  InitTextLayout;

end.
