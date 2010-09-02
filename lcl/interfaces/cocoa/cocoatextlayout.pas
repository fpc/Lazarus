unit CocoaTextLayout;
//todo: Implement TCoreTextLayout using CoreText API for newer OSes

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
    fBuffer     : WideString;
    fUTF8       : String;
    FDX         : PIntegerArray;

    FLayout     : ATSUTextLayout;
    FStyle      : ATSUStyle;

    FTextBefore : ATSUTextMeasurement;
    FTextAfter  : ATSUTextMeasurement;
    FAscent     : ATSUTextMeasurement;
    FDescent    : ATSUTextMeasurement;

    FValidSize  : Boolean;
    procedure RecountSize;
    procedure DoJustify(iLineRef: ATSULineRef; var Handled: Boolean);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetFont(AFont: TCocoaFont); override;
    procedure SetText(UTF8Text: PChar; ByteSize: Integer); override;
    function GetSize: TSize; override;
    procedure Draw(cg: CGContextRef; X, Y: Integer; DX: PInteger); override;
  end;

  { TCoreTextLayout }

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

var
  ATSUDirectUPP : ATSUDirectLayoutOperationOverrideUPP = nil; //NewATSUDirectLayoutOperationOverrideUPP(@ATSUCallback)

function ATSUCallback(iCurrentOperation: ATSULayoutOperationSelector; iLineRef: ATSULineRef; iRefCon: UInt32; iOperationCallbackParameterPtr: UnivPtr;
  var oCallbackStatus: ATSULayoutOperationCallbackStatus ): OSStatus; {$ifdef DARWIN}mwpascal;{$endif}
var
  Buffer  : TASTUITextLayout;
  Handled : Boolean;
begin
  Result := noErr;
  Buffer := TASTUITextLayout(iRefCon);
  oCallbackStatus:=kATSULayoutOperationCallbackStatusHandled;

  if Assigned(Buffer) then
    Buffer.DoJustify(iLineRef, Handled);
end;

procedure TASTUITextLayout.DoJustify(iLineRef: ATSULineRef; var Handled: Boolean);
type
	ATSLayoutRecord1 = packed record
		glyphID: ATSGlyphRef;
		flags: ATSGlyphInfoFlags;
		originalOffset: ByteCount;
		realPos: Fixed;
	end;

type
  TATSLayoutRecordArray = array [Word] of ATSLayoutRecord1;
  PATSLayoutRecordArray = ^TATSLayoutRecordArray;
var
  i, ofs  : Integer;
  Layouts   : PATSLayoutRecordArray;
  LayCount  : ItemCount;
begin
  if not Assigned(FDX) then Exit;
  Laycount:=0;
  ATSUDirectGetLayoutDataArrayPtrFromLineRef( iLineRef,
    kATSUDirectDataLayoutRecordATSLayoutRecordVersion1, true, @Layouts, Laycount);
  if Assigned(Layouts) and (Laycount>0) then
  begin
    ofs:=0;
    for i:=0 to LayCount-1 do
    begin
      Layouts^[i].realPos:=Long2Fix(ofs);
      inc(ofs, FDX^[i]);
    end;
  end;
  ATSUDirectReleaseLayoutDataArrayPtr(iLineRef, kATSUDirectDataLayoutRecordATSLayoutRecordCurrent, @Layouts );
  Handled:=True;
end;


procedure TASTUITextLayout.Draw(cg:CGContextRef;X,Y:Integer;DX:PInteger);
var
  MX, MY    : Integer;

  Tag       : ATSUAttributeTag;
  Size      : ByteCount;
  Value     : ATSUAttributeValuePtr;
  OverSpec  : ATSULayoutOperationOverrideSpecifier;
begin
  if not Assigned(cg) then Exit;
  if not FValidSize then RecountSize;

  MX:=0;
  MY:=0;
  Tag := kATSUCGContextTag;
  Size := sizeOf(CGContextRef);
  Value := @cg;
  ATSUSetLayoutControls(FLayout, 1, @Tag, @Size, @Value);

  Tag := kATSULayoutOperationOverrideTag;
  Size := sizeof (ATSULayoutOperationOverrideSpecifier);
  Value := @OverSpec;
  FillChar(OverSpec, sizeof(OverSpec), 0);
  if Assigned(Dx) then begin
    FDX := PIntegerArray(Dx);
    OverSpec.operationSelector := kATSULayoutOperationPostLayoutAdjustment;
    if not Assigned(ATSUDirectUPP) then ATSUDirectUPP:=NewATSUDirectLayoutOperationOverrideUPP(@ATSUCallback);
    OverSpec.overrideUPP := ATSUDirectUPP;
  end else
    FDX:=nil;
  ATSUSetLayoutControls (FLayout, 1, @Tag, @Size, @Value);

  ATSUDrawText(FLayout, kATSUFromTextBeginning, kATSUToTextEnd,
    IntToFix(X)- FTextBefore + MX, IntToFix(Y) - FAscent + MY);
end;

procedure InitTextLayout;
begin
  if not Assigned(TextLayoutClass) then begin
    TextLayoutClass:=TASTUITextLayout;
  end;
end;

procedure ReleaseTextLayout;
begin
  if Assigned(ATSUDirectUPP) then DisposeATSUDirectLayoutOperationOverrideUPP(ATSUDirectUPP);
end;

initialization
  InitTextLayout;

finalization
  ReleaseTextLayout;

end.
