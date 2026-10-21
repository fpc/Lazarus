{
 carbonrichmemo.pas 
 
 Author: Dmitry 'skalogryz' Boyarintsev 

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit CarbonRichMemo;

{$mode objfpc}{$H+}

interface

uses
  MacOSAll,

  LCLType, Classes, SysUtils,

  Controls, Graphics, StdCtrls,

  RichMemo, WSRichMemo,

  CarbonDef, CarbonUtils,
  CarbonProc, CarbonEdits;

type

  { TCarbonRichEdit }

  TCarbonRichEdit = class(TCarbonMemo)
  private
    fCaretVisible : Boolean;
    fisBackBlack  : Boolean;
    target  : EventTargetRef;
  protected
    function GetCreationOptions: TXNFrameOptions; override;
    procedure PostDraw(acontext: CGContextRef);
    procedure RegisterEvents; override;
  public
    function GetIndexedRunInfoFromRange(iIndex: ItemCount;   iStartOffset, iEndOffset: TXNOffset;
      var oRunStartOffset, oRunEndOffset: TXNOffset;
      oRunDataType: TXNDataTypePtr; iTypeAttributeCount: ItemCount;
      ioTypeAttributes: TXNTypeAttributesPtr): Boolean;
    function GetContinuousTypeAttributes(var oContinuousFlags: TXNContinuousFlags;
      iCount: ItemCount; var ioTypeAttributes: array of TXNTypeAttributes): Boolean;
    function SetTypeAttributes(iCount: ItemCount; const iTypeAttributes: array of TXNTypeAttributes;
      StartOffset, EndOffset: Integer): Boolean;
    procedure SetColor(const AColor: TColor); override;
    procedure InDelText(const text: WideString; replstart, repllength: Integer);
  end;


  { TCarbonWSCustomRichMemo }

  TCarbonWSCustomRichMemo = class(TWSCustomRichMemo)
    class procedure CutToClipboard(const AWinControl: TWinControl); override;
    class procedure CopyToClipboard(const AWinControl: TWinControl); override;
    class procedure PasteFromClipboard(const AWinControl: TWinControl); override;

    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class function GetStyleRange(const AWinControl: TWinControl; TextStart: Integer;
      var RangeStart, RangeLen: Integer): Boolean; override;
    class function GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer;
      var Params: TIntFontParams): Boolean; override;
    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      {Mask: TTextStyleMask;} const Params: TIntFontParams); override;
    class procedure SetParaAlignment(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AAlign: TIntParaAlignment); override;
    class procedure SetHideSelection(const ACustomEdit: TCustomEdit; AHideSelection: Boolean); override;
    class procedure InDelText(const AWinControl: TWinControl; const TextUTF8: String; DstStart, DstLen: Integer); override;
    class function LoadRichText(const AWinControl: TWinControl; Src: TStream): Boolean; override;
    class function SaveRichText(const AWinControl: TWinControl; Dst: TStream): Boolean; override;

    {$ifdef RMCARBONSELSTART}
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    {$endif}
  end;

implementation

type
  TIntCustomRichMemo = class(TCustomRichMemo);

// Notes:

// http://developer.apple.com/DOCUMENTATION/Carbon/Reference/Multilingual_Text_Engine/Reference/reference.html
// TXNFlattenObjectToCFDataRef
//
//oDataRef
//   On input, points to a structure of type CFDataRef. On output, points to a flattened
//   version of the text object in the format specified by the iTXNDataType parameter.
//   You are responsible to retain the returned CFDataRef.
//
// It's unclear (though expected), if a user is responsible to release returned CFData object.
//
// Releasing is necessary, as noted this mailling list discussion
// http://lists.apple.com/archives/carbon-dev/2005/Feb/msg00657.html


const
  TXNAttributesMax = 10;

function GetATSUFontName(AStyle: ATSUStyle): String;
var
  fontid: ATSUFontID;
begin
  ATSUGetAttribute(AStyle, kATSUFontTag, sizeof(ATSUFontID), @fontid, nil);
  Result := CarbonFontIDToFontName(fontid);
end;

function GetATSUFontSize(ASTyle: ATSUStyle): Integer;
var
  sz  : fixed;
begin
  ATSUGetAttribute(AStyle, kATSUSizeTag, sizeof(fixed), @sz, nil);
  Result := Fix2Long(sz);
end;

procedure GetATSUFontRGBAColor(AStyle: ATSUStyle; var r,g,b,a: Byte);
var
  rgba : ATSURGBAlphaColor;
begin
  ATSUGetAttribute(AStyle, kATSURGBAlphaColorTag, sizeof(ATSURGBAlphaColor), @rgba, nil);
  r := Round(rgba.red*255);
  g := Round(rgba.green*255);
  b := Round(rgba.blue*255);
  a := Round(rgba.alpha*255);
end;

function GetATSUFontColor(AStyle: ATSUStyle): TColor;
var
  r,g,b,a: Byte;
begin
  GetATSUFontRGBAColor(AStyle, r,g,b,a);
  Result := (b shl 16) or (g shl 8) or r;
end;

function GetATSUFontStyles(AStyle: ATSUStyle): TFontStyles;
var
  b : Boolean;
begin
  b:=false;
  Result := [];
  ATSUGetAttribute(AStyle, kATSUQDBoldfaceTag, sizeof(Boolean), @b, nil);
  if b then Include(Result, fsBold);
  ATSUGetAttribute(AStyle, kATSUQDItalicTag, sizeof(Boolean), @b, nil);
  if b then Include(Result, fsItalic);
  ATSUGetAttribute(AStyle, kATSUQDUnderlineTag, sizeof(Boolean), @b, nil);
  if b then Include(Result, fsUnderline);
  ATSUGetAttribute(AStyle, kATSUStyleStrikeThroughTag , sizeof(Boolean), @b, nil);
  if b then Include(Result, fsStrikeOut);
end;

function GetValidRichEdit(AWinControl: TWinControl): TCarbonRichEdit;
begin
  if Assigned(AWinControl) and (AWinControl.Handle<>0) and (TObject(AWinControl.Handle) is TCarbonRichEdit) then
    Result := TCarbonRichEdit(AWinControl.Handle)
  else
    Result := nil;
end;

procedure AttrSetFontName(const FontName: String; var Attr: TXNTypeAttributes);
begin
  Attr.tag := kATSUFontTag;
  Attr.size := SizeOf(ATSUFontID);
  Attr.data.dataValue := FindCarbonFontID(FontName);
end;

procedure AttrSetColor(var MacColor: RGBColor; var Attr: TXNTypeAttributes);
begin
  Attr.tag := kTXNQDFontColorAttribute;
  Attr.size := kTXNQDFontColorAttributeSize;
  Attr.data.dataPtr := @MacColor;
end;

procedure AttrSetSize(FontSize: Integer; var Attr: TXNTypeAttributes);
begin
  Attr.tag := kTXNQDFontSizeAttribute;
  Attr.size := kTXNQDFontSizeAttributeSize;
  Attr.data.dataValue := Long2Fix(FontSize);
end;

procedure AttrSetStyle(FontStyle: TFontStyles; var Attr: TXNTypeAttributes);
begin
  Attr.tag := kTXNQDFontStyleAttribute;
  Attr.size := kTXNQDFontStyleAttributeSize;
  Attr.data.dataValue := FontStyleToQDStyle(FontStyle)
end;

procedure AttrSetATSUStyle(AStyle: ATSUStyle; var Attr: TXNTypeAttributes);
begin
  Attr.tag := kTXNATSUIStyle;
  Attr.size := kTXNATSUIStyleSize;
  Attr.data.dataPtr := astyle;
end;

procedure ParamsToTXNAttribs({ParamsMask: TTextStyleMask;} const Params: TIntFontParams;
  var Attr: array of TXNTypeAttributes; var AttrCount: Integer; var MacColor: RGBColor);
begin
  AttrCount := 0;
  //todo: replace QuickDraw style by ATSU style

  //if tsm_Color in ParamsMask then begin
    MacColor := ColorToRGBColor(Params.Color);
    AttrSetColor(MacColor, Attr[AttrCount] );
    inc(AttrCount);
  //end;
  

  //if tsm_Name in ParamsMask then begin
    AttrSetFontName(Params.Name, Attr[AttrCount] );
    inc(AttrCount);
  //end;

  //if tsm_Size in ParamsMask then begin
    AttrSetSize(Params.Size, Attr[AttrCount] );
    inc(AttrCount);
  //end;

  //if tsm_Styles in ParamsMask then begin
    AttrSetStyle(Params.Style, Attr[AttrCount]);
    inc(AttrCount);
  //end;
end;

{ TCarbonWSCustomRichMemo }

class procedure TCarbonWSCustomRichMemo.CutToClipboard(const AWinControl: TWinControl);
var
  memo      : TCarbonRichEdit;
begin
  memo := GetValidRichEdit(AWinControl);
  if not Assigned(memo) then Exit;
  TXNCut(memo.GetTextObject);
end;

class procedure TCarbonWSCustomRichMemo.CopyToClipboard(const AWinControl: TWinControl);
var
  memo      : TCarbonRichEdit;
begin
  memo := GetValidRichEdit(AWinControl);
  if not Assigned(memo) then Exit;
  TXNCopy(memo.GetTextObject);
end;

class procedure TCarbonWSCustomRichMemo.PasteFromClipboard(const AWinControl:TWinControl);
var
  memo      : TCarbonRichEdit;
begin
  memo := GetValidRichEdit(AWinControl);
  if not Assigned(memo) then Exit;
  TXNPaste(memo.GetTextObject);
end;

class function TCarbonWSCustomRichMemo.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  RE: TCarbonRichEdit;
begin
  RE:=TCarbonRichEdit.Create(AWinControl, AParams);
  Result := TLCLIntfHandle(RE);
  Re.SetWordWrap(TCustomRichMemo(AWinControl).WordWrap);
  Re.SetBorderVisible(TCustomRichMemo(AWinControl).BorderStyle=bsSingle);
end;

class function TCarbonWSCustomRichMemo.GetStyleRange(const AWinControl: TWinControl;
  TextStart: Integer; var RangeStart, RangeLen: Integer): Boolean;
var
  edit      : TCarbonRichEdit;
  RngStart  : TXNOffset;
  RngEnd    : TXNOffset;
begin
  Result := false;
  edit := GetValidRichEdit(AWinControl);
  if not Assigned(edit) then Exit;

  Result := edit.GetIndexedRunInfoFromRange(0, TextStart, TextStart, RngStart, RngEnd, nil, 0, nil);
  if Result then begin
    RangeStart := RngStart;
    RangeLen := RngEnd - RngStart;
  end;   

end;

class function TCarbonWSCustomRichMemo.GetTextAttributes(const AWinControl: TWinControl;
  TextStart: Integer; var Params: TIntFontParams): Boolean;
var
  edit  : TCarbonRichEdit;
  attr  : array [0..2] of TXNTypeAttributes;
  flags   : TXNContinuousFlags;

  astyle  : ATSUStyle;
  maccolor  : RGBColor;

  oStart  : TXNOffset;
  oEnd    : TXNOffset;
  txobj   : TXNObject;
begin
  Result := false;
  edit := GetValidRichEdit(AWinControl);
  if not Assigned(edit) then Exit;

  txobj := HITextViewGetTXNObject(edit.Widget);
  if not Assigned(txobj) then Exit;
  
  TXNGetSelection(txobj, oStart, oEnd);
  TXNSetSelection(txobj, TextStart, TextStart+1);
 
  ATSUCreateStyle(astyle);
  AttrSetATSUStyle(astyle, attr[0]);
  AttrSetStyle([], attr[1]);
  FillChar(maccolor, sizeof(maccolor), 0);
  AttrSetColor(maccolor, attr[2]);

  Result := edit.GetContinuousTypeAttributes(flags, 3, attr);
  Params.Name := GetATSUFontName(astyle);
  Params.Color := RGBColorToColor(maccolor);
  Params.Style := GetATSUFontStyles(astyle) + QDStyleToFontStyle(attr[1].data.dataValue);
  Params.Size := GetATSUFontSize(astyle);

  ATSUDisposeStyle(astyle);

  TXNSetSelection(txobj, oStart, oEnd);
end;

class procedure TCarbonWSCustomRichMemo.SetTextAttributes(const AWinControl: TWinControl;
  TextStart, TextLen: Integer; {Mask: TTextStyleMask; }const Params: TIntFontParams);
var
  memo      : TCarbonRichEdit;
  Attr      : array [0..TXNAttributesMax-1] of TXNTypeAttributes;
  Count     : Integer;
  maccolor  : RGBColor;
begin
  memo := GetValidRichEdit(AWinControl);
  if not Assigned(memo) then Exit;

  ParamsToTXNAttribs(Params, attr, Count, maccolor);

  memo.SetTypeAttributes(Count, Attr, TextStart, TextStart+TextLen);
end;

class procedure TCarbonWSCustomRichMemo.SetParaAlignment(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AAlign: TIntParaAlignment);
begin
 { Doesn't seem to be possible. See this:
  http://lists.apple.com/archives/carbon-dev/2005/Jun/msg01068.html }
end;

class procedure TCarbonWSCustomRichMemo.SetHideSelection(const ACustomEdit: TCustomEdit; AHideSelection: Boolean);
begin
  //todo:
end;

class procedure TCarbonWSCustomRichMemo.InDelText(const AWinControl: TWinControl; const TextUTF8: String; DstStart, DstLen: Integer);
var
  memo   : TCarbonRichEdit;
begin
  memo := GetValidRichEdit(AWinControl);
  if not Assigned(memo) then Exit;
  memo.InDelText(UTF8Decode(TextUTF8), DstStart, DstLen);
end;

function GetTempFileUniqueName(forcedir: Boolean=true): String;
var
  g : TGUID;
  d : String;
begin
  repeat
    CreateGUID(g);
    Result := GetTempFileName +  GUIDToString(g) +'.rtf';
  until not FileExists(Result);
  if forcedir then begin
    d := ExtractFileDir(Result);
    if not DirectoryExists(d) then ForceDirectories(d);
  end;
end;

class function TCarbonWSCustomRichMemo.LoadRichText(const AWinControl: TWinControl;
  Src: TStream): Boolean;
var
  edit  : TCarbonRichEdit;
  filename: String;
  fs  : TFileStream;
  cf  : CFStringRef;
  url : CFURLRef;
begin
  Result := false;
  edit := GetValidRichEdit(AWinControl);
  if not Assigned(edit) then Exit;

  Result := false;
  filename := GetTempFileUniqueName;

  try
    fs := TFileStream.Create(filename, fmCreate);
    try
      fs.CopyFrom(Src, Src.Size - Src.Position);
    finally
      fs.Free;
    end;

    CreateCFString(filename, cf);
    url := CFURLCreateWithFileSystemPath (kCFAllocatorDefault, cf, kCFURLPOSIXPathStyle, false);
    try
      TXNSelectAll(HITextViewGetTXNObject(edit.Widget));
      Result := TXNSetDataFromCFURLRef( HITextViewGetTXNObject(edit.Widget), url, kTXNStartOffset, kTXNEndOffset) = noErr;
    finally
      CFRelease(url);
      FreeCFString(cf);
    end;

  except
    Result := false;
  end;

  if FileExists(filename) then DeleteFile(filename);
end;

class function TCarbonWSCustomRichMemo.SaveRichText(const AWinControl: TWinControl;
  Dst: TStream): Boolean;
var
  edit  : TCarbonRichEdit;
  data  : CFDataRef;
  sz    : Integer;
  ptr   : PByteArray;
begin
  Result := false;
  edit := GetValidRichEdit(AWinControl);
  if not Assigned(edit) or not Assigned(Dst) then Exit;

  Result := TXNFlattenObjectToCFDataRef(HITextViewGetTXNObject(edit.Widget), kTXNRichTextFormatData, data) = noErr;
  if not Result and Assigned(data) then Exit;

  try
    sz := CFDataGetLength(data);
    ptr := CFDataGetBytePtr(data);
    if Assigned(ptr) and (sz > 0) then Dst.Write(ptr^, sz);
    Result := false;
  finally
    // see TXNFlattenObjectToCFDataRef notes
    CFRelease(data);
  end;
end;
{$ifdef RMCARBONSELSTART}
class procedure TCarbonWSCustomRichMemo.SetSelStart(
  const ACustomEdit: TCustomEdit; NewStart: integer);
var
  edit : TCarbonRichEdit;
  sl   : Integer;
begin
  edit := GetValidRichEdit(ACustomEdit);
  if Assigned(edit) then begin
    edit.SetSelStart(NewStart);
    TXNShowSelection( HITextViewGetTXNObject(  edit.Widget ), false);

    TIntCustomRichMemo(ACustomEdit).DoSelectionChange;
  end;
end;
{$ENDIF}

{ TCarbonRichEdit }

function TCarbonRichEdit.GetCreationOptions: TXNFrameOptions;
begin
  Result := kOutputTextInUnicodeEncodingMask;
end;

procedure TCarbonRichEdit.PostDraw(acontext: CGContextRef);
var
  point : HIPoint;
  obj   : TXNObject;
  pst,
  pend  : TXNOffset;
  //x,y   : Integer;
  //w,h     : Fixed;
  //linenum : UInt32;
  r       : CGRect;
  //flags   : TXNContinuousFlags;
  //attr    : TXNTypeAttributes;
  event   : EventRef;
  ref     : ControlRef;
  refcon  : Integer;
  //ofs     : LongWord;
  hh      : Word;
const
  caretHorzOffset = 1;
  caretHorzWidth  = 2;
begin
  if not fisBackBlack then Exit;

  fCaretVisible := not fCaretVisible ;
  if not fCaretVisible then Exit;

  obj := HITextViewGetTXNObject(Widget);
  if not Assigned(obj) then Exit;

  TXNGetSelection(obj, pst, pend);
  if TXNOffsetToHIPoint(obj, pst, point) <> noErr then Exit;

  // getting current line-height, anyone knows a better way?
  CreateEvent(kCFAllocatorDefault, kEventClassTextInput, kEventTextInputOffsetToPos, 0, 0, event);
  ref := Widget;
  refcon := 0;
  SetEventParameter(event, kEventParamTextInputSendComponentInstance, typeControlRef, sizeof(Widget), ref);
  SetEventParameter(event, kEventParamTextInputSendRefCon, typeLongInteger, sizeof(refcon), @refcon);
  SetEventParameter(event, kEventParamTextInputSendTextOffset, typeLongInteger, sizeof(pst), @pst);
  SendEventToEventTarget(event, target);
  GetEventParameter(event, kEventParamTextInputReplyLineHeight, typeShortInteger, nil, sizeof(hh), nil, @hh);
  CFRelease(event);

  point.x := point.x-caretHorzOffset;
  r.origin := point;
  r.size.width:=caretHorzWidth;
  r.size.height:=hh;

  CGContextSetRGBFillColor(aContext, 1,1,1,1);
  CGContextFillRect(aContext, r);
end;

function CarbonRichEdit_ChangeSel(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  sofs, eofs: TXNOffset;
  sofs2, eofs2: TXNOffset;
begin
  // selection before
  TXNGetSelection( HITextViewGetTXNObject(AWidget.Widget), sofs, eofs);

  Result := CallNextEventHandler(ANextHandler, AEvent);

  // selection after
  TXNGetSelection( HITextViewGetTXNObject(AWidget.Widget), sofs2, eofs2);
  // seems like something has changed!

  // Sorry, for the direct access!
  if (sofs<>sofs2) or (eofs<>eofs2) then
    TIntCustomRichMemo( AWidget.LCLObject ).DoSelectionChange;
end;

function CarbonRichEdit_Draw(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  context  : CGContextRef;
begin
  Result := CallNextEventHandler(ANextHandler, AEvent);
  if Assigned(AWidget) then
  begin
    GetEventParameter(AEvent, kEventParamCGContextRef, typeCGContextRef, nil, sizeof(context), nil, @context);
    if not Assigned(context) then Exit;
    TCarbonRichEdit(AWidget).PostDraw(context);
  end;
end;

procedure TCarbonRichEdit.RegisterEvents;
var
  TmpSpec: EventTypeSpec;
  TmpSpecArr: array [0..2] of EventTypeSpec;
begin
  inherited RegisterEvents;

  if GetEditPart >= 0 then
  begin
    TmpSpec := MakeEventSpec(kEventClassControl, kEventControlDraw);
    InstallControlEventHandler(Widget,
      RegisterEventHandler(@CarbonRichEdit_Draw),
      1, @TmpSpec, Pointer(Self), nil);
  end;

  // It's unclear, if there's a better way for tracking change of selection
  TmpSpecArr[0]:=MakeEventSpec(kEventClassKeyboard, kEventRawKeyRepeat); // by keyboard
  TmpSpecArr[1]:=MakeEventSpec(kEventClassKeyboard, kEventRawKeyDown);
  TmpSpecArr[2]:=MakeEventSpec(kEventClassControl, kEventControlTrack);  // by mouse

  InstallControlEventHandler(Widget,
    RegisterEventHandler(@CarbonRichEdit_ChangeSel),
    3, @TmpSpecArr, Pointer(Self), nil);

  target :=HIViewGetEventTarget(Widget);
end;

function TCarbonRichEdit.GetIndexedRunInfoFromRange(iIndex: ItemCount;
  iStartOffset, iEndOffset: TXNOffset;
  var oRunStartOffset, oRunEndOffset: TXNOffset;
  oRunDataType: TXNDataTypePtr; iTypeAttributeCount: ItemCount;
  ioTypeAttributes: TXNTypeAttributesPtr): Boolean;
begin
  Result := TXNGetIndexedRunInfoFromRange( HITextViewGetTXNObject(Widget),
    iIndex, iStartOffset, iEndOffset, @oRunStartOffset, @oRunEndOffset,
    oRunDataType, iTypeAttributeCount, ioTypeAttributes ) = noErr;
end;

function TCarbonRichEdit.GetContinuousTypeAttributes(
  var oContinuousFlags: TXNContinuousFlags; iCount: ItemCount;
  var ioTypeAttributes: array of TXNTypeAttributes): Boolean;
begin
  Result := TXNGetContinuousTypeAttributes(HITextViewGetTXNObject(Widget),
              oContinuousFlags, iCount, @ioTypeAttributes[0]) = noErr;
end;

function TCarbonRichEdit.SetTypeAttributes(iCount: ItemCount;
  const iTypeAttributes: array of TXNTypeAttributes; StartOffset,
  EndOffset: Integer): Boolean;
begin
  Result := TXNSetTypeAttributes(HITextViewGetTXNObject(Widget), iCount,
              @iTypeAttributes[0], StartOffset, EndOffset) = noErr;
end;

procedure TCarbonRichEdit.SetColor(const AColor: TColor);
begin
  fisBackBlack := AColor = clBlack;
  inherited SetColor(AColor);
end;

procedure TCarbonRichEdit.InDelText(const text: WideString; replstart, repllength: Integer); 
var
  data    : UnivPtr;
  datasz  : ByteCount;
  replend : Integer;
begin
  if text = '' then begin
    data := nil;
    datasz := 0;
  end else begin
    data := @text[1];
    datasz := length(text)*2;
  end;
  if repllength < 0 then replend  := kTXNEndOffset
  else replend := replstart+repllength;                                                                                         
  TXNSetData(HITextViewGetTXNObject(Widget), kTXNUnicodeTextData, data, datasz, replstart, replend);
end;

end.

