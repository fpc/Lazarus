unit CocoaRichMemo;

interface

{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch objectivec2}

uses
  CocoaAll, Classes, SysUtils,
  LCLType, Graphics, Controls, StdCtrls,
  CocoaPrivate, CocoaTextEdits, CocoaUtils, CocoaWSCommon, CocoaWSStdCtrls,
  {$ifndef RMLCL18} // it can be defined in the package's CustomOptions -dRMLCL18
  CocoaScrollers, // this unit was introduced in summer-fall 2018 with Lazarus 2.0 release
  {$endif}
  WSRichMemo, RichMemo;

type
  TCocoaRichView = objcclass(TCocoaTextView)
  public
    scale : Double;
  end;

  { TCocoaWSCustomRichMemo }

  TCocoaWSCustomRichMemo = class(TWSCustomRichMemo)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class function GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer;
      var Params: TIntFontParams): Boolean; override;
          { Returns True if passed text styles are indentical }
    class function FPisEqual(const FP, FPRef : TFontParams) : boolean;
          { Returns with the range (in 0 based bytes) of the style that TextStart is in }
    class function GetStyleRange(const AWinControl: TWinControl; TextStart: Integer;
                                       var RangeStart, RangeLen: Integer): Boolean; override;
    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const Params: TIntFontParams); override;

    class procedure SetParaAlignment(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AAlign: TIntParaAlignment); override;
    class function GetParaAlignment(const AWinControl: TWinControl; TextStart: Integer;
      var AAlign: TIntParaAlignment): Boolean; override;

    class function GetParaMetric(const AWinControl: TWinControl; TextStart: Integer;
      var AMetric: TIntParaMetric): Boolean; override;
    class procedure SetParaMetric(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AMetric: TIntParaMetric); override;

    class procedure SetParaTabs(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AStopList: TTabStopList); override;
    class function GetParaTabs(const AWinControl: TWinControl; TextStart: integer;
      var AStopList: TTabStopList): Boolean; override;

    class procedure SetZoomFactor(const AWinControl: TWinControl; AZoomFactor: Double); override;

    class procedure InDelText(const AWinControl: TWinControl;
      const TextUTF8: String; DstStart, DstLen: Integer); override;
    class function LoadRichText(const AWinControl: TWinControl; Source: TStream): Boolean; override;
    class function SaveRichText(const AWinControl: TWinControl; Dest: TStream): Boolean; override;

    class procedure SetTransparentBackground(const AWinControl: TWinControl; ATransparent: Boolean); override;
  end;

implementation

function MemoTextView(AWinControl: TWinControl): TCocoaTextView;
begin
  if not Assigned(AWinControl) or (AWinControl.Handle=0) then
    Result := nil
  else
    Result := TCocoaTextView(NSScrollView(AWinControl.Handle).documentView);
end;

function ParaRange(txt: NSString; textOffset, TextLen: Integer): NSRange; overload;
begin
  Result.location:=textOffset;
  if textOffset+TextLen>txt.length then
    Result.length:=txt.length-textOffset
  else
    Result.length:=TextLen;
  Result:=txt.paragraphRangeForRange(Result);
end;

function ParaRange(txt: NSTextStorage; textOffset, TextLen: Integer): NSRange; inline; overload;
begin
  Result:=ParaRange(txt.string_, textOffset, textLen);
end;

function TextRange(txt: NSTextStorage; textOffset, TextLen: Integer): NSRange; inline; overload;
begin
  Result.location:=textOffset;
   if textOffset+TextLen>txt.length then
     Result.length:=txt.length-textOffset
   else
     Result.length:=TextLen;
end;


function GetDict(txt: NSTextStorage; textOffset: integer): NSDictionary;
begin
  if textOffset>=txt.string_.length then
    textOffset:=txt.string_.length-1;
  Result := txt.attributesAtIndex_effectiveRange(textOffset, nil);
end;

function GetPara(txt: NSTextStorage; textOffset: integer; isReadOnly, useDefault: Boolean): NSParagraphStyle;
var
  dict: NSDictionary;
  op  : NSParagraphStyle;
begin
  Result:=nil;
  if not Assigned(txt) then Exit;
  dict:=GetDict(txt, textOffset);
  op:=nil;
  if Assigned(dict) then
    op:=NSParagraphStyle(  dict.objectForKey(NSParagraphStyleAttributeName) );

  if not Assigned(op) then begin
    if not useDefault then Exit;
    op:=NSParagraphStyle.defaultParagraphStyle;
  end;

  if isReadOnly then
    Result := op
  else
    Result := op.mutableCopyWithZone(nil)
end;

function GetWritePara(txt: NSTextStorage; textOffset: integer): NSMutableParagraphStyle;
begin
  Result:=NSMutableParagraphStyle(GetPara(txt, textOffset, false, true));
end;

function GetReadPara(txt: NSTextStorage; textOffset: integer; useDefault: Boolean = false): NSParagraphStyle;
begin
  Result:=GetPara(txt, textOffset, true, useDefault);
end;

type
  TNSFontParams = record
    font      : NSFont;
    fontColor : NSColor;
    backColor : NSColor;
    ulnStyle  : NSNumber;
    strStyle  : NSNumber;
  end;

procedure ReadNSFontParams(dict: NSDictionary; var params: TNSFontParams);
begin
  if not Assigned(dict) then FillChar(params, sizeof(params),0)
  else begin
    params.font:=dict.objectForKey(NSFontAttributeName);
    params.fontColor:=dict.objectForKey(NSForegroundColorAttributeName);
    params.backColor:=dict.objectForKey(NSBackgroundColorAttributeName);
    params.ulnStyle:=dict.objectForKey(NSUnderlineStyleAttributeName);
    params.strStyle:=dict.objectForKey(NSStrikethroughStyleAttributeName);
  end;
end;

{ TCocoaWSCustomRichMemo }

const
  VerticalScrollerVisible: array[TScrollStyle] of boolean = (
 {ssNone          } false,
 {ssHorizontal    } false,
 {ssVertical      } true,
 {ssBoth          } true,
 {ssAutoHorizontal} false,
 {ssAutoVertical  } true,
 {ssAutoBoth      } true
  );

  HorizontalScrollerVisible: array[TScrollStyle] of boolean = (
 {ssNone          } false,
 {ssHorizontal    } true,
 {ssVertical      } false,
 {ssBoth          } true,
 {ssAutoHorizontal} true,
 {ssAutoVertical  } false,
 {ssAutoBoth      } true
  );

  ScrollerAutoHide: array[TScrollStyle] of boolean = (
 {ssNone          } false,
 {ssHorizontal    } false,
 {ssVertical      } false,
 {ssBoth          } false,
 {ssAutoHorizontal} true,
 {ssAutoVertical  } true,
 {ssAutoBoth      } true
  );

class function TCocoaWSCustomRichMemo.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  txt: TCocoaRichView;
  ns: NSString;
  scr: TCocoaScrollView;
  nr:NSRect;
  r:TRect;
begin
  scr := TCocoaScrollView(NSView(TCocoaScrollView.alloc).lclInitWithCreateParams(AParams));

  nr.origin.x:=0;
  nr.origin.x:=0;
  nr.size.height:=0;
  nr.size.width:=AParams.Width;

  txt := TCocoaRichView.alloc.initwithframe(nr);
  txt.scale := 1.0;

  scr.setDocumentView(txt);

  scr.setHasVerticalScroller(VerticalScrollerVisible[TMemo(AWinControl).ScrollBars]);
  scr.setHasHorizontalScroller(HorizontalScrollerVisible[TMemo(AWinControl).ScrollBars]);
  scr.setAutohidesScrollers(ScrollerAutoHide[TMemo(AWinControl).ScrollBars]);

  if TCustomMemo(AWinControl).BorderStyle=bsSingle then
     scr.setBorderType(NSBezelBorder);

  nr:=scr.documentVisibleRect;
  txt.setFrame(nr);
  txt.textContainer.setLineFragmentPadding(0);

  txt.callback := TLCLCommonCallback.Create(txt, AWinControl, scr);
  ns := NSStringUtf8(AParams.Caption);
  txt.setString(ns);
  ns.release;

  scr.callback := txt.callback;
  Result := TLCLIntfHandle(scr);

  TextViewSetWordWrap(txt, scr, TCustomMemo(AWinControl).WordWrap);
  TextViewSetAllignment(txt, TCustomMemo(AWinControl).Alignment);
  txt.wantReturns := TCustomMemo(AWinControl).WantReturns;
  txt.callback.SetTabSuppress(not TCustomMemo(AWinControl).WantTabs);
end;

class function TCocoaWSCustomRichMemo.GetTextAttributes(
  const AWinControl: TWinControl; TextStart: Integer; var Params: TIntFontParams
  ): Boolean;
var
  view  : TCocoaTextView;
  txt   : NSTextStorage;
  dict  : NSDictionary;

  prm   : TNSFontParams;
  trt   : NSFontSymbolicTraits;
begin
  InitFontParams(Params);
  view:=MemoTextView(AWinControl);
  Result:=false;
  if not Assigned(view) then Exit;

  txt:=view.textStorage;
  if (txt.length = 0) then begin
    // there's no text! using the control information
    Params.Name := txt.font.displayName.UTF8String;
    Params.Size := Round(txt.font.pointSize);
    Params.HasBkClr := false;
    exit;
  end;

  if (TextStart >= txt.length) then
    TextStart := txt.length;
  dict:=GetDict(txt, textStart);
  ReadNSFontParams(dict, prm);
  if Assigned(prm.font) then begin
    Params.Name:=NSStringToString(prm.font.familyName);
    Params.Size:=round(prm.font.pointSize);
    trt:=prm.font.fontDescriptor.symbolicTraits;
    if (trt and NSFontItalicTrait) > 0 then Include(Params.Style, fsItalic);
    if (trt and NSFontBoldTrait) > 0 then Include(Params.Style, fsBold);
  end;
  if Assigned(prm.ulnStyle) and (prm.ulnStyle.integerValue <> NSUnderlineStyleNone) then
    Include(Params.Style, fsUnderline);
  if Assigned(prm.strStyle) and (prm.strStyle.integerValue <> NSUnderlineStyleNone) then
    Include(Params.Style, fsStrikeOut);
  if Assigned(prm.fontColor) then
    Params.Color:=NSColorToColorRef(prm.fontColor);
  Params.HasBkClr:=Assigned(prm.backColor);
  if Params.HasBkClr then
    Params.BkColor:=NSColorToColorRef(prm.backColor);
  Result:=true;
end;


class function TCocoaWSCustomRichMemo.FPisEqual(const FP, FPRef : TFontParams) : boolean;
begin                        // DRB - an addition function to help GetStyleRange()
  Result := false;
  if (FP.name = FPRef.name) and (FP.size = FPRef.size) and (FP.style = FPRef.style)
        and (FP.bkColor = FPRef.bkcolor) and (FP.HasBkClr = FPRef.HasBkClr)
        and (FP.VScriptPos = FPRef.VScriptPos) then
  Result := true
end;

class function TCocoaWSCustomRichMemo.GetStyleRange(const AWinControl: TWinControl;
                              TextStart: Integer; var RangeStart, RangeLen: Integer): Boolean;
var
  txt : TCocoaTextView;
  TextLength : integer;       // all text, in bytes (2 byte line endings)
  FPRef, FP : TIntFontParams;
begin                         // DRB - new method to implement GetStypeRange() on Cocoa.
  Result := False;
  txt:=MemoTextView(AWinControl);
  TextLength := txt.textStorage.string_.length;
  if (TextStart < 0) or (TextStart >= TextLength) then exit;
  if not GetTextAttributes(AWinControl, TextStart, FPRef) then exit;
  RangeStart := TextStart;
  repeat
        dec(RangeStart);
        if RangeStart <= 0 then break;
        if not GetTextAttributes(AWinControl, RangeStart, FP) then exit;
  until not FPisEqual(FP, FPRef);
  inc(RangeStart);
  RangeLen := TextStart - RangeStart;
  repeat
        if RangeStart + RangeLen >= TextLength then break;
        inc(RangeLen);
        if not GetTextAttributes(AWinControl, RangeStart + RangeLen, FP) then exit;
  until not FPisEqual(FP, FPRef);
  Result:=true;
end;

function FindFont(const FamilyName: String; astyle: TFontStyles): NSFontDescriptor;
var
  fd  : NSFontDescriptor;
  cfd : NSFontDescriptor;
  old : NSFontDescriptor;
  fdd : NSFontDescriptor;
  trt : NSFontSymbolicTraits;
  ns  : NSString;
  i   : Integer;
const
  fallback : array [0..1] of NSFontSymbolicTraits = (NSFontItalicTrait, NSFontBoldTrait);
begin
  trt:=0;
  ns:=NSStringUtf8(FamilyName);
  if fsItalic in aStyle then trt:=trt or NSFontItalicTrait;
  if fsBold in aStyle then trt:=trt or NSFontBoldTrait;

  fd:=NSFontDescriptor(NSFontDescriptor.alloc).initWithFontAttributes(nil);
  cfd:=fd;
  try
    fd:=fd.fontDescriptorWithFamily(ns);
    fd:=fd.fontDescriptorWithSymbolicTraits(trt);

    fdd:=fd.matchingFontDescriptorWithMandatoryKeys(nil);
    i:=0;
    while not Assigned(fdd) and (i<length(fallback)) do begin
      trt:=trt and (not fallback[i]);
      fd:=fd.fontDescriptorWithSymbolicTraits(trt);
      fdd:=fd.matchingFontDescriptorWithMandatoryKeys(nil);
      inc(i);
    end;
    Result:=fdd;
  finally
    ns.release;
    cfd.release;
  end;
end;

class procedure TCocoaWSCustomRichMemo.SetTextAttributes(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const Params: TIntFontParams);
var
  view  : TCocoaTextView;
  txt   : NSTextStorage;

  fd    : NSFontDescriptor;
  font  : NSFont;
  rng   : NSRange;
  clr   : NSColor;
  num   : NSNumber;
  arr   : NSArray;

begin
  view:=MemoTextView(AWinControl);
  if not Assigned(view) then Exit;

  txt:=view.textStorage;

  rng := TextRange(txt, TextStart, textLen);

  fd:=FindFont(Params.Name, Params.Style);
  font:=NSFont.fontWithDescriptor_size(fd, Params.Size);

  txt.addAttribute_value_range(NSFontAttributeName, font, rng);
  // fd.release;

  if fsUnderline in Params.Style then begin
    num:=NSNumber.numberWithInt(NSUnderlineStyleSingle);
    txt.addAttribute_value_range(NSUnderlineStyleAttributeName, num, rng);
  end else
    txt.removeAttribute_range(NSUnderlineStyleAttributeName, rng);

  if fsStrikeOut in Params.Style then begin
    num:=NSNumber.numberWithInt(NSUnderlineStyleSingle);
    txt.addAttribute_value_range(NSStrikethroughStyleAttributeName, num, rng);
  end else
    txt.removeAttribute_range(NSStrikethroughStyleAttributeName, rng);

  clr:=ColorToNSColor(Params.Color);
  txt.addAttribute_value_range(NSForegroundColorAttributeName, clr, rng);

  if Params.HasBkClr then begin
    clr:=ColorToNSColor(Params.BkColor);
    txt.addAttribute_value_range(NSBackgroundColorAttributeName, clr, rng);
  end else
    txt.removeAttribute_range(NSBackgroundColorAttributeName, rng);
end;

class procedure TCocoaWSCustomRichMemo.SetParaAlignment(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AAlign: TIntParaAlignment);
var
  txt : TCocoaTextView;
  rng : NSRange;
const
  TxtAlign : array [TIntParaAlignment] of integer = (
  {$ifdef CPUAARCH64}
   NSLeftTextAlignment, 2, 1, NSJustifiedTextAlignment  // got reversed in OS11
  {$else}
   NSLeftTextAlignment,  NSRightTextAlignment, NSCenterTextAlignment, NSJustifiedTextAlignment
  {$endif}
   );
begin
  txt:=MemoTextView(AWinControl);
  if not Assigned(txt) then Exit;

  rng.location:=TextStart;
  rng.length:=TextLen;
  if TextStart+TextLen>txt.textStorage.string_.length then
    rng.length:=txt.textStorage.string_.length-TextStart;

  rng:=txt.textStorage.string_.paragraphRangeForRange(rng);
  txt.setAlignment_range(TxtAlign[AAlign], rng);
end;

class function TCocoaWSCustomRichMemo.GetParaAlignment(
  const AWinControl: TWinControl; TextStart: Integer;
  var AAlign: TIntParaAlignment): Boolean;
var
  txt : TCocoaTextView;
  rng : NSRange;
  cur : NSRange;
  al  : NSTextAlignment;
const
  TxtAlign : array [TIntParaAlignment] of integer = (
   NSLeftTextAlignment,  NSRightTextAlignment, NSCenterTextAlignment, NSJustifiedTextAlignment
  );
begin
  txt:=MemoTextView(AWinControl);
  if not Assigned(txt) then begin
    Result:=false;
    Exit;
  end;

  cur:=txt.selectedRange;
  rng.location:=TextStart;
  rng.length:=1;
  if TextStart+1>txt.textStorage.string_.length then
    rng.length:=txt.textStorage.string_.length-TextStart;


  rng:=txt.textStorage.string_.paragraphRangeForRange(rng);
  txt.setSelectedRange(rng);
  al:=txt.alignment;
  case al of
    NSRightTextAlignment: AAlign:=paRight;
    NSCenterTextAlignment: AAlign:=paCenter;
    NSJustifiedTextAlignment: AAlign:=paJustify;
  else
    AAlign:=paLeft;
  end;
  txt.setSelectedRange(cur);
  Result:=true;
end;

class function TCocoaWSCustomRichMemo.GetParaMetric(
  const AWinControl: TWinControl; TextStart: Integer;
  var AMetric: TIntParaMetric): Boolean;
var
  view  : TCocoaTextView;
  txt   : NSTextStorage;
  par   : NSParagraphStyle;
begin
  InitParaMetric(AMetric);
  view:=MemoTextView(AWinControl);
  Result:=false;
  if not Assigned(view) then Exit;

  txt:=view.textStorage;
  par:=GetReadPara(txt, TextStart, true);

  AMetric.FirstLine := par.firstLineHeadIndent;
  AMetric.HeadIndent := par.headIndent;
  AMetric.TailIndent := par.tailIndent;
  AMetric.LineSpacing := par.lineSpacing;
  AMetric.SpaceAfter := par.paragraphSpacing;
  AMetric.SpaceBefore := par.paragraphSpacingBefore;

  Result:=true;
end;

class procedure TCocoaWSCustomRichMemo.SetParaMetric(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AMetric: TIntParaMetric);
var
  view  : TCocoaTextView;
  txt   : NSTextStorage;
  par   : NSMutableParagraphStyle;
begin
  view:=MemoTextView(AWinControl);
  if not Assigned(view) then Exit;

  txt:=view.textStorage;
  par:=GetWritePara(txt, TextStart);

  par.setFirstLineHeadIndent(AMetric.FirstLine);
  par.setHeadIndent(AMetric.HeadIndent);
  par.setTailIndent(AMetric.TailIndent);
  par.setLineSpacing(AMetric.LineSpacing);
  par.setParagraphSpacing(AMetric.SpaceAfter);
  par.setParagraphSpacingBefore(AMetric.SpaceBefore);

  txt.addAttribute_value_range( NSParagraphStyleAttributeName, par, ParaRange(txt, TextStart, textLen));
end;

class procedure TCocoaWSCustomRichMemo.SetParaTabs(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AStopList: TTabStopList);
var
  view  : TCocoaTextView;
  txt   : NSTextStorage;
  par   : NSMutableParagraphStyle;
  tabs  : NSMutableArray;
  tab   : NSTextTab;
  i     : Integer;
  rng   : NSRange;
const
  TabAlignMap : array [TTabAlignment] of NSTextTabType = (
    NSLeftTabStopType,    // tabLeft,
    NSCenterTabStopType,  // tabCenter,
    NSRightTabStopType,   // tabRight,
    NSDecimalTabStopType, // tabDecimal
    NSLeftTabStopType     // tabWordBar - not supported
  );
begin
  view:=MemoTextView(AWinControl);
  if not Assigned(view) then Exit;

  txt := view.textStorage;
  par:=GetWritePara(txt, TextStart);

  if AStopList.Count>0 then begin
    tabs := NSMutableArray.alloc.init;
    for i:=0 to AStopList.Count-1 do begin
      tab := NSTextTab.alloc.initWithType_location( TabAlignMap[AStopList.Tabs[i].Align], AStopList.Tabs[i].Offset );
      tabs.addObject( tab );
      tab.release;
    end;
  end;
  par.setTabStops(tabs);
  txt.addAttribute_value_range( NSParagraphStyleAttributeName, par, ParaRange(txt, TextStart, textLen));
end;

class function TCocoaWSCustomRichMemo.GetParaTabs(
  const AWinControl: TWinControl; TextStart: integer;
  var AStopList: TTabStopList): Boolean;
var
  view  : TCocoaTextView;
  txt   : NSTextStorage;
  par   : NSParagraphStyle;
  tabs  : NSArray;
  tab   : NSTextTab;
  i     : Integer;
begin
  InitTabStopList(AStopList);
  view:=MemoTextView(AWinControl);
  Result:=false;
  if not Assigned(view) then Exit;

  txt:=view.textStorage;
  par:=GetReadPara(txt, textStart, false);
  if not Assigned(par) then Exit;

  tabs:=par.tabStops;
  if not Assigned(tabs) then Exit;
  AStopList.Count:=tabs.count;
  SetLength(AStopList.Tabs, AStopList.Count);
  for i:=0 to tabs.count-1 do begin
    tab:=NSTextTab(tabs.objectAtIndex(i));
    AStopList.Tabs[i].Offset:=tab.location;
    case tab.tabStopType of
      NSCenterTabStopType: AStopList.Tabs[i].Align:= tabCenter;
      NSRightTabStopType:  AStopList.Tabs[i].Align:= tabRight;
      NSDecimalTabStopType: AStopList.Tabs[i].Align:= tabDecimal;
    else
      AStopList.Tabs[i].Align:=tabLeft;
    end;
  end;
end;

class procedure TCocoaWSCustomRichMemo.SetZoomFactor(
  const AWinControl: TWinControl; AZoomFactor: Double);
var
  view  : TCocoaRichView;
  sz    : NSSize;
begin
  view:=TCocoaRichView(MemoTextView(AWinControl));
  if not Assigned(view) then Exit;

  // reset Scaling
  if view.scale<>1.0 then begin
    sz.width:=1/view.scale;
    sz.height:=1/view.scale;
    view.scaleUnitSquareToSize(sz);
  end;

  // set new scaling
  sz.width:=AZoomFactor;
  sz.height:=AZoomFactor;
  view.scaleUnitSquareToSize(sz);
  view.layoutManager.ensureLayoutForTextContainer(view.textContainer);

  view.scale:=AZoomFactor;
end;

class procedure TCocoaWSCustomRichMemo.InDelText(
  const AWinControl: TWinControl; const TextUTF8: String; DstStart,
  DstLen: Integer);
var
  txt : TCocoaTextView;
  str : NSString;
begin
  txt:=MemoTextView(AWinControl);
  if not Assigned(txt) then Exit;

  str := NSStringUtf8(TextUtf8);
  txt.textStorage.replaceCharactersInRange_withString(NSMakeRange(DstStart, DstLen), str);
  str.release;
end;

class function TCocoaWSCustomRichMemo.LoadRichText(
  const AWinControl: TWinControl; Source: TStream): Boolean;
var
  data: NSMutableData;
  rng : NSRange;
  txt : TCocoaTextView;
begin
  //todo: avoid copying data.
  Result:=false;
  if not Assigned(Source) or not Assigned(AWinControl) or (AWinControl.Handle=0) then Exit;

  txt:=MemoTextView(AWinControl);
  if Source.size>0 then begin
    data:=NSMutableData(NSMutableData.alloc).initWithLength(Source.size);
    Source.Read(data.mutableBytes^, Source.Size);
    rng.length:=txt.textStorage.string_.length;
    rng.location:=0;
    txt.replaceCharactersInRange_withRTF(rng, data);
    data.release;
  end;
  Result:=true;
end;

class function TCocoaWSCustomRichMemo.SaveRichText(
  const AWinControl: TWinControl; Dest: TStream): Boolean;
var
  rng : NSRange;
  txt : TCocoaTextView;
  rtf : NSData;
  sz  : NSUInteger;
  dt  : PByteArray;
  i   : NSUInteger;
  chsz : Integer; // chunk size
begin
  //todo: avoid copying data.
  Result:=false;
  if not Assigned(Dest) or not Assigned(AWinControl) or (AWinControl.Handle=0) then Exit;

  txt:=MemoTextView(AWinControl);

  rng.length:=txt.textStorage.string_.length;
  rng.location:=0;
  rtf:=txt.RTFFromRange(rng);
  sz :=rtf.length;
  if (sz>0) then begin
    dt:=PByteArray(rtf.bytes);
    i:=0;
    // have to do all of that, because Stream.Write(,  Int32);
    // while size can be > 2Gb
    while sz>0 do begin
      if sz>MaxInt then chsz:=MaxInt
      else chsz := sz;
      Dest.Write(dt[i], chsz);
      dec(sz, chsz);
      inc(i, chsz);
    end;
  end;

  Result:=true;
end;

class procedure TCocoaWSCustomRichMemo.SetTransparentBackground(
  const AWinControl: TWinControl; ATransparent: Boolean);
var
  txt : TCocoaTextView;
begin
  txt:=MemoTextView(AWinControl);
  if not Assigned(txt) then Exit;

  txt.setDrawsBackground(not ATransparent);
  txt.enclosingScrollView.setDrawsBackground(not ATransparent);
end;

end.
