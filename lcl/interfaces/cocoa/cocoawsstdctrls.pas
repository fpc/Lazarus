{ $Id: carbonwsstdctrls.pp 15309 2008-06-04 22:12:59Z vincents $}
{
 *****************************************************************************
 *                              CocoaWSStdCtrls.pp                           *
 *                              ---------------                              *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CocoaWSStdCtrls;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$include cocoadefines.inc}

interface

uses
  // Libs
  CocoaAll, Classes, sysutils,
  // LCL
  Controls, StdCtrls, Graphics, LCLType, Forms,
  // Widgetset
  WSStdCtrls, WSLCLClasses,
  // LCL Cocoa
  CocoaPrivate, CocoaCommonCallback, CocoaGDIObjects,
  CocoaConst, CocoaConfig, CocoaUtils,
  CocoaScrolling, CocoaTextEdits, CocoaCustomControl, CocoaGroupBox,
  CocoaWSScrolling, CocoaWSTextEdits, CocoaWSButtons;

type

  { TCocoaWSScrollBar }

  TCocoaWSScrollBar = class(TWSScrollBar)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure SetKind(const AScrollBar: TCustomScrollBar; const AIsHorizontal: Boolean); override;
    class procedure SetParams(const AScrollBar: TCustomScrollBar); override;
  end;

  { TCocoaWSCustomGroupBox }

  TCocoaWSCustomGroupBox = class(TWSCustomGroupBox)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
  end;

  { TCocoaWSToggleBox }

  TCocoaWSToggleBox = class(TWSToggleBox)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
  end;

  { TCocoaWSCustomStaticText }

  TCocoaWSCustomStaticText = class(TWSCustomStaticText)
  private
  protected
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment); override;
  end;

implementation

{ TCocoaWSCustomStaticText }

class function TCocoaWSCustomStaticText.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  lclStaticText: TCustomStaticText absolute AWinControl;
  field: NSTextField;
begin
  field := TCocoaWSTextControlUtil.createTextField(AWinControl, AParams);
  {$ifdef BOOLFIX}
  field.setBezeled_(Ord(False));
  field.setDrawsBackground_(Ord(False));
  field.setEditable_(Ord(False));
  field.setSelectable_(Ord(False));
  {$else}
  field.setBezeled(False);
  field.setDrawsBackground(False);
  field.setEditable(False);
  field.setSelectable(False);
  {$endif}
  TCocoaTextControlUtil.setAllignment(field, lclStaticText.Alignment);
  Result:=TLCLHandle(field);
end;

class procedure TCocoaWSCustomStaticText.SetAlignment(
  const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment);
begin
  if not Assigned(ACustomStaticText) or (not ACustomStaticText.HandleAllocated) or (ACustomStaticText.Handle=0) then
    exit;
  TCocoaTextControlUtil.setAllignment(NSTextField(ACustomStaticText.Handle), NewAlignment);
end;

{ TCocoaWSToggleBox }

class function TCocoaWSToggleBox.CreateHandle(const AWinControl:TWinControl;
  const AParams:TCreateParams):TLCLHandle;
var
  btn: NSButton;
  cl: NSButtonCell;
begin
  btn := TCocoaWSButtonUtil.allocButton(AWinControl, TLCLCheckBoxCallback, AParams,
           CocoaConfigToggleBox.bezelStyle, CocoaConfigToggleBox.buttonType);
  cl := NSButtonCell(NSButton(btn).cell);
  cl.setShowsStateBy(cl.showsStateBy or NSContentsCellMask);
  Result := TLCLHandle(btn);
end;

{ TCocoaWSScrollBar }

class function TCocoaWSScrollBar.CreateHandle(const AWinControl:TWinControl;
  const AParams:TCreateParams):TLCLHandle;
var
  scr : TCocoaScrollBar;
  prm : TCreateParams;
const
  ScrollBase = 15; // the shorter size of the scroller. There's a NSScroller class method for that
begin
  prm := AParams;
  // forcing the initial size to follow the designated kind of the scroll
  if (TCustomScrollBar(AWinControl).Kind = sbVertical) then begin
    prm.Width:=ScrollBase;
    prm.Height:=ScrollBase*4;
  end else
  begin
    prm.Width:=ScrollBase*4;
    prm.Height:=ScrollBase;
  end;

  // for the Scroller created separately through TCocoaWSScrollBar.CreateHandle(),
  // due to the lack of the control over the Layout by TCocoaManualScrollView,
  // only the Legacy Style can be used for compatibility.
  // it's the same logical relationship as NSScrollView and NSScroller.
  scr:= TCocoaWSScrollingUtil.createLegacyScroller;
  scr.lclInitWithCreateParams(prm);
  scr.callback:=TLCLCommonCallback.Create(scr, AWinControl);

  // OnChange (scrolling) event handling
  scr.setTarget(scr);
  scr.setAction(objcselector('actionScrolling:'));

  scr.minInt:=TCustomScrollBar(AWinControl).Min;
  scr.maxInt:=TCustomScrollBar(AWinControl).Max;
  scr.pageInt:=TCustomScrollBar(AWinControl).PageSize;
  scr.largeInc:=abs(TCustomScrollBar(AWinControl).LargeChange);
  scr.smallInc:=abs(TCustomScrollBar(AWinControl).SmallChange);
  if scr.largeInc=0 then scr.largeInc:=1;
  if scr.smallInc=0 then scr.smallInc:=1;

  Result:=TLCLHandle(scr);

  scr.lclSetFrame( Bounds(AParams.X, AParams.Y, AParams.Width, AParams.Height));
end;

// vertical/horizontal in Cocoa is set automatically according to
// the geometry of the scrollbar, it cannot be forced to an unusual value
class procedure TCocoaWSScrollBar.SetKind(const AScrollBar: TCustomScrollBar; const AIsHorizontal: Boolean);
begin
  // the scroll type can be changed when creating a scroll.
  // since the size got changed, we have to create the handle
  RecreateWnd(AScrollBar);
end;

class procedure TCocoaWSScrollBar.SetParams(const AScrollBar:TCustomScrollBar);
var
  lScroller: TCocoaScrollBar;
  sz : integer;
begin
  if not Assigned(AScrollBar) then Exit;
  lScroller := TCocoaScrollBar(AScrollBar.Handle);
  if (lScroller = nil) then Exit;
  sz:=AScrollBar.Max - AScrollBar.PageSize;
  if sz > 0 then
  begin
    lScroller.setDoubleValue( AScrollBar.Position / sz );
    lScroller.setKnobProportion( AScrollBar.PageSize / AScrollBar.Max );
  end;
end;

{ TCocoaWSCustomGroupBox }

class function TCocoaWSCustomGroupBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  box: TCocoaGroupBox;
  cap: NSString;
  lGroupBoxContents: TCocoaCustomControl;
  ns: NSRect;
  cb: TLCLCommonCallback;
  //str: string;
begin
  box := NSView(TCocoaGroupBox.alloc).lclInitWithCreateParams(AParams);
  if Assigned(box) then
  begin
    cb:= TLCLCommonCallback.Create(box, AWinControl);
    cb.traits:= [TCocoaCbTrait.blockUpDown];
    box.callback:= cb;
    cap := NSStringUTF8(AParams.Caption);
    box.setTitle(cap);
    cap.release;

    // set a content view in order to be able to customize drawing for labels/color
    ns := NSMakeRect(AParams.X, AParams.Y, AParams.Width, AParams.Height);
    lGroupBoxContents := TCocoaCustomControl.alloc.initWithFrame(ns);
    lGroupBoxContents.callback:= box.callback; //TLCLCustomControlCallback.Create(lGroupBoxContents, AWinControl);
    //str := Format('%X=%X', [PtrUInt(box.callback), PtrUInt(lGroupBoxContents.callback)]);
    lGroupBoxContents.autorelease;
    box.setContentView(lGroupBoxContents);
  end;
  Result := TLCLHandle(box);
end;

class function TCocoaWSCustomGroupBox.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  box: TCocoaGroupBox;
begin
  box := TCocoaGroupBox(AWinControl.Handle);
  Result:=Assigned(box);
  if Result then
    AText := NSStringToString(box.title);
end;

class procedure TCocoaWSCustomGroupBox.SetText(const AWinControl: TWinControl; const AText: String);
var
  box: TCocoaGroupBox;
begin
  box := TCocoaGroupBox(AWinControl.Handle);
  box.setTitle(TCocoaControlUtil.toMacOSTitle(AText));
end;

class procedure TCocoaWSCustomGroupBox.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  box: TCocoaGroupBox;
  fn : NSFont;
begin
  if not AWinControl.HandleAllocated then Exit;
  box := TCocoaGroupBox(AWinControl.Handle);
  fn := TCocoaFont(AFont.Reference.Handle).Font;
  if AFont.Size = 0 then
    fn := NSFont.fontWithDescriptor_size(fn.fontDescriptor, NSFont.smallSystemFontSize);
  box.setTitleFont(fn);
end;

end.

