{ $Id: $}
{                  --------------------------------------------
                  cocoascrollers.pas  -  Cocoa internal classes
                  --------------------------------------------

 This unit contains the private classhierarchy for the Cocoa implemetations

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CocoaScrollerImpl;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$interfaces corba}
{$include cocoadefines.inc}

interface

uses
  Classes, SysUtils,
  MacOSAll, CocoaAll,
  CocoaScrolling, CocoaConfig;

function CocoaScrollStyleManagerCreatorImpl(
  const sv: NSView;
  const style: NSScrollerStyle ): TCocoaScrollStyleManager;

implementation

type

  { TCocoaScrollBarEffectAlpha }

  TCocoaScrollBarEffectAlpha = objcclass(TCocoaScrollBarEffect)
  private
    procedure onAlphaTimer( timer:NSTimer ); message 'onAlphaTimer:';
  protected
    currentAlpha: Double;
    stepAlpha: Double;
    maxAlpha: Double;

    manager: TCocoaScrollBarStyleManager;
    scroller: NSScroller;
    alphaTimer: NSTimer;
  protected
    procedure fade( inc:Double; max:Double );
      message 'fadeInc:max:';
    procedure onDestroy; override;
  end;

  { TCocoaScrollBarEffectOverlay }

  TCocoaScrollBarEffectOverlay = objcclass(TCocoaScrollBarEffectAlpha)
  private
    procedure onDelayShowingTimer( timer:NSTimer ); message 'onDelayShowingTimer:';
    procedure onDelayHidingTimer( timer:NSTimer ); message 'onDelayHidingTimer:';
    procedure onExpandTimer( timer:NSTimer ); message 'onExpandTimer:';

    procedure setDelayTimer( timeInterval:Double; onTimer:SEL );
      message 'setDelayTimer:timeInterval:';
  protected
    currentKnobPosition: Double;
    currentKnobProportion: Double;
    entered: Boolean;
    expandedSize: Integer;

    delayTimer: NSTimer;
    expandTimer: NSTimer;
  protected
    procedure setDelayShowingTimer; message 'setDelayShowingTimer';
    procedure setDelayHidingTimer; message 'setDelayHidingTimer';
    procedure setExpandTimer; message 'setExpandTimer';

    procedure onDestroy; override;
  public
    procedure cancelDelay; message 'cancelDelay';
  end;

  { TCocoaScrollStyleManagerLegacy }

  TCocoaScrollStyleManagerLegacy = class(TCocoaScrollStyleManager)
  protected
    function isBarOccupyBound: Boolean; override;
  public
    procedure onKnobValueUpdated( scroller:NSScroller;
      var knobPosition:Double; var knobProportion:Double ); override;
    procedure onDrawKnob( scroller:NSScroller );  override;
    function onDrawKnobSlot( scroller:NSScroller; var slotRect: NSRect ):
      Boolean; override;
    procedure onMouseEntered( scroller:NSScroller );  override;
    procedure onMouseExited( scroller:NSScroller ); override;
    function createScrollBarEffect( scroller:NSScroller ): TCocoaScrollBarEffect; override;
    procedure availScrollBar( scroller:NSScroller; available:Boolean ); override;
    function isAvailableScrollBar( scroller:NSScroller ): Boolean; override;
    procedure showScrollBar( scroller:NSScroller; now:Boolean=True ); override;
    procedure tempHideScrollBar( scroller:NSScroller ); override;
  end;

  { TCocoaScrollStyleManagerOverlay }

  TCocoaScrollStyleManagerOverlay = class(TCocoaScrollStyleManager)
  private
    procedure doShowScrollBar( scroller:NSScroller; now:Boolean=True );
  protected
    function isBarOccupyBound: Boolean; override;
  public
    procedure onKnobValueUpdated( scroller:NSScroller;
      var knobPosition:Double; var knobProportion:Double ); override;
    procedure onDrawKnob( scroller:NSScroller );  override;
    function onDrawKnobSlot( scroller:NSScroller; var slotRect: NSRect ):
      Boolean; override;
    procedure onMouseEntered( scroller:NSScroller );  override;
    procedure onMouseExited( scroller:NSScroller ); override;
    function createScrollBarEffect( scroller:NSScroller ): TCocoaScrollBarEffect; override;
    procedure availScrollBar( scroller:NSScroller; available:Boolean ); override;
    function isAvailableScrollBar( scroller:NSScroller ): Boolean; override;
    procedure showScrollBar( scroller:NSScroller; now:Boolean=True ); override;
    procedure tempHideScrollBar( scroller:NSScroller ); override;
  end;

function CocoaScrollStyleManagerCreatorImpl(
  const sv: NSView;
  const style: NSScrollerStyle ): TCocoaScrollStyleManager;
var
  manualScrollView: TCocoaManualScrollView Absolute sv;
begin
  if style = NSScrollerStyleLegacy then begin
    if Assigned(sv) then
      Result:= TCocoaScrollStyleManagerLegacy.createForScrollView( manualScrollView )
    else
      Result:= TCocoaScrollStyleManagerLegacy.createForScrollBar;
  end else begin
    Result:= TCocoaScrollStyleManagerOverlay.createForScrollView( manualScrollView );
  end;
end;

{ TCocoaScrollBarEffectAlpha }

procedure TCocoaScrollBarEffectAlpha.onAlphaTimer(timer: NSTimer);
var
  done: Boolean = false;
begin
  if timer<>alphaTimer then begin
    timer.invalidate;
    Exit;
  end;

  self.currentAlpha:= self.currentAlpha + self.stepAlpha;
  if self.currentAlpha < 0.01 then
    self.currentAlpha:= 0;

  if self.stepAlpha > 0 then begin
    if self.currentAlpha >= self.maxAlpha then begin
      self.currentAlpha:= self.maxAlpha;
      done:= True;
    end;
  end else begin
    if self.currentAlpha <= self.maxAlpha then begin
      self.currentAlpha:= self.maxAlpha;
      done:= True;
    end;
  end;

  if done then begin
    timer.invalidate;
    alphaTimer:= nil;
  end;

  self.scroller.setNeedsDisplay_(True);
end;

procedure TCocoaScrollBarEffectAlpha.fade( inc: Double; max: Double );
begin
  if Assigned(self.alphaTimer) then
    self.alphaTimer.invalidate;

  self.stepAlpha:= inc;
  self.maxAlpha:= max;
  self.alphaTimer:= NSTimer.scheduledTimerWithTimeInterval_target_selector_userInfo_repeats(
    CocoaConfigScroller.fadeTimeInterval,
    self,
    ObjCSelector('onAlphaTimer:'),
    nil,
    true );
end;

procedure TCocoaScrollBarEffectAlpha.onDestroy;
begin
  if Assigned(self.alphaTimer) then begin
    self.alphaTimer.invalidate;
    self.alphaTimer:= nil;
  end;
end;

{ TCocoaScrollBarEffectOverlay }

procedure TCocoaScrollBarEffectOverlay.onDelayShowingTimer( timer: NSTimer );
begin
  self.delayTimer:= nil;
  self.manager.showScrollBar( self.scroller );
end;

procedure TCocoaScrollBarEffectOverlay.onDelayHidingTimer( timer:NSTimer );
begin
  self.delayTimer:= nil;
  self.fade( CocoaConfigScroller.overlay.bar.alphaFadeStep,
             CocoaConfigScroller.overlay.bar.alphaFadeTo );
end;

procedure TCocoaScrollBarEffectOverlay.onExpandTimer(timer: NSTimer);
var
  done: Boolean = false;
begin
  if timer<>expandTimer then begin
    timer.invalidate;
    Exit;
  end;

  if self.expandedSize < CocoaConfigScroller.overlay.bar.expandSize then begin
    self.expandedSize:= self.expandedSize + 1;
  end else begin
    done:= True;
  end;

  if done then begin
    timer.invalidate;
    self.expandTimer:= nil;
  end;

  self.scroller.setNeedsDisplay_(True);
end;

procedure TCocoaScrollBarEffectOverlay.setDelayTimer(
  timeInterval:Double; onTimer:SEL );
begin
  if Assigned(self.expandTimer) then begin
    self.expandTimer.invalidate;
    self.expandTimer:= nil;
  end;

  self.cancelDelay;

  self.delayTimer:= NSTimer.scheduledTimerWithTimeInterval_target_selector_userInfo_repeats(
    timeInterval,
    self,
    onTimer,
    nil,
    false );
end;

procedure TCocoaScrollBarEffectOverlay.setDelayShowingTimer;
begin
  self.setDelayTimer(
    CocoaConfigScroller.overlay.bar.autoShowDelayTime,
    ObjCSelector('onDelayShowingTimer:') );
end;

procedure TCocoaScrollBarEffectOverlay.setDelayHidingTimer;
begin
  self.setDelayTimer(
    CocoaConfigScroller.overlay.bar.autoHideDelayTime,
    ObjCSelector('onDelayHidingTimer:') );
end;

procedure TCocoaScrollBarEffectOverlay.setExpandTimer;
begin
  self.cancelDelay;

  if Assigned(self.expandTimer) then
    self.expandTimer.invalidate;

  self.expandTimer:= NSTimer.scheduledTimerWithTimeInterval_target_selector_userInfo_repeats(
    CocoaConfigScroller.overlay.bar.expandTimeInterval,
    self,
    ObjCSelector('onExpandTimer:'),
    nil,
    True );
end;

procedure TCocoaScrollBarEffectOverlay.onDestroy;
begin
  self.cancelDelay;
  if Assigned(self.expandTimer) then begin
    self.expandTimer.invalidate;
    self.expandTimer:= nil;
  end;
  inherited onDestroy;
end;

procedure TCocoaScrollBarEffectOverlay.cancelDelay;
begin
  if Assigned(self.delayTimer) then begin
    self.delayTimer.invalidate;
    self.delayTimer:= nil;
  end;
end;

{ TCocoaScrollViewStyleManagerLegacy }

function TCocoaScrollStyleManagerLegacy.createScrollBarEffect( scroller:NSScroller ):
  TCocoaScrollBarEffect;
var
  effect: TCocoaScrollBarEffectAlpha;
begin
  effect:= TCocoaScrollBarEffectAlpha.alloc.Init;
  effect.scroller:= scroller;
  effect.manager:= self;
  effect.currentAlpha:= CocoaConfigScroller.legacy.knob.alpha;
  Result:= effect;
end;

procedure TCocoaScrollStyleManagerLegacy.availScrollBar(
  scroller: NSScroller; available: Boolean);
begin
  if NOT Assigned(scroller) then
    Exit;

  if NOT available then begin
    scroller.setHidden( True );
    Exit;
  end;

  if scroller.knobProportion = 1 then
    Exit;

  if scroller.isHidden then
  begin
    scroller.setHidden( false );
    scroller.setAlphaValue( 1 );
  end;
end;

function TCocoaScrollStyleManagerLegacy.isAvailableScrollBar(scroller: NSScroller
  ): Boolean;
begin
  Result:= Assigned(scroller) and NOT scroller.isHidden and
           (0<scroller.knobProportion) and (scroller.knobProportion<1);;
end;

procedure TCocoaScrollStyleManagerLegacy.showScrollBar(
  scroller: NSScroller; now:Boolean );
begin
  if NOT Assigned(scroller) then
    Exit;

  if scroller.knobProportion = 1 then
    Exit;

  scroller.setHidden( False );
  scroller.setAlphaValue( 1 );
  scroller.setNeedsDisplay_( True );
end;

procedure TCocoaScrollStyleManagerLegacy.tempHideScrollBar(scroller: NSScroller
  );
begin
end;

procedure TCocoaScrollStyleManagerLegacy.onDrawKnob(scroller: NSScroller);
var
  scrollBar: TCocoaScrollBar Absolute scroller;
  rect: NSRect;
  path: NSBezierPath;
  color: CIColor;
  alpha: CGFloat;
begin
  rect:= scrollBar.rectForPart(NSScrollerKnob);
  rect:= NSInsetRect(rect, 1, 1);
  if scrollBar.IsHorizontal then begin
    rect.origin.y:= rect.origin.y + CocoaConfigScroller.legacy.knob.pos;
    rect.size.height:= rect.size.height - CocoaConfigScroller.legacy.knob.shrunkSize;
  end else begin
    rect.origin.x:= rect.origin.x + CocoaConfigScroller.legacy.knob.pos;
    rect.size.width:= rect.size.width - CocoaConfigScroller.legacy.knob.shrunkSize;
  end;

  alpha:= TCocoaScrollBarEffectAlpha(scrollBar.effect).currentAlpha;
  path:= NSBezierPath.bezierPathWithRoundedRect_xRadius_yRadius(
         rect,
         CocoaConfigScroller.legacy.knob.radius,
         CocoaConfigScroller.legacy.knob.radius );
  color:= CIColor.alloc.initWithColor( CocoaConfigScroller.legacy.knob.color() );
  NSColor.colorWithRed_green_blue_alpha(
    color.red,
    color.green,
    color.blue,
    alpha ).set_;
  path.fill;
end;

function TCocoaScrollStyleManagerLegacy.onDrawKnobSlot(scroller: NSScroller;
  var slotRect: NSRect): Boolean;
begin
  Result:= true;
end;

function TCocoaScrollStyleManagerLegacy.isBarOccupyBound: Boolean;
begin
  Result:= True;
end;

procedure TCocoaScrollStyleManagerLegacy.onKnobValueUpdated( scroller:NSScroller;
  var knobPosition:Double; var knobProportion:Double );
begin
end;

procedure TCocoaScrollStyleManagerLegacy.onMouseEntered(scroller: NSScroller
  );
var
  effect: TCocoaScrollBarEffectAlpha;
begin
  effect:= TCocoaScrollBarEffectAlpha(TCocoaScrollBar(scroller).effect);
  effect.fade( CocoaConfigScroller.legacy.knob.fadeStep,
               CocoaConfigScroller.legacy.knob.alphaBlack );
end;

procedure TCocoaScrollStyleManagerLegacy.onMouseExited(scroller: NSScroller
  );
var
  effect: TCocoaScrollBarEffectAlpha;
begin
  effect:= TCocoaScrollBarEffectAlpha(TCocoaScrollBar(scroller).effect);
  effect.fade( -CocoaConfigScroller.legacy.knob.fadeStep,
               CocoaConfigScroller.legacy.knob.alpha );
end;

{ TCocoaScrollStyleManagerOverlay }

procedure TCocoaScrollStyleManagerOverlay.onDrawKnob(scroller: NSScroller);
var
  scrollBar: TCocoaScrollBar Absolute scroller;
  effect: TCocoaScrollBarEffectOverlay;
  rect: NSRect;
  path: NSBezierPath;
  radius: CGFloat;
begin
  effect:= TCocoaScrollBarEffectOverlay(scrollBar.effect);

  scroller.setAlphaValue( effect.currentAlpha );
  if effect.currentAlpha = 0 then begin
    scroller.setHidden( true );
    effect.expandedSize:= 0;
    Exit;
  end;

  rect:= scrollBar.rectForPart(NSScrollerKnob);
  rect:= NSInsetRect(rect, 1, 1);
  if scrollBar.IsHorizontal then begin
    rect.origin.y:= rect.origin.y
                  + CocoaConfigScroller.overlay.knob.pos
                  - effect.expandedSize;
    rect.size.height:= rect.size.height
                     - CocoaConfigScroller.overlay.knob.shrunkSize
                     + effect.expandedSize;
  end else begin
    rect.origin.x:= rect.origin.x
                  + CocoaConfigScroller.overlay.knob.pos
                  - effect.expandedSize;
    rect.size.width:= rect.size.width
                    - CocoaConfigScroller.overlay.knob.shrunkSize
                    + effect.expandedSize;
  end;

  radius:= CocoaConfigScroller.overlay.knob.radius + effect.expandedSize/2;
  path:= NSBezierPath.bezierPathWithRoundedRect_xRadius_yRadius( rect, radius, radius );
  CocoaConfigScroller.overlay.knob.color().set_;
  path.fill;

  if scroller.knobProportion < 1 then
    scroller.setHidden( false );
end;

function TCocoaScrollStyleManagerOverlay.onDrawKnobSlot(scroller: NSScroller;
  var slotRect: NSRect): Boolean;
var
  scrollBar: TCocoaScrollBar Absolute scroller;
  effect: TCocoaScrollBarEffectOverlay;
begin
  effect:= TCocoaScrollBarEffectOverlay(scrollBar.effect);
  Result:= effect.expandedSize>0;
end;

function TCocoaScrollStyleManagerOverlay.isBarOccupyBound: Boolean;
begin
  Result:= False;
end;

procedure TCocoaScrollStyleManagerOverlay.onKnobValueUpdated( scroller:NSScroller;
  var knobPosition:Double; var knobProportion:Double );
var
  scrollBar: TCocoaScrollBar Absolute scroller;
  effect: TCocoaScrollBarEffectOverlay;
  slotRect: NSRect;
  slotSize: CGFloat;
begin
  effect:= TCocoaScrollBarEffectOverlay(scrollBar.effect);

  slotRect:= scroller.rectForPart(NSScrollerKnobSlot);
  if scrollBar.IsHorizontal then
    slotSize:= slotRect.size.width
  else
    slotSize:= slotRect.size.height;

  if knobProportion*slotSize <= CocoaConfigScroller.overlay.knob.minSize then begin
    if slotSize<=CocoaConfigScroller.overlay.knob.minSize then
      knobProportion:= 0.99
    else
      knobProportion:= CocoaConfigScroller.overlay.knob.minSize/slotSize;
  end;

  if (effect.currentKnobPosition=knobPosition) and (effect.currentKnobProportion=knobProportion) then
    Exit;

  if effect.currentKnobPosition <> knobPosition then
    _scrollView.onBarScrolled( scroller );

  effect.currentKnobPosition:= knobPosition;
  effect.currentKnobProportion:= knobProportion;

  self.doShowScrollBar( scroller );
end;

procedure TCocoaScrollStyleManagerOverlay.onMouseEntered(scroller: NSScroller);
var
  scrollBar: TCocoaScrollBar Absolute scroller;
  effect: TCocoaScrollBarEffectOverlay;
begin
  effect:= TCocoaScrollBarEffectOverlay(scrollBar.effect);
  effect.entered:= True;
  effect.setExpandTimer;
  scroller.setNeedsDisplay_(true);
end;

procedure TCocoaScrollStyleManagerOverlay.onMouseExited(scroller: NSScroller);
var
  scrollBar: TCocoaScrollBar Absolute scroller;
  effect: TCocoaScrollBarEffectOverlay;
begin
  effect:= TCocoaScrollBarEffectOverlay(scrollBar.effect);
  effect.entered:= False;
  effect.setDelayHidingTimer;
end;

function TCocoaScrollStyleManagerOverlay.createScrollBarEffect(
  scroller: NSScroller): TCocoaScrollBarEffect;
var
  effect: TCocoaScrollBarEffectOverlay;
begin
  effect:= TCocoaScrollBarEffectOverlay.alloc.Init;
  effect.scroller:= scroller;
  effect.manager:= self;
  effect.currentKnobPosition:= -1;
  effect.currentKnobProportion:= -1;
  effect.currentAlpha:= CocoaConfigScroller.overlay.bar.alpha;
  Result:= effect;
end;

procedure TCocoaScrollStyleManagerOverlay.availScrollBar(
  scroller: NSScroller; available: Boolean);
begin
  if NOT Assigned(scroller) then
    Exit;

  if scroller.knobProportion=1 then begin
    scroller.setAlphaValue( 0 );
    scroller.setHidden( True );
  end;
end;

function TCocoaScrollStyleManagerOverlay.isAvailableScrollBar(scroller: NSScroller
  ): Boolean;
begin
  Result:= Assigned(scroller) and (0<scroller.knobProportion) and (scroller.knobProportion<1);
end;

procedure TCocoaScrollStyleManagerOverlay.doShowScrollBar(
  scroller: NSScroller; now:Boolean );
var
  scrollBar: TCocoaScrollBar Absolute scroller;
  effect: TCocoaScrollBarEffectOverlay;
begin
  effect:= TCocoaScrollBarEffectOverlay(scrollBar.effect);

  if effect.currentKnobProportion = 1 then begin
    scroller.setAlphaValue( 0 );
    scroller.setHidden( True );
    Exit;
  end;

  if NOT now then begin
    effect.setDelayShowingTimer;
    Exit;
  end;

  if NOT _scrollView.isTapping(scroller) and NOT effect.entered then
    effect.setDelayHidingTimer;

  // on old versions of macOS, alpha=0 is considered hidden.
  // that is, to be truly visible, not only Hidden=false, but Alpha must also be set.
  // otherwise it is considered hidden and setNeedsDisplay() does not take effect.
  effect.currentAlpha:= CocoaConfigScroller.overlay.bar.alpha;
  scroller.setAlphaValue( effect.currentAlpha );
  scroller.setHidden( False );
  scroller.setNeedsDisplay_( true );
end;

procedure TCocoaScrollStyleManagerOverlay.showScrollBar(
  scroller: NSScroller; now:Boolean );
begin
  if NOT isAvailableScrollBar(scroller) then
    Exit;

  doShowScrollBar( scroller, now );
end;

procedure TCocoaScrollStyleManagerOverlay.tempHideScrollBar(scroller: NSScroller
  );
var
  scrollBar: TCocoaScrollBar Absolute scroller;
  effect: TCocoaScrollBarEffectOverlay;
begin
  if NOT isAvailableScrollBar(scroller) then
    Exit;

  effect:= TCocoaScrollBarEffectOverlay(scrollBar.effect);
  effect.cancelDelay;

  scroller.setAlphaValue( 0 );
  scroller.setHidden( True );
  effect.expandedSize:= 0;
end;

end.

