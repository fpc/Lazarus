unit CocoaWSComCtrls;

interface

{$mode delphi}
{$modeswitch objectivec2}
{$include cocoadefines.inc}

uses
  Classes, Math, SysUtils,
  LCLType, Controls, ComCtrls,
  WSComCtrls,
  MacOSAll, CocoaAll,
  CocoaPrivate, CocoaCallback, CocoaCommonCallback, CocoaWSCommon, CocoaConfig, CocoaUtils,
  CocoaButtons, CocoaStatusBar, CocoaProgressIndicator, CocoaSlider;

type

  { TCocoaWSStatusBar }

  TCocoaWSStatusBar = class(TWSStatusBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure Update(const AStatusBar: TStatusBar); override;
    //
    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
  end;

  { TCocoaWSProgressBar }

  TCocoaWSProgressBar = class(TWSProgressBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure ApplyChanges(const AProgressBar: TCustomProgressBar); override;
    class procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); override;
    class procedure SetStyle(const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle); override;
  end;

  { TCocoaWSCustomUpDown }

  TCocoaWSCustomUpDown = class(TWSCustomUpDown)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure SetIncrement(const AUpDown: TCustomUpDown; AValue: Double); override;
    class procedure SetMaxPosition(const AUpDown: TCustomUpDown; AValue: Double); override;
    class procedure SetMinPosition(const AUpDown: TCustomUpDown; AValue: Double); override;
    class procedure SetPosition(const AUpDown: TCustomUpDown; AValue: Double); override;
    class procedure SetWrap(const AUpDown: TCustomUpDown; ADoWrap: Boolean); override;
  end;

  { TCocoaWSTrackBar }

  TCocoaWSTrackBar = class(TWSTrackBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure ApplyChanges(const ATrackBar: TCustomTrackBar); override;
    class function  GetPosition(const ATrackBar: TCustomTrackBar): integer; override;
    class procedure SetPosition(const ATrackBar: TCustomTrackBar; const {%H-}NewPosition: integer); override;
    class procedure SetOrientation(const ATrackBar: TCustomTrackBar; const AOrientation: TTrackBarOrientation); override;
    class procedure SetTick(const ATrackBar: TCustomTrackBar; const ATick: integer); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
  end;

implementation

type

  { TUpdownCommonCallback }

  TUpdownCommonCallback = class(TLCLCommonCallback, IStepperCallback)
    procedure BeforeChange(var Allowed: Boolean);
    procedure Change(NewValue: Double; isUpPressed: Boolean; var Allowed: Boolean);
    procedure UpdownClick(isUpPressed: Boolean);
  end;

type
  TAccessUpDown = class(TCustomUpDown);

{ TUpdownCommonCallback }

procedure TUpdownCommonCallback.BeforeChange(var Allowed: Boolean);
begin
  if Assigned( TAccessUpDown(Target).OnChanging ) then
    TAccessUpDown(Target).OnChanging(Target, Allowed);
end;

procedure TUpdownCommonCallback.Change(NewValue: Double; isUpPressed: Boolean;
  var Allowed: Boolean);
const
  UpDownDir : array [Boolean] of TUpDownDirection = (updUp, updDown);
begin
  if Assigned( TAccessUpDown(Target).OnChangingEx ) then
    TAccessUpDown(Target).OnChangingEx(Target, Allowed,
      Round(NewValue), UpDownDir[isUpPressed]);
end;

procedure TUpdownCommonCallback.UpdownClick(isUpPressed: Boolean);
const
  UpDownBtn : array [Boolean] of TUDBtnType = (btPrev, btNext);
begin
  TAccessUpDown(Target).Position := NSStepper(Owner).intValue;
  if Assigned( TAccessUpDown(Target).OnClick ) then
    TAccessUpDown(Target).OnClick( Target, UpDownBtn[isUpPressed]);
end;

{ TCocoaWSCustomUpDown }

class function TCocoaWSCustomUpDown.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle;
var
  lResult: TCocoaStepper;
begin
  lResult := TCocoaStepper.alloc.lclInitWithCreateParams(AParams);
  if Assigned(lResult) then
  begin
    lResult.callback := TUpdownCommonCallback.Create(lResult, AWinControl);
    //small constrol size looks like carbon
    //lResult.setControlSize(NSSmallControlSize);
    lResult.setTarget(lResult);
    lResult.setAction(objcselector('stepperAction:'));
    lResult.setMinValue( 0 );
    lResult.setMaxValue( 100 );
    lResult.setValueWraps( False );
    lResult.setAutorepeat( True );
  end;
  Result := TLCLHandle(lResult);
end;

class procedure TCocoaWSCustomUpDown.SetMinPosition(
  const AUpDown: TCustomUpDown; AValue: Double);
begin
  if not Assigned(AUpDown) or not AUpDown.HandleAllocated then Exit;
  TCocoaStepper(AUpDown.Handle).setMinValue(AValue);
end;

class procedure TCocoaWSCustomUpDown.SetMaxPosition(
  const AUpDown: TCustomUpDown; AValue: Double);
begin
  if not Assigned(AUpDown) or not AUpDown.HandleAllocated then Exit;
  TCocoaStepper(AUpDown.Handle).setMaxValue(AValue);
end;

class procedure TCocoaWSCustomUpDown.SetPosition(const AUpDown: TCustomUpDown;
  AValue: Double);
begin
  if not Assigned(AUpDown) or not AUpDown.HandleAllocated then Exit;
  TCocoaStepper(AUpDown.Handle).lastValue := AValue;
  TCocoaStepper(AUpDown.Handle).setDoubleValue(AValue);
end;

class procedure TCocoaWSCustomUpDown.SetIncrement(const AUpDown: TCustomUpDown;
  AValue: Double);
begin
  if not Assigned(AUpDown) or not AUpDown.HandleAllocated then Exit;
  TCocoaStepper(AUpDown.Handle).setIncrement(AValue);
end;

class procedure TCocoaWSCustomUpDown.SetWrap(const AUpDown: TCustomUpDown;
  ADoWrap: Boolean);
begin
  if not Assigned(AUpDown) or not AUpDown.HandleAllocated then Exit;
  TCocoaStepper(AUpDown.Handle).setValueWraps(ADoWrap);
end;

{ TCocoaWSStatusBar }

class function TCocoaWSStatusBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  lResult: TCocoaStatusBar;
  cell   : NSButtonCell;
  cb : TStatusBarCallback;
begin
  Result := 0;
  lResult := TCocoaStatusBar.alloc.lclInitWithCreateParams(AParams);
  if not Assigned(lResult) then Exit;
  Result := TLCLHandle(lResult);

  cb := TStatusBarCallback.Create(lResult, AWinControl);
  lResult.callback := cb;
  lResult.barcallback := cb;
  cb.BlockCocoaUpDown := true;
  //lResult.StatusBar := TStatusBar(AWinControl);

  //todo: get rid of Cells and replace them with views!
  cell:=NSButtonCell(NSButtonCell.alloc).initTextCell(nil);
  // NSSmallSquareBezelStyle aka "Gradient button", is the best looking
  // candidate for the status bar panel. Could be changed to any NSCell class
  // since CocoaStatusBar doesn't suspect any specific cell type.
  cell.setBezelStyle(NSSmallSquareBezelStyle);
  cell.setFont( NSFont.systemFontOfSize( NSFont.smallSystemFontSize ));

  cell.setLineBreakMode(NSLineBreakByClipping);
  //cell.setLineBreakMode(NSLineBreakByTruncatingTail);

  lResult.panelCell := cell;
end;

class procedure TCocoaWSStatusBar.PanelUpdate(const AStatusBar: TStatusBar;
  PanelIndex: integer);
begin
  // todo: can make more effecient
  Update(AStatusBar);
end;

class procedure TCocoaWSStatusBar.SetPanelText(const AStatusBar: TStatusBar;
  PanelIndex: integer);
begin
  Update(AStatusBar);
end;

class procedure TCocoaWSStatusBar.Update(const AStatusBar: TStatusBar);
begin
  if not Assigned(AStatusBar) or not (AStatusBar.HandleAllocated) then Exit;
  {$ifdef BOOLFIX}
  TCocoaStatusBar(AStatusBar.Handle).setNeedsDisplay__(Ord(true));
  {$else}
  TCocoaStatusBar(AStatusBar.Handle).setNeedsDisplay_(true);
  {$endif}
end;

class procedure TCocoaWSStatusBar.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth := 0;
  PreferredHeight := CocoaConfigStatusBar.defaultHeight;
end;

{ TCocoaWSProgressBar }

class function TCocoaWSProgressBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  lResult: TCocoaProgressIndicator;
begin
  lResult := TCocoaProgressIndicator.alloc.lclInitWithCreateParams(AParams);
  if Assigned(lResult) then
  begin
    lResult.callback := TLCLCommonCallback.Create(lResult, AWinControl);
    lResult.startAnimation(nil);
    //small constrol size looks like carbon
    //lResult.setControlSize(NSSmallControlSize);
  end;
  Result := TLCLHandle(lResult);
end;

class procedure TCocoaWSProgressBar.ApplyChanges(
  const AProgressBar: TCustomProgressBar);
var
  ind : NSProgressIndicator;
begin
  if not Assigned(AProgressBar) or not AProgressBar.HandleAllocated then Exit;
  ind:=NSProgressIndicator(AProgressBAr.Handle);
  ind.setMaxValue(AProgressBar.Max);
  ind.setMinValue(AProgressBar.Min);
  ind.setDoubleValue(AProgressBar.Position);
  SetStyle(AProgressBar, AProgressBar.Style);
end;

class procedure TCocoaWSProgressBar.SetPosition(
  const AProgressBar: TCustomProgressBar; const NewPosition: integer);
begin
  if AProgressBar.HandleAllocated then
    NSProgressIndicator(AProgressBar.Handle).setDoubleValue(NewPosition);
end;

class procedure TCocoaWSProgressBar.SetStyle(
  const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle);
begin
  if AProgressBar.HandleAllocated then
  begin
    NSProgressIndicator(AProgressBar.Handle).setIndeterminate(NewStyle = pbstMarquee);
    NSProgressIndicator(AProgressBar.Handle).startAnimation(nil);
  end;
end;

{ TCocoaWSTrackBar }

{------------------------------------------------------------------------------
  Method:  TCocoaWSTrackBar.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new track bar with the specified parameters
 ------------------------------------------------------------------------------}
class function TCocoaWSTrackBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  lResult: TCocoaSlider;
begin
  lResult := TCocoaSlider.alloc.lclInitWithCreateParams(AParams);
  if Assigned(lResult) then
  begin
    lResult.callback := TLCLCommonCallback.Create(lResult, AWinControl);
    lResult.setTarget(lResult);
    lResult.setAction(objcselector('sliderAction:'));
  end;
  Result := TLCLHandle(lResult);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSTrackBar.ApplyChanges
  Params:  ATrackBar - LCL custom track bar

  Sets the parameters (Min, Max, Position, Ticks) of slider
 ------------------------------------------------------------------------------}
class procedure TCocoaWSTrackBar.ApplyChanges(const ATrackBar: TCustomTrackBar);
var
  lSlider: TCocoaSlider;
  lTickCount, lTrackBarLength: Integer;
begin
  if not Assigned(ATrackBar) or not ATrackBar.HandleAllocated then Exit;
  lSlider := TCocoaSlider(ATrackBar.Handle);
  lSlider.setMaxValue(ATrackBar.Max);
  lSlider.setMinValue(ATrackBar.Min);
  lSlider.setIntValue(ATrackBar.Position);
  lSlider.intval := ATrackBar.Position;
  lSlider.setContinuous(true);
  lSlider.setAltIncrementValue(1); // forcing the slider to switch by 1 by the keyboard

  // Ticks
  if ATrackBar.TickStyle = tsAuto then
  begin
    // this should only apply to Auto
    // and for Manual it should drawn manually
    if ATrackBar.Frequency <> 0 then
      lTickCount := (ATrackBar.Max-ATrackBar.Min) div ATrackBar.Frequency + 1
    else
      lTickCount := (ATrackBar.Max-ATrackBar.Min);

    // Protection from too frequent ticks.
    // 1024 is a number of "too much" ticks, based on a common
    // screen resolution 1024 x 768
    // Protects ticks from "disappearing" when trackbar is resized
    // and is temporary too narrow to fit the trackbar
    if TickCount > 1024 then
    begin
      if ATrackBar.Orientation = trHorizontal then
        lTrackBarLength := ATrackBar.Width
      else
        lTrackBarLength := ATrackBar.Height;

      lTickCount := Min(lTickCount, lTrackBarLength);
    end;
  end else if ATrackBar.TickStyle = tsManual then
  begin
    lTickCount := 2;
  end else
    lTickCount := 0;

  lSlider.lclSetManTickDraw(ATrackBar.TickStyle = tsManual);

  lSlider.setNumberOfTickMarks(lTickCount);

  if ATrackBar.TickMarks = tmTopLeft then
    lSlider.setTickMarkPosition(NSTickMarkAbove)
  else
    lSlider.setTickMarkPosition(NSTickMarkBelow);
  lSlider.setNeedsDisplay_(true);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSTrackBar.GetPosition
  Params:  ATrackBar - LCL custom track bar
  Returns: Position of slider
 ------------------------------------------------------------------------------}
class function TCocoaWSTrackBar.GetPosition(const ATrackBar: TCustomTrackBar
  ): integer;
var
  lSlider: TCocoaSlider;
begin
  if not Assigned(ATrackBar) or not ATrackBar.HandleAllocated then
  begin
    Result := 0;
    Exit;
  end;
  lSlider := TCocoaSlider(ATrackBar.Handle);
  Result := lSlider.intValue();
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSTrackBar.SetPosition
  Params:  ATrackBar - LCL custom track bar
           NewPosition  - New position

  Sets the position of slider
 ------------------------------------------------------------------------------}
class procedure TCocoaWSTrackBar.SetPosition(const ATrackBar: TCustomTrackBar;
  const NewPosition: integer);
var
  lSlider: TCocoaSlider;
begin
  if not Assigned(ATrackBar) or not ATrackBar.HandleAllocated then Exit;
  lSlider := TCocoaSlider(ATrackBar.Handle);
  lSlider.setIntValue(ATrackBar.Position);
end;

// Cocoa auto-detects the orientation based on width/height and there seams
// to be no way to force it
class procedure TCocoaWSTrackBar.SetOrientation(const ATrackBar: TCustomTrackBar;
  const AOrientation: TTrackBarOrientation);
begin
  if not Assigned(ATrackBar) or not ATrackBar.HandleAllocated then Exit;
  if (AOrientation = trHorizontal) and (ATrackBar.Height >= ATrackBar.Width) then
    ATrackBar.Width := ATrackBar.Height + 1
  else if (AOrientation = trVertical) and (ATrackBar.Width >= ATrackBar.Height) then
    ATrackBar.Height := ATrackBar.Width + 1;
end;

class procedure TCocoaWSTrackBar.SetTick(const ATrackBar: TCustomTrackBar; const ATick: integer);
var
  lSlider: TCocoaSlider;
begin
  if not Assigned(ATrackBar) or not ATrackBar.HandleAllocated then Exit;
  lSlider := TCocoaSlider(ATrackBar.Handle);
  lSlider.lclAddManTick(ATick);
end;

class procedure TCocoaWSTrackBar.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
var
  lSlider : TCocoaSlider;
  trk     : TCustomTrackBar;
  frm     : NSRect;
begin
  if not Assigned(AWinControl) or not AWinControl.HandleAllocated then Exit;
  trk := TCustomTrackBar(AWinControl);
  lSlider := TCocoaSlider(AWinControl.Handle);
  frm := lSlider.frame;
  try
    if trk.Orientation = trVertical then
      lSlider.setFrame(NSMakeRect(0,0,5,10))
    else
      lSlider.setFrame(NSMakeRect(0,0,10,5));

    TCocoaWSWinControl.GetPreferredSize(AWinControl,PreferredWidth, PreferredHeight, WithThemeSpace);
    if trk.Orientation = trVertical then
      PreferredHeight := 0
    else
      PreferredWidth := 0;
  finally
    lSlider.setFrame(frm);
  end;
end;

end.
