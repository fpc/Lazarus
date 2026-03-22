{
 *****************************************************************************
 *                              CocoaWSButtons.pp                            *
 *                              --------------                               *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit cocoawsbuttons;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  // libs
  MacOSAll, CocoaAll, SysUtils, Math,
  // LCL
  Classes, Controls, Buttons, StdCtrls, LCLType, LCLProc, Graphics, GraphType, ImgList,
  LCLMessageGlue, LMessages,
  // widgetset
  WSButtons, WSStdCtrls, WSLCLClasses,
  // LCL Cocoa
  CocoaPrivate, CocoaGDIObjects, CocoaWSPrivate, CocoaCommonCallback,
  CocoaUtils, cocoa_extra, CocoaButtons;

type

  { TLCLButtonCallback }

  TLCLButtonCallback = class(TLCLCommonCallback, IButtonCallback)
  public
    procedure ButtonClick; virtual;
    procedure Draw(ControlContext: NSGraphicsContext; const bounds, dirty: NSRect); override;
    procedure GetAllowMixedState(var allowed: Boolean); virtual;
  end;
  TLCLButtonCallBackClass = class of TLCLButtonCallBack;

  { TCocoaWSButton }

  TCocoaWSButton = class(TWSButton)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class function GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class function GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
      var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
      override;
  end;

  { TLCLCheckBoxCallback }

  TLCLCheckBoxCallback = class(TLCLButtonCallBack)
  public
    procedure ButtonClick; override;
    procedure GetAllowMixedState(var allowed: Boolean); override;
  end;

  { TCocoaWSCustomCheckBox }

  TCocoaWSCustomCheckBox = class(TWSCustomCheckBox)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class function RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;
    //
    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; {%H-}WithThemeSpace: Boolean); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class function GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class function GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; override;
  end;

  { TLCLRadioButtonCallback }

  TLCLRadioButtonCallback = class(TLCLCheckBoxCallback)
  public
    procedure ButtonClick; override;
  end;

  { TCocoaWSRadioButton }

  TCocoaWSRadioButton = class(TWSRadioButton)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;
  end;

  { TCocoaWSBitBtn }

  TCocoaWSBitBtn = class(TWSBitBtn)
  private
    class function  LCLGlyphPosToCocoa(ALayout: TButtonLayout): NSCellImagePosition;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    //
    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    //
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); override;
    class procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout); override;
  end;

  { TCocoaWSSpeedButton }

  TCocoaWSSpeedButton = class(TWSSpeedButton)
  published
  end;

  { TCocoaWSButtonUtil }

  TCocoaWSButtonUtil = class
  public
    class function allocButton(
      const ATarget: TWinControl;
      const ACallBackClass: TLCLButtonCallBackClass;
      const AParams: TCreateParams;
      const btnBezel: NSBezelStyle;
      const btnType: NSButtonType ): TCocoaButton;
    class procedure setState(
      const btn: NSButton;
      const NewState: TCheckBoxState;
      const SkipChangeEvent: Boolean = True );
    class procedure switchRadioButton(
      const checkedRadio: NSButton );
  end;

implementation

class function TCocoaWSButtonUtil.allocButton(
  const ATarget: TWinControl;
  const ACallBackClass: TLCLButtonCallBackClass;
  const AParams: TCreateParams;
  const btnBezel: NSBezelStyle;
  const btnType: NSButtonType ): TCocoaButton;
begin
  case btnType of
    NSMomentaryLightButton,
    NSMomentaryChangeButton,
    NSMomentaryPushInButton:
      Result:= TCocoaButton.alloc;
    else
      Result:= TCocoaButtonNeedFocusRing.alloc;
  end;

  if Assigned(Result) then begin
    Result:= Result.lclInitWithCreateParams(AParams);
    TCocoaButton(Result).callback := ACallBackClass.Create(Result, ATarget);

    Result.setTitle(TCocoaControlUtil.toMacOSTitle(AParams.Caption));
    if btnBezel <> 0 then
      Result.setBezelStyle(btnBezel);
    Result.setButtonType(btnType);
  end;
end;

class procedure TCocoaWSButtonUtil.setState(
  const btn: NSButton;
  const NewState: TCheckBoxState;
  const SkipChangeEvent: Boolean = True );
const
  buttonState: array [TcheckBoxState] of NSInteger =
    (NSOffState, NSOnState, NSMixedState);
var
  cb : IButtonCallback;
begin
  if NewState = cbGrayed then
    {$ifdef BOOLFIX}
    btn.setAllowsMixedState_(Ord(true));
    {$else}
    btn.setAllowsMixedState(true);
    {$endif}
  if SkipChangeEvent and (btn.isKindOfClass(TCocoaButton)) then
  begin
    //todo: This place needs a cleanup!
    //      Assigning state, while having callback removed
    //      TCocoaButton.setState is causing OnChange event, if callback is not nil
    cb := TCocoaButton(btn).callback;
    TCocoaButton(btn).callback := nil;
    btn.setState(buttonState[NewState]);
    TCocoaButton(btn).callback := cb;
  end
  else
    btn.setState(buttonState[NewState]);
end;

class procedure TCocoaWSButtonUtil.switchRadioButton(
  const checkedRadio: NSButton );
var
  SubView : NSView;
begin
  if not Assigned(checkedRadio) then Exit;
  for SubView in checkedRadio.superView.subviews do
    if (SubView <> checkedRadio) and (SubView.lclGetTarget is TRadioButton) then
    begin
      NSButton(SubView).setState(NSOffState);
    end;
end;

{ TLCLButtonCallback }

procedure TLCLButtonCallback.ButtonClick;
begin
  if not Owner.lclIsEnabled() then Exit;
  SendSimpleMessage(Target, LM_CLICKED);
end;

procedure TLCLButtonCallback.Draw(ControlContext: NSGraphicsContext;
  const bounds, dirty: NSRect);
var
  PS: PPaintStruct;
  nsr:NSRect;
  ctx: TCocoaContext;
begin
  // todo: think more about draw call while previous draw still active
  ctx := TCocoaContext.Create(ControlContext);
  ctx.isControlDC := True;
  try
    nsr:=dirty;
    nsr.origin.y:=bounds.size.height-dirty.origin.y-dirty.size.height;
    New(PS);
    try
      FillChar(PS^, SizeOf(TPaintStruct), 0);
      PS^.hdc := HDC(ctx);
      PS^.rcPaint := TCocoaTypeUtil.toRect(nsr);
      LCLSendPaintMsg(Target, HDC(ctx), PS);
    finally
      Dispose(PS);
    end;
  finally
    FreeAndNil(ctx);
  end;
end;

procedure TLCLButtonCallback.GetAllowMixedState(var allowed: Boolean);
begin

end;

{ TCocoaWSButton }

{------------------------------------------------------------------------------
  Method:  TCocoaWSButton.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Cocoa interface

  Creates new button control in Cocoa interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCocoaWSButton.CreateHandle(
  const AWinControl: TWinControl;
  const AParams: TCreateParams ): TLCLHandle;
const
  // these heights were received from Xcode interface builder,
  // where the height cannot be changed for a button control the actual size
  // of the button (the difference between top pixel and bottom pixel,
  // is less than frame size also
  PUSHBTN_REG_HEIGHT   = 20;
  PUSHBTN_SMALL_HEIGHT = 17;
  PUSHBTN_MINI_HEIGHT  = 14;
var
  btn: TCocoaButton;
begin
  btn := TCocoaWSButtonUtil.allocButton(AWinControl, TLCLButtonCallback, AParams, NSRoundedBezelStyle, NSMomentaryLightButton);
  btn.smallHeight := PUSHBTN_SMALL_HEIGHT;
  btn.miniHeight := PUSHBTN_MINI_HEIGHT;
  btn.adjustFontToControlSize:=true;
  Result := TLCLHandle(btn);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSButton.SetDefault
  Params:  AButton  - LCL button control
           ADefault

  Sets button default indication in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSButton.SetDefault(const AButton: TCustomButton; ADefault: Boolean);
var
  cf: NSString;
const
  DefEq: array [Boolean] of String = (#0, #13);
begin
  if not AButton.HandleAllocated then
    Exit;
  cf := NSStringUtf8(DefEq[ADefault]);
  NSButton(AButton.Handle).setKeyEquivalent(cf);
  cf.release;
end;

class procedure TCocoaWSButton.SetText(const AWinControl: TWinControl; const AText: String);
var
  btn : NSButton;
begin
  btn := NSButton(AWinControl.Handle);
  btn.setTitle(TCocoaControlUtil.toMacOSTitle(AText));

  if NOT ((AWinControl is TCustomButton) or (AWinControl is TToggleBox)) then
    Exit;

  if AText.IndexOf(char(LineEnding)) >= 0 then
    btn.setBezelStyle( NSRegularSquareBezelStyle );
end;

class function TCocoaWSButton.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
begin
  // The text is static, so let the LCL fallback to FCaption
  Result := false;
end;

class function TCocoaWSButton.GetTextLen(const AWinControl: TWinControl;
  var ALength: Integer): Boolean;
begin
  // The text is static, so let the LCL fallback to FCaption
  Result := false;
end;

class procedure TCocoaWSButton.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
begin
  if not (AWinControl.HandleAllocated) then Exit;
  TCocoaWSWinControl.SetFont(AWinControl, AFont);
  TCocoaButton(AWinControl.Handle).adjustFontToControlSize := (AFont.Name = 'default')
    and (AFont.Size = 0);
end;

class procedure TCocoaWSButton.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
var
  button: TCocoaButton;
  size: NSSize;
begin
  button:= TCocoaButton(AWinControl.Handle);
  size := button.fittingSize();
  TCocoaViewUtil.setSize(button, Round(size.height), button.miniHeight, button.smallHeight, button.adjustFontToControlSize);
  size := button.fittingSize();
  if button.controlSize = NSRegularControlSize then begin
    size.width:= size.width - 12;
    size.height:= size.height - 6;
    size:= TCocoaButtonUtil.adjustSizeIfNecessary(button, size);
  end;
  PreferredWidth:= Round(size.width);
  PreferredHeight:= Round(size.height);
end;

{ TLCLCheckBoxCallback }

procedure TLCLCheckBoxCallback.ButtonClick;
begin
  inherited;
  if not Owner.lclIsEnabled() then Exit;
  SendSimpleMessage(Target, LM_CHANGED);
  // todo: win32 has something about dbcheckbox handling here. so maybe we need to handle it special too
end;

procedure TLCLCheckBoxCallback.GetAllowMixedState(var allowed: Boolean);
begin
  allowed := TCustomCheckBox(Target).AllowGrayed;
end;

{ TCocoaWSCustomCheckBox }

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomCheckBox.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Cocoa interface

  Creates new check box in Cocoa interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCocoaWSCustomCheckBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  btn: TCocoaButton;
  cb: IButtonCallback;
begin
  btn := TCocoaWSButtonUtil.allocButton(AWinControl, TLCLCheckBoxCallBack, AParams, 0, NSSwitchButton);
  // changes in AllowGrayed are never sent to WS!
  // so it should be checked at create time (and at SetNextState?)
  if TCustomCheckBox(AWinControl).AllowGrayed then
    {$ifdef BOOLFIX}
    NSButton(btn).setAllowsMixedState_(Ord(true));
    {$else}
    NSButton(btn).setAllowsMixedState(true);
    {$endif}
    ;
  Result := TLCLHandle(btn);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomCheckBox.RetrieveState
  Params:  ACustomCheckBox - LCL custom check box
  Returns: State of check box

  Retrieves the state of check box in Cocoa interface
 ------------------------------------------------------------------------------}
class function TCocoaWSCustomCheckBox.RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
var
  state : NSInteger;
begin
  Result := cbUnchecked;
  if not ACustomCheckBox.HandleAllocated then
    Exit;
  state := NSButton(ACustomCheckBox.Handle).state;
  case state of
    NSOnState: Result := cbChecked;
    NSMixedState: Result := cbGrayed;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomCheckBox.SetState
  Params:  ACustomCheckBox - LCL custom check box
           NewState        - New state of check box

  Sets the new state of check box in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSCustomCheckBox.SetState(
  const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
const
  buttonState: array [TcheckBoxState] of NSInteger = (NSOffState, NSOnState, NSMixedState);
begin
  if not ACustomCheckBox.HandleAllocated then Exit;
  TCocoaWSButtonUtil.setState(NSButton(ACustomCheckBox.Handle), NewState);
end;

class procedure TCocoaWSCustomCheckBox.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
var
  lButton: NSButton;
begin
  if not AWinControl.HandleAllocated then Exit;
  lButton := NSButton(AWinControl.Handle);

  lButton.sizeToFit();
  PreferredWidth := round(lButton.bounds.size.width);
  PreferredHeight := round(lButton.bounds.size.height);
end;

class procedure TCocoaWSCustomCheckBox.SetText(const AWinControl: TWinControl;
  const AText: String);
begin
  TCocoaWSButton.SetText(AWinControl, AText);
end;

class function TCocoaWSCustomCheckBox.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
begin
  Result := TCocoaWSButton.GetText(AWinControl, AText);
end;

class function TCocoaWSCustomCheckBox.GetTextLen(
  const AWinControl: TWinControl; var ALength: Integer): Boolean;
begin
  Result := TCocoaWSButton.GetTextLen(AWinControl, ALength);
end;

{ TLCLRadioButtonCallback }

procedure TLCLRadioButtonCallback.ButtonClick;
var
  SubView: NSView;
begin
  if not Owner.lclIsEnabled() then Exit;
  if NSButton(Owner).state = NSOnState then
    TCocoaWSButtonUtil.switchRadioButton(NSButton(Owner));
  inherited ButtonClick;
end;

{ TCocoaWSRadioButton }

class function TCocoaWSRadioButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  btn: TCocoaButton;
begin
  btn := TCocoaWSButtonUtil.allocButton(AWinControl, TLCLRadioButtonCallback, AParams, 0, NSRadioButton);
  Result := TLCLHandle(btn);
end;

class procedure TCocoaWSRadioButton.SetState(
  const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
var
  btn : NSButton;
begin
  if not ACustomCheckBox.HandleAllocated then Exit;
  btn := NSButton(ACustomCheckBox.Handle);
  if NewState = cbChecked then
    TCocoaWSButtonUtil.switchRadioButton(btn);
  TCocoaWSButtonUtil.setState(btn, NewState);
end;

{ TCocoaWSBitBtn }

class function TCocoaWSBitBtn.LCLGlyphPosToCocoa(ALayout: TButtonLayout
  ): NSCellImagePosition;
begin
  case ALayout of
  blGlyphLeft:   Result := NSImageLeft;
  blGlyphRight:  Result := NSImageRight;
  blGlyphTop:    Result := NSImageAbove;
  blGlyphBottom: Result := NSImageBelow;
  else
    Result := NSNoImage;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSBitBtn.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new bevel button with bitmap in Cocoa interface with the
  specified parameters
 ------------------------------------------------------------------------------}
class function TCocoaWSBitBtn.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  btn: NSButton;
begin
  btn := TCocoaWSButtonUtil.allocButton(AWinControl, TLCLButtonCallBack, AParams, NSRegularSquareBezelStyle, NSMomentaryPushInButton);
  Result := TLCLHandle(btn);
end;

class procedure TCocoaWSBitBtn.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
var
  lButton: TCustomBitBtn absolute AWinControl;
  lButtonHandle: TCocoaButton;
  Size: NSSize;
begin
  if not AWinControl.HandleAllocated then Exit;

  lButtonHandle := TCocoaButton(AWinControl.Handle);
  Size := lButtonHandle.fittingSize();
  if lButton.Glyph <> nil then
    Size.Height := Max(Size.Height, lButton.Glyph.Height + 6); // This nr is arbitrary

  Size:= TCocoaButtonUtil.adjustSizeIfNecessary( lButtonHandle, Size );
  PreferredWidth := Round(Size.Width);
  PreferredHeight := Round(Size.Height);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSBitBtn.SetGlyph
  Params:  ABitBtn - LCL custom bitmap button
           AValue  - Bitmap

  Sets the bitmap of bevel button in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSBitBtn.SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph);
var
  Img: NSImage;
  AGlyph: TBitmap;
  lButtonHandle: TCocoaButton;
  AIndex: Integer;
  AEffect: TGraphicsDrawEffect;
  AImgRes: TScaledImageListResolution;
  ImgSize: NSSize;
  ScaleFactor: Double;
begin
  //WriteLn('[TCocoaWSBitBtn.SetGlyph]');
  Img := nil;
  if ABitBtn.CanShowGlyph(True) then
  begin
    AGlyph := TBitmap.Create;
    ScaleFactor := ABitBtn.GetCanvasScaleFactor;
    AValue.GetImageIndexAndEffect(bsUp, ABitBtn.Font.PixelsPerInch,
      ScaleFactor, AImgRes, AIndex, AEffect);
    AImgRes.GetBitmap(AIndex, AGlyph, AEffect);
    Img := TCocoaBitmap(AGlyph.Handle).image;
    if AImgRes.Resolution.ImageList.Scaled and not SameValue(ScaleFactor, 1) then // resize only if the image list is scaled
    begin
      ImgSize := Img.size;
      ImgSize.height := ImgSize.height / ScaleFactor;
      ImgSize.width := ImgSize.width / ScaleFactor;
      Img.setSize(ImgSize);
    end;
    lButtonHandle := TCocoaButton(ABitBtn.Handle);
    lButtonHandle.setImage(Img);
    lButtonHandle.setImagePosition(LCLGlyphPosToCocoa(ABitBtn.Layout));
    lButtonHandle.setImageScaling(NSImageScaleNone); // do not scale - retina scaling is done above with Img.setSize
    if Assigned(lButtonHandle.Glyph) then
      FreeAndNil(lButtonHandle.Glyph);
    lButtonHandle.Glyph := AGlyph;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSBitBtn.SetLayout
  Params:  ABitBtn - LCL custom bitmap button
           AValue  - Bitmap and caption layout

  Sets the bitmap and caption layout of bevel button in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSBitBtn.SetLayout(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonLayout);
var
  ImagePos: NSCellImagePosition;
begin

  if ABitBtn.CanShowGlyph(True) then
    ImagePos := LCLGlyphPosToCocoa(AValue)
  else
    ImagePos := NSNoImage;
  NSButton(ABitBtn.Handle).SetImagePosition(ImagePos);
end;

end.
