{ $Id$}
{
 *****************************************************************************
 *                             CarbonWSForms.pp                              *
 *                               ------------                                *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CarbonWSForms;

{$mode objfpc}{$H+}

interface

uses
  // libs
  FPCMacOSAll, CarbonUtils, CarbonExtra,
  // LCL
  Controls, Forms, Graphics, LCLType, LMessages, LCLProc, Classes,
  // widgetset
  WSForms, WSLCLClasses, WSProc,
  // interface
  CarbonDef, CarbonProc, CarbonPrivate,
  CarbonWSControls;

type

  { TCarbonWSScrollingWinControl }

  TCarbonWSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
  public
  end;

  { TCarbonWSScrollBox }

  TCarbonWSScrollBox = class(TWSScrollBox)
  private
  protected
  public
  end;

  { TCarbonWSCustomFrame }

  TCarbonWSCustomFrame = class(TWSCustomFrame)
  private
  protected
  public
  end;

  { TCarbonWSFrame }

  TCarbonWSFrame = class(TWSFrame)
  private
  protected
  public
  end;

  { TCarbonWSCustomForm }
  TCarbonWSCustomFormClass = class of TCarbonWSCustomForm;
  TCarbonWSCustomForm = class(TWSCustomForm)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure AddControl(const AControl: TControl); override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class procedure Invalidate(const AWinControl: TWinControl); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
    
    class procedure CloseModal(const ACustomForm: TCustomForm); override;
    class procedure ShowModal(const ACustomForm: TCustomForm); override;
    
    class procedure SetBorderIcons(const AForm: TCustomForm; const ABorderIcons: TBorderIcons); override;
    class procedure SetFormBorderStyle(const AForm: TCustomForm; const AFormBorderStyle: TFormBorderStyle); override;
  end;

  { TCarbonWSForm }

  TCarbonWSForm = class(TWSForm)
  private
  protected
  public
  end;

  { TCarbonWSHintWindow }

  TCarbonWSHintWindow = class(TWSHintWindow)
  private
  protected
  public
  end;

  { TCarbonWSScreen }

  TCarbonWSScreen = class(TWSScreen)
  private
  protected
  public
  end;

  { TCarbonWSApplicationProperties }

  TCarbonWSApplicationProperties = class(TWSApplicationProperties)
  private
  protected
  public
  end;


implementation

{ TCarbonWSCustomForm }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the window in Carbon interface

  Creates new window in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Window: WindowRef;
  Root: ControlRef;
  NewBounds: FPCMacOSAll.Rect;
  Info: PWidgetInfo;
  NewWindowClass: Integer;
  MinSize, MaxSize: HISize;
  Attributes: WindowAttributes;
begin
  Result := 0;

  NewBounds.Left := AParams.X;
  NewBounds.Top := AParams.Y;
  NewBounds.Right := AParams.X + AParams.Width;
  NewBounds.Bottom := AParams.Y + AParams.Height;
  
  DebugLn('TCarbonWSCustomForm.CreateHandle NewBounds=',dbgs(NewBounds),' Title=',AParams.Caption);

  // apply appropriate form border style and form style
  case (AWinControl as TCustomForm).FormStyle of
  fsStayOnTop, fsSplash: NewWindowClass := kUtilityWindowClass;
  else
    NewWindowClass := kDocumentWindowClass;
  end;
  
  case (AWinControl as TCustomForm).BorderStyle of
  bsNone: Attributes := kWindowNoTitleBarAttribute;
  bsToolWindow, bsSingle:
    Attributes := kWindowCloseBoxAttribute or kWindowCollapseBoxAttribute;
  bsSizeable:
    Attributes := kWindowCloseBoxAttribute or kWindowCollapseBoxAttribute
      or kWindowFullZoomAttribute or kWindowResizableAttribute;
  bsDialog: Attributes := kWindowCloseBoxAttribute;
  bsSizeToolWin: Attributes := kWindowCloseBoxAttribute or kWindowResizableAttribute;
  end;

  if CreateNewWindow(NewWindowClass,
                     Attributes or
                     kWindowCompositingAttribute or
                     kWindowStandardHandlerAttribute or
                     kWindowLiveResizeAttribute or
                     kWindowInWindowMenuAttribute,
                     NewBounds, Window
                    ) = noErr
  then Result := TLCLIntfHandle(Window);
  if Result = 0 then Exit;

  SetText(AWinControl, AParams.Caption);
  SetColor(AWinControl);
  CreateRootControl(Window, @Root);

  Info := CreateWndWidgetInfo(Window, AWinControl);
  TCarbonPrivateHandleClass(WSPrivate).RegisterEvents(Info);
  
  MinSize.width := AWinControl.Constraints.EffectiveMinWidth;
  MinSize.height := AWinControl.Constraints.EffectiveMinHeight;
  MaxSize.width := AWinControl.Constraints.EffectiveMaxWidth;
  MaxSize.height := AWinControl.Constraints.EffectiveMaxHeight;
  if MaxSize.width <= 0 then MaxSize.width := 10000;
  if MaxSize.height <= 0 then MaxSize.height := 10000;
  
  SetWindowResizeLimits(Window,@MinSize,@MaxSize);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.DestroyHandle
  Params:  AWinControl - LCL control
  Returns: Nothing

  Destroys window in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomForm.DestroyHandle(const AWinControl: TWinControl);
var
  Root: ControlRef;
begin
  if not WSCheckHandleAllocated(AWinControl, 'DestroyHandle') then Exit;

  DebugLn('DestroyHandle ' + AWinControl.Name);

  // free root control
  GetRootControl(WindowRef(AWinControl.Handle), Root);
  DisposeControl(Root);

  DisposeWindow(WindowRef(AWinControl.Handle));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.AddControl
  Params:  AControl - LCL control to add
  Returns: Nothing

  Adds new control to parent window in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomForm.AddControl(const AControl: TControl);
var
  Root: ControlRef;
begin
  if not WSCheckHandleAllocated(AControl.Parent, 'AddControl') then Exit;
  
  DebugLn('AddControl ' + AControl.Name + ' in ' + AControl.Parent.Name);

  GetRootControl(WindowRef(AControl.Parent.Handle), Root);
  EmbedControl(ControlRef((AControl as TWinControl).Handle), Root);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.GetText
  Params:  AWinControl - LCL control
           AText       - Text
  Returns: If the function succeeds

  Retrieves the text (caption) of window in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomForm.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  CFString: CFStringRef;
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetText') then Exit;

  Result := CopyWindowTitleAsCFString(WindowRef(AWinControl.Handle), CFString) = NoErr;
  try
    if Result = False then Exit;

    AText := CarbonStringToString(CFString);
  finally
    FreeCarbonString(CFString);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.SetBounds
  Params:  AWinControl - LCL control
           ALeft, ATop, AWidth, AHeight - Coordinates
  Returns: Nothing

  Sets the bounds of window in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomForm.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
var
  NewBounds: FPCMacOSAll.Rect;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetBounds') then Exit;

  NewBounds.Left := AWinControl.Left;
  NewBounds.Top := AWinControl.Top;
  NewBounds.Right := AWinControl.Left + AWinControl.Width;
  NewBounds.Bottom := AWinControl.Top + AWinControl.Height;
  DebugLn('TCarbonWSCustomForm.SetBounds ', dbgsName(AWinControl), ' NewBounds=', dbgs(NewBounds));

  if SetWindowBounds(WindowRef(AWinControl.Handle), kWindowContentRgn, NewBounds) <> NoErr then
  begin
    DebugLn('TCarbonWSCustomForm.SetBounds ', dbgsName(AWinControl),' failed');
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.SetColor
  Params:  AWinControl - LCL control
  Returns: Nothing

  Sets color of window in Carbon interface according to the LCL control
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomForm.SetColor(const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor') then Exit;
  
  SetWindowContentColor(WindowRef(AWinControl.Handle), ColorToCarbonColor(AWinControl.Color));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.SetFont
  Params:  AWinControl - LCL control
           AFont       - Font
  Returns: Nothing

  Sets the font of window in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomForm.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetFont') then Exit;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.SetText
  Params:  AWinControl - LCL control
           AText       - Text
  Returns: Nothing

  Sets the text (caption) of window in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomForm.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  CFString: CFStringRef;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetText') then Exit;

  CreateCarbonString(AText, CFString);
  try
    SetWindowTitleWithCFString(WindowRef(AWinControl.Handle), CFString);
  finally
    FreeCarbonString(CFString);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.Invalidate
  Params:  AWinControl - LCL control
  Returns: Nothing

  Invalidates window in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomForm.Invalidate(const AWinControl: TWinControl);
var
 AWndRect:  FPCMacOSAll.Rect;
 Result: Boolean;
begin
  if not WSCheckHandleAllocated(AWinControl, 'Invalidate') then Exit;

  Result := GetWindowBounds(WindowRef(AWinControl.Handle), kWindowContentRgn, AWndRect) <> 0;
  if Result then InvalWindowRect(WindowRef(AWinControl.Handle), AWndRect);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.ShowHide
  Params:  AWinControl - LCL control
  Returns: Nothing

  Changes visibility of window in Carbon interface according to the LCL control
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomForm.ShowHide(const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWinControl, 'ShowHide') then Exit;
  
  if AWinControl.Visible then
    FPCMacOSAll.ShowWindow(WindowRef(AWinControl.Handle))
  else
    FPCMacOSAll.HideWindow(WindowRef(AWinControl.Handle));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.CloseModal
  Params:  ACustomForm - LCL custom form
  Returns: Nothing

  Closes modal window in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomForm.CloseModal(const ACustomForm: TCustomForm);
begin
  if not WSCheckHandleAllocated(ACustomForm, 'CloseModal') then Exit;
  
  FPCMacOSAll.SetWindowModality(WindowRef(ACustomForm.Handle),
    kWindowModalityNone, nil);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.ShowModal
  Params:  ACustomForm - LCL custom form
  Returns: Nothing

  Shows modal window in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
  if not WSCheckHandleAllocated(ACustomForm, 'ShowModal') then Exit;

  SetWindowModality(WindowRef(ACustomForm.Handle),
    kWindowModalityAppModal, nil);
  SelectWindow(WindowRef(ACustomForm.Handle));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.SetBorderIcons
  Params:  AForm        - LCL custom form
           ABorderIcons - Border icons
  Returns: Nothing

  Sets the border icons of window in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
  const ABorderIcons: TBorderIcons);
var
  AttrsSet, AttrsClear: WindowAttributes;
begin
  if not WSCheckHandleAllocated(AForm, 'SetBorderIcons') then Exit;

  AttrsSet := 0;
  AttrsClear := 0;
  
  if (biMinimize in ABorderIcons) and (biSystemMenu in ABorderIcons) then
    AttrsSet := AttrsSet or kWindowCollapseBoxAttribute
  else
    AttrsClear := AttrsClear or kWindowCollapseBoxAttribute;
    
  if (biMaximize in ABorderIcons) and (biSystemMenu in ABorderIcons) then
    AttrsSet := AttrsSet or kWindowFullZoomAttribute
  else
    AttrsClear := AttrsClear or kWindowFullZoomAttribute;
    
  if biSystemMenu in ABorderIcons then
    AttrsSet := AttrsSet or kWindowCloseBoxAttribute
  else
    AttrsClear := AttrsClear or kWindowCloseBoxAttribute;
    
  ChangeWindowAttributes(HIWindowRef(AForm.Handle), AttrsSet, AttrsClear);
end;

class procedure TCarbonWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
begin
  RecreateWnd(AForm);
end;


initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollingWinControl, TCarbonWSScrollingWinControl);
//  RegisterWSComponent(TScrollBox, TCarbonWSScrollBox);
//  RegisterWSComponent(TCustomFrame, TCarbonWSCustomFrame);
//  RegisterWSComponent(TFrame, TCarbonWSFrame);
  RegisterWSComponent(TCustomForm, TCarbonWSCustomForm, TCarbonPrivateWindow);
//  RegisterWSComponent(TForm, TCarbonWSForm);
//  RegisterWSComponent(THintWindow, TCarbonWSHintWindow);
//  RegisterWSComponent(TScreen, TCarbonWSScreen);
//  RegisterWSComponent(TApplicationProperties, TCarbonWSApplicationProperties);
////////////////////////////////////////////////////

end.
