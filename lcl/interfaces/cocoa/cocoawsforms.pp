{ $Id: cocoawsforms.pp 12783 2007-11-08 11:45:39Z tombo $}
{
 *****************************************************************************
 *                             CocoaWSForms.pp                               *
 *                               ------------                                *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit CocoaWSForms;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  // RTL,FCL
  MacOSAll, CocoaAll, Classes,
  // LCL
  Controls, Graphics, LCLType, LMessages, LCLProc,
  // Widgetset
  WSForms, WSLCLClasses, WSProc, LCLMessageGlue,
  // LCL Cocoa
  CocoaPrivate, CocoaUtils, CocoaWSCommon, CocoaWSStdCtrls;

type
  { TLCLWindowCallback }

  TLCLWindowCallback=class(TWindowCallback)
  public
    Target  : TControl;
    constructor Create(AOwner: NSWindow; ATarget: TControl);
    procedure Activate; override;
    procedure Deactivate; override;
    procedure CloseQuery(var CanClose: Boolean); override;
    procedure Close; override;
    procedure Resize; override;
  end;


  { TCocoaWSScrollingWinControl }

  TCocoaWSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
  public
//    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
//    class procedure ScrollBy(const AWinControl: TScrollingWinControl; const DeltaX, DeltaY: integer); override;
  end;

  { TCocoaWSScrollBox }

  TCocoaWSScrollBox = class(TWSScrollBox)
  private
  protected
  public
  end;

  { TCocoaWSCustomFrame }

  TCocoaWSCustomFrame = class(TWSCustomFrame)
  private
  protected
  public
  end;

  { TCocoaWSFrame }

  TCocoaWSFrame = class(TWSFrame)
  private
  protected
  public
  end;

  { TCocoaWSCustomForm }
  TCocoaWSCustomFormClass = class of TCocoaWSCustomForm;
  TCocoaWSCustomForm = class(TWSCustomForm)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class procedure ShowHide(const AWinControl: TWinControl); override; //TODO: rename to SetVisible(control, visible)

    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class function  GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;

//    class procedure CloseModal(const ACustomForm: TCustomForm); override;
//    class procedure ShowModal(const ACustomForm: TCustomForm); override;
    
//    class procedure SetBorderIcons(const AForm: TCustomForm; const ABorderIcons: TBorderIcons); override;
//    class procedure SetFormBorderStyle(const AForm: TCustomForm; const AFormBorderStyle: TFormBorderStyle); override;

    {need to override these }
    class function  GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class function  GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  { TCocoaWSForm }

  TCocoaWSForm = class(TWSForm)
  private
  protected
  public
  end;

  { TCocoaWSHintWindow }

  TCocoaWSHintWindow = class(TWSHintWindow)
  private
  protected
  public
//    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCocoaWSScreen }

  TCocoaWSScreen = class(TWSScreen)
  private
  protected
  public
  end;

  { TCocoaWSApplicationProperties }

  TCocoaWSApplicationProperties = class(TWSApplicationProperties)
  private
  protected
  public
  end;


implementation

{ TLCLWindowCallback }

constructor TLCLWindowCallback.Create(AOwner: NSWindow; ATarget: TControl);
begin
  inherited Create(AOwner);
  Target:=ATarget;
end;

procedure TLCLWindowCallback.Activate;
begin
  LCLSendActivateMsg(Target, True, false);
end;

procedure TLCLWindowCallback.Deactivate;
begin
  LCLSendDeactivateStartMsg(Target);
end;

procedure TLCLWindowCallback.CloseQuery(var CanClose: Boolean);
begin
  // Message results : 0 - do nothing, 1 - destroy window
  CanClose:=LCLSendCloseQueryMsg(Target)>0;
end;

procedure TLCLWindowCallback.Close;
begin
  LCLSendCloseUpMsg(Target);
end;

procedure TLCLWindowCallback.Resize;
var
  sz  : NSSize;
  r   : TRect;
begin
  sz := Owner.frame.size;
  TCocoaWSCustomForm.GetClientBounds(TWinControl(Target), r);
  if Assigned(Target) then
    LCLSendSizeMsg(Target, Round(sz.width), Round(sz.height), SIZENORMAL);
end;


{ TCocoaWSCustomForm }

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomForm.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the window in Cocoa interface

  Creates new window in Cocoa interface with the specified parameters
 ------------------------------------------------------------------------------}

class function TCocoaWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  win : TCocoaWindow;
  cnt : TCocoaCustomControl;
const
  WinMask = NSTitledWindowMask or NSClosableWindowMask or NSMiniaturizableWindowMask or NSResizableWindowMask;
begin
  win := TCocoaWindow(TCocoaWindow.alloc);

  if not Assigned(win) then begin
    Result:=0;
    Exit;
  end;

  win:=TCocoaWindow(win.initWithContentRect_styleMask_backing_defer(CreateParamsToNSRect(AParams), WinMask, NSBackingStoreBuffered, False));
  TCocoaWindow(win).callback:=TLCLCommonCallback.Create(win, AWinControl);
  TCocoaWindow(win).wincallback:=TLCLWindowCallback.Create(win, AWinControl);
  win.setDelegate(win);
  win.setTitle(NSStringUtf8(AWinControl.Caption));
  win.setAcceptsMouseMovedEvents(True);

  cnt:=TCocoaCustomControl.alloc.init;
  cnt.callback:=TCocoaWindow(win).callback;
  win.setContentView(cnt);

  Result := TLCLIntfHandle(win);
end;

class procedure TCocoaWSCustomForm.ShowHide(const AWinControl: TWinControl);
var
  win : NSWindow;
begin
  win:=NSWindow(AWinControl.Handle);
  if not Assigned(win) then Exit;

  if AWinControl.Visible then
    win.orderFrontRegardless
  else
    win.orderOut(nil);
end;

class function TCocoaWSCustomForm.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  win   : TCocoaWindow;
begin
  win:=TCocoaWindow(AWinControl.Handle);
  Result:=Assigned(win);
  if not Result then Exit;
  AText:=NSStringToString(win.title);
  Result:=true;
end;

class function TCocoaWSCustomForm.GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean;
var
  win   : TCocoaWindow;
begin
  win:=TCocoaWindow(AWinControl.Handle);
  Result:=Assigned(win);
  if not Result then Exit;
  ALength:=win.title.length;
end;

class procedure TCocoaWSCustomForm.SetText(const AWinControl: TWinControl; const AText: String);
var
  win   : TCocoaWindow;
begin
  win:=TCocoaWindow(AWinControl.Handle);
  if not Assigned(win) then Exit;
  win.setTitle(NSStringUtf8(AText));
end;

class function TCocoaWSCustomForm.GetClientBounds(const AWinControl: TWinControl; var ARect: TRect): Boolean;
begin
  Result:=AWinControl.Handle<>0;
  if not Result then Exit;
  ARect:=NSObject(AWinControl.Handle).lclClientFrame;
end;

class function TCocoaWSCustomForm.GetClientRect(const AWinControl: TWinControl; var ARect: TRect): Boolean;
var
  x,y : Integer;
begin
  Result:=AWinControl.Handle<>0;
  if not Result then Exit;
  ARect:=NSObject(AWinControl.Handle).lclClientFrame;
  x:=0;y:=0;
  NSObject(AWinControl.Handle).lclLocalToScreen(x,y);
  MoveRect(ARect, x,y);
end;

class procedure TCocoaWSCustomForm.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
begin
  if AWinControl.Handle=0 then Exit;
  {todo: setFrame_display(, true)? }
  //sf:=NSScreen.mainScreen.frame;
  NSObject(AWinControl.Handle).lclSetFrame(Bounds(ALeft, ATop, AWidth, AHeight));

  //LCLToCocoaRect( GetNSRect(ALeft,ATop,AWidth,AHeight), sf, wf);
  //NSWindow(AWinControl.Handle).setFrame_display(wf, false);
  //NSWindow(AWinControl.Handle).setFrame_display( GetNSRect(ALeft,ATop, AWidth, AHeight), false);
  //NSWindow(AWinControl.Handle).setFrameTopLeftPoint( GetNSPoint(ALeft, ATop));
end;

end.
