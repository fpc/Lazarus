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
  // Libs
  MacOSAll, CocoaAll,
  // LCL
  Controls, Forms, Graphics, LCLType, LMessages, LCLProc, Classes,
  // Widgetset
  WSForms, WSLCLClasses, WSProc,
  // LCL Cocoa
  CocoaPrivate, CocoaUtils, CocoaWSCommon;

type

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
const
  WinMask = NSTitledWindowMask or NSClosableWindowMask or NSMiniaturizableWindowMask or NSResizableWindowMask;
begin
  win := TCocoaWindow(TCocoaWindow.alloc);
  if not Assigned(win) then begin
    Result:=0;
    Exit;
  end;
  TCocoaWindow(win).callback:=TControlCallback.Create(win, AWinControl);
  win.initWithContentRect_styleMask_backing_defer(CreateParamsToNSRect(AParams), WinMask, NSBackingStoreBuffered, False);
  win.setTitle(NSStringUtf8(AWinControl.Caption));

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
  title : NSString;
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
  ns    : NSString;
begin
  win:=TCocoaWindow(AWinControl.Handle);
  if not Assigned(win) then Exit;
  win.setTitle(NSStringUtf8(AText));
end;

end.
