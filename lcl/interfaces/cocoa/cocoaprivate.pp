{ $Id: $}
{                  --------------------------------------------
                  cocoaprivate.pp  -  Cocoa internal classes
                  --------------------------------------------

 This unit contains the private classhierarchy for the Carbon implemetations
 This hierarchy reflects (more or less) the Carbon widget hierarchy

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
unit CocoaPrivate;

{$mode delphi}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  // Libs
{$ifdef ver2_2_0}
  FPCMacOSAll,
{$else}
  MacOSAll,
{$endif}
  objc, foundation, appkit,
  // LCL
  LMessages, LCLMessageGlue, LCLProc, LCLType, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, Menus;
  
const
  Str_Button_OnClick = 'ButtonOnClick';
  
type

  { TCocoaForm }
  
  TCocoaForm = class(TObject)
  public
    { classes }
    MainWindow: NSWindow;
    MainWindowView: NSView;
    { strings and sizes }
    CFTitle: CFStringRef;
    MainWindowRect: NSRect;
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams);
  end;
  
  { TCocoaControl }

  TCocoaControl = class(NSObject)
  public
    { classes }
    ParentView: NSView;
    Control: NSControl;
    LCLControl: TWinControl;
    { strings and sizes }
    CFTitle: CFStringRef;
    ControlRect: NSRect;
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams);
    procedure InitializeFields;
    procedure InitializeControl;
  end;

  { TCocoaButton }

  TCocoaButton = class(TCocoaControl)
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams);
    function Button: NSButton;
    procedure AddMethods; override;
    { Objective-c Methods }
    class procedure ButtonOnClick(_self: objc.id; _cmd: SEL; sender: objc.id); cdecl;
  end;

implementation


{ TCocoaForm }

constructor TCocoaForm.Create(const AWinControl: TWinControl; const AParams: TCreateParams);
begin
  inherited Create;

  MainWindowRect.origin.x := AWinControl.Left;
  MainWindowRect.origin.y := AWinControl.Top;
  MainWindowRect.size.width := AWinControl.Width;
  MainWindowRect.size.height := AWinControl.Height;

  MainWindow := NSWindow.initWithContentRect_styleMask_backing_defer(MainWindowRect,
    NSTitledWindowMask or NSClosableWindowMask or NSMiniaturizableWindowMask or NSResizableWindowMask,
    NSBackingStoreBuffered, LongBool(NO));
  MainWindowView := NSView.CreateWithHandle(MainWindow.contentView);

  CFTitle := CFStringCreateWithPascalString(nil, AWinControl.Caption, kCFStringEncodingUTF8);
  MainWindow.setTitle(CFTitle);
end;

{ TCocoaControl }

constructor TCocoaControl.Create(const AWinControl: TWinControl;
  const AParams: TCreateParams);
begin
  { The class is registered on the Objective-C runtime before the NSObject constructor is called }
  if not CreateClassDefinition(ClassName(), Str_NSObject) then Exception.Create('Failed to create objc class: ' + ClassName());

  inherited Create;

  // Initializes information fields
  LCLControl := AWinControl;
  InitializeFields();
end;

procedure TCocoaControl.InitializeFields;
var
  ParentHeight: Single;
begin
  ParentHeight := 0;
  
  CFTitle := CFStringCreateWithPascalString(nil, LCLControl.Caption, kCFStringEncodingUTF8);

  // Get's information form the parent
  if LCLControl.Parent <> nil then
  begin
     if LCLControl.Parent is TCustomForm then
     begin
       ParentView := TCocoaForm(LCLControl.Parent.Handle).MainWindowView;
       ParentHeight := ParentView.frame.size.height;
     end;
  end;

  // Calculates the position on the Screen
  // Cocoa and LCL declare differently the coordinates system
  // In LCL (0,0) is in the top-left corner without title,
  // and in Cocoa it is in the bottom-left corner
  ControlRect.origin.x := LCLControl.Left;
  ControlRect.origin.y := ParentHeight - LCLControl.Top;
  ControlRect.size.width := LCLControl.Width;
  ControlRect.size.height := LCLControl.Height;
end;

procedure TCocoaControl.InitializeControl;
begin
  Control.setTag(PtrInt(Self));
end;

{ TCocoaButton }

constructor TCocoaButton.Create(const AWinControl: TWinControl;
  const AParams: TCreateParams);
begin
  inherited Create(AWinControl, AParams);

  Control := NSButton.initWithFrame(ControlRect);
  
  InitializeControl();
  
  Button.setTitle(CFTitle);
  Button.setBezelStyle(NSRoundedBezelStyle);
  Button.setAction(sel_registerName(PChar(Str_Button_OnClick)));
  Button.setTarget(Handle);

  if ParentView <> nil then ParentView.addSubview(Button.Handle);
end;

function TCocoaButton.Button: NSButton;
begin
  Result := NSButton(Control);
end;

procedure TCocoaButton.AddMethods;
begin
  AddMethod(Str_Button_OnClick, 'v@:@', Pointer(ButtonOnClick));
end;

class procedure TCocoaButton.ButtonOnClick(_self: objc.id; _cmd: SEL; sender: objc.id); cdecl;
var
  VSelf: TCocoaButton;
  VNSControl: NSControl;
begin
  VNSControl := NSControl.CreateWithHandle(sender);
  try
    VSelf := TCocoaButton(VNSControl.tag);
    VSelf.LCLControl.OnClick(VSelf.LCLControl);
  finally
    VNSControl.Handle := nil;
    VNSControl.Free;
  end;
end;

end.

