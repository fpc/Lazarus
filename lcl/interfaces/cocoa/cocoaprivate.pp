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

{$mode objfpc}{$H+}

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

end.


