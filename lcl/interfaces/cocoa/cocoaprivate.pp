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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
{$modeswitch objectivec1}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  // Libs
  MacOSAll, CocoaAll, CocoaUtils;

type

  { TCommonCallback }

  TCommonCallback = class(TObject)
  public
    Owner : NSObject;
    constructor Create(AOwner: NSObject);
    procedure MouseDown(x,y: Integer); virtual; abstract;
    procedure MouseUp(x,y: Integer); virtual; abstract;
  end;

  { TCocoaWindowContentView }

  TCocoaWindowContentView = objcclass(NSView)
  public
    procedure drawRect(r: NSRect); override;
  end;

  { TCocoaButton }

  TCocoaButton = objcclass(NSButton)
    callback  : TCommonCallback;
    function acceptsFirstResponder: Boolean; override;
    procedure mouseDown(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
  end;

  TCocoaTextField = objcclass(NSTextField)
    callback  : TCommonCallback;
    function acceptsFirstResponder: Boolean; override;
  end;

  TCocoaTextView = objcclass(NSTextView)
    callback  : TCommonCallback;
    function acceptsFirstResponder: Boolean; override;
  end;

  TCocoaWindow = objcclass(NSWindow)
  public
    callback  : TCommonCallback;
    function acceptsFirstResponder: Boolean; override;
    procedure mouseUp(event: NSEvent); override;
    procedure mouseDown(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
  end;

implementation

{ TCocoaWindowContentView }

procedure TCocoaWindowContentView.drawRect(r: NSRect);
begin
  //LCLSendPaintMsg();
  inherited drawRect(r);
end;

{ TCocoaButton }

function TCocoaButton.acceptsFirstResponder: Boolean;
begin
  Result:=true;
end;

procedure TCocoaButton.mouseUp(event: NSEvent);
var
  mp : NSPoint;
begin
  writeln('TCocoaButton.mouseUp mouse up event!');
  mp:=event.locationInWindow;
  callback.MouseUp(round(mp.x), round(mp.y));
  inherited mouseUp(event);
end;

procedure TCocoaButton.mouseDown(event: NSEvent);
var
  mp : NSPoint;
begin
  writeln('TCocoaButton.mouseDown mouse down event!');
  mp:=event.locationInWindow;
  callback.MouseDown(round(mp.x), round(mp.y));
  inherited mouseDown(event);
end;

procedure TCocoaButton.mouseDragged(event: NSEvent);
begin
  inherited mouseDragged(event);
end;

procedure TCocoaButton.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaButton.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

procedure TCocoaButton.mouseMoved(event: NSEvent);
begin
  inherited mouseMoved(event);
end;

{ TCocoaTextField }

function TCocoaTextField.acceptsFirstResponder: Boolean;
begin
  Result:=true;
end;

{ TCocoaTextView }

function TCocoaTextView.acceptsFirstResponder: Boolean;
begin
  Result:=true;
end;

{ TCocoaWindow }

function TCocoaWindow.acceptsFirstResponder: Boolean;
begin
  Result:=true;
end;

procedure TCocoaWindow.mouseUp(event: NSEvent);
var
  mp : NSPoint;
begin
  writeln('TCocoaWindow.mouseUp event!');

  mp:=event.locationInWindow;
  callback.MouseUp(round(mp.x), round(mp.y));
  inherited mouseUp(event);
end;

procedure TCocoaWindow.mouseDown(event: NSEvent);
var
  mp : NSPoint;
begin
  writeln('TCocoaWindow.mouseDown event!');
  mp:=event.locationInWindow;
  callback.MouseDown(round(mp.x), round(mp.y));
  inherited mouseDown(event);
end;

procedure TCocoaWindow.mouseDragged(event: NSEvent);
begin
  inherited mouseDragged(event);
end;

procedure TCocoaWindow.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaWindow.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

procedure TCocoaWindow.mouseMoved(event: NSEvent);
begin
  inherited mouseMoved(event);
end;


{ TCommonCallback }

constructor TCommonCallback.Create(AOwner: NSObject);
begin
  Owner:=AOwner;
end;

end.

