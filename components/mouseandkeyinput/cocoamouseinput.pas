{ CocoaMouseInput

  Copyright (C) 2019 Sammarco Francesco

  This source is free software; you can redistribute it and/or modify it under the terms of the
  GNU General Public License as published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
  even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free Software
  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit CocoaMouseInput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms,
  CocoaUtils, CocoaAll, CGGeometry, CGEvent, CGEventTypes, CFBase,
  MouseInputIntf;

type

  { TCocoaMouseInput }

  TCocoaMouseInput = class(TMouseInput)
  protected
    procedure DoDown(Button: TMouseButton); override;
    procedure DoMove(ScreenX, ScreenY: Integer); override;
    procedure DoUp(Button: TMouseButton); override;
  end;

function InitializeMouseInput: TMouseInput;


implementation

function InitializeMouseInput: TMouseInput;
begin
  Result := TCocoaMouseInput.Create;
end;

const
  MouseButtonToCocoaButton: array [TMouseButton] of Integer =
    (kCGMouseButtonLeft, kCGMouseButtonRight, kCGMouseButtonCenter,3,4);


{ TCocoaMouseInput }

procedure TCocoaMouseInput.DoDown(Button: TMouseButton);
var
  MyMouseEvent : CGEventRef;
begin
     if Button = mbLeft then
        MyMouseEvent := CGEventCreateMouseEvent(nil, 1, CGPointMake(Mouse.CursorPos.x, Mouse.CursorPos.y), MouseButtonToCocoaButton[Button])
     else
        MyMouseEvent := CGEventCreateMouseEvent(nil, 3, CGPointMake(Mouse.CursorPos.x, Mouse.CursorPos.y), MouseButtonToCocoaButton[Button]);
     CGEventPost(kCGHIDEventTap,MyMouseEvent);
     CFRelease(MyMouseEvent);
end;

procedure TCocoaMouseInput.DoMove(ScreenX, ScreenY: Integer);
var
  MyMouseEvent : CGEventRef;
begin
     MyMouseEvent := CGEventCreateMouseEvent(nil, 5, CGPointMake(ScreenX, ScreenY), MouseButtonToCocoaButton[mbLeft]);
     CGEventPost(kCGHIDEventTap,MyMouseEvent);
     CFRelease(MyMouseEvent);
end;

procedure TCocoaMouseInput.DoUp(Button: TMouseButton);
var
  MyMouseEvent : CGEventRef;
begin
     if Button = mbLeft then
        MyMouseEvent := CGEventCreateMouseEvent(nil, 2, CGPointMake(Mouse.CursorPos.x, Mouse.CursorPos.y), MouseButtonToCocoaButton[Button])
     else
        MyMouseEvent := CGEventCreateMouseEvent(nil, 4, CGPointMake(Mouse.CursorPos.x, Mouse.CursorPos.y), MouseButtonToCocoaButton[Button]);
     CGEventPost(kCGHIDEventTap,MyMouseEvent);
     CFRelease(MyMouseEvent);
end;

end.

