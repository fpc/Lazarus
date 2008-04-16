{ $Id$}
{
 *****************************************************************************
 *                               WSControls.pp                               * 
 *                               -------------                               * 
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
unit WSControls;

{$mode objfpc}{$H+}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
  Classes,
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Controls, Graphics, LCLType,
////////////////////////////////////////////////////
  WSLCLClasses, WSImgList,
  { TODO: remove when CreateHandle/Component code moved }
  InterfaceBase;

type
  { TWSDragImageList }

  TWSDragImageList = class(TWSCustomImageList)
    class function BeginDrag(const ADragImageList: TDragImageList; Window: HWND; AIndex, X, Y: Integer): Boolean; virtual;
    class function DragMove(const ADragImageList: TDragImageList; X, Y: Integer): Boolean; virtual;
    class procedure EndDrag(const ADragImageList: TDragImageList); virtual;
    class function HideDragImage(const ADragImageList: TDragImageList;
      ALockedWindow: HWND; DoUnLock: Boolean): Boolean; virtual;
    class function ShowDragImage(const ADragImageList: TDragImageList;
      ALockedWindow: HWND; X, Y: Integer; DoLock: Boolean): Boolean; virtual;
  end;

  TWSDragImageListClass = class of TWSDragImageList;
  
  { TWSControl }

  TWSControl = class(TWSLCLComponent)
    class procedure AddControl(const AControl: TControl); virtual;
  end;

  TWSControlClass = class of TWSControl;

  { TWSWinControl }

  TWSZPosition = (wszpBack, wszpFront);
  
  { TWSWinControl }

  TWSWinControl = class(TWSControl)
    class function  CanFocus(const AWincontrol: TWinControl): Boolean; virtual;
    
    class function  GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean; virtual;
    class function  GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean; virtual;
    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); virtual;
    class function  GetDefaultClientRect(const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect): boolean; virtual;
    class function GetDesignInteractive(const AWinControl: TWinControl; AClientPos: TPoint): Boolean; virtual;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; virtual;
    class function  GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; virtual;

    class procedure SetBiDiMode(const AWinControl: TWinControl; const ABiDiMode: TBiDiMode); virtual;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); virtual;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); virtual;
    class procedure SetColor(const AWinControl: TWinControl); virtual;
    class procedure SetChildZPosition(const AWinControl, AChild: TWinControl; const AOldPos, ANewPos: Integer; const AChildren: TFPList); virtual;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); virtual;
    class procedure SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer); virtual;
    class procedure SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer); virtual;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); virtual;
    class procedure SetCursor(const AWinControl: TWinControl; const ACursor: HCursor); virtual;
    class procedure SetShape(const AWinControl: TWinControl; const AShape: HBITMAP); virtual;

    { TODO: move AdaptBounds: it is only used in winapi interfaces }
    class procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean); virtual;
          
    class procedure ConstraintsChange(const AWinControl: TWinControl); virtual;
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; virtual;
    class procedure DestroyHandle(const AWinControl: TWinControl); virtual;
    class procedure Invalidate(const AWinControl: TWinControl); virtual;
    class procedure PaintTo(const AWinControl: TWinControl; ADC: HDC; X, Y: Integer); virtual;
    class procedure ShowHide(const AWinControl: TWinControl); virtual; //TODO: rename to SetVisible(control, visible)
  end;
  TWSWinControlClass = class of TWSWinControl;

  { TWSGraphicControl }

  TWSGraphicControl = class(TWSControl)
  end;

  { TWSCustomControl }

  TWSCustomControl = class(TWSWinControl)
  end;

  { TWSImageList }

  TWSImageList = class(TWSDragImageList)
  end;


implementation


{ TWSControl }

class procedure TWSControl.AddControl(const AControl: TControl);
begin
end;

{ TWSWinControl }

class procedure TWSWinControl.AdaptBounds(const AWinControl: TWinControl;
  var Left, Top, Width, Height: integer; var SuppressMove: boolean);
begin
end;

class procedure TWSWinControl.ConstraintsChange(const AWinControl: TWinControl);
begin
end;

class function TWSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  // For now default to the old creation routines
  Result := 0;
end;

class procedure TWSWinControl.DestroyHandle(const AWinControl: TWinControl);
begin
end;

class function TWSWinControl.CanFocus(const AWincontrol: TWinControl): Boolean;
begin
  // lets consider that by deafult all WinControls can be focused
  Result := True;
end;

class function TWSWinControl.GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
  // for now default to the WinAPI version
  Result := WidgetSet.GetClientBounds(AWincontrol.Handle, ARect);
end;

class function TWSWinControl.GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
  // for now default to the WinAPI version
  Result := WidgetSet.GetClientRect(AWincontrol.Handle, ARect);
end;

{------------------------------------------------------------------------------
  Function: TWSWinControl.GetText
  Params:  Sender: The control to retrieve the text from
  Returns: the requested text

  Retrieves the text from a control. 
 ------------------------------------------------------------------------------}
class function TWSWinControl.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
begin
  Result := false;
end;
  
class function TWSWinControl.GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean;
var
  S: String;
begin
  Result := GetText(AWinControl, S);
  if Result
  then ALength := Length(S);
end;

class procedure TWSWinControl.SetBiDiMode(const AWinControl: TWinControl;
  const ABiDiMode: TBiDiMode);
begin
end;

class procedure TWSWinControl.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth := 0;
  PreferredHeight := 0;
end;

class function TWSWinControl.GetDefaultClientRect(
  const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer;
  var aClientRect: TRect): boolean;
begin
  Result:=false;
end;

class function TWSWinControl.GetDesignInteractive(
  const AWinControl: TWinControl; AClientPos: TPoint): Boolean;
begin
  Result := False;
end;

class procedure TWSWinControl.Invalidate(const AWinControl: TWinControl);
begin
end;

class procedure TWSWinControl.PaintTo(const AWinControl: TWinControl; ADC: HDC;
  X, Y: Integer);
begin

end;

class procedure TWSWinControl.SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer);
begin
end;
    
class procedure TWSWinControl.SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
begin
end;

class procedure TWSWinControl.SetChildZPosition(
  const AWinControl, AChild: TWinControl; const AOldPos, ANewPos: Integer;
  const AChildren: TFPList);
begin
end;

class procedure TWSWinControl.SetColor(const AWinControl: TWinControl);
begin
end;

class procedure TWSWinControl.SetCursor(const AWinControl: TWinControl; const ACursor: HCursor);
begin
end;

class procedure TWSWinControl.SetShape(const AWinControl: TWinControl;
  const AShape: HBITMAP);
begin
end;

class procedure TWSWinControl.SetFont(const AWinControl: TWinControl; const AFont: TFont);
begin
end;

class procedure TWSWinControl.SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer);
begin
end;

class procedure TWSWinControl.SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer);
begin
end;

{------------------------------------------------------------------------------
  Method: TWSWinControl.SetLabel
  Params:  AWinControl - the calling object
           AText       - String to be set as label/text for a control
  Returns: Nothing

  Sets the label text on a widget
 ------------------------------------------------------------------------------}
class procedure TWSWinControl.SetText(const AWinControl: TWinControl; const AText: String);
begin
end;

class procedure TWSWinControl.ShowHide(const AWinControl: TWinControl);
begin
end;

{ TWSDragImageList }

class function TWSDragImageList.BeginDrag(const ADragImageList: TDragImageList;
  Window: HWND; AIndex, X, Y: Integer): Boolean;
begin
  Result := False;
end;

class function TWSDragImageList.DragMove(const ADragImageList: TDragImageList;
  X, Y: Integer): Boolean;
begin
  Result := False;
end;

class procedure TWSDragImageList.EndDrag(const ADragImageList: TDragImageList);
begin
end;

class function TWSDragImageList.HideDragImage(const ADragImageList: TDragImageList;
  ALockedWindow: HWND; DoUnLock: Boolean): Boolean;
begin
  Result := False;
end;

class function TWSDragImageList.ShowDragImage(const ADragImageList: TDragImageList;
  ALockedWindow: HWND; X, Y: Integer; DoLock: Boolean): Boolean;
begin
  Result := False;
end;

initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TDragImageList, TWSDragImageList);
  RegisterWSComponent(TControl, TWSControl);
  RegisterWSComponent(TWinControl, TWSWinControl);
//  RegisterWSComponent(TGraphicControl, TWSGraphicControl);
//  RegisterWSComponent(TCustomControl, TWSCustomControl);
//  RegisterWSComponent(TImageList, TWSImageList);
////////////////////////////////////////////////////
end.
