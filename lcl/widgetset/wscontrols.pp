{
 *****************************************************************************
 *                               WSControls.pp                               * 
 *                               -------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSControls;

{$mode objfpc}{$H+}
{$I lcl_defines.inc}

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
  Classes, Types,
////////////////////////////////////////////////////
// To get as little as possible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Controls, Graphics, LCLType,
////////////////////////////////////////////////////
  WSLCLClasses, WSImgList,
  { TODO: remove when CreateHandle/Component code moved }
  InterfaceBase, WSFactory;

const
  DefBtnColors: array[TDefaultColorType] of TColor = (
 { dctBrush } clBtnFace,
 { dctFont  } clBtnText
  );
type
  { TWSDragImageList }

  TWSDragImageListClass = class of TWSDragImageList;
  TWSDragImageList = class(TWSCustomImageList_CallWS)
  private class var
    FWSDragImageList_Impl: TWSDragImageListClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
    class function BeginDrag(const ADragImageList: TDragImageList; Window: HWND; AIndex, X, Y: Integer): Boolean; virtual;
    class function DragMove(const ADragImageList: TDragImageList; X, Y: Integer): Boolean; virtual;
    class procedure EndDrag(const ADragImageList: TDragImageList); virtual;
    class function HideDragImage(const ADragImageList: TDragImageList;
      ALockedWindow: HWND; DoUnLock: Boolean): Boolean; virtual;
    class function ShowDragImage(const ADragImageList: TDragImageList;
      ALockedWindow: HWND; X, Y: Integer; DoLock: Boolean): Boolean; virtual;
  end;

  { TWSLazAccessibleObject }

  TWSLazAccessibleObjectClass = class of TWSLazAccessibleObject;
  TWSLazAccessibleObject = class(TWSObject)
  private class var
    FWSLazAccessibleObject_Impl: TWSLazAccessibleObjectClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
    class function CreateHandle(const AObject: TLazAccessibleObject): HWND; virtual;
    class procedure DestroyHandle(const AObject: TLazAccessibleObject); virtual;
    class procedure SetAccessibleDescription(const AObject: TLazAccessibleObject; const ADescription: string); virtual;
    class procedure SetAccessibleValue(const AObject: TLazAccessibleObject; const AValue: string); virtual;
    class procedure SetAccessibleRole(const AObject: TLazAccessibleObject; const ARole: TLazAccessibilityRole); virtual;
    class procedure SetPosition(const AObject: TLazAccessibleObject; const AValue: TPoint); virtual;
    class procedure SetSize(const AObject: TLazAccessibleObject; const AValue: TSize); virtual;
  end;

  { TWSControl }

  TWSControlClass = class of TWSControl;
  TWSControl = class(TWSLCLComponent)
  private class var
    FWSControl_Impl: TWSControlClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
    class procedure AddControl(const AControl: TControl); virtual;
    class function GetConstraints(const AControl: TControl; const AConstraints: TObject): Boolean; virtual;
    class function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; virtual;
    class procedure ConstraintWidth(const AControl: TControl; const AConstraints: TObject; var aWidth: integer); virtual;
    class procedure ConstraintHeight(const AControl: TControl; const AConstraints: TObject; var aHeight: integer); virtual;
  end;

  { TWSControl_CallWS }

  TWSControl_CallWS = class(TWSControl)
  public
    class procedure AddControl(const AControl: TControl); override;
    class function GetConstraints(const AControl: TControl; const AConstraints: TObject): Boolean; override;
    class function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;
    class procedure ConstraintWidth(const AControl: TControl; const AConstraints: TObject; var aWidth: integer); override;
    class procedure ConstraintHeight(const AControl: TControl; const AConstraints: TObject; var aHeight: integer); override;
  end;

  { TWSWinControl }

  TWSZPosition = (wszpBack, wszpFront);
  
  { TWSWinControl }

  TWSWinControlClass = class of TWSWinControl;
  TWSWinControl = class(TWSControl_CallWS)
  private class var
    FWSWinControl_Impl: TWSWinControlClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
    class function  CanFocus(const AWincontrol: TWinControl): Boolean; virtual;
    
    class function  GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean; virtual;
    class function  GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean; virtual;
    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); virtual;
    class function  GetDefaultClientRect(const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect): boolean; virtual;
    class function GetDesignInteractive(const AWinControl: TWinControl; AClientPos: TPoint): Boolean; virtual;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; virtual;
    class function  GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; virtual;

    class procedure SetBiDiMode(const AWinControl: TWinControl; UseRightToLeftAlign, UseRightToLeftReading, UseRightToLeftScrollBar : Boolean); virtual;
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
    class procedure DefaultWndHandler(const AWinControl: TWinControl; var AMessage); virtual;
    class procedure Invalidate(const AWinControl: TWinControl); virtual;
    class procedure PaintTo(const AWinControl: TWinControl; ADC: HDC; X, Y: Integer); virtual;
    class procedure Repaint(const AWinControl: TWinControl); virtual;
    class procedure ShowHide(const AWinControl: TWinControl); virtual; //TODO: rename to SetVisible(control, visible)
    class procedure ScrollBy(const AWinControl: TWinControl; DeltaX, DeltaY: integer); virtual;
  end;

  { TWSWinControl_CallWS }

  TWSWinControl_CallWS = class(TWSWinControl)
  public
    class function  CanFocus(const AWincontrol: TWinControl): Boolean; override;

    class function  GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class function  GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    class function  GetDefaultClientRect(const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect): boolean; override;
    class function GetDesignInteractive(const AWinControl: TWinControl; AClientPos: TPoint): Boolean; override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class function  GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; override;

    class procedure SetBiDiMode(const AWinControl: TWinControl; UseRightToLeftAlign, UseRightToLeftReading, UseRightToLeftScrollBar : Boolean); override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetChildZPosition(const AWinControl, AChild: TWinControl; const AOldPos, ANewPos: Integer; const AChildren: TFPList); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer); override;
    class procedure SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class procedure SetCursor(const AWinControl: TWinControl; const ACursor: HCursor); override;
    class procedure SetShape(const AWinControl: TWinControl; const AShape: HBITMAP); override;

    { TODO: move AdaptBounds: it is only used in winapi interfaces }
    class procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean); override;

    class procedure ConstraintsChange(const AWinControl: TWinControl); override;
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure DefaultWndHandler(const AWinControl: TWinControl; var AMessage); override;
    class procedure Invalidate(const AWinControl: TWinControl); override;
    class procedure PaintTo(const AWinControl: TWinControl; ADC: HDC; X, Y: Integer); override;
    class procedure Repaint(const AWinControl: TWinControl); override;
    class procedure ShowHide(const AWinControl: TWinControl); override; //TODO: rename to SetVisible(control, visible)
    class procedure ScrollBy(const AWinControl: TWinControl; DeltaX, DeltaY: integer); override;
  end;

  { TWSGraphicControl }

  TWSGraphicControl = class(TWSControl_CallWS)
  published
  end;

  { TWSCustomControl }

  TWSCustomControlClass = class of TWSCustomControl;
  TWSCustomControl = class(TWSWinControl_CallWS)
  private class var
    FWSCustomControl_Impl: TWSCustomControlClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
  end;

  { TWSImageList }

  TWSImageList = class(TWSDragImageList)
  published
  end;

procedure RegisterDragImageList;
procedure RegisterLazAccessibleObject;
procedure RegisterControl;
procedure RegisterWinControl;
procedure RegisterGraphicControl;
procedure RegisterCustomControl;

implementation

{ TWSLazAccessibleObject }

class function TWSLazAccessibleObject.GetImplementation: TWSObjectClass;
begin
  Result:= FWSLazAccessibleObject_Impl;
end;

class procedure TWSLazAccessibleObject.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSLazAccessibleObject_Impl := TWSLazAccessibleObjectClass(AImpl);
end;

class function TWSLazAccessibleObject.CreateHandle(
  const AObject: TLazAccessibleObject): HWND;
begin
  Result := 0;
end;

class procedure TWSLazAccessibleObject.DestroyHandle(
  const AObject: TLazAccessibleObject);
begin

end;

class procedure TWSLazAccessibleObject.SetAccessibleDescription(const AObject: TLazAccessibleObject; const ADescription: string);
begin

end;

class procedure TWSLazAccessibleObject.SetAccessibleValue(const AObject: TLazAccessibleObject; const AValue: string);
begin

end;

class procedure TWSLazAccessibleObject.SetAccessibleRole(const AObject: TLazAccessibleObject; const ARole: TLazAccessibilityRole);
begin

end;

class procedure TWSLazAccessibleObject.SetPosition(
  const AObject: TLazAccessibleObject; const AValue: TPoint);
begin

end;

class procedure TWSLazAccessibleObject.SetSize(
  const AObject: TLazAccessibleObject; const AValue: TSize);
begin

end;

{ TWSControl }

class function TWSControl.GetImplementation: TWSObjectClass;
begin
  Result := FWSControl_Impl;
end;

class procedure TWSControl.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSControl_Impl := TWSControlClass(AImpl);
end;

class procedure TWSControl.AddControl(const AControl: TControl);
begin
end;

class function TWSControl.GetConstraints(const AControl: TControl; const AConstraints: TObject): Boolean;
begin
  Result := WidgetSet.GetControlConstraints(AConstraints);
end;

class function TWSControl.GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor;
begin
  Result := clDefault;
end;

class procedure TWSControl.ConstraintWidth(const AControl: TControl;
  const AConstraints: TObject; var aWidth: integer);
begin

end;

class procedure TWSControl.ConstraintHeight(const AControl: TControl;
  const AConstraints: TObject; var aHeight: integer);
begin

end;

{ TWSControl_CallWS }

class procedure TWSControl_CallWS.AddControl(const AControl: TControl);
begin
  FWSControl_Impl.AddControl(AControl);
end;

class function TWSControl_CallWS.GetConstraints(const AControl: TControl;
  const AConstraints: TObject): Boolean;
begin
  Result := FWSControl_Impl.GetConstraints(AControl, AConstraints);
end;

class function TWSControl_CallWS.GetDefaultColor(const AControl: TControl;
  const ADefaultColorType: TDefaultColorType): TColor;
begin
  Result := FWSControl_Impl.GetDefaultColor(AControl, ADefaultColorType);
end;

class procedure TWSControl_CallWS.ConstraintWidth(const AControl: TControl;
  const AConstraints: TObject; var aWidth: integer);
begin
  FWSControl_Impl.ConstraintWidth(AControl, AConstraints, aWidth);
end;

class procedure TWSControl_CallWS.ConstraintHeight(const AControl: TControl;
  const AConstraints: TObject; var aHeight: integer);
begin
  FWSControl_Impl.ConstraintHeight(AControl, AConstraints, aHeight);
end;

{ TWSWinControl }

class function TWSWinControl.GetImplementation: TWSObjectClass;
begin
  Result := FWSWinControl_Impl;
end;

class procedure TWSWinControl.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSWinControl_Impl := TWSWinControlClass(AImpl);
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

class procedure TWSWinControl.DefaultWndHandler(const AWinControl: TWinControl; var AMessage);
begin
  WidgetSet.CallDefaultWndHandler(AWinControl, AMessage);
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

class procedure TWSWinControl.SetBiDiMode(const AWinControl: TWinControl; UseRightToLeftAlign, UseRightToLeftReading, UseRightToLeftScrollBar : Boolean);
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

class procedure TWSWinControl.Repaint(const AWinControl: TWinControl);
begin
  AWinControl.Invalidate;
  AWinControl.Update;
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

class procedure TWSWinControl.ScrollBy(const AWinControl: TWinControl; DeltaX, DeltaY: integer);
begin
  AWinControl.Invalidate;
end;

{ TWSWinControl_CallWS }

class function TWSWinControl_CallWS.CanFocus(const AWincontrol: TWinControl
  ): Boolean;
begin
  Result:= FWSWinControl_Impl.CanFocus(AWincontrol);
end;

class function TWSWinControl_CallWS.GetClientBounds(
  const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
  Result:= FWSWinControl_Impl.GetClientBounds(AWincontrol, ARect);
end;

class function TWSWinControl_CallWS.GetClientRect(
  const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
  Result:= FWSWinControl_Impl.GetClientRect(AWincontrol, ARect);
end;

class procedure TWSWinControl_CallWS.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  FWSWinControl_Impl.GetPreferredSize(AWinControl, PreferredWidth, PreferredHeight,
    WithThemeSpace);
end;

class function TWSWinControl_CallWS.GetDefaultClientRect(
  const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer;
  var aClientRect: TRect): boolean;
begin
  Result:= FWSWinControl_Impl.GetDefaultClientRect(AWinControl, aLeft, aTop, aWidth,
    aHeight, aClientRect);
end;

class function TWSWinControl_CallWS.GetDesignInteractive(
  const AWinControl: TWinControl; AClientPos: TPoint): Boolean;
begin
  Result:= FWSWinControl_Impl.GetDesignInteractive(AWinControl, AClientPos);
end;

class function TWSWinControl_CallWS.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
begin
  Result:= FWSWinControl_Impl.GetText(AWinControl, AText);
end;

class function TWSWinControl_CallWS.GetTextLen(const AWinControl: TWinControl;
  var ALength: Integer): Boolean;
begin
  Result:= FWSWinControl_Impl.GetTextLen(AWinControl, ALength);
end;

class procedure TWSWinControl_CallWS.SetBiDiMode(
  const AWinControl: TWinControl; UseRightToLeftAlign, UseRightToLeftReading,
  UseRightToLeftScrollBar: Boolean);
begin
  FWSWinControl_Impl.SetBiDiMode(AWinControl, UseRightToLeftAlign,
    UseRightToLeftReading, UseRightToLeftScrollBar);
end;

class procedure TWSWinControl_CallWS.SetBorderStyle(
  const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
begin
  FWSWinControl_Impl.SetBorderStyle(AWinControl, ABorderStyle);
end;

class procedure TWSWinControl_CallWS.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
begin
  FWSWinControl_Impl.SetBounds(AWinControl, ALeft, ATop, AWidth, AHeight);
end;

class procedure TWSWinControl_CallWS.SetColor(const AWinControl: TWinControl);
begin
  FWSWinControl_Impl.SetColor(AWinControl);
end;

class procedure TWSWinControl_CallWS.SetChildZPosition(const AWinControl,
  AChild: TWinControl; const AOldPos, ANewPos: Integer; const AChildren: TFPList
  );
begin
  FWSWinControl_Impl.SetChildZPosition(AWinControl, AChild, AOldPos, ANewPos, AChildren);
end;

class procedure TWSWinControl_CallWS.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
begin
  FWSWinControl_Impl.SetFont(AWinControl, AFont);
end;

class procedure TWSWinControl_CallWS.SetPos(const AWinControl: TWinControl;
  const ALeft, ATop: Integer);
begin
  FWSWinControl_Impl.SetPos(AWinControl, ALeft, ATop);
end;

class procedure TWSWinControl_CallWS.SetSize(const AWinControl: TWinControl;
  const AWidth, AHeight: Integer);
begin
  FWSWinControl_Impl.SetSize(AWinControl, AWidth, AHeight);
end;

class procedure TWSWinControl_CallWS.SetText(const AWinControl: TWinControl;
  const AText: String);
begin
  FWSWinControl_Impl.SetText(AWinControl, AText);
end;

class procedure TWSWinControl_CallWS.SetCursor(const AWinControl: TWinControl;
  const ACursor: HCursor);
begin
  FWSWinControl_Impl.SetCursor(AWinControl, ACursor);
end;

class procedure TWSWinControl_CallWS.SetShape(const AWinControl: TWinControl;
  const AShape: HBITMAP);
begin
  FWSWinControl_Impl.SetShape(AWinControl, AShape);
end;

class procedure TWSWinControl_CallWS.AdaptBounds(
  const AWinControl: TWinControl; var Left, Top, Width, Height: integer;
  var SuppressMove: boolean);
begin
  FWSWinControl_Impl.AdaptBounds(AWinControl, Left, Top, Width, Height, SuppressMove);
end;

class procedure TWSWinControl_CallWS.ConstraintsChange(
  const AWinControl: TWinControl);
begin
  FWSWinControl_Impl.ConstraintsChange(AWinControl);
end;

class function TWSWinControl_CallWS.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result:= FWSWinControl_Impl.CreateHandle(AWinControl, AParams);
end;

class procedure TWSWinControl_CallWS.DestroyHandle(
  const AWinControl: TWinControl);
begin
  FWSWinControl_Impl.DestroyHandle(AWinControl);
end;

class procedure TWSWinControl_CallWS.DefaultWndHandler(
  const AWinControl: TWinControl; var AMessage);
begin
  FWSWinControl_Impl.DefaultWndHandler(AWinControl, AMessage);
end;

class procedure TWSWinControl_CallWS.Invalidate(const AWinControl: TWinControl);
begin
  FWSWinControl_Impl.Invalidate(AWinControl);
end;

class procedure TWSWinControl_CallWS.PaintTo(const AWinControl: TWinControl;
  ADC: HDC; X, Y: Integer);
begin
  FWSWinControl_Impl.PaintTo(AWinControl, ADC, X, Y);
end;

class procedure TWSWinControl_CallWS.Repaint(const AWinControl: TWinControl);
begin
  FWSWinControl_Impl.Repaint(AWinControl);
end;

class procedure TWSWinControl_CallWS.ShowHide(const AWinControl: TWinControl);
begin
  FWSWinControl_Impl.ShowHide(AWinControl);
end;

class procedure TWSWinControl_CallWS.ScrollBy(const AWinControl: TWinControl;
  DeltaX, DeltaY: integer);
begin
  FWSWinControl_Impl.ScrollBy(AWinControl, DeltaX, DeltaY);
end;

{ TWSDragImageList }

class function TWSDragImageList.GetImplementation: TWSObjectClass;
begin
  Result:= FWSDragImageList_Impl;
end;

class procedure TWSDragImageList.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSDragImageList_Impl := TWSDragImageListClass(AImpl);
end;

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

{ TWSCustomControl }

class function TWSCustomControl.GetImplementation: TWSObjectClass;
begin
  Result:= FWSCustomControl_Impl;
end;

class procedure TWSCustomControl.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSCustomControl_Impl := TWSCustomControlClass(AImpl);
end;

{ WidgetSetRegistration }

procedure RegisterDragImageList;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterDragImageList then
    RegisterWSComponent(TDragImageList, TWSDragImageList);
  Done := True;
end;

procedure RegisterLazAccessibleObject;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterLazAccessibleObject then
    RegisterWSLazAccessibleObject(TWSLazAccessibleObject);
  Done := True;
end;

procedure RegisterControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterControl then
    RegisterWSComponent(TControl, TWSControl);
  Done := True;
end;

procedure RegisterWinControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterWinControl then
    RegisterWSComponent(TWinControl, TWSWinControl);
  Done := True;
end;

procedure RegisterGraphicControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterGraphicControl;
//  if not WSRegisterGraphicControl then
//    RegisterWSComponent(TGraphicControl, TWSGraphicControl);
  Done := True;
end;

procedure RegisterCustomControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomControl;
//  if not WSRegisterCustomControl then
//    RegisterWSComponent(TCustomControl, TWSCustomControl);
  Done := True;
end;

end.
