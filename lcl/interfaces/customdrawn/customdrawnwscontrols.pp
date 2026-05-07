{
 *****************************************************************************
 *                         CustomDrawnWSControls.pp                          *
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CustomDrawnWSControls;

{$mode objfpc}{$H+}

{$I customdrawndefines.inc}

interface

uses
  // LCL
  SysUtils, Classes, Types,
  //
  Controls, LCLType, LCLProc, Forms, Graphics,
  lazcanvas, lazregions,
  // Widgetset
  InterfaceBase, WSProc, WSControls, WSLCLClasses, customdrawnint,
  customdrawnproc;

type

  { TCDWSDragImageListResolution }

  TCDWSDragImageListResolution = class(TWSDragImageListResolution)
  published
{    class function BeginDrag(const ADragImageList: TDragImageListResolution; Window: HWND; AIndex, X, Y: Integer): Boolean; override;
    class function DragMove(const ADragImageList: TDragImageListResolution; X, Y: Integer): Boolean; override;
    class procedure EndDrag(const ADragImageList: TDragImageListResolution); override;
    class function HideDragImage(const ADragImageList: TDragImageListResolution;
      ALockedWindow: HWND; DoUnLock: Boolean): Boolean; override;
    class function ShowDragImage(const ADragImageList: TDragImageListResolution;
      ALockedWindow: HWND; X, Y: Integer; DoLock: Boolean): Boolean; override;}
  end;

  { TCDWSLazAccessibleObject }

  TCDWSLazAccessibleObject = class(TWSLazAccessibleObject)
  public
    class function CreateHandle(const AObject: TLazAccessibleObject): HWND; override;
    class procedure DestroyHandle(const AObject: TLazAccessibleObject); override;
    class procedure SetAccessibleDescription(const AObject: TLazAccessibleObject; const ADescription: string); override;
    class procedure SetAccessibleValue(const AObject: TLazAccessibleObject; const AValue: string); override;
    class procedure SetAccessibleRole(const AObject: TLazAccessibleObject; const ARole: TLazAccessibilityRole); override;
  end;

  { TCDWSControl }

  TCDWSControl = class(TWSControl)
  published
  end;

  { TCDWSWinControl }

  TCDWSWinControl = class(TWSWinControl)
  published
    //class procedure AddControl(const AControl: TControl); override;

    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;


    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
    class procedure SetChildZPosition(const AWinControl, AChild: TWinControl;
                                      const AOldPos, ANewPos: Integer;
                                      const AChildren: TFPList); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;

    class procedure ConstraintsChange(const AWinControl: TWinControl); override;
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure Invalidate(const AWinControl: TWinControl); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;

{    class function  CanFocus(const AWinControl: TWinControl): Boolean; override;
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure Invalidate(const AWinControl: TWinControl); override;
    class procedure AddControl(const AControl: TControl); override;
    class function  GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class function  GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class function GetDesignInteractive(const AWinControl: TWinControl; AClientPos: TPoint): Boolean; override;

    class procedure SetBiDiMode(const AWinControl: TWinControl; UseRightToLeftAlign, UseRightToLeftReading, UseRightToLeftScrollBar : Boolean); override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
    class procedure SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer); override;
    class procedure SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer); override;
    class procedure ShowHide(const AWinControl: TWinControl); override; //TODO: rename to SetVisible(control, visible)
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetCursor(const AWinControl: TWinControl; const ACursor: HCURSOR); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetShape(const AWinControl: TWinControl; const AShape: HBITMAP); override;

    class procedure GetPreferredSize(const AWinControl: TWinControl;
      var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;

    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;

    class procedure SetChildZPosition(const AWinControl, AChild: TWinControl;
                                      const AOldPos, ANewPos: Integer;
                                      const AChildren: TFPList); override;

    class procedure ConstraintsChange(const AWinControl: TWinControl); override;
    class procedure PaintTo(const AWinControl: TWinControl; ADC: HDC; X, Y: Integer); override;}
  end;

  { TCDWSGraphicControl }

  TCDWSGraphicControl = class(TWSGraphicControl)
  published
  end;

  { TCDWSCustomControl }

  TCDWSCustomControl = class(TWSCustomControl)
  published
//    class function CreateHandle(const AWinControl: TWinControl;
//          const AParams: TCreateParams): TLCLHandle; override;
  end;

  { TCDWSImageList }

  TCDWSImageList = class(TWSImageList)
  published
  end;

implementation

uses
  {$ifdef CD_Cocoa}
  customdrawn_cocoaproc,
  {$endif}
  customdrawnwsforms;

{ TCDWSLazAccessibleObject }

{$ifdef CD_Cocoa}
class function TCDWSLazAccessibleObject.CreateHandle(
  const AObject: TLazAccessibleObject): HWND;
begin
  Result := 0;
  if AObject = nil then Exit;

  // If this is a top-level window, then use the window Handle
  if AObject.OwnerControl is TCustomForm then
  begin
    Result := HWND(TCocoaWindow(TCustomForm(AObject.OwnerControl).Handle).CocoaForm);
    Exit;
  end;

  // Otherwise create a new handle
  Result := HWND(TCocoaAccessibleObject.alloc.init);
  TCocoaAccessibleObject(Result).LCLAcc := AObject;
  TCocoaAccessibleObject(Result).LCLControl := AObject.OwnerControl;
  TCocoaAccessibleObject(Result).LCLInjectedControl := nil;
end;

class procedure TCDWSLazAccessibleObject.DestroyHandle(
  const AObject: TLazAccessibleObject);
var
  lAccessibleHandle: TCocoaAccessibleObject;
begin
  if AObject.OwnerControl is TCustomForm then
    Exit;

  lAccessibleHandle := TCocoaAccessibleObject(AObject.Handle);
  lAccessibleHandle.release;
end;

class procedure TCDWSLazAccessibleObject.SetAccessibleDescription(
  const AObject: TLazAccessibleObject; const ADescription: string);
begin

end;

class procedure TCDWSLazAccessibleObject.SetAccessibleValue(
  const AObject: TLazAccessibleObject; const AValue: string);
begin

end;

class procedure TCDWSLazAccessibleObject.SetAccessibleRole(
  const AObject: TLazAccessibleObject; const ARole: TLazAccessibilityRole);
begin

end;
{$else}
class function TCDWSLazAccessibleObject.CreateHandle(
  const AObject: TLazAccessibleObject): HWND;
begin
  Result := 0;
end;

class procedure TCDWSLazAccessibleObject.DestroyHandle(
  const AObject: TLazAccessibleObject);
begin
end;

class procedure TCDWSLazAccessibleObject.SetAccessibleDescription(
  const AObject: TLazAccessibleObject; const ADescription: string);
begin

end;

class procedure TCDWSLazAccessibleObject.SetAccessibleValue(
  const AObject: TLazAccessibleObject; const AValue: string);
begin

end;

class procedure TCDWSLazAccessibleObject.SetAccessibleRole(
  const AObject: TLazAccessibleObject; const ARole: TLazAccessibilityRole);
begin

end;
{$endif}

class function  TCDWSWinControl.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
begin
  AText := '';
  Result := false;
end;

class procedure TCDWSWinControl.SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
begin
  RecreateWnd(AWinControl);
end;

class procedure TCDWSWinControl.SetChildZPosition(
  const AWinControl, AChild: TWinControl; const AOldPos, ANewPos: Integer;
  const AChildren: TFPList);
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetChildZPosition')
  then Exit;
  if not WSCheckHandleAllocated(AChild, 'SetChildZPosition (child)')
  then Exit;

{  if ANewPos = 0 // bottom
  then AfterWnd := HWND_BOTTOM
  else if ANewPos >= AChildren.Count - 1
  then AfterWnd := HWND_TOP
  else begin
    // Search for the first child above us with a handle
    // the child list is reversed form the windows order.
    // So the first window is the top window and is the last child
    // if we don't find a allocated handle then we are effectively not moved
    AfterWnd := 0;
    if AOldPos > ANewPos
    then StopPos := AOldPos              // The child is moved to the bottom, oldpos is on top of it
    else StopPos := AChildren.Count - 1; // the child is moved to the top

    for n := ANewPos + 1 to StopPos do
    begin
      Child := TWinControl(AChildren[n]);
      if Child.HandleAllocated
      then begin
        AfterWnd := Child.Handle;
        Break;
      end;
    end;

    if AfterWnd = 0 then Exit; // nothing to do
  end;
  Windows.SetWindowPos(AChild.Handle, AfterWnd, 0, 0, 0, 0,
    SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOOWNERZORDER or
    SWP_NOSIZE or SWP_NOSENDCHANGING);}
end;

{------------------------------------------------------------------------------
  Method:  SetBounds
  Params:  AWinControl                  - the object which invoked this function
           ALeft, ATop, AWidth, AHeight - new dimensions for the control
  Pre:     AWinControl.HandleAllocated
  Returns: Nothing

  Resize a window
 ------------------------------------------------------------------------------}
class procedure TCDWSWinControl.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
var
  lCDWinControl: TCDWinControl;
  lFormPos: TPoint;
begin
  //WriteLn(Format('[TCDWSWinControl.SetBounds Control=%s:%s x=%d y=%d w=%d h=%d',
  //  [AWinControl.Name, AWinControl.ClassName, ALeft, ATop, AWidth, AHeight]));
  lCDWinControl := TCDWinControl(AWinControl.Handle);
  { Region must be in form-relative coords because that is the space
    FindControlWhichReceivedEvent / Region.IsPointInRegion use, and the
    space CreateHandle initialised the region in (via
    FindControlPositionRelativeToTheForm). ALeft/ATop here are
    parent-relative; using them directly puts the region in the wrong
    space and breaks hit-testing for any control whose parent is not
    the form. }
  lFormPos := FindControlPositionRelativeToTheForm(AWinControl);
  lCDWinControl.Region.SetAsSimpleRectRegion(Bounds(lFormPos.X, lFormPos.Y, AWidth, AHeight));
  Invalidate(AWinControl);
end;

class procedure TCDWSWinControl.SetColor(const AWinControl: TWinControl);
begin
end;

class procedure TCDWSWinControl.SetFont(const AWinControl: TWinControl; const AFont: TFont);
begin
end;

class procedure TCDWSWinControl.SetText(const AWinControl: TWinControl; const AText: string);
begin
end;

class procedure TCDWSWinControl.ConstraintsChange(const AWinControl: TWinControl);
begin
end;

class function TCDWSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  lCDWinControl, lCDParent: TCDWinControl;
  lControlPosInForm: TPoint;
begin
  lCDWinControl := TCDWinControl.Create;
  lCDWinControl.WinControl := AWinControl;
  lCDWinControl.Region := TLazRegionWithChilds.Create;
  lCDWinControl.Region.UserData := AWinControl;
  lControlPosInForm := FindControlPositionRelativeToTheForm(AWinControl);
  lCDWinControl.Region.SetAsSimpleRectRegion(Bounds(lControlPosInForm.X, lControlPosInForm.Y, AParams.Width, AParams.Height));

  Result := HWND(lCDWinControl);

  // Adding on a form
  if AWinControl.Parent is TCustomForm then
  begin
    AddCDWinControlToForm(TCustomForm(AWinControl.Parent), lCDWinControl);
  end
  // Adding on another control
  else if AWinControl.Parent is TWinControl then
  begin
    lCDParent := TCDWinControl(AWinControl.Parent.Handle);
    if lCDParent.Children = nil then lCDParent.Children := TFPList.Create;
    lCDParent.Children.Add(lCDWinControl);
    lCDParent.Region.Childs.Add(lCDWinControl.Region);
  end;
end;

class procedure TCDWSWinControl.DestroyHandle(const AWinControl: TWinControl);
begin
end;

class procedure TCDWSWinControl.Invalidate(const AWinControl: TWinControl);
begin
  // lpRect = nil updates entire client area of window
  CDWidgetset.InvalidateRect(AWinControl.Handle, nil, true);
end;

{ Maintain region-tree consistency on visibility change. The region
  tree under TCDWinControl is purely geometric (TLazRegionWithChilds in
  lazregions.pas has no concept of LCL controls or visibility), and the
  hit-test descends it in geometric Z-order. If a control becomes
  hidden but its region stays in the parent's region tree, hit-tests
  on its (now-empty) bounds shadow visible siblings underneath -- so a
  click on a now-visible control whose bounds overlap with the bounds
  of a hidden sibling can be routed to the hidden one. So:
  on show, ensure the region is present in the parent's children list;
  on hide, take it out.

  The form-parented case is excluded: a form's handle is a TCDForm, not
  a TCDWinControl (they are sibling classes under TCDBaseControl), so
  TCDWinControl(form.Handle).Region would type-pun into TCDForm.LCLForm,
  and Region.Childs would land inside TComponent.FComponents, corrupting
  the form's owned-components list. CreateHandle already
  excludes this case by routing form-parented controls through
  AddCDWinControlToForm rather than parent.Region.Childs.Add. }
class procedure TCDWSWinControl.ShowHide(const AWinControl: TWinControl);
var
  lCDWinControl, lCDParent: TCDWinControl;
  idx: Integer;
begin
  if csDestroyingHandle in AWinControl.ControlState then Exit;
  if csDestroying in AWinControl.ComponentState then Exit;
  if not AWinControl.HandleAllocated then Exit;
  if not (AWinControl.Parent is TWinControl) then Exit;
  if AWinControl.Parent is TCustomForm then Exit;
  if csDestroying in AWinControl.Parent.ComponentState then Exit;
  if not AWinControl.Parent.HandleAllocated then Exit;
  lCDWinControl := TCDWinControl(AWinControl.Handle);
  lCDParent := TCDWinControl(AWinControl.Parent.Handle);
  if (lCDWinControl = nil) or (lCDParent = nil)
    or (lCDWinControl.Region = nil) or (lCDParent.Region = nil)
    or (lCDParent.Region.Childs = nil) then Exit;

  idx := lCDParent.Region.Childs.IndexOf(lCDWinControl.Region);
  if AWinControl.Visible then
  begin
    if idx < 0 then
      lCDParent.Region.Childs.Add(lCDWinControl.Region);
  end
  else
  begin
    if idx >= 0 then
      lCDParent.Region.Childs.Delete(idx);
  end;
end;

end.
