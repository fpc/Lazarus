{
 *****************************************************************************
 *                         CustomDrawnWSControls.pp                          *
 *                              ---------------                              * 
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
unit CustomDrawnWSControls;

{$mode objfpc}{$H+}

interface

uses
  // LCL
  SysUtils, Classes, Types, Controls, LCLType, LCLProc, Forms, Graphics,
  // Widgetset
  InterfaceBase, WSProc, WSControls, WSLCLClasses;

type

  { TCDWSDragImageList }

  TCDWSDragImageList = class(TWSDragImageList)
  published
{    class function BeginDrag(const ADragImageList: TDragImageList; Window: HWND; AIndex, X, Y: Integer): Boolean; override;
    class function DragMove(const ADragImageList: TDragImageList; X, Y: Integer): Boolean; override;
    class procedure EndDrag(const ADragImageList: TDragImageList); override;
    class function HideDragImage(const ADragImageList: TDragImageList;
      ALockedWindow: HWND; DoUnLock: Boolean): Boolean; override;
    class function ShowDragImage(const ADragImageList: TDragImageList;
      ALockedWindow: HWND; X, Y: Integer; DoLock: Boolean): Boolean; override;}
  end;

  { TCDWSControl }

  TCDWSControl = class(TWSControl)
  published
  end;

  { TCDWSWinControl }

  TCDWSWinControl = class(TWSWinControl)
  published
{    class function  CanFocus(const AWinControl: TWinControl): Boolean; override;
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
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
//          const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCDWSImageList }

  TCDWSImageList = class(TWSImageList)
  published
  end;

implementation

(*{------------------------------------------------------------------------------
  Method: TCDWSCustomControl.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TCDWSCustomControl.CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle;
var
  CDCustomControl: TCDCustomControl;
begin
  {$ifdef VerboseCD}
    WriteLn('> TCDWSCustomControl.CreateHandle for ',dbgsname(AWinControl));
  {$endif}

  CDCustomControl := TCDCustomControl.Create(AWinControl, AParams);
  CDCustomControl.setFrameShape(TBorderStyleToCDFrameShapeMap[TCustomControl(AWinControl).BorderStyle]);
  CDCustomControl.viewportNeeded;
  CDCustomControl.AttachEvents;
  Result := TLCLIntfHandle(CDCustomControl);

  {$ifdef VerboseCD}
    WriteLn('< TCDWSCustomControl.CreateHandle for ',dbgsname(AWinControl),' Result: ', dbgHex(Result));
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: TCDWSWinControl.CanFocus
  Params:  TWinControl
  Returns: Boolean
 ------------------------------------------------------------------------------}
class function TCDWSWinControl.CanFocus(const AWinControl: TWinControl): Boolean;
var
  Widget: TCDWidget;
begin
  if AWinControl.HandleAllocated then
  begin
    Widget := TCDWidget(AWinControl.Handle);
    Result := (Widget.getFocusPolicy <> CDNoFocus);
  end else
    Result := False;
end;

{------------------------------------------------------------------------------
  Method: TCDWSWinControl.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TCDWSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  CDWidget: TCDWidget;
begin

  {$ifdef VerboseCD}
    WriteLn('> TCDWSWinControl.CreateHandle for ',dbgsname(AWinControl));
  {$endif}
  CDWidget := TCDWidget.Create(AWinControl, AParams);

  CDWidget.AttachEvents;

  // Finalization

  Result := TLCLIntfHandle(CDWidget);

  {$ifdef VerboseCD}
    WriteLn('< TCDWSWinControl.CreateHandle for ',dbgsname(AWinControl),' Result: ', dbgHex(Result));
  {$endif}
end;

{------------------------------------------------------------------------------
  Method: TCDWSWinControl.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSWinControl.DestroyHandle(const AWinControl: TWinControl);
begin
  TCDWidget(AWinControl.Handle).Release;
end;

{------------------------------------------------------------------------------
  Method: TCDWSWinControl.Invalidate
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSWinControl.Invalidate(const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWinControl, 'Invalidate') then
    Exit;

  TCDWidget(AWinControl.Handle).Update;
end;

class procedure TCDWSWinControl.AddControl(const AControl: TControl);
var
  Child: TCDWidget;
  Parent: TCDWidget;
begin
  if (AControl is TWinControl) and (TWinControl(AControl).HandleAllocated) then
  begin
    Child := TCDWidget(TWinControl(AControl).Handle);
    Parent := TCDWidget(AControl.Parent.Handle);
    if Child.getParent <> Parent.GetContainerWidget then
    begin
      Child.BeginUpdate;
      Child.setParent(Parent.GetContainerWidget);
      Child.EndUpdate;
    end;
  end;
end;

class function TCDWSWinControl.GetClientBounds(const AWincontrol: TWinControl;
  var ARect: TRect): Boolean;
begin
  Result := False;
  if not WSCheckHandleAllocated(AWinControl, 'GetClientBounds') then
    Exit;

  ARect := TCDWidget(AWinControl.Handle).getClientBounds;
  Result := True;
end;

class function TCDWSWinControl.GetClientRect(const AWincontrol: TWinControl;
  var ARect: TRect): Boolean;
begin
  Result := False;
  if not WSCheckHandleAllocated(AWinControl, 'GetClientRect') then
    Exit;
    
  ARect := TCDWidget(AWinControl.Handle).getClientBounds;
  OffsetRect(ARect, -ARect.Left, -ARect.Top);
  Result := True;
end;

class function TCDWSWinControl.GetDesignInteractive(
  const AWinControl: TWinControl; AClientPos: TPoint): Boolean;
begin
  Result := False;
  if not WSCheckHandleAllocated(AWinControl, 'GetDesignInteractive') then
    Exit;
end;

class procedure TCDWSWinControl.SetBiDiMode(const AWinControl : TWinControl;
  UseRightToLeftAlign, UseRightToLeftReading, UseRightToLeftScrollBar : Boolean
  );
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetBiDiMode') then
    Exit;

  TCDWidget(AWinControl.Handle).setLayoutDirection(TLayoutDirectionMap[UseRightToLeftAlign]);
end;

class procedure TCDWSWinControl.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  if AWinControl.HandleAllocated then
    TCDWidget(AWinControl.Handle).PreferredSize(PreferredWidth,
      PreferredHeight, WithThemeSpace);
end;

class function TCDWSWinControl.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
begin
  Result := False;
  if not WSCheckHandleAllocated(AWincontrol, 'GetText') then
    Exit;

  Result := not TCDWidget(AWinControl.Handle).getTextStatic;
  if Result then
    AText := UTF16ToUTF8(TCDWidget(AWinControl.Handle).getText);
end;

class procedure TCDWSWinControl.SetText(const AWinControl: TWinControl;
  const AText: string);
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetText') then
    Exit;
  TCDWidget(AWinControl.Handle).BeginUpdate;
  TCDWidget(AWinControl.Handle).setText(GetUtf8String(AText));
  TCDWidget(AWinControl.Handle).EndUpdate;
end;

class procedure TCDWSWinControl.SetChildZPosition(const AWinControl,
                AChild: TWinControl; const AOldPos, ANewPos: Integer; const AChildren: TFPList);
var
  n: Integer;
  Child: TWinControl;
  Reorder: TFPList;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetChildZPosition') then
    Exit;
  if not WSCheckHandleAllocated(AChild, 'SetChildZPosition (child)') then
    Exit;

  if (ANewPos <= 0) or (ANewPos >= AChildren.Count - 1) then
  begin
    // simple
    if ANewPos <= 0 then // bottom
      TCDWidget(AChild.Handle).lowerWidget
    else
      TCDWidget(AChild.Handle).raiseWidget;
  end else
  begin
    if (ANewPos >= 0) and (ANewPos < AChildren.Count -1) then
    begin
      Reorder := TFPList.Create;
      for n := AChildren.Count - 1 downto 0 do
        Reorder.Add(AChildren[n]);
      Child := TWinControl(Reorder[ANewPos + 1]);
      if Child.HandleAllocated then
        TCDWidget(AChild.Handle).stackUnder(TCDWidget(Child.Handle).Widget)
      else
        TCDWidget(AChild.Handle).lowerWidget;
      Reorder.Free;
    end;
  end;
end;

class procedure TCDWSWinControl.ConstraintsChange(const AWinControl: TWinControl);
const
  CDMaxContraint = $FFFFFF;
var
  Widget: TCDWidget;
  MaxW, MaxH: Integer;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'ConstraintsChange') then
    Exit;
    
  Widget := TCDWidget(AWinControl.Handle);
  with AWinControl do
  begin
    Widget.setMinimumSize(Constraints.MinWidth, Constraints.MinHeight);
    if Constraints.MaxWidth = 0 then
      MaxW := CDMaxContraint
    else
      MaxW := Constraints.MaxWidth;
    if Constraints.MaxHeight = 0 then
      MaxH := CDMaxContraint
    else
      MaxH := Constraints.MaxHeight;
    Widget.setMaximumSize(MaxW, MaxH);
  end;
end;

class procedure TCDWSWinControl.PaintTo(const AWinControl: TWinControl;
  ADC: HDC; X, Y: Integer);
var
  Context: TCDDeviceContext absolute ADC;
  Widget: TCDWidget;
  Pixmap: TCDPixmap;
  DCSize: TSize;
  APoint: TCDPoint;
  ARect, GRect: TRect;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'PaintTo') or (ADC = 0) then
    Exit;

  Widget := TCDWidget(AWinControl.Handle);
  ARect := Widget.getFrameGeometry;
  GRect := Widget.getGeometry;
  with DCSize, ARect do
  begin
    cx := Right - Left;
    cy := Bottom - Top;
  end;
  Pixmap := TCDPixmap.Create(@DCSize);
  OffsetRect(GRect, -ARect.Left, -ARect.Top);
  Pixmap.grabWidget(Widget.Widget, 0, 0);
  APoint := CDPoint(X + GRect.Left, Y + GRect.Top);
  ARect := Rect(0, 0, Pixmap.getWidth, Pixmap.getHeight);
  Context.drawPixmap(@APoint, Pixmap.Handle, @ARect);
  Pixmap.Free;
end;

{------------------------------------------------------------------------------
  Method: TCDWSWinControl.SetBounds
  Params:  AWinControl - the calling object
           ALeft, ATop - Position
           AWidth, AHeight - Size
  Returns: Nothing

  Sets the position and size of a widget
 ------------------------------------------------------------------------------}
class procedure TCDWSWinControl.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
var
  R: TRect;
  Box: TCDWidget;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetBounds') then
    Exit;
  R := Rect(ALeft, ATop, AWidth, AHeight);

  Box := nil;
  if Assigned(AWinControl.Parent) and
    AWinControl.Parent.HandleAllocated then
      Box := TCDWidget(AWinControl.Parent.Handle);

  if Assigned(Box) and
    (Box.ChildOfComplexWidget = ccwScrollingWinControl) then
  begin
    R := Rect(ALeft - TCDCustomControl(Box).horizontalScrollBar.getValue,
      ATop - TCDCustomControl(Box).verticalScrollBar.getValue, AWidth, AHeight);
  end;

  TCDWidget(AWinControl.Handle).BeginUpdate;
  with R do
  begin
    TCDWidget(AWinControl.Handle).move(Left, Top);
    TCDWidget(AWinControl.Handle).resize(Right, Bottom);
  end;
  TCDWidget(AWinControl.Handle).EndUpdate;
end;

{------------------------------------------------------------------------------
  Method: TCDWSWinControl.SetPos
  Params:  AWinControl - the calling object
           ALeft, ATop - Position
  Returns: Nothing

  Sets the position of a widget
 ------------------------------------------------------------------------------}
class procedure TCDWSWinControl.SetPos(const AWinControl: TWinControl;
  const ALeft, ATop: Integer);
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetPos') then
    Exit;

  TCDWidget(AWinControl.Handle).BeginUpdate;
  TCDWidget(AWinControl.Handle).move(ALeft, ATop);
  TCDWidget(AWinControl.Handle).EndUpdate;
end;

{------------------------------------------------------------------------------
  Method: TCDWSWinControl.SetSize
  Params:  AWinControl     - the calling object
           AWidth, AHeight - Size
  Returns: Nothing

  Sets the size of a widget
 ------------------------------------------------------------------------------}
class procedure TCDWSWinControl.SetSize(const AWinControl: TWinControl;
  const AWidth, AHeight: Integer);
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetSize') then
    Exit;
  TCDWidget(AWinControl.Handle).BeginUpdate;
  TCDWidget(AWinControl.Handle).resize(AWidth, AHeight);
  TCDWidget(AWinControl.Handle).EndUpdate;
end;

{------------------------------------------------------------------------------
  Method: TCDWSWinControl.ShowHide
  Params:  AWinControl     - the calling object

  Returns: Nothing

  Shows or hides a widget.
 ------------------------------------------------------------------------------}
class procedure TCDWSWinControl.ShowHide(const AWinControl: TWinControl);
var
  Widget: TCDWidget;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'ShowHide') then
    Exit;

  Widget := TCDWidget(AWinControl.Handle);

  Widget.BeginUpdate;
  Widget.setVisible(AWinControl.HandleObjectShouldBeVisible);
  Widget.EndUpdate;
end;

{------------------------------------------------------------------------------
  Method: TCDWSWinControl.SetColor
  Params:  AWinControl     - the calling object
  Returns: Nothing

  Sets the color of the widget.
 ------------------------------------------------------------------------------}
class procedure TCDWSWinControl.SetColor(const AWinControl: TWinControl);
var
  QColor: TQColor;
  ColorRef: TColorRef;
  CDWidget: TCDWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor') then
    Exit;

  CDWidget := TCDWidget(AWinControl.Handle);
  CDWidget.BeginUpdate;
  CDWidget.WidgetState := CDWidget.WidgetState + [CDwsColorUpdating];
  try
    // Get the color numeric value (system colors are mapped to numeric colors depending on the widget style)
    if AWinControl.Color = clDefault then
      CDWidget.SetDefaultColor(dctBrush)
    else
    begin
      ColorRef := ColorToRGB(AWinControl.Color);

      // Fill QColor
      QColor_fromRgb(@QColor,Red(ColorRef),Green(ColorRef),Blue(ColorRef));

      // Set color of the widget to QColor
      CDWidget.SetColor(@QColor);
    end;
  finally
    CDWidget.WidgetState := CDWidget.WidgetState - [CDwsColorUpdating];
    CDWidget.EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCDWSWinControl.SetCursor
  Params:  AWinControl     - the calling object
  Returns: Nothing

  Sets the cursor of the widget.
 ------------------------------------------------------------------------------}
class procedure TCDWSWinControl.SetCursor(const AWinControl: TWinControl; const ACursor: HCURSOR);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetCursor') then
    Exit;
  if ACursor <> 0 then
    TCDWidget(AWinControl.Handle).SetCursor(TCDCursor(ACursor).Handle)
  else
    TCDWidget(AWinControl.Handle).SetCursor(nil);
end;

{------------------------------------------------------------------------------
  Method: TCDWSWinControl.SetFont
  Params:  AWinControl - the calling object, AFont - object font
  Returns: Nothing

  Sets the font of the widget.
 ------------------------------------------------------------------------------}
class procedure TCDWSWinControl.SetFont(const AWinControl: TWinControl; const AFont: TFont);
var
  CDWidget: TCDWidget;
  QColor: TQColor;
  ColorRef: TColorRef;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetFont') then
    Exit;

  CDWidget := TCDWidget(AWinControl.Handle);
  CDWidget.BeginUpdate;
  CDWidget.WidgetState := CDWidget.WidgetState + [CDwsFontUpdating];
  try
    CDWidget.SetLCLFont(TCDFont(AFont.Reference.Handle));
    CDWidget.setFont(TCDFont(AFont.Reference.Handle).FHandle);

    // tscrollbar, ttrackbar etc.
    if not CDWidget.CanChangeFontColor then
    begin
      with CDWidget do
      begin
        Palette.ForceColor := True;
        setDefaultColor(dctFont);
        Palette.ForceColor := False;
      end;
      exit;
    end;

    if AFont.Color = clDefault then
      CDWidget.SetDefaultColor(dctFont)
    else
    begin
      ColorRef := ColorToRGB(AFont.Color);
      QColor_fromRgb(@QColor,Red(ColorRef),Green(ColorRef),Blue(ColorRef));
      CDWidget.SetTextColor(@QColor);
    end;
  finally
    CDWidget.WidgetState := CDWidget.WidgetState - [CDwsFontUpdating];
    CDWidget.EndUpdate;
  end;
end;

class procedure TCDWSWinControl.SetShape(const AWinControl: TWinControl;
  const AShape: HBITMAP);
var
  Widget: TCDWidget;
  Shape: TCDImage;
  AMask: QBitmapH;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetShape') then
    Exit;
  Widget := TCDWidget(AWinControl.Handle);

  if AShape <> 0 then
  begin
    Shape := TCDImage(AShape);
    // invert white/black
    Shape.invertPixels;
    AMask := Shape.AsBitmap;
    Widget.setMask(AMask);
    QBitmap_destroy(AMask);
    // invert back
    Shape.invertPixels;
  end
  else
    Widget.clearMask;
end;

class procedure TCDWSWinControl.SetBorderStyle(const AWinControl: TWinControl;
  const ABorderStyle: TBorderStyle);
var
  Widget: TCDWidget;
  CDEdit: ICDEdit;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetBorderStyle') then
    Exit;
    
  Widget := TCDWidget(AWinControl.Handle);
  CDEdit := nil;
  if Widget is TCDFrame then
    TCDFrame(Widget).setFrameShape(TBorderStyleToCDFrameShapeMap[ABorderStyle])
  else
  if Supports(Widget, ICDEdit, CDEdit) then
    CDEdit.setBorder(ABorderStyle = bsSingle);
end;

{ TCDWSDragImageList }

class function TCDWSDragImageList.BeginDrag(
  const ADragImageList: TDragImageList; Window: HWND; AIndex, X, Y: Integer): Boolean;
var
  ABitmap: TBitmap;
begin
  ABitmap := TBitmap.Create;
  ADragImageList.GetBitmap(AIndex, ABitmap);

  if (ABitmap.Handle = 0) or (ABitmap.Width = 0) or (ABitmap.Height = 0) then
  begin
    Result := False;
    Exit;
  end;

  Result := TCDWidgetset(Widgetset).DragImageList_BeginDrag(
    TCDImage(ABitmap.Handle).FHandle, ADragImageList.DragHotSpot);
  if Result then
    TCDWidgetset(Widgetset).DragImageList_DragMove(X, Y);
  ABitmap.Free;
end;

class function TCDWSDragImageList.DragMove(
  const ADragImageList: TDragImageList; X, Y: Integer): Boolean;
begin
  Result := TCDWidgetset(Widgetset).DragImageList_DragMove(X, Y);
end;

class procedure TCDWSDragImageList.EndDrag(const ADragImageList: TDragImageList);
begin
  TCDWidgetset(Widgetset).DragImageList_EndDrag;
end;

class function TCDWSDragImageList.HideDragImage(
  const ADragImageList: TDragImageList; ALockedWindow: HWND; DoUnLock: Boolean
  ): Boolean;
begin
  Result := True;
  if DoUnlock then
  begin
    TCDWidgetset(Widgetset).DragImageLock := False;
    Result := TCDWidgetset(Widgetset).DragImageList_SetVisible(False);
  end;
end;

class function TCDWSDragImageList.ShowDragImage(
  const ADragImageList: TDragImageList; ALockedWindow: HWND; X, Y: Integer;
  DoLock: Boolean): Boolean;
begin
  Result := TCDWidgetset(Widgetset).DragImageLock;
  if not DoLock then
  begin
    if not Result then
      Result := TCDWidgetset(Widgetset).DragImageList_SetVisible(True);
  end else
  begin
    TCDWidgetset(Widgetset).DragImageLock := True;
    Result := TCDWidgetset(Widgetset).DragImageList_DragMove(X, Y) and
      TCDWidgetset(Widgetset).DragImageList_SetVisible(True);
  end;
end;*)

end.
