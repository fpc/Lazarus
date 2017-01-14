{
 *****************************************************************************
 *                              MuiWSControls.pp                              *
 *                              ---------------                              *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit MuiWSControls;

{$mode objfpc}{$H+}

interface

uses
  // FCL
  Classes, sysutils,
  // LCL
  Controls, LCLType,  Graphics, tagsparamshelper,
  // Widgetset
  MUIBaseUnit, WSControls, WSLCLClasses, Utility, Mui;

type

  { TMuiWSDragImageList }

  TMuiWSDragImageList = class(TWSDragImageList)
  private
  protected
  public
  end;

  { TMuiWSControl }

  TMuiWSControl = class(TWSControl)
  private
  protected
  public

  end;

  { TMuiWSWinControl }

  TMuiWSWinControl = class(TWSWinControl)
  private
  protected
  published

    class procedure AddControl(const AControl: TControl); override;
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure Invalidate(const AWinControl: TWinControl); override;
    class function  GetClientRect(const AWincontrol: TWinControl;
                             var ARect: TRect): Boolean; override;
    class function  GetDefaultClientRect(const AWinControl: TWinControl;
                             const aLeft, aTop, aWidth, aHeight: integer;
                             var aClientRect: TRect): Boolean; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                            var PreferredWidth, PreferredHeight: integer;
                            WithThemeSpace: Boolean); override;
    class procedure PaintTo(const AWinControl: TWinControl; ADC: HDC; X, Y: Integer); override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer); override;
    class procedure SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer); override;
    class procedure ShowHide(const AWinControl: TWinControl); override; //TODO: rename to SetVisible(control, visible)
    class procedure SetColor(const AWinControl: TWinControl); override;
    //class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    //class procedure SetText(const AWinControl: TWinControl; const AText: string); override;

{    class procedure AddControl(const AControl: TControl); override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;}

    //class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;

{    class procedure ConstraintsChange(const AWinControl: TWinControl); override;}
  end;

  { TMuiWSGraphicControl }

  TMuiWSGraphicControl = class(TWSGraphicControl)
  private
  protected
  published
  end;

  { TMuiWSCustomControl }

  TMuiWSCustomControl = class(TWSCustomControl)
  private
  protected
  public
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;

  { TMuiWSImageList }

  TMuiWSImageList = class(TWSImageList)
  private
  protected
  public
  end;


implementation


{ TMuiWSWinControl }


class procedure TMuiWSWinControl.AddControl(const AControl: TControl);
begin
  inherited;
  //writeln('AddControl ', AControl.classname);
end;

{------------------------------------------------------------------------------
  Method: TMuiWSWinControl.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TMuiWSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  MuiPanel: TMuiArea;
  TagList: TATagList;
begin
  //writeln(AWinControl.classname,' create');
  TagList.Clear;
  //TagList.AddTags([
  //  MUIA_FillArea, NativeUInt(False)
  //  ]);
  MuiPanel := TMuiArea.Create(LCLGroupClass, TagList);
  With MuiPanel do
  begin
    Left := AParams.X;
    Top := AParams.Y;
    Width := AParams.Width;
    Height := AParams.Height;
    PasObject := AWinControl;
    Caption := PChar(AParams.Caption);
  end;
  if AWinControl.Parent <> NIL then
  begin
    MuiPanel.Parent := TMuiObject(AWinControl.Parent.Handle);
  end;
  //
  Result := TLCLIntfHandle(MuiPanel);
end;

{------------------------------------------------------------------------------
  Method: TMuiWSWinControl.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TMuiWSWinControl.DestroyHandle(const AWinControl: TWinControl);
begin
  //TMuiPrivateWidget(AWinControl.Handle).Free;
  if Assigned(AWinControl) then
  begin
    if AWinControl.Handle <> 0 then
      TMuiObject(AWinControl.Handle).Free;
    AWinControl.Handle := 0;
  end;
end;

{------------------------------------------------------------------------------
  Method: TMuiWSWinControl.Invalidate
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TMuiWSWinControl.Invalidate(const AWinControl: TWinControl);
var
  Widget: TMuiObject;
begin
  Widget := TMuiObject(AWinControl.Handle);
  if Assigned(Widget) then
  begin
    if MUIApp.InsidePaint then
      MUIApp.AddInvalidatedObject(Widget)
    else
      Widget.DoMUIdraw;
  end;
end;

class function TMuiWSWinControl.GetClientRect(const AWincontrol: TWinControl;
  var ARect: TRect): Boolean;
begin
  ARect := TMuiObject(AWinControl.Handle).GetClientRect;
  Result:=True;
end;

class function TMuiWSWinControl.GetDefaultClientRect(
  const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer;
  var aClientRect: TRect): Boolean;
begin
  {AClientRect.top := ALeft;
  AClientRect.Left := ATop;
  AClientRect.Right := AWidth;
  AClientRect.Bottom := AHeight;}
  //writeln(AWincontrol.classname,' Get client Default Rect ',ALeft,', ', ATop,' -' , AWidth,' x ', AHeight);
  Result:=False;
end;

class procedure TMuiWSWinControl.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  if Assigned(AWinControl) then
  begin
    PreferredHeight:=20;//AWinControl.Height;
    PreferredWidth:=20;//AWinControl.Width;
  end;
  //writeln('Prefered Size',PreferredHeight,',',PreferredWidth);
end;

class procedure TMuiWSWinControl.PaintTo(const AWinControl: TWinControl;
  ADC: HDC; X, Y: Integer);
//var
//  AADC: TMuiDeviceContext absolute ADC;
begin
  inherited;
  //writeln('PaintTo: ', X,', ',y);
  //TMuiPrivateWidget(AWinControl.Handle).PaintTo(AADC.fpgCanvas,X,Y);

end;

{------------------------------------------------------------------------------
  Method: TMuiWSWinControl.SetBounds
  Params:  AWinControl - the calling object
           ALeft, ATop - Position
           AWidth, AHeight - Size
  Returns: Nothing

  Sets the position and size of a widget
 ------------------------------------------------------------------------------}
class procedure TMuiWSWinControl.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
var
  MuiObj: TMuiObject;
begin
  if not Assigned(AWincontrol) then
    Exit;
  if TObject(AWinControl.Handle) is TMuiObject then
  begin
    MuiObj := TMuiObject(AWinControl.Handle);
    MuiObj.BlockRedraw := True;
    MuiObj.Left := ALeft;
    MuiObj.Top := ATop;
    MuiObj.Width := AWidth;
    MuiObj.BlockRedraw := False;
    MuiObj.Height := AHeight;
  end;
end;

{------------------------------------------------------------------------------
  Method: TMuiWSWinControl.SetPos
  Params:  AWinControl - the calling object
           ALeft, ATop - Position
  Returns: Nothing

  Sets the position of a widget
 ------------------------------------------------------------------------------}
class procedure TMuiWSWinControl.SetPos(const AWinControl: TWinControl;
  const ALeft, ATop: Integer);
var
  MuiObj: TMuiObject;
begin
  if not Assigned(AWincontrol) then
    Exit;
  if TObject(AWinControl.Handle) is TMuiObject then
  begin
    MuiObj := TMuiObject(AWinControl.Handle);
    MuiObj.BlockRedraw := True;
    MuiObj.Left := ALeft;
    MuiObj.BlockRedraw := False;
    MuiObj.Top := ATop;
  end;
end;

{------------------------------------------------------------------------------
  Method: TMuiWSWinControl.SetSize
  Params:  AWinControl     - the calling object
           AWidth, AHeight - Size
  Returns: Nothing

  Sets the size of a widget
 ------------------------------------------------------------------------------}
class procedure TMuiWSWinControl.SetSize(const AWinControl: TWinControl;
  const AWidth, AHeight: Integer);
var
  MuiObj: TMuiObject;
begin
  if not Assigned(AWincontrol) then
    Exit;
  //writeln(AWincontrol.classname,' got resize: ', AWidth);
  if TObject(AWinControl.Handle) is TMuiObject then
  begin
    MuiObj := TMuiObject(AWinControl.Handle);
    MuiObj.BlockRedraw := True;
    MuiObj.Width := AWidth;
    MuiObj.BlockRedraw := False;
    MuiObj.Height := AHeight;
  end;
end;

{------------------------------------------------------------------------------
  Method: TMuiWSWinControl.ShowHide
  Params:  AWinControl     - the calling object
  Returns: Nothing

  Shows or hides a widget.
 ------------------------------------------------------------------------------}
class procedure TMuiWSWinControl.ShowHide(const AWinControl: TWinControl);
var
  MuiObject: TMUIObject;
begin
  if not Assigned(AWincontrol) then
    Exit;
  if TObject(AWinControl.Handle) is TMuiObject then
  begin
    MuiObject := TMuiObject(AWinControl.Handle);
    MuiObject.Visible := AWinControl.Visible;
  end;
end;

class procedure TMuiWSWinControl.SetColor(const AWinControl: TWinControl);
var
  MUIObject: TMUIArea;
begin
  if not Assigned(AWincontrol) then
    Exit;
  if TObject(AWinControl.Handle) is TMUIArea then
  begin
    MuiObject := TMUIArea(AWinControl.Handle);
    MuiObject.Color := AWinControl.Color;
  end;
end;

(*
class procedure TMuiWSWinControl.SetColor(const AWinControl: TWinControl);
//var
//  FPWidget: TfpgWidget;
begin
//  FPWidget := TMuiPrivateWidget(AWinControl.Handle).Widget;
//  FPWidget.BackgroundColor := TColorToTfpgColor(AWinControl.Color);
end;

class function TMuiWSWinControl.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
//var
//  FPPrivateWidget: TMuiPrivateWidget;
begin
//  FPPrivateWidget := TMuiPrivateWidget(AWinControl.Handle);
//  Result := FPPrivateWidget.HasStaticText;
//  if Result then AText := FPPrivateWidget.GetText;
end;

class procedure TMuiWSWinControl.SetText(const AWinControl: TWinControl;
  const AText: string);
//var
//  FPPrivateWidget: TMuiPrivateWidget;
begin
//  FPPrivateWidget := TMuiPrivateWidget(AWinControl.Handle);
//  FPPrivateWidget.SetText(AText);
end;

class procedure TMuiWSWinControl.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
//var
//  FPPrivateWidget: TMuiPrivateWindow;
begin
//  FPPrivateWidget := TMuiPrivateWindow(AWinControl.Handle);
//  FPPrivateWidget.Font:=AFont;
end; *)

class function  TMuiWSCustomControl.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
begin
  writeln('create Custom Control');
  Result := 0;
end;

class procedure TMuiWSCustomControl.DestroyHandle(const AWinControl: TWinControl);
begin
  writeln('Destroy Customcontrol');
end;

end.
