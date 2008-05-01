{ $Id: FpGuiwscontrols.pp 5319 2004-03-17 20:11:29Z marc $}
{
 *****************************************************************************
 *                              FpGuiWSControls.pp                              * 
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
unit FpGuiWSControls;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  fpguiwsprivate,
  // LCL
  Controls, LCLType,
  // Widgetset
  fpguiproc, WSControls, WSLCLClasses;

type

  { TFpGuiWSDragImageList }

  TFpGuiWSDragImageList = class(TWSDragImageList)
  private
  protected
  public
  end;

  { TFpGuiWSControl }

  TFpGuiWSControl = class(TWSControl)
  private
  protected
  public
  end;

  { TFpGuiWSWinControl }

  TFpGuiWSWinControl = class(TWSWinControl)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure Invalidate(const AWinControl: TWinControl); override;
  public
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer); override;
    class procedure SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer); override;
    class procedure ShowHide(const AWinControl: TWinControl); override; //TODO: rename to SetVisible(control, visible)
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetCursor(const AWinControl: TWinControl; const ACursor: HCursor); override;

    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;

{    class procedure AddControl(const AControl: TControl); override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;

    class procedure SetChildZPosition(const AWinControl, AChild: TWinControl;
                                      const AOldPos, ANewPos: Integer;
                                      const AChildren: TFPList); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;

    class procedure ConstraintsChange(const AWinControl: TWinControl); override;}
  end;

  { TFpGuiWSGraphicControl }

  TFpGuiWSGraphicControl = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TFpGuiWSCustomControl }

  TFpGuiWSCustomControl = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TFpGuiWSImageList }

  TFpGuiWSImageList = class(TWSImageList)
  private
  protected
  public
  end;


implementation

uses
  gfx_widget;

{ TFpGuiWSWinControl }

{------------------------------------------------------------------------------
  Method: TFpGuiWSWinControl.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TFpGuiWSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  {$ifdef VerboseFPGUI}
    WriteLn('TFpGuiWSWinControl.CreateHandle for ',AWinControl.Name);
  {$endif}

  Result := TLCLIntfHandle(TFPGUIPrivateWidget.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWSWinControl.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TFpGuiWSWinControl.DestroyHandle(const AWinControl: TWinControl);
begin
  TFPGUIPrivateWidget(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWSWinControl.Invalidate
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TFpGuiWSWinControl.Invalidate(const AWinControl: TWinControl);
var
  FPWidget: TfpgWidget;
begin
  FPWidget := TFPGUIPrivateWidget(AWinControl.Handle).Widget;
  FPWIdget.Invalidate;
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWSWinControl.SetBounds
  Params:  AWinControl - the calling object
           ALeft, ATop - Position
           AWidth, AHeight - Size
  Returns: Nothing

  Sets the position and size of a widget
 ------------------------------------------------------------------------------}
class procedure TFpGuiWSWinControl.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
var
  FPWidget: TfpgWidget;
begin
  FPWidget := TFPGUIPrivateWidget(AWinControl.Handle).Widget;
  FPWIdget.SetPosition(ALeft, ATop, AWidth, AHeight);
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWSWinControl.SetPos
  Params:  AWinControl - the calling object
           ALeft, ATop - Position
  Returns: Nothing

  Sets the position of a widget
 ------------------------------------------------------------------------------}
class procedure TFpGuiWSWinControl.SetPos(const AWinControl: TWinControl;
  const ALeft, ATop: Integer);
var
  FPWidget: TfpgWidget;
begin
  FPWidget := TFPGUIPrivateWidget(AWinControl.Handle).Widget;
  FPWIdget.SetPosition(ALeft, ATop, AWinControl.Width, AWinControl.Height);
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWSWinControl.SetSize
  Params:  AWinControl     - the calling object
           AWidth, AHeight - Size
  Returns: Nothing

  Sets the size of a widget
 ------------------------------------------------------------------------------}
class procedure TFpGuiWSWinControl.SetSize(const AWinControl: TWinControl;
  const AWidth, AHeight: Integer);
var
  FPWidget: TfpgWidget;
begin
  FPWidget := TFPGUIPrivateWidget(AWinControl.Handle).Widget;
  FPWIdget.SetPosition(AWinControl.Left, AWinControl.Top, AWidth, AHeight);
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWSWinControl.ShowHide
  Params:  AWinControl     - the calling object
  Returns: Nothing

  Shows or hides a widget.
 ------------------------------------------------------------------------------}
class procedure TFpGuiWSWinControl.ShowHide(const AWinControl: TWinControl);
var
  FPWidget: TfpgWidget;
begin
  FPWidget := TFPGUIPrivateWidget(AWinControl.Handle).Widget;
  FPWidget.Visible := not FPWidget.Visible;
end;

class procedure TFpGuiWSWinControl.SetColor(const AWinControl: TWinControl);
var
  FPWidget: TfpgWidget;
begin
  FPWidget := TFPGUIPrivateWidget(AWinControl.Handle).Widget;
  FPWidget.BackgroundColor := TColorToTfpgColor(AWinControl.Color);
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWSWinControl.SetCursor
  Params:  AWinControl     - the calling object
  Returns: Nothing

  Sets the cursor of the widget.
 ------------------------------------------------------------------------------}
class procedure TFpGuiWSWinControl.SetCursor(const AWinControl: TWinControl;
  const ACursor: HCursor);
begin

end;

class function TFpGuiWSWinControl.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  FPPrivateWidget: TFPGUIPrivateWidget;
begin
  FPPrivateWidget := TFPGUIPrivateWidget(AWinControl.Handle);
  Result := FPPrivateWidget.HasStaticText;
  if Result then AText := FPPrivateWidget.GetText;
end;

class procedure TFpGuiWSWinControl.SetText(const AWinControl: TWinControl;
  const AText: string);
var
  FPPrivateWidget: TFPGUIPrivateWidget;
begin
  FPPrivateWidget := TFPGUIPrivateWidget(AWinControl.Handle);
  FPPrivateWidget.SetText(AText);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TDragImageList, TFpGuiWSDragImageList);
//  RegisterWSComponent(TControl, TFpGuiWSControl);
  RegisterWSComponent(TWinControl, TFpGuiWSWinControl);
//  RegisterWSComponent(TGraphicControl, TFpGuiWSGraphicControl);
//  RegisterWSComponent(TCustomControl, TFpGuiWSCustomControl);
//  RegisterWSComponent(TImageList, TFpGuiWSImageList);
////////////////////////////////////////////////////
end.
