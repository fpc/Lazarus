{ $Id$}
{
 *****************************************************************************
 *                                QtWSSpin.pp                                * 
 *                                -----------                                * 
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
unit QtWSSpin;

{$mode delphi}{$H+}

interface

uses
  // Bindings
{$ifdef USE_QT_4_2}
  qt42,
{$else}
  qt4,
{$endif}
  qtwidgets,
  // LCL
  Spin, SysUtils, Controls, Classes, LCLType, LCLProc, LCLIntf, Forms,
  // Widgetset
  WSSpin, WSLCLClasses;

type

  { TQtWSCustomFloatSpinEdit }

  TQtWSCustomFloatSpinEdit = class(TWSCustomFloatSpinEdit)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class function  GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): single; override;
    class procedure UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit); override;
(*
    class function  GetSelStart(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): integer; virtual;
    class function  GetSelLength(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): integer; virtual;

    class procedure SetSelStart(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewStart: integer); virtual;
    class procedure SetSelLength(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewLength: integer); virtual;

  TODO: seperation into properties instead of bulk update
    class procedure SetIncrement(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewIncrement: single); virtual;
    class procedure SetMinValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewValue: single); virtual;
    class procedure SetMaxValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewValue: single); virtual;
    class procedure SetValueEmpty(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewEmpty: boolean); virtual;
    *)

  end;

  { TQtWSFloatSpinEdit }

  TQtWSFloatSpinEdit = class(TWSFloatSpinEdit)
  private
  protected
  public
  end;


implementation

{ TQtWSCustomFloatSpinEdit }

{------------------------------------------------------------------------------
  Method: TQtWSCustomFloatSpinEdit.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomFloatSpinEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  QtSpinBox: TQtSpinBox;
  QtFloatSpinBox: TQtFloatSpinBox;
  Hook: QAbstractSpinBox_hookH;
  Method: TMethod;
  FIsFloat: Boolean;
begin

  {qt4 have two different QSpinBoxes, one is QSpinBox (integer), another is QDoubleSpinBox (double) }

  FIsFloat := TCustomFloatSpinEdit(AWinControl).DecimalPlaces > 0;
  
  if FIsFloat then
    QtFloatSpinBox := TQtFloatSpinBox.Create(AWinControl, AParams)
  else
    QtSpinBox := TQtSpinBox.Create(AWinControl, AParams);
  
  if FIsFloat then
  begin
    Hook := QAbstractSpinBox_hook_create(QtFloatSpinBox.Widget);
    TEventFilterMethod(Method) := QtFloatSpinBox.EventFilter;
    QObject_hook_hook_events(Hook, Method);
    {TODO: what TLMessage should be sended ? }
    QAbstractSpinBox_editingFinished_Event(Method) := QtFloatSpinBox.SignalEditingFinished;
    QAbstractSpinBox_hook_hook_editingFinished(QAbstractSpinBox_hook_create(QtFloatSpinBox.Widget), Method);

    QDoubleSpinBox_valueChanged_Event(Method) := QtFloatSpinBox.SignalValueChanged;
    QDoubleSpinBox_hook_hook_valueChanged(QDoubleSpinBox_hook_create(QtFloatSpinBox.Widget), Method);

  end else
  begin
    Hook := QAbstractSpinBox_hook_create(QtSpinBox.Widget);
    TEventFilterMethod(Method) := QtSpinBox.EventFilter;
    QObject_hook_hook_events(Hook, Method);
    {TODO: what TLMessage should be sended ?     }
    QAbstractSpinBox_editingFinished_Event(Method) := QtSpinBox.SignalEditingFinished;
    QAbstractSpinBox_hook_hook_editingFinished(QAbstractSpinBox_hook_create(QtSpinBox.Widget), Method);

    QSpinBox_valueChanged_Event(Method) := QtSpinBox.SignalValueChanged;
    QSpinBox_hook_hook_valueChanged(QSpinBox_hook_create(QtSpinBox.Widget), Method);
  end;
    
  
  if FIsFloat then
  Result := THandle(QtFloatSpinBox)
  else
  Result := THandle(QtSpinBox);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomFloatSpinEdit.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomFloatSpinEdit.DestroyHandle(const AWinControl: TWinControl);
begin

  if TCustomFloatSpinEdit(AWinControl).DecimalPlaces > 0 then
  TQtFloatSpinBox(AWinControl.Handle).Free
  else
  TQtSpinBox(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

class function  TQtWSCustomFloatSpinEdit.GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): single;
begin
  if ACustomFloatSpinEdit.DecimalPlaces > 0 then
  Result := QDoubleSpinBox_value(QDoubleSpinBoxH(TQtFloatSpinBox(ACustomFloatSpinEdit.Handle).Widget))
  else
  Result := QSpinBox_value(QSpinBoxH(TQtFloatSpinBox(ACustomFloatSpinEdit.Handle).Widget));
end;

class procedure TQtWSCustomFloatSpinEdit.UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
var
   QtSpinEdit: TQtSpinBox;
   QtFloatSpinEdit: TQtFloatSpinBox;
begin

  if ACustomFloatSpinEdit.DecimalPlaces > 0 then
  begin
    QtFloatSpinEdit := TQtFloatSpinBox(ACustomFloatSpinEdit.Handle);
    QDoubleSpinBox_setDecimals(QDoubleSpinBoxH(QtFloatSpinEdit.Widget), ACustomFloatSpinEdit.DecimalPlaces);
    QDoubleSpinBox_setValue(QDoubleSpinBoxH(QtFloatSpinEdit.Widget), ACustomFloatSpinEdit.Value);
    QDoubleSpinBox_setMinimum(QDoubleSpinBoxH(QtFloatSpinEdit.Widget), ACustomFloatSpinEdit.MinValue);
    QDoubleSpinBox_setMaximum(QDoubleSpinBoxH(QtFloatSpinEdit.Widget), ACustomFloatSpinEdit.MaxValue);
    QDoubleSpinBox_setSingleStep(QDoubleSpinBoxH(QtFloatSpinEdit.Widget), ACustomFloatSpinEdit.Increment);
  end else
  begin
    QtSpinEdit := TQtSpinBox(ACustomFloatSpinEdit.Handle);
    QSpinBox_setValue(QSpinBoxH(QtSpinEdit.Widget), Round(ACustomFloatSpinEdit.Value));
    QSpinBox_setMinimum(QSpinBoxH(QtSpinEdit.Widget), Round(ACustomFloatSpinEdit.MinValue));
    QSpinBox_setMaximum(QSpinBoxH(QtSpinEdit.Widget), Round(ACustomFloatSpinEdit.MaxValue));
    QSpinBox_setSingleStep(QSpinBoxH(QtSpinEdit.Widget), Round(ACustomFloatSpinEdit.Increment));
  end;
  
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomFloatSpinEdit, TQtWSCustomFloatSpinEdit);
//  RegisterWSComponent(TFloatSpinEdit, TQtWSFloatSpinEdit);
////////////////////////////////////////////////////
end.
