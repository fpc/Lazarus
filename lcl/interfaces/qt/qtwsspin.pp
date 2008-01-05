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

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Bindings
{$ifdef USE_QT_4_3}
  qt43,
{$else}
  qt4,
{$endif}
  qtwidgets,
  // LCL
  Spin, SysUtils, Controls, Classes, LCLType, LCLProc, LCLIntf, Forms, StdCtrls,
  // Widgetset
  WsProc, WSSpin, WSLCLClasses;

type

  { TQtWSCustomFloatSpinEdit }

  TQtWSCustomFloatSpinEdit = class(TWSCustomFloatSpinEdit)
  private
  protected
    class procedure InternalUpdateControl(const ASpinWidget: TQtAbstractSpinBox;
      const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit); override;

    class function GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): single; override;

  (*TODO: seperation into properties instead of bulk update
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

class procedure TQtWSCustomFloatSpinEdit.InternalUpdateControl(
  const ASpinWidget: TQtAbstractSpinBox;
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
begin
  if ASpinWidget is TQtFloatSpinBox then
    TQtFloatSpinBox(ASpinWidget).setDecimals(ACustomFloatSpinEdit.DecimalPlaces);

  ASpinWidget.setValue(ACustomFloatSpinEdit.Value);
  ASpinWidget.setMinimum(ACustomFloatSpinEdit.MinValue);
  ASpinWidget.setMaximum(ACustomFloatSpinEdit.MaxValue);
  ASpinWidget.setSingleStep(ACustomFloatSpinEdit.Increment);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomFloatSpinEdit.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomFloatSpinEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtSpinBox: TQtAbstractSpinBox;
begin
  // qt4 has two different QSpinBoxes, one is QSpinBox (integer), another is QDoubleSpinBox (double)

  if TCustomFloatSpinEdit(AWinControl).DecimalPlaces > 0 then
    QtSpinBox := TQtFloatSpinBox.Create(AWinControl, AParams)
  else
    QtSpinBox := TQtSpinBox.Create(AWinControl, AParams);
  
  QtSpinBox.AttachEvents;
  
  InternalUpdateControl(QtSpinBox, TCustomFloatSpinEdit(AWinControl));
  
  Result := TLCLIntfHandle(QtSpinBox);
end;

class function  TQtWSCustomFloatSpinEdit.GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): single;
begin
  Result := TQtAbstractSpinBox(ACustomFloatSpinEdit.Handle).getValue;
end;

class procedure TQtWSCustomFloatSpinEdit.UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
var
  CurrentSpinWidget: TQtAbstractSpinBox;
begin
  if not WSCheckHandleAllocated(ACustomFloatSpinEdit, 'UpdateControl') then
    Exit;
    
  CurrentSpinWidget := TQtAbstractSpinBox(ACustomFloatSpinEdit.Handle);
  if ((ACustomFloatSpinEdit.DecimalPlaces > 0) and (CurrentSpinWidget is TQtSpinBox)) or
     ((ACustomFloatSpinEdit.DecimalPlaces = 0) and (CurrentSpinWidget is TQtFloatSpinBox)) then
       RecreateWnd(ACustomFloatSpinEdit)
  else
    InternalUpdateControl(CurrentSpinWidget, ACustomFloatSpinEdit);
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
