{ $Id$}
{
 *****************************************************************************
 *                                QtWSSpin.pp                                * 
 *                                -----------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit QtWSSpin;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Bindings
  qt4,
  qtwidgets,
  // LCL
  Spin, SysUtils, Controls, Classes, LCLType, LCLProc, LCLIntf, Forms, StdCtrls,
  //RTL
  Math,
  // Widgetset
  WsProc, WSSpin, WSLCLClasses;

type

  { TQtWSCustomFloatSpinEdit }

  TQtWSCustomFloatSpinEdit = class(TWSCustomFloatSpinEdit)
  private
  protected
    class procedure InternalUpdateControl(const ASpinWidget: TQtAbstractSpinBox;
      const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit); override;

    class function GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double; override;
    class procedure SetAlignment(const ACustomEdit: TCustomEdit; const AAlignment: TAlignment); override;

  (*TODO: seperation into properties instead of bulk update
    class procedure SetIncrement(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewIncrement: Double); virtual;
    class procedure SetMinValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewValue: Double); virtual;
    class procedure SetMaxValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewValue: Double); virtual;
    class procedure SetValueEmpty(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewEmpty: boolean); virtual;
    *)

  end;

implementation

{ TQtWSCustomFloatSpinEdit }

class procedure TQtWSCustomFloatSpinEdit.InternalUpdateControl(
  const ASpinWidget: TQtAbstractSpinBox;
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
begin
  ASpinWidget.BeginUpdate;
  try
    if ASpinWidget is TQtFloatSpinBox then
      TQtFloatSpinBox(ASpinWidget).setDecimals(ACustomFloatSpinEdit.DecimalPlaces);
    if (ACustomFloatSpinEdit.MaxValue > ACustomFloatSpinEdit.MinValue) then
    begin
      ASpinWidget.setMinimum(ACustomFloatSpinEdit.MinValue);
      ASpinWidget.setMaximum(ACustomFloatSpinEdit.MaxValue);
    end else
    begin
      if ASpinWidget is TQtFloatSpinBox then
      begin
        ASpinWidget.setMinimum(-MaxDouble);
        ASpinWidget.setMaximum(MaxDouble);
      end else
      begin
        ASpinWidget.setMinimum(-MaxInt);
        ASpinWidget.setMaximum(MaxInt);
      end;
    end;
    ASpinWidget.setSingleStep(ACustomFloatSpinEdit.Increment);
  finally
    ASpinWidget.EndUpdate;
  end;
  // trigger OnChange of spin edits
  ASpinWidget.setValue(ACustomFloatSpinEdit.Value);
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

  QtSpinBox.setBorder(TCustomFloatSpinEdit(AWinControl).BorderStyle = bsSingle);
  InternalUpdateControl(QtSpinBox, TCustomFloatSpinEdit(AWinControl));

  QtSpinBox.AttachEvents;
  
  Result := TLCLIntfHandle(QtSpinBox);
end;

class function  TQtWSCustomFloatSpinEdit.GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double;
begin
  Result := TQtAbstractSpinBox(ACustomFloatSpinEdit.Handle).getValue;
end;

class procedure TQtWSCustomFloatSpinEdit.SetAlignment(
  const ACustomEdit: TCustomEdit; const AAlignment: TAlignment);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetAlignment') then
    Exit;
  TQtSpinBox(ACustomEdit.Handle).setAlignment(AlignmentMap[AAlignment]);
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

end.
