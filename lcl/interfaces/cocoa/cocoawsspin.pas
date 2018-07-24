{
 *****************************************************************************
 *                                CocoaWSSpin.pas                            *
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
unit CocoaWSSpin;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  // rtl+lcl
  Controls, Spin, LCLType,
  // widgetset
  WSSpin, WSLCLClasses,
  // cocoa ws
  CocoaPrivate, CocoaWSCommon, CocoaTextEdits;

type

  { TCocoaWSCustomFloatSpinEdit }

  TCocoaWSCustomFloatSpinEdit = class(TWSCustomFloatSpinEdit)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double; override;
    class procedure UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit); override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

implementation

procedure UpdateControlLCLToCocoa(src: TCustomFloatSpinEdit; dst: TCocoaSpinEdit);
begin
  dst.UpdateControl(src.MinValue, src.MaxValue, src.Increment, src.Value, src.DecimalPlaces);
end;

{ TCocoaWSCustomFloatSpinEdit }

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomFloatSpinEdit.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new spin edit in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCocoaWSCustomFloatSpinEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  lSpin: TCocoaSpinEdit;
begin
  lSpin := TCocoaSpinEdit.alloc.lclInitWithCreateParams(AParams);
  Result := TLCLIntfHandle(lSpin);
  if Result = 0 then Exit;
  lSpin.decimalPlaces := -1;
  lSpin.CreateSubcontrols(AParams);
  lSpin.callback := TLCLCommonCallback.Create(lSpin, AWinControl);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomFloatSpinEdit.GetValue
  Params:  ACustomFloatSpinEdit - LCL custom float spin edit
  Returns: The float spin edit value
 ------------------------------------------------------------------------------}
class function TCocoaWSCustomFloatSpinEdit.GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double;
var
  lSpin: TCocoaSpinEdit;
begin
  Result := 0;
  if ACustomFloatSpinEdit = nil then Exit;
  if not ACustomFloatSpinEdit.HandleAllocated then Exit;
  lSpin := TCocoaSpinEdit(ACustomFloatSpinEdit.Handle);

  Result := lSpin.Stepper.doubleValue();
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomFloatSpinEdit.UpdateControl
  Params:  ACustomFloatSpinEdit - LCL custom float spin edit
  
  Update the value, min, max and increment of custom float spin edit in Cocoa
  interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSCustomFloatSpinEdit.UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
var
  lSpin: TCocoaSpinEdit;
begin
  if ACustomFloatSpinEdit = nil then Exit;
  if not ACustomFloatSpinEdit.HandleAllocated then Exit;
  lSpin := TCocoaSpinEdit(ACustomFloatSpinEdit.Handle);
  UpdateControlLCLToCocoa(ACustomFloatSpinEdit, lSpin);
end;

class procedure TCocoaWSCustomFloatSpinEdit.SetBounds(
  const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer);
var
  lSpin: TCocoaSpinEdit;
  ACustomFloatSpinEdit: TCustomFloatSpinEdit absolute AWinControl;
begin
  if ACustomFloatSpinEdit = nil then Exit;
  if not ACustomFloatSpinEdit.HandleAllocated then Exit;
  lSpin := TCocoaSpinEdit(ACustomFloatSpinEdit.Handle);

  TCocoaWSWinControl.SetBounds(AWinControl, ALeft, ATop, AWidth, AHeight);
  lSpin.PositionSubcontrols(ALeft, ATop, AWidth, AHeight);
end;

end.
