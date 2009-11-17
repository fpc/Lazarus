{ $Id$}
{
 *****************************************************************************
 *                                CarbonWSSpin.pp                            * 
 *                                -----------                                * 
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
unit CarbonWSSpin;

{$mode objfpc}{$H+}

interface

// defines
{$I carbondefines.inc}

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Controls, Spin, StdCtrls, LCLType,
////////////////////////////////////////////////////
  WSSpin, WSLCLClasses;

type

  { TCarbonWSCustomFloatSpinEdit }

  TCarbonWSCustomFloatSpinEdit = class(TWSCustomFloatSpinEdit)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double; override;
    class procedure UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit); override;
  end;

implementation

uses
  CarbonEdits, CarbonDef, CarbonDbgConsts;

{ TCarbonWSCustomFloatSpinEdit }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomFloatSpinEdit.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new spin edit in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomFloatSpinEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonSpinEdit.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomFloatSpinEdit.GetValue
  Params:  ACustomFloatSpinEdit - LCL custom float spin edit
  Returns: The float spin edit value
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomFloatSpinEdit.GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double;
begin
  Result := 0;
  if not CheckHandle(ACustomFloatSpinEdit, Self, 'GetValue') then Exit;
  
  Result := TCarbonSpinEdit(ACustomFloatSpinEdit.Handle).Value;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomFloatSpinEdit.UpdateControl
  Params:  ACustomFloatSpinEdit - LCL custom float spin edit
  
  Update the value, min, max and increment of custom float spin edit in Carbon
  interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomFloatSpinEdit.UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
begin
  if not CheckHandle(ACustomFloatSpinEdit, Self, 'UpdateControl') then Exit;
  TCarbonSpinEdit(ACustomFloatSpinEdit.Handle).UpdateControl;
end;

end.
