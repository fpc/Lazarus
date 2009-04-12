{ $Id$}
{
 *****************************************************************************
 *                                 WSSpin.pp                                 * 
 *                                 ---------                                 * 
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
unit WSSpin;

{$mode objfpc}{$H+}

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
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Spin,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, WSStdCtrls, WSFactory;

type
  { TWSCustomFloatSpinEdit }

  TWSCustomFloatSpinEdit = class(TWSCustomEdit)
  published
    class function  GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): double; virtual;

(*  TODO: seperation into properties instead of bulk update
    class procedure SetIncrement(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewIncrement: Double); virtual;
    class procedure SetMinValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewValue: Double); virtual;
    class procedure SetMaxValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewValue: Double); virtual;
    class procedure SetValueEmpty(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewEmpty: boolean); virtual;
*)

    class procedure UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit); virtual;
  end;
  TWSCustomFloatSpinEditClass = class of TWSCustomFloatSpinEdit;

  { WidgetSetRegistration }

  procedure RegisterCustomFloatSpinEdit;

implementation

{ TWSCustomFloatSpinEdit }

class function TWSCustomFloatSpinEdit.GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): double;
begin
  Result := 0.0;
end;

class procedure TWSCustomFloatSpinEdit.UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
begin
end;

  { WidgetSetRegistration }

procedure RegisterCustomFloatSpinEdit;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterCustomFloatSpinEdit then
    RegisterWSComponent(TCustomFloatSpinEdit, TWSCustomFloatSpinEdit);
  Done := True;
end;

end.
