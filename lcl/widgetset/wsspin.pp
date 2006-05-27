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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  WSLCLClasses, WSControls;

type
  { TWSCustomFloatSpinEdit }

  TWSCustomFloatSpinEdit = class(TWSWinControl)
    class function  GetSelStart(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): integer; virtual;
    class function  GetSelLength(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): integer; virtual;
    class function  GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): single; virtual;

    class procedure SetSelStart(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewStart: integer); virtual;
    class procedure SetSelLength(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewLength: integer); virtual;
   
(*  TODO: seperation into properties instead of bulk update
    class procedure SetIncrement(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewIncrement: single); virtual;
    class procedure SetMinValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewValue: single); virtual;
    class procedure SetMaxValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewValue: single); virtual;
    class procedure SetValueEmpty(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewEmpty: boolean); virtual;
*)

    class procedure UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit); virtual;
  end;
  TWSCustomFloatSpinEditClass = class of TWSCustomFloatSpinEdit;

  { TWSFloatSpinEdit }

  TWSFloatSpinEdit = class(TWSCustomFloatSpinEdit)
  end;


implementation

{ TWSCustomFloatSpinEdit }

class function  TWSCustomFloatSpinEdit.GetSelStart(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): integer;
begin
  result := -1;
end;

class function  TWSCustomFloatSpinEdit.GetSelLength(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): integer;
begin
  result := 0;
end;

class function  TWSCustomFloatSpinEdit.GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): single;
begin
  result := 0.0;
end;

class procedure TWSCustomFloatSpinEdit.SetSelStart(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewStart: integer);
begin
end;

class procedure TWSCustomFloatSpinEdit.SetSelLength(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewLength: integer);
begin
end;

class procedure TWSCustomFloatSpinEdit.UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
begin
end;

initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomFloatSpinEdit, TWSCustomFloatSpinEdit);
//  RegisterWSComponent(TFloatSpinEdit, TWSFloatSpinEdit);
////////////////////////////////////////////////////
end.