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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  { TWSCustomSpinEdit }

  TWSCustomSpinEdit = class(TWSWinControl)
    class function  GetSelStart(const ACustomSpinEdit: TCustomSpinEdit): integer; virtual;
    class function  GetSelLength(const ACustomSpinEdit: TCustomSpinEdit): integer; virtual;
    class function  GetValue(const ACustomSpinEdit: TCustomSpinEdit): single; virtual;

    class procedure SetSelStart(const ACustomSpinEdit: TCustomSpinEdit; NewStart: integer); virtual;
    class procedure SetSelLength(const ACustomSpinEdit: TCustomSpinEdit; NewLength: integer); virtual;

    class procedure UpdateControl(const ACustomSpinEdit: TCustomSpinEdit); virtual;
  end;
  TWSCustomSpinEditClass = class of TWSCustomSpinEdit;

  { TWSSpinEdit }

  TWSSpinEdit = class(TWSCustomSpinEdit)
  end;


implementation

{ TWSCustomSpinEdit }

function  TWSCustomSpinEdit.GetSelStart(const ACustomSpinEdit: TCustomSpinEdit): integer;
begin
  result := -1;
end;

function  TWSCustomSpinEdit.GetSelLength(const ACustomSpinEdit: TCustomSpinEdit): integer;
begin
  result := 0;
end;

function  TWSCustomSpinEdit.GetValue(const ACustomSpinEdit: TCustomSpinEdit): single;
begin
  result := 0.0;
end;

procedure TWSCustomSpinEdit.SetSelStart(const ACustomSpinEdit: TCustomSpinEdit; NewStart: integer);
begin
end;

procedure TWSCustomSpinEdit.SetSelLength(const ACustomSpinEdit: TCustomSpinEdit; NewLength: integer);
begin
end;

procedure TWSCustomSpinEdit.UpdateControl(const ACustomSpinEdit: TCustomSpinEdit);
begin
end;

initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomSpinEdit, TWSCustomSpinEdit);
//  RegisterWSComponent(TSpinEdit, TWSSpinEdit);
////////////////////////////////////////////////////
end.
