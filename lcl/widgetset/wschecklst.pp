{ $Id$}
{
 *****************************************************************************
 *                               WSCheckLst.pp                               * 
 *                               -------------                               * 
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
unit WSCheckLst;

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
  CheckLst,
////////////////////////////////////////////////////
  WSLCLClasses, WSStdCtrls;

type
  { TWSCheckListBox }

  TWSCustomCheckListBox = class(TWSCustomListBox)
    class function  GetChecked(const ACheckListBox: TCustomCheckListBox; 
      const AIndex: integer): boolean; virtual;
    class procedure SetChecked(const ACheckListBox: TCustomCheckListBox; 
      const AIndex: integer; const AChecked: boolean); virtual;
  end;
  TWSCustomCheckListBoxClass = class of TWSCustomCheckListBox;


implementation

class function  TWSCustomCheckListBox.GetChecked(const ACheckListBox: TCustomCheckListBox;
  const AIndex: integer): boolean;
begin
  Result := false;
end;

class procedure TWSCustomCheckListBox.SetChecked(const ACheckListBox: TCustomCheckListBox;
  const AIndex: integer; const AChecked: boolean);
begin
end;

initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCustomCheckListBox, TWSCustomCheckListBox);
////////////////////////////////////////////////////
end.