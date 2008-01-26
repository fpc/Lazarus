{ $Id: wsdesigner.pp 11897 2007-09-01 02:46:24Z marc $}
{
 *****************************************************************************
 *                               WSDesigner.pp                                * 
 *                               ------------                                * 
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
unit WSDesigner;

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
  Classes, RubberBand,
  WsControls;

type
  { TWsCustomRubberBand }

  TWsCustomRubberBand = class(TWsWinControl)
    class procedure SetShape(ARubberBand: TCustomRubberBand; AShape: TRubberBandShape); virtual; overload;
  end;
  TWsCustomRubberBandClass = class of TWsCustomRubberBand;

implementation

{ TWsCustomRubberBand }

class procedure TWsCustomRubberBand.SetShape(ARubberBand: TCustomRubberBand;
  AShape: TRubberBandShape);
begin
end;

initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
// RegisterWSComponent(TCustomRubberBand, TWSCustomRubberBand);
////////////////////////////////////////////////////
end.
