{ $Id$}
{
 *****************************************************************************
 *                                WSGrids.pp                                 * 
 *                                ----------                                 * 
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
unit WSGrids;

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
  Controls, LCLType, Grids,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, WSFactory;

type
  { TWSCustomGrid }

  TWSCustomGrid = class(TWSCustomControl)
  published
    class procedure SendCharToEditor(AEditor:TWinControl; Ch: TUTF8Char); virtual;
    class function InvalidateStartY(const FixedHeight, RowOffset: Integer): integer; virtual;
  end;
  TWSCustomGridClass = class of TWSCustomgrid;

  { WidgetSetRegistration }

  function RegisterCustomGrid: Boolean;

implementation
uses
  LCLIntf, LCLProc;

{ TWSCustomGrid }

class procedure TWSCustomGrid.SendCharToEditor(AEditor:TWinControl;
  Ch: TUTF8Char);
var
  GMsg: TGridMessage;
begin
  GMsg.LclMsg.Msg:=GM_SETVALUE;
  if Ch=#8 then // backspace
    GMsg.Value:=''
  else
    GMsg.Value:=Ch;
  AEditor.Dispatch(GMsg);
end;

class function TWSCustomGrid.InvalidateStartY(const FixedHeight, RowOffset: Integer): Integer;
begin
  result := FixedHeight;
end;

{ WidgetSetRegistration }

function RegisterCustomGrid: Boolean;
const
  Done: Boolean = False;
begin
  Result := False;
  if Done then exit;
  if not WSRegisterCustomGrid then
    RegisterWSComponent(TCustomGrid, TWSCustomGrid);
  Done := True;
  Result := True;
end;

end.
