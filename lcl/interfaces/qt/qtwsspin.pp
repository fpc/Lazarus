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
 *  See the file COPYING.LCL, included in this distribution,                 *
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

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  Spin,
////////////////////////////////////////////////////
  WSSpin, WSLCLClasses;

type

  { TQtWSCustomSpinEdit }

  TQtWSCustomSpinEdit = class(TWSCustomSpinEdit)
  private
  protected
  public
  end;

  { TQtWSSpinEdit }

  TQtWSSpinEdit = class(TWSSpinEdit)
  private
  protected
  public
  end;


implementation

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCustomSpinEdit, TQtWSCustomSpinEdit);
//  RegisterWSComponent(TSpinEdit, TQtWSSpinEdit);
////////////////////////////////////////////////////
end.
