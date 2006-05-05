{ $Id$}
{
 *****************************************************************************
 *                               Gtk2WSSpin.pp                               * 
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
unit Gtk2WSSpin;

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

  { TGtk2WSCustomFloatSpinEdit }

  TGtk2WSCustomFloatSpinEdit = class(TWSCustomFloatSpinEdit)
  private
  protected
  public
  end;

  { TGtk2WSFloatSpinEdit }

  TGtk2WSFloatSpinEdit = class(TWSFloatSpinEdit)
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
//  RegisterWSComponent(TCustomFloatSpinEdit, TGtk2WSCustomFloatSpinEdit);
//  RegisterWSComponent(TFloatSpinEdit, TGtk2WSFloatSpinEdit);
////////////////////////////////////////////////////
end.