{ $Id$}
{
 *****************************************************************************
 *                               gtk2wsspin.pp                               * 
 *                               -------------                               * 
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
unit gtk2wsspin;

{$mode objfpc}{H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as litle as posible circles,
// Uncomment only when needed for registration
////////////////////////////////////////////////////
//  spin,
////////////////////////////////////////////////////
  wsspin, wslclclasses;

type

  { TGtk2WSCustomSpinEdit }

  TGtk2WSCustomSpinEdit = class(TWSCustomSpinEdit)
  private
  protected
  public
  end;

  { TGtk2WSSpinEdit }

  TGtk2WSSpinEdit = class(TWSSpinEdit)
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
//  RegisterWSComponent(TCustomSpinEdit, TGtk2WSCustomSpinEdit);
//  RegisterWSComponent(TSpinEdit, TGtk2WSSpinEdit);
////////////////////////////////////////////////////
end.
