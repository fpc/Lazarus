{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSMaskEdit.pp                             * 
 *                             -----------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit Gtk2WSMaskEdit;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  MaskEdit,
////////////////////////////////////////////////////
  WSMaskEdit, WSLCLClasses;

type

  { TGtk2WSCustomMaskEdit }

  TGtk2WSCustomMaskEdit = class(TWSCustomMaskEdit)
  private
  protected
  public
  end;

  { TGtk2WSMaskEdit }

  TGtk2WSMaskEdit = class(TWSMaskEdit)
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
//  RegisterWSComponent(TCustomMaskEdit, TGtk2WSCustomMaskEdit);
//  RegisterWSComponent(TMaskEdit, TGtk2WSMaskEdit);
////////////////////////////////////////////////////
end.