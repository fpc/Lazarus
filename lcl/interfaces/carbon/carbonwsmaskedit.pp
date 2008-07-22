{ $Id$}
{
 *****************************************************************************
 *                              CarbonWSMaskEdit.pp                              * 
 *                              ---------------                              * 
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
unit CarbonWSMaskEdit;

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

  { TCarbonWSCustomMaskEdit }

  TCarbonWSCustomMaskEdit = class(TWSCustomMaskEdit)
  private
  protected
  public
  end;

  { TCarbonWSMaskEdit }

  TCarbonWSMaskEdit = class(TWSMaskEdit)
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
//  RegisterWSComponent(TCustomMaskEdit, TCarbonWSCustomMaskEdit);
//  RegisterWSComponent(TMaskEdit, TCarbonWSMaskEdit);
////////////////////////////////////////////////////
end.