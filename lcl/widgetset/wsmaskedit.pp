{ $Id$}
{
 *****************************************************************************
 *                               WSMaskEdit.pp                               * 
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
unit WSMaskEdit;

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
  WSLCLClasses, WSStdCtrls;

type

  { TWSCustomMaskEdit }

  TWSCustomMaskEdit = class(TWSCustomEdit)
  private
  protected
  public
  end;

  { TWSMaskEdit }

  TWSMaskEdit = class(TWSCustomMaskEdit)
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
//  RegisterWSComponent(TCustomMaskEdit, TWSCustomMaskEdit);
//  RegisterWSComponent(TMaskEdit, TWSMaskEdit);
////////////////////////////////////////////////////
end.
