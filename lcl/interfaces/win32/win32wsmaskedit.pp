{ $Id$}
{
 *****************************************************************************
 *                            win32wsmaskedit.pp                             * 
 *                            ------------------                             * 
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
unit win32wsmaskedit;

{$mode objfpc}{H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as litle as posible circles,
// Uncomment only when needed for registration
////////////////////////////////////////////////////
//  maskedit,
////////////////////////////////////////////////////
  wsmaskedit, wslclclasses;

type

  { TWin32WSCustomMaskEdit }

  TWin32WSCustomMaskEdit = class(TWSCustomMaskEdit)
  private
  protected
  public
  end;

  { TWin32WSMaskEdit }

  TWin32WSMaskEdit = class(TWSMaskEdit)
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
//  RegisterWSComponent(TCustomMaskEdit, TWin32WSCustomMaskEdit);
//  RegisterWSComponent(TMaskEdit, TWin32WSMaskEdit);
////////////////////////////////////////////////////
end.
