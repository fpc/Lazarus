{ $Id$}
{
 *****************************************************************************
 *                              Win32WSArrow.pp                              * 
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
unit Win32WSArrow;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Arrow,
////////////////////////////////////////////////////
  WSArrow, WSLCLClasses;

type

  { TWin32WSArrow }

  TWin32WSArrow = class(TWSArrow)
  private
  protected
  public
    class procedure SetType(const AArrow: TArrow; const AArrowType: TArrowType; 
      const AShadowType: TShadowType); override;
  end;


implementation

{ TWin32WSArrow }

class procedure TWin32WSArrow.SetType(const AArrow: TArrow; const AArrowType: TArrowType;
  const AShadowType: TShadowType);
begin
  // TODO: implement me!
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TArrow, TWin32WSArrow);
////////////////////////////////////////////////////
end.
