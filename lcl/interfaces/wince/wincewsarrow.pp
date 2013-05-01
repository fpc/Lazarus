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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit WinCEWSArrow;

{$mode objfpc}{$H+}

interface

uses
  // Libs
  Windows,
  // LCL
  Arrow, Graphics, win32compat,
  // Widgetset
  WSArrow, WSLCLClasses;

type

  { TWinCEWSArrow }

  TWinCEWSArrow = class(TWSArrow)
  published
    class procedure SetType(const AArrow: TArrow; const AArrowType: TArrowType; 
      const AShadowType: TShadowType); override;
  end;


implementation

{ TWinCEWSArrow }

class procedure TWinCEWSArrow.SetType(const AArrow: TArrow; const AArrowType: TArrowType;
  const AShadowType: TShadowType);
begin
  // TODO: implement me!
end;

end.
