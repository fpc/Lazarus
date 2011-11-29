{
 *****************************************************************************
 *                           CustomDrawnWSArrow.pp                           *
 *                               ------------                                * 
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
unit CustomDrawnWSArrow;

{$mode objfpc}{$H+}

interface

//{$I qtdefines.inc}

uses
  Types,
  // LCL
  SysUtils, Controls, LCLType, LCLProc, Graphics, Arrow,
  // widgetset
  WSArrow, WSLCLClasses;

type

  { TCDWSArrow }

  // The default implementation is good enough, but we need to register this
  // class anyway because otherwise we get crashes
  TCDWSArrow = class(TWSArrow)
  published
    {class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetType(const AArrow: TArrow; const AArrowType: TArrowType;
      const AShadowType: TShadowType); override;
    class procedure DrawArrow(const AArrow: TArrow; const ACanvas: TCanvas);
       override;}
  end;


implementation

end.
