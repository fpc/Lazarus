{ $Id$}
{
 *****************************************************************************
 *                              gtkwsbuttons.pp                              * 
 *                              ---------------                              * 
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
unit gtkwsbuttons;

{$mode objfpc}{H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as litle as posible circles,
// Uncomment only when needed for registration
////////////////////////////////////////////////////
//  buttons,
////////////////////////////////////////////////////
  wsbuttons, wslclclasses;

type

  { TGtkWSButton }

  TGtkWSButton = class(TWSButton)
  private
  protected
  public
  end;

  { TGtkWSBitBtn }

  TGtkWSBitBtn = class(TWSBitBtn)
  private
  protected
  public
  end;

  { TGtkWSSpeedButton }

  TGtkWSSpeedButton = class(TWSSpeedButton)
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
//  RegisterWSComponent(TButton, TGtkWSButton);
//  RegisterWSComponent(TBitBtn, TGtkWSBitBtn);
//  RegisterWSComponent(TSpeedButton, TGtkWSSpeedButton);
////////////////////////////////////////////////////
end.
