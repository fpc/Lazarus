{ $Id$}
{
 *****************************************************************************
 *                             GtkWSControls.pp                              * 
 *                             ----------------                              * 
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
unit GtkWSControls;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  Controls,
////////////////////////////////////////////////////
  WSControls, WSLCLClasses;

type

  { TGtkWSDragImageList }

  TGtkWSDragImageList = class(TWSDragImageList)
  private
  protected
  public
  end;

  { TGtkWSControl }

  TGtkWSControl = class(TWSControl)
  private
  protected
  public
  end;

  { TGtkWSWinControl }

  TGtkWSWinControl = class(TWSWinControl)
  private
  protected
  public
  end;

  { TGtkWSGraphicControl }

  TGtkWSGraphicControl = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TGtkWSCustomControl }

  TGtkWSCustomControl = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TGtkWSImageList }

  TGtkWSImageList = class(TWSImageList)
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
//  RegisterWSComponent(TDragImageList, TGtkWSDragImageList);
//  RegisterWSComponent(TControl, TGtkWSControl);
//  RegisterWSComponent(TWinControl, TGtkWSWinControl);
//  RegisterWSComponent(TGraphicControl, TGtkWSGraphicControl);
//  RegisterWSComponent(TCustomControl, TGtkWSCustomControl);
//  RegisterWSComponent(TImageList, TGtkWSImageList);
////////////////////////////////////////////////////
end.
