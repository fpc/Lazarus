{ $Id$}
{
 *****************************************************************************
 *                             gtkwsfilectrl.pp                              * 
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
unit gtkwsfilectrl;

{$mode objfpc}{H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as litle as posible circles,
// Uncomment only when needed for registration
////////////////////////////////////////////////////
//  filectrl,
////////////////////////////////////////////////////
  wsfilectrl, wslclclasses;

type

  { TGtkWSCustomFileListBox }

  TGtkWSCustomFileListBox = class(TWSCustomFileListBox)
  private
  protected
  public
  end;

  { TGtkWSFileListBox }

  TGtkWSFileListBox = class(TWSFileListBox)
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
//  RegisterWSComponent(TCustomFileListBox, TGtkWSCustomFileListBox);
//  RegisterWSComponent(TFileListBox, TGtkWSFileListBox);
////////////////////////////////////////////////////
end.
