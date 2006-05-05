{ $Id$}
{
 *****************************************************************************
 *                             GnomeWSDialogs.pp                             * 
 *                             -----------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit GnomeWSDialogs;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  Dialogs,
////////////////////////////////////////////////////
  WSDialogs, WSLCLClasses;

type

  { TGnomeWSCommonDialog }

  TGnomeWSCommonDialog = class(TWSCommonDialog)
  private
  protected
  public
  end;

  { TGnomeWSFileDialog }

  TGnomeWSFileDialog = class(TWSFileDialog)
  private
  protected
  public
  end;

  { TGnomeWSOpenDialog }

  TGnomeWSOpenDialog = class(TWSOpenDialog)
  private
  protected
  public
  end;

  { TGnomeWSSaveDialog }

  TGnomeWSSaveDialog = class(TWSSaveDialog)
  private
  protected
  public
  end;

  { TGnomeWSSelectDirectoryDialog }

  TGnomeWSSelectDirectoryDialog = class(TWSSelectDirectoryDialog)
  private
  protected
  public
  end;

  { TGnomeWSColorDialog }

  TGnomeWSColorDialog = class(TWSColorDialog)
  private
  protected
  public
  end;

  { TGnomeWSColorButton }

  TGnomeWSColorButton = class(TWSColorButton)
  private
  protected
  public
  end;

  { TGnomeWSFontDialog }

  TGnomeWSFontDialog = class(TWSFontDialog)
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
//  RegisterWSComponent(TCommonDialog, TGnomeWSCommonDialog);
//  RegisterWSComponent(TFileDialog, TGnomeWSFileDialog);
//  RegisterWSComponent(TOpenDialog, TGnomeWSOpenDialog);
//  RegisterWSComponent(TSaveDialog, TGnomeWSSaveDialog);
//  RegisterWSComponent(TSelectDirectoryDialog, TGnomeWSSelectDirectoryDialog);
//  RegisterWSComponent(TColorDialog, TGnomeWSColorDialog);
//  RegisterWSComponent(TColorButton, TGnomeWSColorButton);
//  RegisterWSComponent(TFontDialog, TGnomeWSFontDialog);
////////////////////////////////////////////////////
end.