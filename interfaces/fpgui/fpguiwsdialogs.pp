{ $Id: FpGuiwsdialogs.pp 5319 2004-03-17 20:11:29Z marc $}
{
 *****************************************************************************
 *                              FpGuiWSDialogs.pp                               * 
 *                              --------------                               * 
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
unit FpGuiWSDialogs;

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
  LCLType, WSDialogs, WSLCLClasses;

type

  { TFpGuiWSCommonDialog }

  TFpGuiWSCommonDialog = class(TWSCommonDialog)
  private
  protected
  public
  end;

  { TFpGuiWSFileDialog }

  TFpGuiWSFileDialog = class(TWSFileDialog)
  private
  protected
  public
  end;

  { TFpGuiWSOpenDialog }

  TFpGuiWSOpenDialog = class(TWSOpenDialog)
  private
  protected
  public
  end;

  { TFpGuiWSSaveDialog }

  TFpGuiWSSaveDialog = class(TWSSaveDialog)
  private
  protected
  public
  end;

  { TFpGuiWSSelectDirectoryDialog }

  TFpGuiWSSelectDirectoryDialog = class(TWSSelectDirectoryDialog)
  private
  protected
  public
  end;

  { TFpGuiWSColorDialog }

  TFpGuiWSColorDialog = class(TWSColorDialog)
  private
  protected
  public
  end;

  { TFpGuiWSColorButton }

  TFpGuiWSColorButton = class(TWSColorButton)
  private
  protected
  public
  end;

  { TFpGuiWSFontDialog }

  TFpGuiWSFontDialog = class(TWSFontDialog)
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
//  RegisterWSComponent(TCommonDialog, TFpGuiWSCommonDialog);
//  RegisterWSComponent(TFileDialog, TFpGuiWSFileDialog);
//  RegisterWSComponent(TOpenDialog, TFpGuiWSOpenDialog);
//  RegisterWSComponent(TSaveDialog, TFpGuiWSSaveDialog);
//  RegisterWSComponent(TSelectDirectoryDialog, TFpGuiWSSelectDirectoryDialog);
//  RegisterWSComponent(TColorDialog, TFpGuiWSColorDialog);
//  RegisterWSComponent(TColorButton, TFpGuiWSColorButton);
//  RegisterWSComponent(TFontDialog, TFpGuiWSFontDialog);
////////////////////////////////////////////////////
end.
