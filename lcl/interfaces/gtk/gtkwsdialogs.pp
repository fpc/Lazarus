{ $Id$}
{
 *****************************************************************************
 *                              GtkWSDialogs.pp                              * 
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
unit GtkWSDialogs;

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

  { TGtkWSCommonDialog }

  TGtkWSCommonDialog = class(TWSCommonDialog)
  private
  protected
  public
  end;

  { TGtkWSFileDialog }

  TGtkWSFileDialog = class(TWSFileDialog)
  private
  protected
  public
  end;

  { TGtkWSOpenDialog }

  TGtkWSOpenDialog = class(TWSOpenDialog)
  private
  protected
  public
  end;

  { TGtkWSSaveDialog }

  TGtkWSSaveDialog = class(TWSSaveDialog)
  private
  protected
  public
  end;

  { TGtkWSSelectDirectoryDialog }

  TGtkWSSelectDirectoryDialog = class(TWSSelectDirectoryDialog)
  private
  protected
  public
  end;

  { TGtkWSColorDialog }

  TGtkWSColorDialog = class(TWSColorDialog)
  private
  protected
  public
  end;

  { TGtkWSColorButton }

  TGtkWSColorButton = class(TWSColorButton)
  private
  protected
  public
  end;

  { TGtkWSFontDialog }

  TGtkWSFontDialog = class(TWSFontDialog)
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
//  RegisterWSComponent(TCommonDialog, TGtkWSCommonDialog);
//  RegisterWSComponent(TFileDialog, TGtkWSFileDialog);
//  RegisterWSComponent(TOpenDialog, TGtkWSOpenDialog);
//  RegisterWSComponent(TSaveDialog, TGtkWSSaveDialog);
//  RegisterWSComponent(TSelectDirectoryDialog, TGtkWSSelectDirectoryDialog);
//  RegisterWSComponent(TColorDialog, TGtkWSColorDialog);
//  RegisterWSComponent(TColorButton, TGtkWSColorButton);
//  RegisterWSComponent(TFontDialog, TGtkWSFontDialog);
////////////////////////////////////////////////////
end.
