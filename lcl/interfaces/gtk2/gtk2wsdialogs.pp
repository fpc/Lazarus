{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSDialogs.pp                              * 
 *                             ----------------                              * 
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
unit Gtk2WSDialogs;

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

  { TGtk2WSCommonDialog }

  TGtk2WSCommonDialog = class(TWSCommonDialog)
  private
  protected
  public
  end;

  { TGtk2WSFileDialog }

  TGtk2WSFileDialog = class(TWSFileDialog)
  private
  protected
  public
  end;

  { TGtk2WSOpenDialog }

  TGtk2WSOpenDialog = class(TWSOpenDialog)
  private
  protected
  public
  end;

  { TGtk2WSSaveDialog }

  TGtk2WSSaveDialog = class(TWSSaveDialog)
  private
  protected
  public
  end;

  { TGtk2WSSelectDirectoryDialog }

  TGtk2WSSelectDirectoryDialog = class(TWSSelectDirectoryDialog)
  private
  protected
  public
  end;

  { TGtk2WSColorDialog }

  TGtk2WSColorDialog = class(TWSColorDialog)
  private
  protected
  public
  end;

  { TGtk2WSColorButton }

  TGtk2WSColorButton = class(TWSColorButton)
  private
  protected
  public
  end;

  { TGtk2WSFontDialog }

  TGtk2WSFontDialog = class(TWSFontDialog)
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
//  RegisterWSComponent(TCommonDialog, TGtk2WSCommonDialog);
//  RegisterWSComponent(TFileDialog, TGtk2WSFileDialog);
//  RegisterWSComponent(TOpenDialog, TGtk2WSOpenDialog);
//  RegisterWSComponent(TSaveDialog, TGtk2WSSaveDialog);
//  RegisterWSComponent(TSelectDirectoryDialog, TGtk2WSSelectDirectoryDialog);
//  RegisterWSComponent(TColorDialog, TGtk2WSColorDialog);
//  RegisterWSComponent(TColorButton, TGtk2WSColorButton);
//  RegisterWSComponent(TFontDialog, TGtk2WSFontDialog);
////////////////////////////////////////////////////
end.