{ $Id$}
{
 *****************************************************************************
 *                              qtwsdialogs.pp                               * 
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
unit qtwsdialogs;

{$mode objfpc}{H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as litle as posible circles,
// Uncomment only when needed for registration
////////////////////////////////////////////////////
//  dialogs,
////////////////////////////////////////////////////
  wsdialogs, wslclclasses;

type

  { TQtWSCommonDialog }

  TQtWSCommonDialog = class(TWSCommonDialog)
  private
  protected
  public
  end;

  { TQtWSFileDialog }

  TQtWSFileDialog = class(TWSFileDialog)
  private
  protected
  public
  end;

  { TQtWSOpenDialog }

  TQtWSOpenDialog = class(TWSOpenDialog)
  private
  protected
  public
  end;

  { TQtWSSaveDialog }

  TQtWSSaveDialog = class(TWSSaveDialog)
  private
  protected
  public
  end;

  { TQtWSSelectDirectoryDialog }

  TQtWSSelectDirectoryDialog = class(TWSSelectDirectoryDialog)
  private
  protected
  public
  end;

  { TQtWSColorDialog }

  TQtWSColorDialog = class(TWSColorDialog)
  private
  protected
  public
  end;

  { TQtWSColorButton }

  TQtWSColorButton = class(TWSColorButton)
  private
  protected
  public
  end;

  { TQtWSFontDialog }

  TQtWSFontDialog = class(TWSFontDialog)
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
//  RegisterWSComponent(TCommonDialog, TQtWSCommonDialog);
//  RegisterWSComponent(TFileDialog, TQtWSFileDialog);
//  RegisterWSComponent(TOpenDialog, TQtWSOpenDialog);
//  RegisterWSComponent(TSaveDialog, TQtWSSaveDialog);
//  RegisterWSComponent(TSelectDirectoryDialog, TQtWSSelectDirectoryDialog);
//  RegisterWSComponent(TColorDialog, TQtWSColorDialog);
//  RegisterWSComponent(TColorButton, TQtWSColorButton);
//  RegisterWSComponent(TFontDialog, TQtWSFontDialog);
////////////////////////////////////////////////////
end.
