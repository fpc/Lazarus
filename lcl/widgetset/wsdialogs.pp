{ $Id$}
{
 *****************************************************************************
 *                               wsdialogs.pp                                * 
 *                               ------------                                * 
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
unit wsdialogs;

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
  wslclclasses, wscontrols;

type

  { TWSCommonDialog }

  TWSCommonDialog = class(TWSLCLComponent)
  private
  protected
  public
  end;

  { TWSFileDialog }

  TWSFileDialog = class(TWSCommonDialog)
  private
  protected
  public
  end;

  { TWSOpenDialog }

  TWSOpenDialog = class(TWSFileDialog)
  private
  protected
  public
  end;

  { TWSSaveDialog }

  TWSSaveDialog = class(TWSOpenDialog)
  private
  protected
  public
  end;

  { TWSSelectDirectoryDialog }

  TWSSelectDirectoryDialog = class(TWSOpenDialog)
  private
  protected
  public
  end;

  { TWSColorDialog }

  TWSColorDialog = class(TWSCommonDialog)
  private
  protected
  public
  end;

  { TWSColorButton }

  TWSColorButton = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TWSFontDialog }

  TWSFontDialog = class(TWSCommonDialog)
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
//  RegisterWSComponent(TCommonDialog, TWSCommonDialog);
//  RegisterWSComponent(TFileDialog, TWSFileDialog);
//  RegisterWSComponent(TOpenDialog, TWSOpenDialog);
//  RegisterWSComponent(TSaveDialog, TWSSaveDialog);
//  RegisterWSComponent(TSelectDirectoryDialog, TWSSelectDirectoryDialog);
//  RegisterWSComponent(TColorDialog, TWSColorDialog);
//  RegisterWSComponent(TColorButton, TWSColorButton);
//  RegisterWSComponent(TFontDialog, TWSFontDialog);
////////////////////////////////////////////////////
end.
