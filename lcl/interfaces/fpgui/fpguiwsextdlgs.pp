{ $Id: FpGuiwsextdlgs.pp 5319 2004-03-17 20:11:29Z marc $}
{
 *****************************************************************************
 *                              FpGuiWSExtDlgs.pp                               * 
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
unit FpGuiWSExtDlgs;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  ExtDlgs,
////////////////////////////////////////////////////
  WSExtDlgs, WSLCLClasses;

type

  { TFpGuiWSPreviewFileControl }

  TFpGuiWSPreviewFileControl = class(TWSPreviewFileControl)
  private
  protected
  public
  end;

  { TFpGuiWSPreviewFileDialog }

  TFpGuiWSPreviewFileDialog = class(TWSPreviewFileDialog)
  private
  protected
  public
  end;

  { TFpGuiWSOpenPictureDialog }

  TFpGuiWSOpenPictureDialog = class(TWSOpenPictureDialog)
  private
  protected
  public
  end;

  { TFpGuiWSSavePictureDialog }

  TFpGuiWSSavePictureDialog = class(TWSSavePictureDialog)
  private
  protected
  public
  end;

  { TFpGuiWSCalculatorDialog }

  TFpGuiWSCalculatorDialog = class(TWSCalculatorDialog)
  private
  protected
  public
  end;

  { TFpGuiWSCalculatorForm }

  TFpGuiWSCalculatorForm = class(TWSCalculatorForm)
  private
  protected
  public
  end;

  { TFpGuiWSCalendarDialogForm }

  TFpGuiWSCalendarDialogForm = class(TWSCalendarDialogForm)
  private
  protected
  public
  end;

  { TFpGuiWSCalendarDialog }

  TFpGuiWSCalendarDialog = class(TWSCalendarDialog)
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
//  RegisterWSComponent(TPreviewFileControl, TFpGuiWSPreviewFileControl);
//  RegisterWSComponent(TPreviewFileDialog, TFpGuiWSPreviewFileDialog);
//  RegisterWSComponent(TOpenPictureDialog, TFpGuiWSOpenPictureDialog);
//  RegisterWSComponent(TSavePictureDialog, TFpGuiWSSavePictureDialog);
//  RegisterWSComponent(TCalculatorDialog, TFpGuiWSCalculatorDialog);
//  RegisterWSComponent(TCalculatorForm, TFpGuiWSCalculatorForm);
//  RegisterWSComponent(TCalendarDialogForm, TFpGuiWSCalendarDialogForm);
//  RegisterWSComponent(TCalendarDialog, TFpGuiWSCalendarDialog);
////////////////////////////////////////////////////
end.
