{ $Id$}
{
 *****************************************************************************
 *                              QtWSExtDlgs.pp                               * 
 *                              --------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit QtWSExtDlgs;

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

  { TQtWSPreviewFileControl }

  TQtWSPreviewFileControl = class(TWSPreviewFileControl)
  private
  protected
  public
  end;

  { TQtWSPreviewFileDialog }

  TQtWSPreviewFileDialog = class(TWSPreviewFileDialog)
  private
  protected
  public
  end;

  { TQtWSOpenPictureDialog }

  TQtWSOpenPictureDialog = class(TWSOpenPictureDialog)
  private
  protected
  public
  end;

  { TQtWSSavePictureDialog }

  TQtWSSavePictureDialog = class(TWSSavePictureDialog)
  private
  protected
  public
  end;

  { TQtWSCalculatorDialog }

  TQtWSCalculatorDialog = class(TWSCalculatorDialog)
  private
  protected
  public
  end;

  { TQtWSCalculatorForm }

  TQtWSCalculatorForm = class(TWSCalculatorForm)
  private
  protected
  public
  end;

  { TQtWSCalendarDialogForm }

  TQtWSCalendarDialogForm = class(TWSCalendarDialogForm)
  private
  protected
  public
  end;

  { TQtWSCalendarDialog }

  TQtWSCalendarDialog = class(TWSCalendarDialog)
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
//  RegisterWSComponent(TPreviewFileControl, TQtWSPreviewFileControl);
//  RegisterWSComponent(TPreviewFileDialog, TQtWSPreviewFileDialog);
//  RegisterWSComponent(TOpenPictureDialog, TQtWSOpenPictureDialog);
//  RegisterWSComponent(TSavePictureDialog, TQtWSSavePictureDialog);
//  RegisterWSComponent(TCalculatorDialog, TQtWSCalculatorDialog);
//  RegisterWSComponent(TCalculatorForm, TQtWSCalculatorForm);
//  RegisterWSComponent(TCalendarDialogForm, TQtWSCalendarDialogForm);
//  RegisterWSComponent(TCalendarDialog, TQtWSCalendarDialog);
////////////////////////////////////////////////////
end.