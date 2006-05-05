{ $Id$}
{
 *****************************************************************************
 *                             GnomeWSExtDlgs.pp                             * 
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
unit GnomeWSExtDlgs;

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

  { TGnomeWSPreviewFileControl }

  TGnomeWSPreviewFileControl = class(TWSPreviewFileControl)
  private
  protected
  public
  end;

  { TGnomeWSPreviewFileDialog }

  TGnomeWSPreviewFileDialog = class(TWSPreviewFileDialog)
  private
  protected
  public
  end;

  { TGnomeWSOpenPictureDialog }

  TGnomeWSOpenPictureDialog = class(TWSOpenPictureDialog)
  private
  protected
  public
  end;

  { TGnomeWSSavePictureDialog }

  TGnomeWSSavePictureDialog = class(TWSSavePictureDialog)
  private
  protected
  public
  end;

  { TGnomeWSCalculatorDialog }

  TGnomeWSCalculatorDialog = class(TWSCalculatorDialog)
  private
  protected
  public
  end;

  { TGnomeWSCalculatorForm }

  TGnomeWSCalculatorForm = class(TWSCalculatorForm)
  private
  protected
  public
  end;

  { TGnomeWSCalendarDialogForm }

  TGnomeWSCalendarDialogForm = class(TWSCalendarDialogForm)
  private
  protected
  public
  end;

  { TGnomeWSCalendarDialog }

  TGnomeWSCalendarDialog = class(TWSCalendarDialog)
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
//  RegisterWSComponent(TPreviewFileControl, TGnomeWSPreviewFileControl);
//  RegisterWSComponent(TPreviewFileDialog, TGnomeWSPreviewFileDialog);
//  RegisterWSComponent(TOpenPictureDialog, TGnomeWSOpenPictureDialog);
//  RegisterWSComponent(TSavePictureDialog, TGnomeWSSavePictureDialog);
//  RegisterWSComponent(TCalculatorDialog, TGnomeWSCalculatorDialog);
//  RegisterWSComponent(TCalculatorForm, TGnomeWSCalculatorForm);
//  RegisterWSComponent(TCalendarDialogForm, TGnomeWSCalendarDialogForm);
//  RegisterWSComponent(TCalendarDialog, TGnomeWSCalendarDialog);
////////////////////////////////////////////////////
end.