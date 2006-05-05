{ $Id$}
{
 *****************************************************************************
 *                              GtkWSExtDlgs.pp                              * 
 *                              ---------------                              * 
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
unit GtkWSExtDlgs;

{$mode objfpc}{$H+}

interface

uses
  ExtDlgs, WSExtDlgs, WSLCLClasses;

type

  { TGtkWSPreviewFileControl }

  TGtkWSPreviewFileControl = class(TWSPreviewFileControl)
  private
  protected
  public
  end;

  { TGtkWSPreviewFileDialog }

  TGtkWSPreviewFileDialog = class(TWSPreviewFileDialog)
  private
  protected
  public
  end;

  { TGtkWSOpenPictureDialog }

  TGtkWSOpenPictureDialog = class(TWSOpenPictureDialog)
  private
  protected
  public
  end;

  { TGtkWSSavePictureDialog }

  TGtkWSSavePictureDialog = class(TWSSavePictureDialog)
  private
  protected
  public
  end;

  { TGtkWSCalculatorDialog }

  TGtkWSCalculatorDialog = class(TWSCalculatorDialog)
  private
  protected
  public
  end;

  { TGtkWSCalculatorForm }

  TGtkWSCalculatorForm = class(TWSCalculatorForm)
  private
  protected
  public
  end;

  { TGtkWSCalendarDialogForm }

  TGtkWSCalendarDialogForm = class(TWSCalendarDialogForm)
  private
  protected
  public
  end;

  { TGtkWSCalendarDialog }

  TGtkWSCalendarDialog = class(TWSCalendarDialog)
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
//  RegisterWSComponent(TPreviewFileControl, TGtkWSPreviewFileControl);
//  RegisterWSComponent(TPreviewFileDialog, TGtkWSPreviewFileDialog);
//  RegisterWSComponent(TOpenPictureDialog, TGtkWSOpenPictureDialog);
//  RegisterWSComponent(TSavePictureDialog, TGtkWSSavePictureDialog);
//  RegisterWSComponent(TCalculatorDialog, TGtkWSCalculatorDialog);
//  RegisterWSComponent(TCalculatorForm, TGtkWSCalculatorForm);
//  RegisterWSComponent(TCalendarDialogForm, TGtkWSCalendarDialogForm);
//  RegisterWSComponent(TCalendarDialog, TGtkWSCalendarDialog);
////////////////////////////////////////////////////
end.