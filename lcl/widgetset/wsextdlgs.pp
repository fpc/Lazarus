{ $Id$}
{
 *****************************************************************************
 *                               wsextdlgs.pp                                * 
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
unit wsextdlgs;

{$mode objfpc}{H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as litle as posible circles,
// Uncomment only when needed for registration
////////////////////////////////////////////////////
//  extdlgs,
////////////////////////////////////////////////////
  wslclclasses, wscontrols, wsdialogs, wsforms;

type

  { TWSPreviewFileControl }

  TWSPreviewFileControl = class(TWSWinControl)
  private
  protected
  public
  end;

  { TWSPreviewFileDialog }

  TWSPreviewFileDialog = class(TWSOpenDialog)
  private
  protected
  public
  end;

  { TWSOpenPictureDialog }

  TWSOpenPictureDialog = class(TWSPreviewFileDialog)
  private
  protected
  public
  end;

  { TWSSavePictureDialog }

  TWSSavePictureDialog = class(TWSOpenPictureDialog)
  private
  protected
  public
  end;

  { TWSCalculatorDialog }

  TWSCalculatorDialog = class(TWSCommonDialog)
  private
  protected
  public
  end;

  { TWSCalculatorForm }

  TWSCalculatorForm = class(TWSForm)
  private
  protected
  public
  end;

  { TWSCalendarDialogForm }

  TWSCalendarDialogForm = class(TWSForm)
  private
  protected
  public
  end;

  { TWSCalendarDialog }

  TWSCalendarDialog = class(TWSCommonDialog)
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
//  RegisterWSComponent(TPreviewFileControl, TWSPreviewFileControl);
//  RegisterWSComponent(TPreviewFileDialog, TWSPreviewFileDialog);
//  RegisterWSComponent(TOpenPictureDialog, TWSOpenPictureDialog);
//  RegisterWSComponent(TSavePictureDialog, TWSSavePictureDialog);
//  RegisterWSComponent(TCalculatorDialog, TWSCalculatorDialog);
//  RegisterWSComponent(TCalculatorForm, TWSCalculatorForm);
//  RegisterWSComponent(TCalendarDialogForm, TWSCalendarDialogForm);
//  RegisterWSComponent(TCalendarDialog, TWSCalendarDialog);
////////////////////////////////////////////////////
end.
