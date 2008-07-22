{ $Id$}
{
 *****************************************************************************
 *                             Win32WSExtDlgs.pp                             * 
 *                             -----------------                             * 
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
unit Win32WSExtDlgs;

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

  { TWin32WSPreviewFileControl }

  TWin32WSPreviewFileControl = class(TWSPreviewFileControl)
  private
  protected
  public
  end;

  { TWin32WSPreviewFileDialog }

  TWin32WSPreviewFileDialog = class(TWSPreviewFileDialog)
  private
  protected
  public
  end;

  { TWin32WSOpenPictureDialog }

  TWin32WSOpenPictureDialog = class(TWSOpenPictureDialog)
  private
  protected
  public
  end;

  { TWin32WSSavePictureDialog }

  TWin32WSSavePictureDialog = class(TWSSavePictureDialog)
  private
  protected
  public
  end;

  { TWin32WSCalculatorDialog }

  TWin32WSCalculatorDialog = class(TWSCalculatorDialog)
  private
  protected
  public
  end;

  { TWin32WSCalculatorForm }

  TWin32WSCalculatorForm = class(TWSCalculatorForm)
  private
  protected
  public
  end;

  { TWin32WSCalendarDialogForm }

  TWin32WSCalendarDialogForm = class(TWSCalendarDialogForm)
  private
  protected
  public
  end;

  { TWin32WSCalendarDialog }

  TWin32WSCalendarDialog = class(TWSCalendarDialog)
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
//  RegisterWSComponent(TPreviewFileControl, TWin32WSPreviewFileControl);
//  RegisterWSComponent(TPreviewFileDialog, TWin32WSPreviewFileDialog);
//  RegisterWSComponent(TOpenPictureDialog, TWin32WSOpenPictureDialog);
//  RegisterWSComponent(TSavePictureDialog, TWin32WSSavePictureDialog);
//  RegisterWSComponent(TCalculatorDialog, TWin32WSCalculatorDialog);
//  RegisterWSComponent(TCalculatorForm, TWin32WSCalculatorForm);
//  RegisterWSComponent(TCalendarDialogForm, TWin32WSCalendarDialogForm);
//  RegisterWSComponent(TCalendarDialog, TWin32WSCalendarDialog);
////////////////////////////////////////////////////
end.