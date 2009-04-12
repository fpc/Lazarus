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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    * 
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
  published
  end;

  { TQtWSPreviewFileDialog }

  TQtWSPreviewFileDialog = class(TWSPreviewFileDialog)
  published
  end;

  { TQtWSOpenPictureDialog }

  TQtWSOpenPictureDialog = class(TWSOpenPictureDialog)
  published
  end;

  { TQtWSSavePictureDialog }

  TQtWSSavePictureDialog = class(TWSSavePictureDialog)
  published
  end;

  { TQtWSCalculatorDialog }

  TQtWSCalculatorDialog = class(TWSCalculatorDialog)
  published
  end;

  { TQtWSCalculatorForm }

  TQtWSCalculatorForm = class(TWSCalculatorForm)
  published
  end;

  { TQtWSCalendarDialogForm }

  TQtWSCalendarDialogForm = class(TWSCalendarDialogForm)
  published
  end;

  { TQtWSCalendarDialog }

  TQtWSCalendarDialog = class(TWSCalendarDialog)
  published
  end;


implementation

end.
