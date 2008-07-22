{ $Id$}
{
 *****************************************************************************
 *                               WSExtDlgs.pp                                * 
 *                               ------------                                * 
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
unit WSExtDlgs;

{$mode objfpc}{$H+}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  ExtDlgs,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, WSDialogs, WSForms;

type
  { TWSPreviewFileControl }

  TWSPreviewFileControl = class(TWSWinControl)
  end;

  { TWSPreviewFileDialog }

  TWSPreviewFileDialog = class(TWSOpenDialog)
  end;

  { TWSOpenPictureDialog }

  TWSOpenPictureDialog = class(TWSPreviewFileDialog)
  end;

  { TWSSavePictureDialog }

  TWSSavePictureDialog = class(TWSOpenPictureDialog)
  end;

  { TWSCalculatorDialog }

  TWSCalculatorDialog = class(TWSCommonDialog)
  end;

  { TWSCalculatorForm }

  TWSCalculatorForm = class(TWSForm)
  end;

  { TWSCalendarDialogForm }

  TWSCalendarDialogForm = class(TWSForm)
  end;

  { TWSCalendarDialog }

  TWSCalendarDialog = class(TWSCommonDialog)
  end;


implementation

initialization

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