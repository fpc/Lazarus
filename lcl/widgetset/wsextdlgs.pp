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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
  WSLCLClasses, WSControls, WSDialogs, WSForms, WSFactory;

type
  { TWSPreviewFileControl }

  TWSPreviewFileControl = class(TWSWinControl)
  published
  end;

  { TWSPreviewFileDialog }

  TWSPreviewFileDialog = class(TWSOpenDialog)
  published
  end;

  { TWSOpenPictureDialog }

  TWSOpenPictureDialog = class(TWSPreviewFileDialog)
  published
  end;

  { TWSSavePictureDialog }

  TWSSavePictureDialog = class(TWSOpenPictureDialog)
  published
  end;

  { TWSCalculatorDialog }

  TWSCalculatorDialog = class(TWSCommonDialog)
  published
  end;

  { TWSCalculatorForm }

  TWSCalculatorForm = class(TWSForm)
  published
  end;

  { TWSCalendarDialogForm }

  TWSCalendarDialogForm = class(TWSForm)
  published
  end;

  { TWSCalendarDialog }

  TWSCalendarDialog = class(TWSCommonDialog)
  published
  end;

  { WidgetSetRegistration }

  procedure RegisterPreviewFileControl;
  procedure RegisterPreviewFileDialog;
  procedure RegisterOpenPictureDialog;
  procedure RegisterSavePictureDialog;
  procedure RegisterCalculatorDialog;
  procedure RegisterCalculatorForm;
  function RegisterCalculatorPanel: Boolean;
  //procedure RegisterCalendarDialogForm;
  procedure RegisterCalendarDialog;

implementation

{ WidgetSetRegistration }

procedure RegisterPreviewFileControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterPreviewFileControl;
//  if not WSRegisterPreviewFileControl then
//    RegisterWSComponent(TPreviewFileControl, TWSPreviewFileControl);
  Done := True;
end;

procedure RegisterPreviewFileDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterPreviewFileDialog;
//  if not WSRegisterPreviewFileDialog then
//    RegisterWSComponent(TPreviewFileDialog, TWSPreviewFileDialog);
  Done := True;
end;

procedure RegisterOpenPictureDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterOpenPictureDialog;
//  if not WSRegisterOpenPictureDialog then
//    RegisterWSComponent(TOpenPictureDialog, TWSOpenPictureDialog);
  Done := True;
end;

procedure RegisterSavePictureDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterSavePictureDialog;
//  if not WSRegisterSavePictureDialog then
//    RegisterWSComponent(TSavePictureDialog, TWSSavePictureDialog);
  Done := True;
end;

procedure RegisterCalculatorDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCalculatorDialog;
//  if not WSRegisterCalculatorDialog then
//    RegisterWSComponent(TCalculatorDialog, TWSCalculatorDialog);
  Done := True;
end;

procedure RegisterCalculatorForm;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCalculatorForm;
//  if not WSRegisterCalculatorForm then
//    RegisterWSComponent(TCalculatorForm, TWSCalculatorForm);
  Done := True;
end;

function RegisterCalculatorPanel: Boolean;
const
  Done: Boolean = False;
begin
  Result := False;
  if Done then exit;
  // WSRegisterCalculatorPanel;
  Done := True;
  Result := True;
end;

(*procedure RegisterCalendarDialogForm;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCalendarDialogForm;
//  if not WSRegisterCalendarDialogForm then
//    RegisterWSComponent(TCalendarDialogForm, TWSCalendarDialogForm);
  Done := True;
end;*)

procedure RegisterCalendarDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCalendarDialog;
//  if not WSRegisterCalendarDialog then
//    RegisterWSComponent(TCalendarDialog, TWSCalendarDialog);
  Done := True;
end;

end.
