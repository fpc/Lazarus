{ $Id$}
{
 *****************************************************************************
 *                               WSDialogs.pp                                * 
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
unit WSDialogs;

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
  LCLType, Dialogs,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls;

type
  { TWSCommonDialog }

  TWSCommonDialogClass = class of TWSCommonDialog;
  TWSCommonDialog = class(TWSLCLComponent)
    class function  CreateHandle(const ACommonDialog: TCommonDialog): THandle; virtual;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); virtual;
    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); virtual;
  end;

  { TWSFileDialog }

  TWSFileDialog = class(TWSCommonDialog)
  end;

  { TWSOpenDialog }

  TWSOpenDialog = class(TWSFileDialog)
  end;

  { TWSSaveDialog }

  TWSSaveDialog = class(TWSOpenDialog)
  end;

  { TWSSelectDirectoryDialog }

  TWSSelectDirectoryDialog = class(TWSOpenDialog)
  end;

  { TWSColorDialog }

  TWSColorDialog = class(TWSCommonDialog)
  end;

  { TWSColorButton }

  TWSColorButton = class(TWSGraphicControl)
  end;

  { TWSFontDialog }

  TWSFontDialog = class(TWSCommonDialog)
  end;


implementation

class function  TWSCommonDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
begin
  Result := 0;
end;

class procedure TWSCommonDialog.DestroyHandle(const ACommonDialog: TCommonDialog);
begin
end;

class procedure TWSCommonDialog.ShowModal(const ACommonDialog: TCommonDialog);
begin
end;

initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCommonDialog, TWSCommonDialog);
//  RegisterWSComponent(TFileDialog, TWSFileDialog);
//  RegisterWSComponent(TOpenDialog, TWSOpenDialog);
//  RegisterWSComponent(TSaveDialog, TWSSaveDialog);
//  RegisterWSComponent(TSelectDirectoryDialog, TWSSelectDirectoryDialog);
//  RegisterWSComponent(TColorDialog, TWSColorDialog);
//  RegisterWSComponent(TColorButton, TWSColorButton);
//  RegisterWSComponent(TFontDialog, TWSFontDialog);
////////////////////////////////////////////////////
end.
