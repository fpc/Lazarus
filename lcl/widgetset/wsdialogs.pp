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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
  WSLCLClasses, WSControls, WSFactory;

type
  { TWSCommonDialog }

  TWSCommonDialogClass = class of TWSCommonDialog;
  TWSCommonDialog = class(TWSLCLComponent)
  published
    class function  CreateHandle(const ACommonDialog: TCommonDialog): THandle; virtual;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); virtual;
    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); virtual;
  end;

  { TWSFileDialog }

  TWSFileDialog = class(TWSCommonDialog)
  published
  end;

  { TWSOpenDialog }

  TWSOpenDialog = class(TWSFileDialog)
  published
  end;

  { TWSSaveDialog }

  TWSSaveDialog = class(TWSOpenDialog)
  published
  end;

  { TWSSelectDirectoryDialog }

  TWSSelectDirectoryDialog = class(TWSOpenDialog)
  published
  end;

  { TWSColorDialog }

  TWSColorDialog = class(TWSCommonDialog)
  published
  end;

  { TWSColorButton }

  TWSColorButton = class(TWSGraphicControl)
  published
  end;

  { TWSFontDialog }

  TWSFontDialog = class(TWSCommonDialog)
  published
  end;

  { WidgetSetRegistration }

  procedure RegisterCommonDialog;
  procedure RegisterFileDialog;
  procedure RegisterOpenDialog;
  procedure RegisterSaveDialog;
  procedure RegisterSelectDirectoryDialog;
  procedure RegisterColorDialog;
  procedure RegisterColorButton;
  procedure RegisterFontDialog;

implementation

uses
  LResources;

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

{ WidgetSetRegistration }

procedure RegisterCommonDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterCommonDialog then
    RegisterWSComponent(TCommonDialog, TWSCommonDialog);
  RegisterPropertyToSkip(TCommonDialog, 'Ctl3D', 'VCL compatibility property', '');
  Done := True;
end;

procedure RegisterFileDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterFileDialog;
//  if not WSRegisterFileDialog then
//    RegisterWSComponent(TFileDialog, TWSFileDialog);
  Done := True;
end;

procedure RegisterOpenDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterOpenDialog;
//  if not WSRegisterOpenDialog then
//    RegisterWSComponent(TOpenDialog, TWSOpenDialog);
  Done := True;
end;

procedure RegisterSaveDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterSaveDialog;
//  if not WSRegisterSaveDialog then
//    RegisterWSComponent(TSaveDialog, TWSSaveDialog);
  Done := True;
end;

procedure RegisterSelectDirectoryDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterSelectDirectoryDialog;
//  if not WSRegisterSelectDirectoryDialog then
//    RegisterWSComponent(TSelectDirectoryDialog, TWSSelectDirectoryDialog);
  Done := True;
end;

procedure RegisterColorDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterColorDialog;
//  if not WSRegisterColorDialog then
//    RegisterWSComponent(TColorDialog, TWSColorDialog);
  Done := True;
end;

procedure RegisterColorButton;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterColorButton;
//  if not WSRegisterColorButton then
//    RegisterWSComponent(TColorButton, TWSColorButton);
  Done := True;
end;

procedure RegisterFontDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterFontDialog;
//  if not WSRegisterFontDialog then
//    RegisterWSComponent(TFontDialog, TWSFontDialog);
  Done := True;
end;

end.
