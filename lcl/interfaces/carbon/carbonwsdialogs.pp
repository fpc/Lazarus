{ $Id$}
{
 *****************************************************************************
 *                              CarbonWSDialogs.pp                               * 
 *                              --------------                               * 
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
unit CarbonWSDialogs;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  // libs
  FPCMacOSAll,
  // LCL
  Controls, Dialogs, LCLType, LCLProc,
  // widgetset
  WSLCLClasses, WSProc, WSDialogs,
  // interface
  CarbonDef, CarbonProc, CarbonPrivate;
type

  { TCarbonWSCommonDialog }

  TCarbonWSCommonDialog = class(TWSCommonDialog)
  private
  protected
  public
  end;

  { TCarbonWSFileDialog }

  TCarbonWSFileDialog = class(TWSFileDialog)
  private
  protected
  public
  end;

  { TCarbonWSOpenDialog }

  TCarbonWSOpenDialog = class(TWSOpenDialog)
  private
  protected
  public
  end;

  { TCarbonWSSaveDialog }

  TCarbonWSSaveDialog = class(TWSSaveDialog)
  private
  protected
  public
  end;

  { TCarbonWSSelectDirectoryDialog }

  TCarbonWSSelectDirectoryDialog = class(TWSSelectDirectoryDialog)
  private
  protected
  public
  end;

  { TCarbonWSColorDialog }

  TCarbonWSColorDialog = class(TWSColorDialog)
  private
  protected
  public
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TCarbonWSColorButton }

  TCarbonWSColorButton = class(TWSColorButton)
  private
  protected
  public
  end;

  { TCarbonWSFontDialog }

  TCarbonWSFontDialog = class(TWSFontDialog)
  private
  protected
  public
  end;


implementation

{ TCarbonWSColorDialog }

{------------------------------------------------------------------------------
  Method:  TCarbonWSColorDialog.ShowModal
  Params:  ACommonDialog - LCL color dialog
  Returns: Nothing

  Shows Carbon interface color picker
 ------------------------------------------------------------------------------}
class procedure TCarbonWSColorDialog.ShowModal(
  const ACommonDialog: TCommonDialog);
var
  ColorInfo: ColorPickerInfo;
  ColorDialog: TColorDialog;
begin
  ColorDialog := ACommonDialog as TColorDialog;
  
  FillChar(ColorInfo, SizeOf(ColorPickerInfo), 0);
  ColorInfo.theColor.color.rgb := CMRGBColor(ColorToCarbonColor(ColorDialog.Color));
  ColorInfo.theColor.profile := nil;
  ColorInfo.dstProfile := nil;
  ColorInfo.flags := kColorPickerDialogIsModal or kColorPickerDialogIsMoveable or
    kColorPickerInPickerDialog;
  ColorInfo.placeWhere :=  kCenterOnMainScreen;
  ColorInfo.pickerType := 0; // use last picker subtype
  ColorInfo.eventProc := nil;
  ColorInfo.colorProc := nil;
  ColorInfo.prompt := UTF8ToAnsi(ColorDialog.Title); // does not function!

  if PickColor(ColorInfo) = noErr then
    if ColorInfo.newColorChosen then
    begin
      ColorDialog.Color := CarbonColorToColor(
        RGBColor(ColorInfo.theColor.color.rgb));
      ACommonDialog.UserChoice := mrOK;
      Exit;
    end;

  ACommonDialog.UserChoice := mrCancel;
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCommonDialog, TCarbonWSCommonDialog);
//  RegisterWSComponent(TFileDialog, TCarbonWSFileDialog);
//  RegisterWSComponent(TOpenDialog, TCarbonWSOpenDialog);
//  RegisterWSComponent(TSaveDialog, TCarbonWSSaveDialog);
//  RegisterWSComponent(TSelectDirectoryDialog, TCarbonWSSelectDirectoryDialog);
  RegisterWSComponent(TColorDialog, TCarbonWSColorDialog);
//  RegisterWSComponent(TColorButton, TCarbonWSColorButton);
//  RegisterWSComponent(TFontDialog, TCarbonWSFontDialog);
////////////////////////////////////////////////////
end.
