{ $Id$}
{
 *****************************************************************************
 *                             win32wsdialogs.pp                             * 
 *                             -----------------                             * 
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
unit win32wsdialogs;

{$mode objfpc}{H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as litle as posible circles,
// Uncomment only when needed for registration
////////////////////////////////////////////////////
//  dialogs,
////////////////////////////////////////////////////
  wsdialogs, wslclclasses;

type

  { TWin32WSCommonDialog }

  TWin32WSCommonDialog = class(TWSCommonDialog)
  private
  protected
  public
  end;

  { TWin32WSFileDialog }

  TWin32WSFileDialog = class(TWSFileDialog)
  private
  protected
  public
  end;

  { TWin32WSOpenDialog }

  TWin32WSOpenDialog = class(TWSOpenDialog)
  private
  protected
  public
  end;

  { TWin32WSSaveDialog }

  TWin32WSSaveDialog = class(TWSSaveDialog)
  private
  protected
  public
  end;

  { TWin32WSSelectDirectoryDialog }

  TWin32WSSelectDirectoryDialog = class(TWSSelectDirectoryDialog)
  private
  protected
  public
  end;

  { TWin32WSColorDialog }

  TWin32WSColorDialog = class(TWSColorDialog)
  private
  protected
  public
  end;

  { TWin32WSColorButton }

  TWin32WSColorButton = class(TWSColorButton)
  private
  protected
  public
  end;

  { TWin32WSFontDialog }

  TWin32WSFontDialog = class(TWSFontDialog)
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
//  RegisterWSComponent(TCommonDialog, TWin32WSCommonDialog);
//  RegisterWSComponent(TFileDialog, TWin32WSFileDialog);
//  RegisterWSComponent(TOpenDialog, TWin32WSOpenDialog);
//  RegisterWSComponent(TSaveDialog, TWin32WSSaveDialog);
//  RegisterWSComponent(TSelectDirectoryDialog, TWin32WSSelectDirectoryDialog);
//  RegisterWSComponent(TColorDialog, TWin32WSColorDialog);
//  RegisterWSComponent(TColorButton, TWin32WSColorButton);
//  RegisterWSComponent(TFontDialog, TWin32WSFontDialog);
////////////////////////////////////////////////////
end.
