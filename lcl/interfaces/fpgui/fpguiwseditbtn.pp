{ $Id: FpGuiwseditbtn.pp 5319 2004-03-17 20:11:29Z marc $}
{
 *****************************************************************************
 *                              FpGuiWSEditBtn.pp                               * 
 *                              --------------                               * 
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
unit FpGuiWSEditBtn;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  EditBtn,
////////////////////////////////////////////////////
  WSEditBtn, WSLCLClasses;

type

  { TFpGuiWSCustomEditButton }

  TFpGuiWSCustomEditButton = class(TWSCustomEditButton)
  private
  protected
  public
  end;

  { TFpGuiWSEditButton }

  TFpGuiWSEditButton = class(TWSEditButton)
  private
  protected
  public
  end;

  { TFpGuiWSFileNameEdit }

  TFpGuiWSFileNameEdit = class(TWSFileNameEdit)
  private
  protected
  public
  end;

  { TFpGuiWSDirectoryEdit }

  TFpGuiWSDirectoryEdit = class(TWSDirectoryEdit)
  private
  protected
  public
  end;

  { TFpGuiWSDateEdit }

  TFpGuiWSDateEdit = class(TWSDateEdit)
  private
  protected
  public
  end;

  { TFpGuiWSCalcEdit }

  TFpGuiWSCalcEdit = class(TWSCalcEdit)
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
//  RegisterWSComponent(TCustomEditButton, TFpGuiWSCustomEditButton);
//  RegisterWSComponent(TEditButton, TFpGuiWSEditButton);
//  RegisterWSComponent(TFileNameEdit, TFpGuiWSFileNameEdit);
//  RegisterWSComponent(TDirectoryEdit, TFpGuiWSDirectoryEdit);
//  RegisterWSComponent(TDateEdit, TFpGuiWSDateEdit);
//  RegisterWSComponent(TCalcEdit, TFpGuiWSCalcEdit);
////////////////////////////////////////////////////
end.
