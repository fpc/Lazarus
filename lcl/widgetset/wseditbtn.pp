{ $Id$}
{
 *****************************************************************************
 *                               wseditbtn.pp                                * 
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
unit wseditbtn;

{$mode objfpc}{H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as litle as posible circles,
// Uncomment only when needed for registration
////////////////////////////////////////////////////
//  editbtn,
////////////////////////////////////////////////////
  wslclclasses, wsstdctrls;

type

  { TWSCustomEditButton }

  TWSCustomEditButton = class(TWSEdit)
  private
  protected
  public
  end;

  { TWSEditButton }

  TWSEditButton = class(TWSCustomEditButton)
  private
  protected
  public
  end;

  { TWSFileNameEdit }

  TWSFileNameEdit = class(TWSCustomEditButton)
  private
  protected
  public
  end;

  { TWSDirectoryEdit }

  TWSDirectoryEdit = class(TWSCustomEditButton)
  private
  protected
  public
  end;

  { TWSDateEdit }

  TWSDateEdit = class(TWSCustomEditButton)
  private
  protected
  public
  end;

  { TWSCalcEdit }

  TWSCalcEdit = class(TWSCustomEditButton)
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
//  RegisterWSComponent(TCustomEditButton, TWSCustomEditButton);
//  RegisterWSComponent(TEditButton, TWSEditButton);
//  RegisterWSComponent(TFileNameEdit, TWSFileNameEdit);
//  RegisterWSComponent(TDirectoryEdit, TWSDirectoryEdit);
//  RegisterWSComponent(TDateEdit, TWSDateEdit);
//  RegisterWSComponent(TCalcEdit, TWSCalcEdit);
////////////////////////////////////////////////////
end.
