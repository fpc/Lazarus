{ $Id$}
{
 *****************************************************************************
 *                             gnomewseditbtn.pp                             * 
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
unit gnomewseditbtn;

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
  wseditbtn, wslclclasses;

type

  { TGnomeWSCustomEditButton }

  TGnomeWSCustomEditButton = class(TWSCustomEditButton)
  private
  protected
  public
  end;

  { TGnomeWSEditButton }

  TGnomeWSEditButton = class(TWSEditButton)
  private
  protected
  public
  end;

  { TGnomeWSFileNameEdit }

  TGnomeWSFileNameEdit = class(TWSFileNameEdit)
  private
  protected
  public
  end;

  { TGnomeWSDirectoryEdit }

  TGnomeWSDirectoryEdit = class(TWSDirectoryEdit)
  private
  protected
  public
  end;

  { TGnomeWSDateEdit }

  TGnomeWSDateEdit = class(TWSDateEdit)
  private
  protected
  public
  end;

  { TGnomeWSCalcEdit }

  TGnomeWSCalcEdit = class(TWSCalcEdit)
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
//  RegisterWSComponent(TCustomEditButton, TGnomeWSCustomEditButton);
//  RegisterWSComponent(TEditButton, TGnomeWSEditButton);
//  RegisterWSComponent(TFileNameEdit, TGnomeWSFileNameEdit);
//  RegisterWSComponent(TDirectoryEdit, TGnomeWSDirectoryEdit);
//  RegisterWSComponent(TDateEdit, TGnomeWSDateEdit);
//  RegisterWSComponent(TCalcEdit, TGnomeWSCalcEdit);
////////////////////////////////////////////////////
end.
