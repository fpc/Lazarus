{ $Id$}
{
 *****************************************************************************
 *                             Win32WSEditBtn.pp                             * 
 *                             -----------------                             * 
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
unit Win32WSEditBtn;

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

  { TWin32WSCustomEditButton }

  TWin32WSCustomEditButton = class(TWSCustomEditButton)
  private
  protected
  public
  end;

  { TWin32WSEditButton }

  TWin32WSEditButton = class(TWSEditButton)
  private
  protected
  public
  end;

  { TWin32WSFileNameEdit }

  TWin32WSFileNameEdit = class(TWSFileNameEdit)
  private
  protected
  public
  end;

  { TWin32WSDirectoryEdit }

  TWin32WSDirectoryEdit = class(TWSDirectoryEdit)
  private
  protected
  public
  end;

  { TWin32WSDateEdit }

  TWin32WSDateEdit = class(TWSDateEdit)
  private
  protected
  public
  end;

  { TWin32WSCalcEdit }

  TWin32WSCalcEdit = class(TWSCalcEdit)
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
//  RegisterWSComponent(TCustomEditButton, TWin32WSCustomEditButton);
//  RegisterWSComponent(TEditButton, TWin32WSEditButton);
//  RegisterWSComponent(TFileNameEdit, TWin32WSFileNameEdit);
//  RegisterWSComponent(TDirectoryEdit, TWin32WSDirectoryEdit);
//  RegisterWSComponent(TDateEdit, TWin32WSDateEdit);
//  RegisterWSComponent(TCalcEdit, TWin32WSCalcEdit);
////////////////////////////////////////////////////
end.