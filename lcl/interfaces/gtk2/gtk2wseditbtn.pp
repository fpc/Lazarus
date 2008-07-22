{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSEditBtn.pp                              * 
 *                             ----------------                              * 
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
unit Gtk2WSEditBtn;

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

  { TGtk2WSCustomEditButton }

  TGtk2WSCustomEditButton = class(TWSCustomEditButton)
  private
  protected
  public
  end;

  { TGtk2WSEditButton }

  TGtk2WSEditButton = class(TWSEditButton)
  private
  protected
  public
  end;

  { TGtk2WSFileNameEdit }

  TGtk2WSFileNameEdit = class(TWSFileNameEdit)
  private
  protected
  public
  end;

  { TGtk2WSDirectoryEdit }

  TGtk2WSDirectoryEdit = class(TWSDirectoryEdit)
  private
  protected
  public
  end;

  { TGtk2WSDateEdit }

  TGtk2WSDateEdit = class(TWSDateEdit)
  private
  protected
  public
  end;

  { TGtk2WSCalcEdit }

  TGtk2WSCalcEdit = class(TWSCalcEdit)
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
//  RegisterWSComponent(TCustomEditButton, TGtk2WSCustomEditButton);
//  RegisterWSComponent(TEditButton, TGtk2WSEditButton);
//  RegisterWSComponent(TFileNameEdit, TGtk2WSFileNameEdit);
//  RegisterWSComponent(TDirectoryEdit, TGtk2WSDirectoryEdit);
//  RegisterWSComponent(TDateEdit, TGtk2WSDateEdit);
//  RegisterWSComponent(TCalcEdit, TGtk2WSCalcEdit);
////////////////////////////////////////////////////
end.