{ $Id$}
{
 *****************************************************************************
 *                              GtkWSEditBtn.pp                              * 
 *                              ---------------                              * 
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
unit GtkWSEditBtn;

{$mode objfpc}{$H+}

interface

uses
  EditBtn, WSEditBtn, WSLCLClasses;

type

  { TGtkWSCustomEditButton }

  TGtkWSCustomEditButton = class(TWSCustomEditButton)
  private
  protected
  public
  end;

  { TGtkWSEditButton }

  TGtkWSEditButton = class(TWSEditButton)
  private
  protected
  public
  end;

  { TGtkWSFileNameEdit }

  TGtkWSFileNameEdit = class(TWSFileNameEdit)
  private
  protected
  public
  end;

  { TGtkWSDirectoryEdit }

  TGtkWSDirectoryEdit = class(TWSDirectoryEdit)
  private
  protected
  public
  end;

  { TGtkWSDateEdit }

  TGtkWSDateEdit = class(TWSDateEdit)
  private
  protected
  public
  end;

  { TGtkWSCalcEdit }

  TGtkWSCalcEdit = class(TWSCalcEdit)
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
//  RegisterWSComponent(TCustomEditButton, TGtkWSCustomEditButton);
//  RegisterWSComponent(TEditButton, TGtkWSEditButton);
//  RegisterWSComponent(TFileNameEdit, TGtkWSFileNameEdit);
//  RegisterWSComponent(TDirectoryEdit, TGtkWSDirectoryEdit);
//  RegisterWSComponent(TDateEdit, TGtkWSDateEdit);
//  RegisterWSComponent(TCalcEdit, TGtkWSCalcEdit);
////////////////////////////////////////////////////
end.