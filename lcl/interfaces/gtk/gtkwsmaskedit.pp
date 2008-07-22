{ $Id$}
{
 *****************************************************************************
 *                             GtkWSMaskEdit.pp                              * 
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
unit GtkWSMaskEdit;

{$mode objfpc}{$H+}

interface

uses
  MaskEdit, WSMaskEdit, WSLCLClasses;

type

  { TGtkWSCustomMaskEdit }

  TGtkWSCustomMaskEdit = class(TWSCustomMaskEdit)
  private
  protected
  public
  end;

  { TGtkWSMaskEdit }

  TGtkWSMaskEdit = class(TWSMaskEdit)
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
//  RegisterWSComponent(TCustomMaskEdit, TGtkWSCustomMaskEdit);
//  RegisterWSComponent(TMaskEdit, TGtkWSMaskEdit);
////////////////////////////////////////////////////
end.