{ $Id$}
{
 *****************************************************************************
 *                             GtkWSActnList.pp                              * 
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
unit GtkWSActnList;

{$mode objfpc}{$H+}

interface

uses
  ActnList, WSActnList, WSLCLClasses;

type

  { TGtkWSCustomActionList }

  TGtkWSCustomActionList = class(TWSCustomActionList)
  private
  protected
  public
  end;

  { TGtkWSActionList }

  TGtkWSActionList = class(TWSActionList)
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
//  RegisterWSComponent(TCustomActionList, TGtkWSCustomActionList);
//  RegisterWSComponent(TActionList, TGtkWSActionList);
////////////////////////////////////////////////////
end.