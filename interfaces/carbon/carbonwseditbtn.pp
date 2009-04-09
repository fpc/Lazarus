{ $Id$}
{
 *****************************************************************************
 *                              CarbonWSEditBtn.pp                           * 
 *                              --------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CarbonWSEditBtn;

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

  { TCarbonWSCustomEditButton }

  TCarbonWSCustomEditButton = class(TWSCustomEditButton)
  published
  end;

  { TCarbonWSEditButton }

  TCarbonWSEditButton = class(TWSEditButton)
  published
  end;

  { TCarbonWSFileNameEdit }

  TCarbonWSFileNameEdit = class(TWSFileNameEdit)
  published
  end;

  { TCarbonWSDirectoryEdit }

  TCarbonWSDirectoryEdit = class(TWSDirectoryEdit)
  published
  end;

  { TCarbonWSDateEdit }

  TCarbonWSDateEdit = class(TWSDateEdit)
  published
  end;

  { TCarbonWSCalcEdit }

  TCarbonWSCalcEdit = class(TWSCalcEdit)
  published
  end;


implementation

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCustomEditButton, TCarbonWSCustomEditButton);
//  RegisterWSComponent(TEditButton, TCarbonWSEditButton);
//  RegisterWSComponent(TFileNameEdit, TCarbonWSFileNameEdit);
//  RegisterWSComponent(TDirectoryEdit, TCarbonWSDirectoryEdit);
//  RegisterWSComponent(TDateEdit, TCarbonWSDateEdit);
//  RegisterWSComponent(TCalcEdit, TCarbonWSCalcEdit);
////////////////////////////////////////////////////
end.