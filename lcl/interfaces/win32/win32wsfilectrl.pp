{ $Id$}
{
 *****************************************************************************
 *                            Win32WSFileCtrl.pp                             * 
 *                            ------------------                             * 
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
unit Win32WSFileCtrl;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  FileCtrl,
////////////////////////////////////////////////////
  WSFileCtrl, WSLCLClasses;

type

  { TWin32WSCustomFileListBox }

  TWin32WSCustomFileListBox = class(TWSCustomFileListBox)
  published
  end;

  { TWin32WSFileListBox }

  TWin32WSFileListBox = class(TWSFileListBox)
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
//  RegisterWSComponent(TCustomFileListBox, TWin32WSCustomFileListBox);
//  RegisterWSComponent(TFileListBox, TWin32WSFileListBox);
////////////////////////////////////////////////////
end.