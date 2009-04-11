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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
  published
  end;

  { TGtkWSEditButton }

  TGtkWSEditButton = class(TWSEditButton)
  published
  end;

  { TGtkWSFileNameEdit }

  TGtkWSFileNameEdit = class(TWSFileNameEdit)
  published
  end;

  { TGtkWSDirectoryEdit }

  TGtkWSDirectoryEdit = class(TWSDirectoryEdit)
  published
  end;

  { TGtkWSDateEdit }

  TGtkWSDateEdit = class(TWSDateEdit)
  published
  end;

  { TGtkWSCalcEdit }

  TGtkWSCalcEdit = class(TWSCalcEdit)
  published
  end;


implementation

end.
