{%MainUnit ../dialogs.pp}
{******************************************************************************
                                  TColorDialog
 ******************************************************************************

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
{------------------------------------------------------------------------------
  Method: TColorDialog.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TColorDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fCompStyle := csColorDialog;
end;

function TColorDialog.DefaultTitle: string;
begin
  Result:=rsSelectcolorTitle;
end;


