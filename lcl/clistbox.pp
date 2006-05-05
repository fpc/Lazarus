{ $Id$}

{
 /***************************************************************************
                    CListBox.pp  -  TCListBox implementation
                             -------------------
                             Component Library Code


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit CListBox;

{$MODE objfpc}{$H+}

interface

uses
  Classes, Controls, LCLType, StdCtrls;

type
  TCListBox = class(TCustomListBox)
  private
    FListColumns: integer;
  public
    constructor Create(AOwner: TComponent); override;
    property ItemIndex;
    property ListColumns: integer read FListColumns write FListColumns;
  published
    property BorderStyle;
    property ExtendedSelect;
    property Items;
    property MultiSelect;
    property Sorted;
    property Style;
    property Visible; 
  end;

implementation

{------------------------------------------------------------------------------}
{  constructor TCListBox.Create                                                }
{------------------------------------------------------------------------------}
constructor TCListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csCListBox;
  FListColumns := 1;
end;

end.
