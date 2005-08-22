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
 *  See the file COPYING.LCL, included in this distribution,                 *
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

{
  $Log$
  Revision 1.4  2004/08/18 09:31:21  mattias
  removed obsolete unit vclglobals

  Revision 1.3  2002/05/10 06:05:48  lazarus
  MG: changed license to LGPL

  Revision 1.2  2001/06/15 10:31:05  lazarus
  MG: set longstrings as default

  Revision 1.1  2000/07/13 10:28:23  michael
  + Initial import

  Revision 1.2  2000/04/13 21:36:38  lazarus
  MWE:
    * Fixed compiler error when derived from TCustomListBox

  Revision 1.1  2000/04/13 21:25:16  lazarus
  MWE:
    ~ Added some docu and did some cleanup.
  Hans-Joachim Ott <hjott@compuserve.com>:
    * TMemo.Lines works now.
    + TMemo has now a property Scrollbar.
    = TControl.GetTextBuf revised :-)
    + Implementation for CListBox columns added
    * Bug in TGtkCListStringList.Assign corrected.


}
