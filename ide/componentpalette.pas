{  $Id$  }
{
 /***************************************************************************
                          componentpalette.pas
                          --------------------


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:

}
unit ComponentPalette;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Graphics, ExtCtrls, ComponentReg, PackageDefs;

type
  TComponentPalette = class(TBaseComponentPalette)
  private
    FNoteBook: TNotebook;
    fNoteBookNeedsUpdate: boolean;
    procedure SetNoteBook(const AValue: TNotebook);
  protected
    procedure DoEndUpdate(Changed: boolean); override;
  public
    procedure UpdateNoteBookButtons;
    property NoteBook: TNotebook read FNoteBook write SetNoteBook;
  end;

implementation

{ TComponentPalette }

procedure TComponentPalette.SetNoteBook(const AValue: TNotebook);
begin
  if FNoteBook=AValue then exit;
  FNoteBook:=AValue;
  UpdateNoteBookButtons;
end;

procedure TComponentPalette.DoEndUpdate(Changed: boolean);
begin
  if Changed then UpdateNoteBookButtons;
  inherited DoEndUpdate(Changed);
end;

procedure TComponentPalette.UpdateNoteBookButtons;
begin
  if IsUpdating then begin
    fNoteBookNeedsUpdate:=true;
    exit;
  end;
  fNoteBookNeedsUpdate:=false;
end;

end.

