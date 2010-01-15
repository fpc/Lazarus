{
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
    Dialog and functions for multi replace in source editor.
}
unit MultiReplaceDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls, ComCtrls;

type
  TMultiReplaceDialog = class(TForm)
    ItemOptionsCheckgroup: TCHECKGROUP;
    Edit1: TEDIT;
    Edit2: TEDIT;
    FindTextLabel: TLABEL;
    ReplaceWithLabel: TLABEL;
    PropertiesGroupbox: TGROUPBOX;
    ItemsListview: TLISTVIEW;
    ReplaceButton: TBUTTON;
    CancelButton: TBUTTON;
    ItemsGroupbox: TGROUPBOX;
    OptionsGroupbox: TGROUPBOX;
    OriginRadiogroup: TRADIOGROUP;
    DirectionRadiogroup: TRADIOGROUP;
    ScopeRadiogroup: TRADIOGROUP;
  private
  public
  end;

var
  MultiReplaceDialog: TMultiReplaceDialog;

implementation

{$R *.lfm}

end.

