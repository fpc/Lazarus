{
/***************************************************************************
                             doceditor.pas
                             -------------

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
    The form for the documentation editor window.

}
unit DocEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls,
  LazarusIDEStrConsts, IDEProcs, EnvironmentOpts, IDEOptionDefs;

type
  TDocEditorWnd = class(TForm)
    ButtonsPanel: TPANEL;
    ContextTreeview: TTREEVIEW;
    FilesNotebook: TNOTEBOOK;
    ButtonImagelist: TIMAGELIST;
    procedure DocEditorWndCREATE(Sender: TObject);
  private
  public
  end;

var
  DocEditorWnd: TDocEditorWnd;

implementation

{ TDocEditorWnd }

procedure TDocEditorWnd.DocEditorWndCREATE(Sender: TObject);
begin
  Name:=NonModalIDEWindowNames[nmiwDocEditor];
  Caption := lisDocumentationEditor;
  EnvironmentOptions.IDEWindowLayoutList.Apply(Self,Name);

  ButtonImageList:=TImageList.Create(Self);
  with ButtonImageList do
  begin
    Name:='ButtonImageList';
    Width:=24
    Height:=24;
  end;
end;

initialization
  {$I doceditor.lrs}

end.

