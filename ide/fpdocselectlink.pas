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
    Dialog used by the fpdoc editor to create a link.
}
unit FPDocSelectLink;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ButtonPanel;

type

  { TFPDocLinkEditorDlg }

  TFPDocLinkEditorDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    TitleEdit: TEdit;
    TitleLabel: TLabel;
    LinkEdit: TEdit;
    LinkLabel: TLabel;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

function ShowFPDocLinkEditorDialog(out Link, LinkTitle: string): TModalResult;

implementation

function ShowFPDocLinkEditorDialog(out Link, LinkTitle: string): TModalResult;
var
  FPDocLinkEditorDlg: TFPDocLinkEditorDlg;
begin
  Link:='';
  LinkTitle:='';
  FPDocLinkEditorDlg:=TFPDocLinkEditorDlg.Create(nil);
  try
    Result:=FPDocLinkEditorDlg.ShowModal;
    if Result=mrOk then begin
      Link:=FPDocLinkEditorDlg.LinkEdit.Text;
      LinkTitle:=FPDocLinkEditorDlg.TitleEdit.Text;
    end;
  finally
    FPDocLinkEditorDlg.Free;
  end;
end;

{ TFPDocLinkEditorDlg }

procedure TFPDocLinkEditorDlg.FormCreate(Sender: TObject);
begin
  Caption:='Choose a FPDoc link';
  LinkLabel.Caption:='Link target';
  LinkLabel.Hint:='Examples:'#13
                 +'Identifier'#13
                 +'TMyEnum.Enum'#13
                 +'Unitname.Identifier'#13
                 +'#PackageName.UnitName.Identifier';
  TitleLabel.Caption:='Title (leave empty for default)';
  ButtonPanel1.OKButton.Caption:='Ok';
  ButtonPanel1.CancelButton.Caption:='Cancel';
  ButtonPanel1.HelpButton.Caption:='Help';
  
  LinkEdit.Text:='';
  TitleEdit.Text:='';
end;

initialization
  {$I fpdocselectlink.lrs}

end.

