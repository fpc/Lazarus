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

  Author: Michael Van Canneyt
}
unit FrmLink;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ButtonPanel;

type
  { TLinkForm }

  TLinkForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CBTarget: TComboBox;
    ELinkText: TEdit;
    LLinkTarget: TLabel;
    LELinkText: TLabel;
  private
    function GetELT: Boolean;
    function GetL: TStrings;
    function GetLL: String;
    function GetLT: String;
    procedure SetELT(const AValue: Boolean);
    procedure SetL(const AValue: TStrings);
    procedure SetLL(const AValue: String);
    procedure SetLT(const AValue: String);
    { private declarations }
  public
    { public declarations }
    Property Links : TStrings Read GetL Write SetL;
    property Link : String Read GetLL Write SetLL;
    Property LinkText :  String Read GetLT Write SetLT;
    Property EnableLinkText : Boolean Read GetELT Write SetELT;
  end; 

var
  LinkForm: TLinkForm;

implementation

{$R *.lfm}

{ TLinkForm }

function TLinkForm.GetELT: Boolean;
begin
  Result:=ELinkText.Enabled;
end;

function TLinkForm.GetL: TStrings;
begin
  Result:=CBTarget.Items;
end;

function TLinkForm.GetLL: String;
begin
  Result:=CBTarget.Text;
end;

function TLinkForm.GetLT: String;
begin
  Result:=ELinkText.Text;
end;

procedure TLinkForm.SetELT(const AValue: Boolean);
begin
  ELinkText.Enabled:=AValue
end;

procedure TLinkForm.SetL(const AValue: TStrings);
begin
  CBTarget.Items.Assign(AValue);
end;

procedure TLinkForm.SetLL(const AValue: String);
begin
  CBTarget.Text:=AValue;
end;

procedure TLinkForm.SetLT(const AValue: String);
begin
  ELinkText.Text:=AValue;
end;

end.

