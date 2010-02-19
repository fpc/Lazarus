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
unit frmSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, EditBtn, ButtonPanel;

type

  { TSourceForm }

  TSourceForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    EOptions: TEdit;
    FESource: TFileNameEdit;
    LFESource: TLabel;
    LEOptions: TLabel;
  private
    function GetOptions: String;
    function GetSource: String;
    procedure SetOptions(const AValue: String);
    procedure SetSource(const AValue: String);
    { private declarations }
  public
    { public declarations }
    Property Source : String Read GetSource Write SetSource;
    Property Options : String Read GetOptions Write SetOptions;
  end;

var
  SourceForm: TSourceForm;

implementation

{$R *.lfm}

{ TSourceForm }

function TSourceForm.GetOptions: String;
begin
  Result:=EOPtions.Text;
end;

function TSourceForm.GetSource: String;
begin
  Result:=FESource.Text;
end;

procedure TSourceForm.SetOptions(const AValue: String);
begin
  EOptions.Text:=AValue;
end;

procedure TSourceForm.SetSource(const AValue: String);
begin
  FESource.Text:=AValue;
end;


end.

