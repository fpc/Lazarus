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
unit frmExample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  EditBtn, StdCtrls;

type

  { TExampleForm }

  TExampleForm = class(TForm)
    BOK: TButton;
    BCancel: TButton;
    EFileName: TFileNameEdit;
    LEFileName: TLabel;
  private
    function GetExampleName: String;
    procedure SetExampleName(const AValue: String);
    { private declarations }
  public
    { public declarations }
    Property ExampleName : String Read GetExampleName Write SetExampleName;
  end; 

var
  ExampleForm: TExampleForm;

implementation

{ TExampleForm }


{ TExampleForm }

function TExampleForm.GetExampleName: String;
begin
  Result:=EFileName.FileName
end;

procedure TExampleForm.SetExampleName(const AValue: String);
begin
  EFileName.FileName:=AValue;
end;

initialization
  {$I frmexample.lrs}

end.

