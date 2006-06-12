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

  Abstract:
    This example demonstrates the html help components using the turbo power
    ipro browser component to show local html files
}

unit HtmlHelp2Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  LazHelpHTML, StdCtrls, Buttons, HelpIntfs, HtmlHelp2Viewer;

type

  { TForm1 }

  TForm1 = class(TForm)
    HelpButton: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    HTMLHelpDatabase1: THTMLHelpDatabase;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.HelpButtonClick(Sender: TObject);
begin
  // This demonstrates how to show a help item manually:
  ShowHelpOrErrorForKeyword('','HTML/index.html');  // HTML is case sensitive
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  HtmlHelp2Viewer.RegisterHelpViewer;  // This registers the help viewer
                                       // using the iPro viewer
end;

initialization
  {$I htmlhelp2unit1.lrs}

end.

