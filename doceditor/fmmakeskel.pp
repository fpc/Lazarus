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
unit fmMakeskel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, EditBtn;

type

  { TMakeSkelForm }

  TMakeSkelForm = class(TForm)
    BOK: TButton;
    BCancel: TButton;
    CBDisableResults: TCheckBox;
    CBDisableArguments: TCheckBox;
    CBDisableSeeAlso: TCheckBox;
    CBDisableProtected: TCheckBox;
    CBDisablePrivate: TCheckBox;
    CBDisableErrors: TCheckBox;
    EAdditionalOptions: TEdit;
    EPackage: TEdit;
    FEinputFile: TFileNameEdit;
    FEoutputFIle: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LEPackage: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MakeSkelForm: TMakeSkelForm;

implementation

initialization
  {$I fmmakeskel.lrs}

end.

