{ /***************************************************************************
                 AnchorEditor.pas - Lazarus IDE unit
                 -----------------------------------

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
    Editor for editing Anchors, AnchorSide properties.
}
unit AnchorEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, Spin;

type

  { TAnchorDesigner }

  TAnchorDesigner = class(TForm)
    BorderSpaceGroupBox: TGroupBox;
    BottomAnchoredCheckBox: TCheckBox;
    BottomBorderSpaceSpinEdit: TSpinEdit;
    BottomGroupBox: TGroupBox;
    BottomRefBottomSpeedButton: TSpeedButton;
    BottomRefCenterSpeedButton: TSpeedButton;
    BottomRefTopSpeedButton: TSpeedButton;
    BottomSiblingComboBox: TComboBox;
    BottomSiblingLabel: TLabel;
    DecAllBorderSpaceSpeedButton: TSpeedButton;
    IncAllBorderSpaceSpeedButton: TSpeedButton;
    LeftAnchoredCheckBox: TCheckBox;
    LeftBorderSpaceSpinEdit: TSpinEdit;
    LeftGroupBox: TGroupBox;
    LeftRefCenterSpeedButton: TSpeedButton;
    LeftRefLeftSpeedButton: TSpeedButton;
    LeftRefRightSpeedButton: TSpeedButton;
    LeftSiblingComboBox: TComboBox;
    LeftSiblingLabel: TLabel;
    RightAnchoredCheckBox: TCheckBox;
    RightBorderSpaceSpinEdit: TSpinEdit;
    RightGroupBox: TGroupBox;
    RightRefCenterSpeedButton: TSpeedButton;
    RightRefLeftSpeedButton: TSpeedButton;
    RightRefRightSpeedButton: TSpeedButton;
    RightSiblingComboBox: TComboBox;
    RightSiblingLabel: TLabel;
    TopAnchoredCheckBox: TCheckBox;
    TopBorderSpaceSpinEdit: TSpinEdit;
    TopGroupBox: TGroupBox;
    TopRefBottomSpeedButton: TSpeedButton;
    TopRefCenterSpeedButton: TSpeedButton;
    TopRefTopSpeedButton: TSpeedButton;
    TopSiblingComboBox: TComboBox;
    TopSiblingLabel: TLabel;
    procedure AnchorDesignerCreate(Sender: TObject);
  private
  public
  end;

var
  AnchorDesigner: TAnchorDesigner;

implementation

{ TAnchorDesigner }

procedure TAnchorDesigner.AnchorDesignerCreate(Sender: TObject);
begin

end;

initialization
  {$I anchoreditor.lrs}

end.

