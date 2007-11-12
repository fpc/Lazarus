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

  Author: Tom Gregorovic
}
unit UnitMove;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LCLType, LCLIntf;

type

  { TFormMove }

  TFormMove = class(TForm)
    ButtonYes: TButton;
    ButtonNo: TButton;
    ComboBoxJump: TComboBox;
    LabelSrcElement: TLabel;
    LabelDest: TLabel;
    LabelSrc: TLabel;
    ListBoxDest: TListBox;
    StaticText: TStaticText;
    procedure ComboBoxJumpSelect(Sender: TObject);
    procedure ListBoxDestDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormMove: TFormMove;

implementation

{ TFormMove }

procedure TFormMove.ListBoxDestDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
  State: TOwnerDrawState);
begin
  if (Index < 0) or (Index >= ListBoxDest.Items.Count) then Exit;

  with ListBoxDest.Canvas do
  begin
    if odSelected in State then
      Brush.Color := clHighlight
    else
    begin
      Brush.Color := ListBoxDest.Color;
      case Integer(ListBoxDest.Items.Objects[Index]) of
      0: SetTextColor(ListBoxDest.Canvas.Handle, ListBoxDest.Canvas.Font.Color); // empty
      1: SetTextColor(ListBoxDest.Canvas.Handle, clRed);                         // nonempty
      end;
    end;

    FillRect(ARect);
    TextRect(ARect, ARect.Left + 8, ARect.Top + 2, ListBoxDest.Items[Index]);
  end;
end;

procedure TFormMove.ComboBoxJumpSelect(Sender: TObject);
begin
  ListBoxDest.ItemIndex := ListBoxDest.Items.IndexOf(ComboBoxJump.Text);
end;

initialization
  {$I unitmove.lrs}

end.

