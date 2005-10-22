{ Copyright (C) 2005 Mattias Gaertner

  This source is free software; you can redistribute it and/or modifyit under
  the terms of the GNU General Public License as published bythe Free Software
  Foundation; either version 2 of the License, or(at your option) any later
  version.

  This code is distributed in the hope that it will be useful, butWITHOUT ANY
  WARRANTY; without even the implied warranty ofMERCHANTABILITY or FITNESS FOR
  A PARTICULAR PURPOSE.  See the GNUGeneral Public License for more details.

  A copy of the GNU General Public License is available on the WorldWide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can alsoobtain it by writing
  to the Free Software Foundation,Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.

  ---------------------------------------------------------------------------
  Abstract:
    Demonstrates LCL TWinControl.ChildSizing.Layout property.
}
unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, RTTICtrls, ExtCtrls, RTTIGrids;

type

  { TChildsizingLayoutDemoForm }

  TChildsizingLayoutDemoForm = class(TForm)
    ChildSizingGroupBox: TGroupBox;
    LayoutGroupBox: TGroupBox;
    LayoutLabel: TLabel;
    LayoutTIRadioGroup: TTIRadioGroup;
    ButtonCountRadioGroup: TRadioGroup;
    ControlsPerLineTIRadioGroup: TTIRadioGroup;
    ChildSizingTIPropertyGrid: TTIPropertyGrid;
    procedure ButtonCountRadioGroupClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    procedure SetButtonCount(NewCount: integer);
  end;

var
  ChildsizingLayoutDemoForm: TChildsizingLayoutDemoForm;

implementation

{ TChildsizingLayoutDemoForm }

procedure TChildsizingLayoutDemoForm.FormCreate(Sender: TObject);
begin
  LayoutTIRadioGroup.Link.SetObjectAndProperty(LayoutGroupBox.ChildSizing,'Layout');
  ControlsPerLineTIRadioGroup.Link.SetObjectAndProperty(LayoutGroupBox.ChildSizing,'ControlsPerLine');
  ChildSizingTIPropertyGrid.TIObject:=LayoutGroupBox.ChildSizing;

  SetButtonCount(3);
end;

procedure TChildsizingLayoutDemoForm.SetButtonCount(NewCount: integer);
var
  i: Integer;
  x: Integer;
begin
  if NewCount=LayoutGroupBox.ControlCount then exit;
  if ButtonCountRadioGroup.Items.IndexOf(IntToStr(NewCount))<0 then
    NewCount:=StrToIntDef(ButtonCountRadioGroup.Items[2],3);

  LayoutGroupBox.DisableAlign;
  // create buttons
  for i:=0 to NewCount-1 do begin
    if LayoutGroupBox.ControlCount=i then begin
      with TButton.Create(Self) do begin
        if LayoutGroupBox.ChildSizing.Layout=cclNone then begin
          x:=i*20;
          SetBounds(x,x,Width,Height);
        end;
        Name:='Button'+IntToStr(i);
        Parent:=LayoutGroupBox;
      end;
    end;
    // set a caption of various length
    LayoutGroupBox.Controls[i].Caption:=
                            copy(LayoutGroupBox.Controls[i].Name,1,(i mod 5)+3);
  end;
  // free unneeded buttons
  while LayoutGroupBox.ControlCount>NewCount do
    LayoutGroupBox.Controls[LayoutGroupBox.ControlCount-1].Free;
  LayoutGroupBox.EnableAlign;

  // make sure ButtonCountRadioGroup shows the correct count
  ButtonCountRadioGroup.ItemIndex:=
                        ButtonCountRadioGroup.Items.IndexOf(IntToStr(NewCount));
end;

procedure TChildsizingLayoutDemoForm.ButtonCountRadioGroupClick(Sender: TObject);
begin
  SetButtonCount(StrToIntDef(
               ButtonCountRadioGroup.Items[ButtonCountRadioGroup.ItemIndex],3));
end;

initialization
  {$I mainunit.lrs}

end.

