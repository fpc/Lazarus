{  $Id$  }
{
 /***************************************************************************
                          componentpalette.pas
                          --------------------


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

}
unit ComponentPalette;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Graphics, ExtCtrls, Buttons,
  ComponentReg, PackageDefs, LazarusIDEStrConsts;

const
  ComponentPaletteBtnWidth  = 25;
  ComponentPaletteBtnHeight = 25;

type
  TComponentPalette = class(TBaseComponentPalette)
  private
    FNoteBook: TNotebook;
    fNoteBookNeedsUpdate: boolean;
    procedure SetNoteBook(const AValue: TNotebook);
    procedure SelectionToolClick(Sender: TObject);
    procedure ComponentBtnClick(Sender: TObject);
  protected
    procedure DoEndUpdate(Changed: boolean); override;
  public
    procedure UpdateNoteBookButtons;
    property NoteBook: TNotebook read FNoteBook write SetNoteBook;
  end;

implementation

{ TComponentPalette }

procedure TComponentPalette.SetNoteBook(const AValue: TNotebook);
begin
  if FNoteBook=AValue then exit;
  FNoteBook:=AValue;
  UpdateNoteBookButtons;
end;

procedure TComponentPalette.SelectionToolClick(Sender: TObject);
begin

end;

procedure TComponentPalette.ComponentBtnClick(Sender: TObject);
begin

end;

procedure TComponentPalette.DoEndUpdate(Changed: boolean);
begin
  if Changed then UpdateNoteBookButtons;
  inherited DoEndUpdate(Changed);
end;

procedure TComponentPalette.UpdateNoteBookButtons;
var
  i: Integer;
  PageIndex: Integer;
  CurPage: TBaseComponentPage;
  CurNoteBookPage: TPage;
  CurComponent: TPkgComponent;
  CurBtn: TSpeedButton;
  ButtonX: Integer;
  CurPageIndex: Integer;
  j: Integer;
begin
  if IsUpdating then begin
    fNoteBookNeedsUpdate:=true;
    exit;
  end;
  fNoteBookNeedsUpdate:=false;
  if FNoteBook=nil then begin
    ClearButtons;
    exit;
  end;
  // remove every page in the notebook without a visible page
  for i:=FNoteBook.PageCount-1 downto 0 do begin
    PageIndex:=IndexOfPageComponent(FNoteBook.Page[i]);
    if (PageIndex<0) or (not Pages[PageIndex].Visible) then begin
      if PageIndex>=0 then
        Pages[i].PageComponent:=nil;
      FNoteBook.Pages.Delete(i);
    end;
  end;
  // insert a notebook page for every visible palette page
  PageIndex:=0;
  for i:=0 to Count-1 do begin
    if not Pages[i].Visible then continue;
    if Pages[i].PageComponent=nil then begin
      // insert a new notebook page
      FNoteBook.Pages.Insert(PageIndex,Pages[i].PageName);
    end else begin
      // move to the right position
      CurPageIndex:=TPage(Pages[i].PageComponent).PageIndex;
      if CurPageIndex<>PageIndex then
        FNoteBook.Pages.Move(CurPageIndex,PageIndex);
    end;
    inc(PageIndex);
  end;
  // create a speedbutton for every visible component
  for i:=0 to Count-1 do begin
    CurPage:=Pages[i];
    if not CurPage.Visible then continue;
    CurNoteBookPage:=TPage(CurPage.PageComponent);
    ButtonX:=0;
    // create selection button
    if CurPage.SelectButton=nil then begin
      CurBtn:=TSpeedButton.Create(nil);
      CurPage.SelectButton:=CurBtn;
      with CurBtn do begin
        Name:='PaletteSelectBtn'+IntToStr(i);
        Parent:=CurNoteBookPage;
        OnClick := @SelectionToolClick;
        Glyph.LoadFromLazarusResource('tmouse');
        Flat := True;
        GroupIndex:= 1;
        Down := True;
        Hint := lisSelectionTool;
        SetBounds(ButtonX,0,ComponentPaletteBtnWidth,ComponentPaletteBtnHeight);
      end;
    end;
    inc(ButtonX,ComponentPaletteBtnWidth+10);
    // create component buttons
    for j:=0 to CurPage.Count-1 do begin
      CurComponent:=TPkgComponent(CurPage[j]);
      if CurComponent.Visible then begin
        if CurComponent.Button=nil then begin
          CurBtn:=TSpeedButton.Create(nil);
          CurComponent.Button:=CurBtn;
          with CurBtn do begin
            Name:='PaletteBtnPage'+IntToStr(i)+'_'+IntToStr(j)
                  +'_'+CurComponent.ComponentClass.ClassName;
            Parent := CurNoteBookPage;
            Glyph:=CurComponent.GetIconCopy;
            Width := ComponentPaletteBtnWidth;
            Height := ComponentPaletteBtnHeight;
            GroupIndex := 1;
            Flat := true;
            OnClick := @ComponentBtnClick;
            Hint := CurComponent.ComponentClass.ClassName;
            SetBounds(ButtonX,0,ComponentPaletteBtnWidth,ComponentPaletteBtnHeight);
            inc(ButtonX,ComponentPaletteBtnWidth+2);
          end;
        end;
      end else if CurComponent.Button<>nil then begin
        CurComponent.Button.Free;
        CurComponent.Button:=nil;
      end;
    end;
  end;
end;

end.

