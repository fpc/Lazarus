{
 *****************************************************************************
 *                            CustomDrawnWSMenus.pp                          *
 *                               ------------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CustomDrawnWSMenus;

{$mode objfpc}{$H+}

interface

{$I customdrawndefines.inc}

uses
  // Platform specific
  {$ifdef CD_Windows}Windows, customdrawn_WinProc,{$endif}
  {$ifdef CD_Cocoa}MacOSAll, CocoaAll, customdrawn_cocoaproc, CocoaGDIObjects, CocoaUtils,{$endif}
  // LCL
  SysUtils, Classes, Types, Math,
  LCLType, LCLProc, Graphics, Controls, Forms, Menus,
  // Widgetset
  WSMenus, WSLCLClasses;

type

  { TCDWSMenuItem }

  TCDWSMenuItem = class(TWSMenuItem)
  published
    class procedure AttachMenu(const AMenuItem: TMenuItem); override;
    class function CreateHandle(const AMenuItem: TMenuItem): HMENU; override;
    class procedure DestroyHandle(const AMenuItem: TMenuItem); override;
    class procedure SetCaption(const AMenuItem: TMenuItem; const ACaption: string); override;
    class procedure SetShortCut(const AMenuItem: TMenuItem; const ShortCutK1, ShortCutK2: TShortCut); override;
    class procedure SetVisible(const AMenuItem: TMenuItem; const Visible: boolean); override;
    class function SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean; override;
    class function SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean; override;
    class function SetRadioItem(const AMenuItem: TMenuItem; const RadioItem: boolean): boolean; override;
    class function SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean; override;
    class procedure UpdateMenuIcon(const AMenuItem: TMenuItem; const HasIcon: Boolean; const AIcon: TBitmap); override;
  end;

  { TCDWSMenu }

  TCDWSMenu = class(TWSMenu)
  published
    class function  CreateHandle(const AMenu: TMenu): HMENU; override;
{    class procedure SetBiDiMode(const AMenu: TMenu; UseRightToLeftAlign, UseRightToLeftReading : Boolean); override;}
  end;

  { TCDWSMainMenu }

  TCDWSMainMenu = class(TWSMainMenu)
  published
  end;

  { TCDWSPopupMenu }

  TCDWSPopupMenu = class(TWSPopupMenu)
  published
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); override;
  end;


implementation

{$ifdef CD_Cocoa}
  {$include customdrawnwsmenus_cocoa.inc}
  {$define CD_HasNativeWSMenusINC}
{$endif}
{$ifndef CD_HasNativeWSMenusINC}

uses
  StdCtrls, LCLIntf;

type
  TCDPopUpMenuForm = class(TForm)
  public
    Items: array of TStaticText;
    LCLMenu: TPopUpMenu;
    procedure HandleItemClick(ASender: TObject);
  end;

procedure TCDPopUpMenuForm.HandleItemClick(ASender: TObject);
var
  lSelectedItem: PtrInt;
begin
  Self.Close;
  lSelectedItem := TStaticText(ASender).Tag;
  if LCLIntf.OnShowSelectItemDialogResult <> nil then
    LCLIntf.OnShowSelectItemDialogResult(lSelectedItem);
end;

var
  CDPopUpMenus: TFPList; // of TCDPopUpMenuForm

{ TCDWSMenuItem }

class procedure TCDWSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
begin
  inherited AttachMenu(AMenuItem);
end;

class function TCDWSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
begin
  // Fill a dummy value to get a positive result for HandleAllocated
  Result := $FFFFFF;
end;

class procedure TCDWSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
begin

end;

class procedure TCDWSMenuItem.SetCaption(const AMenuItem: TMenuItem;
  const ACaption: string);
begin
  inherited SetCaption(AMenuItem, ACaption);
end;

class procedure TCDWSMenuItem.SetShortCut(const AMenuItem: TMenuItem;
  const ShortCutK1, ShortCutK2: TShortCut);
begin
  inherited SetShortCut(AMenuItem, ShortCutK1, ShortCutK2);
end;

class procedure TCDWSMenuItem.SetVisible(const AMenuItem: TMenuItem;
  const Visible: boolean);
begin
  inherited SetVisible(AMenuItem, Visible);
end;

class function TCDWSMenuItem.SetCheck(const AMenuItem: TMenuItem;
  const Checked: boolean): boolean;
begin
  Result:=inherited SetCheck(AMenuItem, Checked);
end;

class function TCDWSMenuItem.SetEnable(const AMenuItem: TMenuItem;
  const Enabled: boolean): boolean;
begin
  Result:=inherited SetEnable(AMenuItem, Enabled);
end;

class function TCDWSMenuItem.SetRadioItem(const AMenuItem: TMenuItem;
  const RadioItem: boolean): boolean;
begin
  Result:=inherited SetRadioItem(AMenuItem, RadioItem);
end;

class function TCDWSMenuItem.SetRightJustify(const AMenuItem: TMenuItem;
  const Justified: boolean): boolean;
begin
  Result:=inherited SetRightJustify(AMenuItem, Justified);
end;

class procedure TCDWSMenuItem.UpdateMenuIcon(const AMenuItem: TMenuItem;
  const HasIcon: Boolean; const AIcon: TBitmap);
begin
  inherited UpdateMenuIcon(AMenuItem, HasIcon, AIcon);
end;

{ TCDWSMenu }

class function TCDWSMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
  // Fill a dummy value to get a positive result for HandleAllocated
  Result := $FFFFFF;
end;

{ TCDWSPopupMenu }

class procedure TCDWSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X, Y: integer);
var
  i, CurY, MaxWidth, CurWidth, ItemHeight: Integer;
  CurItem: TStaticText;
  CurCDPopUpMenu: TCDPopUpMenuForm;
begin
  if APopUpMenu.Items.Count = 0 then Exit;

  CurCDPopUpMenu := TCDPopUpMenuForm.CreateNew(nil);
  CDPopUpMenus.Add(CurCDPopUpMenu);
  CurCDPopUpMenu.Left := X;
  CurCDPopUpMenu.Top := Y;
  ItemHeight := CurCDPopUpMenu.Canvas.TextHeight('Áç') + 5;
  CurCDPopUpMenu.Height := ItemHeight * APopUpMenu.Items.Count;
  CurY := 0;
  MaxWidth := 0;

  SetLength(CurCDPopUpMenu.Items, APopUpMenu.Items.Count);
  for i := 0 to APopUpMenu.Items.Count-1 do
  begin
    CurItem := TStaticText.Create(CurCDPopUpMenu);
    CurCDPopUpMenu.Items[i] := CurItem;
    CurItem.Top := CurY;
    Inc(CurY, ItemHeight);
    CurItem.Left := 0;
    CurItem.AutoSize := True;
    CurItem.Parent := CurCDPopUpMenu;
    CurItem.Caption := APopUpMenu.Items[i].Caption;
    CurItem.Tag := i;
    CurItem.OnClick := @CurCDPopUpMenu.HandleItemClick;
    CurWidth := CurCDPopUpMenu.Canvas.TextWidth(CurItem.Caption);
    MaxWidth := Max(MaxWidth, CurWidth);
  end;

  CurCDPopUpMenu.Width := MaxWidth;

  CurCDPopUpMenu.Show;
end;

initialization

  CDPopUpMenus := TFPList.Create;

{$endif}

end.
