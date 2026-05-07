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
  StdCtrls, ExtCtrls, LCLIntf;

type
  TCDPopUpMenuForm = class(TForm)
  public
    Items: array of TStaticText;
    Frame: TPanel;
    LCLMenu: TPopUpMenu;
    procedure HandleItemClick(ASender: TObject);
  end;

procedure TCDPopUpMenuForm.HandleItemClick(ASender: TObject);
var
  lSelectedItem: PtrInt;
begin
  Self.Close;
  lSelectedItem := TStaticText(ASender).Tag;
  { Fire the TMenuItem.Click for the picked entry. Without this, a
    direct TPopupMenu.PopUp(X, Y) call would visually open and dismiss
    correctly but never run the user's OnClick handler -- the LCL
    routes those through TMenuItem.Click. The ShowSelectItemDialog
    path (combobox dropdown) creates menu items with no OnClick, so
    Click is a no-op there and the dedicated callback below handles
    it instead. }
  if (LCLMenu <> nil)
     and (lSelectedItem >= 0) and (lSelectedItem < LCLMenu.Items.Count) then
    LCLMenu.Items[lSelectedItem].Click;
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
  if AMenu is TMainMenu then
    Result := HMENU(PtrUInt(AMenu))
  else
    // Fill a dummy value to get a positive result for HandleAllocated.
    Result := $FFFFFF;
end;

{ TCDWSPopupMenu }

class procedure TCDWSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X, Y: integer);
const
  BorderPx = 2;
var
  i, MaxWidth, CurWidth, ItemHeight: Integer;
  CurItem: TStaticText;
  CurCDPopUpMenu: TCDPopUpMenuForm;
  ItemCaption: String;
begin
  if APopUpMenu.Items.Count = 0 then Exit;

  CurCDPopUpMenu := TCDPopUpMenuForm.CreateNew(nil);
  CDPopUpMenus.Add(CurCDPopUpMenu);
  CurCDPopUpMenu.LCLMenu := APopupMenu;
  { BorderStyle=bsNone tells OS-decorating backends (X11, Win32) not
    to wrap the popup in a sizeable / titled window frame. The LCL
    default of bsSizeable is what shipped previously, producing
    inappropriately-decorated popups on those backends. The visible
    frame around the popup comes from the TPanel below. }
  CurCDPopUpMenu.BorderStyle := bsNone;
  CurCDPopUpMenu.Frame := TPanel.Create(CurCDPopUpMenu);
  CurCDPopUpMenu.Frame.Parent := CurCDPopUpMenu;
  CurCDPopUpMenu.Frame.Align := alClient;
  CurCDPopUpMenu.Frame.BevelOuter := bvRaised;
  CurCDPopUpMenu.Frame.BevelWidth := BorderPx;
  CurCDPopUpMenu.Frame.Caption := '';
  CurCDPopUpMenu.Left := X;
  CurCDPopUpMenu.Top := Y;
  ItemHeight := CurCDPopUpMenu.Canvas.TextHeight('Áç') + 5;
  CurCDPopUpMenu.Height := ItemHeight * APopUpMenu.Items.Count + 2 * BorderPx;
  MaxWidth := 0;

  SetLength(CurCDPopUpMenu.Items, APopUpMenu.Items.Count);
  for i := 0 to APopUpMenu.Items.Count-1 do
  begin
    CurItem := TStaticText.Create(CurCDPopUpMenu);
    CurCDPopUpMenu.Items[i] := CurItem;
    { Align=alTop stacks items inside the panel's bevel-adjusted client
      rect (TCustomPanel.AdjustClientRect insets by BevelWidth). This
      is the LCL-idiomatic way to keep children inside a TPanel's bevel
      -- explicit Left/Top are relative to the parent's outer bounds
      and would overpaint the bevel. AutoSize is off so each item
      contributes a uniform ItemHeight to the stack. }
    CurItem.AutoSize := False;
    CurItem.Height := ItemHeight;
    CurItem.Align := alTop;
    CurItem.Parent := CurCDPopUpMenu.Frame;
    ItemCaption := APopUpMenu.Items[i].Caption;
    DeleteAmpersands(ItemCaption);
    CurItem.Caption := ItemCaption;
    CurItem.Tag := i;
    CurItem.OnClick := @CurCDPopUpMenu.HandleItemClick;
    CurWidth := CurCDPopUpMenu.Canvas.TextWidth(CurItem.Caption);
    MaxWidth := Max(MaxWidth, CurWidth);
  end;

  CurCDPopUpMenu.Width := MaxWidth + 2 * BorderPx;

  CurCDPopUpMenu.Show;
end;

initialization

  CDPopUpMenus := TFPList.Create;

{$endif}

end.
