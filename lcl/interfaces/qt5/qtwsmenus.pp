{
 *****************************************************************************
 *                               QtWSMenus.pp                                * 
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
unit QtWSMenus;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Bindings
  qt5,
  qtwidgets, qtobjects, qtproc, QtWsControls,
  // LCL
  SysUtils, Classes, Types, LCLType, LCLProc, Graphics, Controls, Forms, Menus,
  ImgList,
  // Widgetset
  WSMenus, WSLCLClasses;

type

  { TQtWSMenuItem }

  TQtWSMenuItem = class(TWSMenuItem)
  protected
    class function CreateMenuFromMenuItem(const AMenuItem: TMenuItem): TQtMenu;
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

  { TQtWSMenu }

  TQtWSMenu = class(TWSMenu)
  published
    class function  CreateHandle(const AMenu: TMenu): HMENU; override;
    class procedure SetBiDiMode(const AMenu: TMenu; UseRightToLeftAlign, UseRightToLeftReading : Boolean); override;
  end;

  { TQtWSMainMenu }

  TQtWSMainMenu = class(TWSMainMenu)
  published
  end;

  { TQtWSPopupMenu }

  TQtWSPopupMenu = class(TWSPopupMenu)
  published
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); override;
  end;


implementation
uses math;

{ TQtWSMenuItem }

{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.AttachMenu
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
var
  Widget: TQtWidget;
begin
  if not WSCheckMenuItem(AMenuItem, 'AttachMenu') or (AMenuItem.Parent = nil) then
    Exit;
  Widget := TQtWidget(AMenuItem.Parent.Handle);
  if Widget is TQtMenuBar then
    TQtMenuBar(Widget).insertMenu(AMenuItem.Parent.VisibleIndexOf(AMenuItem),
      QMenuH(TQtMenu(AMenuItem.Handle).Widget))
  else
  if Widget is TQtMenu then
    TQtMenu(Widget).insertMenu(AMenuItem.Parent.VisibleIndexOf(AMenuItem),
      QMenuH(TQtMenu(AMenuItem.Handle).Widget), AMenuItem);
end;

class function TQtWSMenuItem.CreateMenuFromMenuItem(const AMenuItem: TMenuItem): TQtMenu;
var
  ImgList: TCustomImageList;
  AImage: TQtImage;
  Bmp: TBitmap;
  AImgList: TImageList;
  ADesktop: QDesktopWidgetH;
  ANumScreens, i, j: Integer;
  AScreen: QWidgetH;
  AWindow: QWindowH;
  DPR: QReal;
  AIcon: QIconH;
  APixmap, AScaledPixmap: QPixmapH;
  TargetPhysW, TargetPhysH: Int64;
  ADPIProcessed: array of double;
  AlreadyProcessed: Boolean;
begin
  Result := TQtMenu.Create(AMenuItem);
  Result.FDeleteLater := False;
  Result.setSeparator(AMenuItem.IsLine);
  Result.setHasSubmenu(AMenuItem.Count > 0);
  if not AMenuItem.IsLine then
  begin
    Result.setText(AMenuItem.Caption{%H-});
    Result.setEnabled(AMenuItem.Enabled);
    Result.setCheckable(AMenuItem.RadioItem or AMenuItem.ShowAlwaysCheckable);
    Result.BeginUpdate;
    Result.setChecked(AMenuItem.Checked);
    Result.EndUpdate;
    Result.setShortcut(AMenuItem.ShortCut, AMenuItem.ShortCutKey2);
    if AMenuItem.HasIcon then
    begin
      ImgList := AMenuItem.GetImageList;
      // we must check so because AMenuItem.HasIcon can return true
      // if Bitmap is setted up but not ImgList.
      if (ImgList <> nil) and (AMenuItem.ImageIndex >= 0) and
        (AMenuItem.ImageIndex < ImgList.Count) then
      begin
        ADPIProcessed := [];
        AIcon := QIcon_Create();
        Bmp := TBitmap.Create;
        try
          ImgList.ResolutionForPPI[16, ScreenInfo.PixelsPerInchX, 1.00].GetBitmap(AMenuItem.ImageIndex, Bmp); // Qt bindings support only 16px icons for menu items
          APixmap := TQtImage(Bmp.Handle).AsPixmap();
          QIcon_addPixmap(AIcon, APixmap, QIconNormal, QIconOn);
          QPixmap_destroy(APixmap);
          SetLength(ADPIProcessed, 1);
          ADPIProcessed[0] := 1.00;
        finally
          Bmp.Free;
        end;
        ADesktop := QApplication_desktop();
        ANumScreens := QDesktopWidget_screenCount(ADesktop);
        for i := 0 to ANumScreens - 1 do
        begin
          AScreen := QDesktopWidget_screen(ADesktop, i);
          AWindow := QWidget_windowHandle(AScreen);

          if AWindow = nil then
            continue;

          AlreadyProcessed := False;
          DPR := QWindow_devicePixelRatio(AWindow);
          for j := 0 to High(ADPIProcessed) do
          begin
            if SameValue(ADPIProcessed[j], DPR, 0.01) then
            begin
              AlreadyProcessed := True;
              break;
            end;
          end;
          if not AlreadyProcessed and (DPR > 1.0) and not SameValue(DPR, 1.00, 0.01) then
          begin
            SetLength(ADPIProcessed,length(ADPIProcessed) + 1);
            ADPIProcessed[High(ADPIProcessed)] := DPR;
            Bmp := TBitmap.Create;
            ImgList.ResolutionForPPI[16, 96, 1.0].GetBitmap(AMenuItem.ImageIndex, Bmp);

            APixmap := TQtImage(Bmp.Handle).AsPixmap();

            TargetPhysW := Round(QPixmap_width(APixmap) * DPR);
            TargetPhysH := Round(QPixmap_height(APixmap) * DPR);

            AScaledPixmap := QPixmap_create();
            QPixmap_scaled(APixmap, AScaledPixmap, TargetPhysW, TargetPhysH, QtIgnoreAspectRatio, QtSmoothTransformation);
            QPixmap_setDevicePixelRatio(AScaledPixmap, DPR);
            QIcon_addPixmap(AIcon, AScaledPixmap, QIconNormal, QIconOn);

            QPixmap_destroy(AScaledPixmap);
            QPixmap_destroy(APixmap);
            Bmp.Free;
          end;
        end;
        if not QIcon_isNull(AIcon) then
          Result.setIcon(AIcon);
        QIcon_destroy(AIcon);
      end else
      if Assigned(AMenuItem.Bitmap) then
      begin
        AImage := TQtImage(AMenuItem.Bitmap.Handle);
        if not AMenuItem.Bitmap.Transparent then
        begin
          AImgList := TImageList.Create(nil);
          AImgList.Width := AMenuItem.Bitmap.Width;
          AImgList.Height := AMenuItem.Bitmap.Height;
          AImgList.AddMasked(AMenuItem.Bitmap, AMenuItem.Bitmap.Canvas.Pixels[0, AMenuItem.Bitmap.Height -1]);
          Bmp := TBitmap.Create;
          AImgList.GetBitmap(0, Bmp);
          AImage := TQtImage(Bmp.Handle);
          Result.setImage(AImage);
          Bmp.Free;
          AImgList.Free;
        end else
          Result.setImage(AImage);
      end;
    end else
      Result.setImage(nil);
  end;
end;
{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.CreateHandle
  Params:  None
  Returns: Nothing

  Creates a Menu Item
 ------------------------------------------------------------------------------}
class function TQtWSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
var
  Menu: TQtMenu;
begin
  {$ifdef VerboseQt}
    WriteLn('trace:> [TQtWSMenuItem.CreateHandle] Caption: ', AMenuItem.Caption,
     ' Subitems: ' + IntToStr(AMenuItem.Count) +' HandleAllocated ? ', AMenuItem.HandleAllocated);

    Write('trace:< [TQtWSMenuItem.CreateHandle]');
  {$endif}
  
  Menu := nil;

  {------------------------------------------------------------------------------
    This case should not happen. A menu item must have a parent, but it seams LCL
   will sometimes create a menu item prior to creating it's parent.
    So, if we arrive here, we must create this item as if it was a TMenu
   ------------------------------------------------------------------------------}
  if (not AMenuItem.HasParent) then
  begin
    {$ifdef VerboseQt}
      Write(' Parent: Menu without parent');
    {$endif}

    Result := TQtWSMenu.CreateHandle(AMenuItem.GetParentMenu);
  end
  {------------------------------------------------------------------------------
    If the parent has no parent, then this item is directly owned by a TMenu
    In this case we have to detect if the parent is a TMainMenu or a TPopUpMenu
   because TMainMenu uses the special Handle QMenuBar while TPopUpMenu can be
   treat like if this menu item was a subitem of another item
   ------------------------------------------------------------------------------}
  else
  if ((not AMenuItem.Parent.HasParent) and (AMenuItem.GetParentMenu is TMainMenu)) then
  begin
    Menu := CreateMenuFromMenuItem(AMenuItem);
    Result := HMENU(Menu);
  end
  {------------------------------------------------------------------------------
    If the parent has a parent, then that item's Handle is necessarely a TQtMenu
   ------------------------------------------------------------------------------}
  else
  begin
    Menu := CreateMenuFromMenuItem(AMenuItem);
    Result := HMENU(Menu);
  end;
  
  if Menu <> nil then
    Menu.AttachEvents;

  {$ifdef VerboseQt}
    WriteLn(' Result: ', dbghex(Result));
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.DestroyHandle
  Params:  None
  Returns: Nothing

  Dealocates a Menu Item
 ------------------------------------------------------------------------------}
class procedure TQtWSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
var
  Obj: TObject;
begin
  {$ifdef VerboseQt}
    WriteLn('[TQtWSMenuItem.DestroyHandle] Caption: ' + AMenuItem.Caption);
  {$endif}

  if Assigned(AMenuItem.Owner) then
  begin
    if (AMenuItem.Owner is TMainMenu) and
      Assigned(TMainMenu(AMenuItem.Owner).Parent) and
      (
      (TMainMenu(AMenuItem.Owner).Parent is TCustomForm) or
      (TMainMenu(AMenuItem.Owner).Parent is TCustomFrame)
      )
       then
    begin
      {do not destroy menuitem handle if parent form handle = 0 - it's
       already destroyed (TCustomForm.DestroyWnd isn't called when
       LM_DESTROY is sent from TQtWidget.SlotDestroy() }
      if not TWinControl(TMainMenu(AMenuItem.Owner).Parent).HandleAllocated then
        exit;
    end;
  end;

  Obj := TObject(AMenuItem.Handle);
  if Obj is TQtMenu then
    TQtMenu(Obj).Release;
end;

{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.SetCaption
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSMenuItem.SetCaption(const AMenuItem: TMenuItem; const ACaption: string);
var
  Widget: TQtWidget;
begin
  {$ifdef VerboseQt}
    WriteLn('[TQtWSMenuItem.SetCaption] Caption: ' + AMenuItem.Caption + ' NewCaption: ', ACaption);
  {$endif}

  if not WSCheckMenuItem(AMenuItem, 'SetCaption') then
    Exit;

  Widget := TQtWidget(AMenuItem.Handle);
  if Widget is TQtMenu then
  begin
    TQtMenu(Widget).setSeparator(ACaption = cLineCaption);
    if ACaption = cLineCaption then
      TQtMenu(Widget).setText('')
    else
      TQtMenu(Widget).setText(ACaption{%H-});
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.SetShortCut
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSMenuItem.SetShortCut(const AMenuItem: TMenuItem;
    const ShortCutK1, ShortCutK2: TShortCut);
var
  Widget: TQtWidget;
begin
  {$ifdef VerboseQt}
    WriteLn('[TQtWSMenuItem.SetCaption] SetShortCut: ' + AMenuItem.Caption);
  {$endif}

  if not WSCheckMenuItem(AMenuItem, 'SetShortCut') then
    Exit;

  Widget := TQtWidget(AMenuItem.Handle);
  if Widget is TQtMenu then
    TQtMenu(Widget).setShortcut(ShortCutK1, ShortCutK2);
end;

{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.SetVisible
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSMenuItem.SetVisible(const AMenuItem: TMenuItem; const Visible: boolean);
begin
  {$ifdef VerboseQt}
    WriteLn('[TQtWSMenuItem.SetVisible] SetShortCut: ' + AMenuItem.Caption + ' Visible: ', Visible);
  {$endif}
  if not WSCheckMenuItem(AMenuItem, 'SetVisible') then
    Exit;
    
  TQtMenu(AMenuItem.Handle).setVisible(Visible);
end;

{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.SetCheck
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSMenuItem.SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean;
begin
  Result := False;

  if not WSCheckMenuItem(AMenuItem, 'SetCheck') then
    Exit;

  TQtMenu(AMenuItem.Handle).BeginUpdate;
  TQtMenu(AMenuItem.Handle).setChecked(Checked);
  TQtMenu(AMenuItem.Handle).EndUpdate;

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.SetEnable
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSMenuItem.SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean;
begin
  Result := False;

  if not WSCheckMenuItem(AMenuItem, 'SetEnable') then
    Exit;

  TQtMenu(AMenuItem.Handle).setEnabled(Enabled);

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.SetRadioItem
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSMenuItem.SetRadioItem(const AMenuItem: TMenuItem; const RadioItem: boolean): boolean;
begin
  Result := False;

  if not WSCheckMenuItem(AMenuItem, 'SetRadioItem') then
    Exit;

  {$ifdef VerboseQt}
    WriteLn('[TQtWSMenuItem.SetRadioItem] AMenuItem: ' + AMenuItem.Name +
      ' Radio ? ',RadioItem);
  {$endif}

  if not RadioItem then
    TQtMenu(AMenuItem.Handle).removeActionGroup;

  TQtMenu(AMenuItem.Handle).setCheckable(RadioItem or AMenuItem.ShowAlwaysCheckable);
  SetCheck(AMenuItem, AMenuItem.Checked);
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.SetRightJustify
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSMenuItem.SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean;
begin
  if not WSCheckMenuItem(AMenuItem, 'SetRightJustify') then
    Exit(False);

  // what should be done here? maybe this?
  TQtMenu(AMenuItem.Handle).setAttribute(QtWA_RightToLeft, Justified);
  Result := True;
end;

class procedure TQtWSMenuItem.UpdateMenuIcon(const AMenuItem: TMenuItem;
  const HasIcon: Boolean; const AIcon: TBitmap);
begin
  if not WSCheckMenuItem(AMenuItem, 'UpdateMenuIcon') then
    Exit;
  if AMenuItem.HasParent then
  begin
    if Assigned(TQtMenu(AMenuItem.handle).actionHandle) then
      QAction_setVisible(TQtMenu(AMenuItem.handle).actionHandle, False);
    AMenuItem.RecreateHandle;
  end;
end;

{ TQtWSMenu }

{------------------------------------------------------------------------------
  Function: TQtWSMenu.CreateHandle
  Params:  None
  Returns: Nothing

  Creates a Menu
 ------------------------------------------------------------------------------}
class function TQtWSMenu.CreateHandle(const AMenu: TMenu): HMENU;
var
  MenuBar: TQtMenuBar;
  Menu: TQtMenu;
  AParent: TComponent;
begin
  Result := 0;
  { If the menu is a main menu, there is no need to create a handle for it.
    It's already created on the window }
  if (AMenu is TMainMenu) then
  begin
    AParent := AMenu.Parent;
    if AParent = nil then
      AParent := AMenu.Owner;
    if Assigned(AParent) and
      ((AParent is TCustomForm) or (AParent is TCustomFrame)) then
    begin
      if (AParent is TCustomForm) then
        MenuBar := TQtMainWindow(TCustomForm(AParent).Handle).MenuBarNeeded
      else
        MenuBar := TQtMainWindow(TCustomFrame(AParent).Handle).MenuBarNeeded;
      Result := HMENU(MenuBar);
    end else
    begin
      Menu := TQtMenu.Create(AMenu.Items);
      Menu.AttachEvents;
      Result := HMENU(Menu);
    end;
  end else
  if (AMenu is TPopUpMenu) then
  begin
    Menu := TQtMenu.Create(AMenu.Items);
    Menu.AttachEvents;
    Result := HMENU(Menu);
  end;

  {$ifdef VerboseQt}
    Write('[TQtWSMenu.CreateHandle] ');
    if (AMenu is TMainMenu) then Write('IsMainMenu ');
    WriteLn(' Handle: ', dbghex(Result), ' Name: ', AMenu.Name);
  {$endif}
end;

class procedure TQtWSMenu.SetBiDiMode(const AMenu : TMenu; UseRightToLeftAlign,
  UseRightToLeftReading : Boolean);
begin
  TQtWidget(AMenu.Handle).setLayoutDirection(TLayoutDirectionMap[UseRightToLeftAlign]);
end;


{ TQtWSPopupMenu }

{------------------------------------------------------------------------------
  Function: TQtWSPopupMenu.Popup
  Params:  None
  Returns: Nothing

  Creates a PopUp menu
 ------------------------------------------------------------------------------}
class procedure TQtWSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X, Y: integer);
var
  Point: TQtPoint;
  Size: TSize;
  Alignment: TPopupAlignment;
begin
  {$ifdef VerboseQt}
    WriteLn('[TQtWSPopupMenu.Popup] APopupMenu.Handle ' + dbghex(APopupMenu.Handle)
     + ' FirstItemName: ' + APopupMenu.Items.Name
     + ' FirstItemWND: ' + IntToStr(APopupMenu.Items.Handle)
     + ' FirstItemCount: ' + IntToStr(APopupMenu.Items.Count));
  {$endif}

  Point.X := X;
  Point.Y := Y;
  Alignment := APopupMenu.Alignment;

  if APopupMenu.IsRightToLeft then
  begin
    if Alignment = paLeft then
      Alignment := paRight
    else
    if Alignment = paRight then
      Alignment := paLeft;
  end;

  case Alignment of
    paCenter:
      begin
        QMenu_sizeHint(QMenuH(TQtMenu(APopupMenu.Handle).Widget), @Size);
        Point.X := Point.X - (Size.cx div 2);
      end;
    paRight:
      begin
        QMenu_sizeHint(QMenuH(TQtMenu(APopupMenu.Handle).Widget), @Size);
        Point.X := Point.X - Size.cx;
      end;
  end;

  if APopupMenu.TrackButton = tbLeftButton then
    TQtMenu(APopupMenu.Handle).trackButton := QtLeftButton
  else
    TQtMenu(APopupMenu.Handle).trackButton := QtRightButton;

  // for win32 compatibility do a blocking call
  TQtMenu(APopupMenu.Handle).Exec(@Point);
end;

end.
