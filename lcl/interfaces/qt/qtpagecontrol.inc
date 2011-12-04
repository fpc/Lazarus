{%MainUnit qtwscomctrls.pp}
{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

const
  QTabWidgetTabPositionMap: array[TTabPosition] of QTabWidgetTabPosition =
  (
{ tpTop    } QTabWidgetNorth,
{ tpBottom } QTabWidgetSouth,
{ tpLeft   } QTabWidgetWest,
{ tpRight  } QTabWidgetEast
  );

{ TQtWSCustomPage }

{------------------------------------------------------------------------------
  Method: TQtWSCustomPage.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TQtWSCustomPage.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtPage: TQtPage;
begin
  {$ifdef VerboseQt}
    WriteLn('Trace:> [TQtWSCustomPage.CreateHandle]');
  {$endif}

  QtPage := TQtPage.Create(AWinControl, AParams);
  QtPage.AttachEvents;

  // Returns the Handle
  Result := TLCLIntfHandle(QtPage);

  {$ifdef VerboseQt}
    WriteLn('Trace:< [TQtWSCustomPage.CreateHandle] Result: ', IntToStr(Result));
  {$endif}
end;

class procedure TQtWSCustomPage.UpdateProperties(const ACustomPage: TCustomPage);
var
  ImageList: TCustomImageList;
  ImageIndex: Integer;
  Bmp: TBitmap;
begin
  ImageList := TCustomTabControl(ACustomPage.Parent).Images;

  if Assigned(ImageList) then
  begin
    ImageIndex := TCustomTabControl(ACustomPage.Parent).GetImageIndex(ACustomPage.PageIndex);
    if (ImageIndex >= 0) and (ImageIndex < ImageList.Count) then
    begin
      Bmp := TBitmap.Create;
      try
        ImageList.GetBitmap(ACustomPage.ImageIndex, Bmp);
        TQtPage(ACustomPage.Handle).setIcon(TQtImage(Bmp.Handle).AsIcon);
      finally
        Bmp.Free;
      end;
    end;
  end;
end;

{ TQtWSCustomNotebook }

{------------------------------------------------------------------------------
  Method: TQtWSCustomTabControl.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TQtWSCustomTabControl.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  QtTabWidget: TQtTabWidget;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtWSCustomTabControl.CreateHandle');
  {$endif}
  QtTabWidget := TQtTabWidget.Create(AWinControl, AParams);
  QtTabWidget.setTabPosition(QTabWidgetTabPositionMap[TCustomTabControl(AWinControl).TabPosition]);
  QtTabWidget.setTabsClosable(nboShowCloseButtons in TCustomTabControl(AWinControl).Options);
  QtTabWidget.AttachEvents;

  // Returns the Handle

  Result := TLCLIntfHandle(QtTabWidget);
end;

class procedure TQtWSCustomTabControl.AddPage(const ATabControl: TCustomTabControl;
  const AChild: TCustomPage; const AIndex: integer);
var
  QtTabWidget: TQtTabWidget;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtWSCustomTabControl.AddPage');
  {$endif}
  QtTabWidget := TQtTabWidget(ATabControl.Handle);
  QtTabWidget.setUpdatesEnabled(False);
  QtTabWidget.BeginUpdate;
  try
    QtTabWidget.insertTab(AIndex, TQtPage(AChild.Handle).Widget,
      GetUtf8String(AChild.Caption));
  finally
    QtTabWidget.EndUpdate;
    QtTabWidget.setUpdatesEnabled(True);
  end;
  TQtPage(AChild.Handle).ChildOfComplexWidget := ccwTabWidget;
  TQtWsCustomPage.UpdateProperties(AChild);
end;

class procedure TQtWSCustomTabControl.MovePage(const ATabControl: TCustomTabControl;
  const AChild: TCustomPage; const NewIndex: integer);
var
  TabWidget: TQtTabWidget;
  Index: Integer;
  Page: TQtPage;
begin
  Page := TQtPage(AChild.Handle);
  TabWidget := TQtTabWidget(ATabControl.Handle);
  Index := AChild.PageIndex;
  if Index < 0 then
    Index := ATabControl.IndexOf(AChild);

  TabWidget.BeginUpdate;
  TabWidget.setUpdatesEnabled(false);
  TabWidget.removeTab(Index);
  TabWidget.insertTab(NewIndex, Page.Widget, Page.getIcon, Page.getText);
  TabWidget.setUpdatesEnabled(true);
  if TabWidget.getCurrentIndex <> NewIndex then
    TabWidget.setCurrentWidget(Page);
  TabWidget.EndUpdate;
end;

class procedure TQtWSCustomTabControl.RemovePage(const ATabControl: TCustomTabControl;
  const AIndex: integer);
var
  TabWidget: TQtTabWidget;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtWSCustomTabControl.RemovePage');
  {$endif}
  TabWidget := TQtTabWidget(ATabControl.Handle);
  TabWidget.setUpdatesEnabled(false);
  TabWidget.BeginUpdate;
  try
    TabWidget.removeTab(AIndex);
  finally
    TabWidget.EndUpdate;
    TabWidget.setUpdatesEnabled(true);
  end;
end;

class function TQtWSCustomTabControl.GetCapabilities: TCTabControlCapabilities;
begin
  Result := [nbcShowCloseButtons];
end;

class function TQtWSCustomTabControl.GetDesignInteractive(
  const AWinControl: TWinControl; AClientPos: TPoint): Boolean;
var
  TabWidget: TQtTabWidget;
  TabBar: TQtTabBar;
  TabIndex: Integer;
  p: TQtPoint;
begin
  Result := False;
  if not WSCheckHandleAllocated(AWinControl, 'GetDesignInteractive') then
    Exit;
  TabWidget := TQtTabWidget(AWinControl.Handle);
  TabBar := TabWidget.TabBar;
  p := QtPoint(AClientPos.x, AClientPos.y);
  TabIndex := QTabBar_tabAt(QTabBarH(TabBar.Widget), @p);
  Result := (TabIndex >= 0) and (TabWidget.getCurrentIndex <> TabIndex);
end;

class function TQtWSCustomTabControl.GetTabIndexAtPos(
  const ATabControl: TCustomTabControl; const AClientPos: TPoint): integer;
var
  TabWidget: TQtTabWidget;
  NewPos: TPoint;
  R: TRect;
begin
  TabWidget := TQtTabWidget(ATabControl.Handle);
  NewPos := AClientPos;
  R := TabWidget.TabBar.getGeometry;
  case ATabControl.TabPosition of
    tpTop: if NewPos.Y < 0 then NewPos.Y := R.Bottom + NewPos.Y;
    tpLeft: if NewPos.X < 0 then NewPos.X := R.Left + NewPos.X;
    tpRight: NewPos.X := R.Right - NewPos.X;
    tpBottom: NewPos.Y := R.Bottom - NewPos.Y;
  end;
  Result := TabWidget.tabAt(NewPos);
end;

class function TQtWSCustomTabControl.GetTabRect(const ATabControl: TCustomTabControl;
  const AIndex: Integer): TRect;
var
  TabWidget: TQtTabWidget;
begin
  Result := Rect(-1, -1, -1, -1);
  if not WSCheckHandleAllocated(ATabControl, 'GetTabRect') then
    Exit;
  TabWidget := TQtTabWidget(ATabControl.Handle);
  Result := TabWidget.TabBar.GetTabRect(AIndex);
  case ATabControl.TabPosition of
    tpTop: OffsetRect(Result, 0, -Result.Bottom);
    tpLeft: OffsetRect(Result, -Result.Right, 0);
    tpRight: OffsetRect(Result, Result.Left, 0);
    tpBottom: OffsetRect(Result, Result.Top, 0);
  end;
end;

class procedure TQtWSCustomTabControl.SetPageIndex(
  const ATabControl: TCustomTabControl; const AIndex: integer);
var
  TabWidget: TQtTabWidget;
begin
  if not WSCheckHandleAllocated(ATabControl, 'SetPageIndex') then
    Exit;
  TabWidget := TQtTabWidget(ATabControl.Handle);

  if (AIndex < 0) or (AIndex > ATabControl.PageCount - 1) then
    exit;

  TabWidget.BeginUpdate;
  if ATabControl.Page[AIndex].HandleAllocated then
    TabWidget.setCurrentWidget(TQtWidget(ATabControl.Page[AIndex].Handle));
  TabWidget.EndUpdate;
end;

class procedure TQtWSCustomTabControl.SetTabCaption(
  const ATabControl: TCustomTabControl; const AChild: TCustomPage;
  const AText: string);
var
  Index: Integer;
begin
  Index := AChild.PageIndex;
  if Index < 0 then
    Index := ATabControl.IndexOf(AChild);
  TQtTabWidget(ATabControl.Handle).setTabText(Index, GetUtf8String(AText));
end;

class procedure TQtWSCustomTabControl.SetTabPosition(
  const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition);
begin
  TQtTabWidget(ATabControl.Handle).SetTabPosition(QTabWidgetTabPositionMap[ATabPosition]);
end;

class procedure TQtWSCustomTabControl.ShowTabs(const ATabControl: TCustomTabControl;
  AShowTabs: boolean);
var
  TabWidget: TQtTabWidget;
begin
  TabWidget := TQtTabWidget(ATabControl.Handle);
  if TabWidget.TabBar <> nil then
    TabWidget.ShowTabs := AShowTabs;
end;

class procedure TQtWSCustomTabControl.UpdateProperties(const ATabControl: TCustomTabControl);
begin
  TQtTabWidget(ATabControl.Handle).setTabsClosable(nboShowCloseButtons in ATabControl.Options);
end;

