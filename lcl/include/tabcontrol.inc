{%MainUnit ../comctrls.pp}

{******************************************************************************
                                  TTabControl
 ******************************************************************************

  Author: Mattias Gaertner

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
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

{ TTabControlStrings }

procedure TTabControlStrings.SetHotTrack(const AValue: Boolean);
begin
  if FHotTrack=AValue then exit;
  FHotTrack:=AValue;
end;

procedure TTabControlStrings.SetImages(const AValue: TCustomImageList);
begin
  if FImages=AValue then exit;
  FImages:=AValue;
end;

procedure TTabControlStrings.SetMultiLine(const AValue: Boolean);
begin
  if FMultiLine=AValue then exit;
  FMultiLine:=AValue;
end;

procedure TTabControlStrings.SetMultiSelect(const AValue: Boolean);
begin
  if FMultiSelect=AValue then exit;
  FMultiSelect:=AValue;
end;

procedure TTabControlStrings.SetOwnerDraw(const AValue: Boolean);
begin
  if FOwnerDraw=AValue then exit;
  FOwnerDraw:=AValue;
end;

procedure TTabControlStrings.SetRaggedRight(const AValue: Boolean);
begin
  if FRaggedRight=AValue then exit;
  FRaggedRight:=AValue;
end;

procedure TTabControlStrings.SetScrollOpposite(const AValue: Boolean);
begin
  if FScrollOpposite=AValue then exit;
  FScrollOpposite:=AValue;
end;

procedure TTabControlStrings.SetTabHeight(const AValue: Smallint);
begin
  if FTabHeight=AValue then exit;
  FTabHeight:=AValue;
end;

procedure TTabControlStrings.SetTabWidth(const AValue: Smallint);
begin
  if FTabWidth=AValue then exit;
  FTabWidth:=AValue;
end;

constructor TTabControlStrings.Create(TheTabControl: TTabControl);
begin
  inherited Create;
  FTabControl:=TheTabControl;
  FHotTrack:=false;
  FMultiLine:=false;
  FMultiSelect:=false;
  FOwnerDraw:=false;
  FRaggedRight:=false;
  FScrollOpposite:=false;
  FTabHeight:=0;
  FTabWidth:=0;
end;

procedure TTabControlStrings.TabControlBoundsChange;
begin

end;

function TTabControlStrings.IndexOfTabAt(X, Y: Integer): Integer;
begin
  Result:=0;
end;

function TTabControlStrings.GetHitTestInfoAt(X, Y: Integer): THitTests;
begin
  Result:=[];
end;

function TTabControlStrings.TabRect(Index: Integer): TRect;
begin
  FillChar(Result,SizeOf(Result),0);
end;

function TTabControlStrings.RowCount: Integer;
begin
  Result:=1;
end;

procedure TTabControlStrings.ScrollTabs(Delta: Integer);
begin
end;

procedure TTabControlStrings.UpdateTabImages;
begin
end;

procedure TTabControlStrings.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TTabControlStrings.EndUpdate;
begin
  if FUpdateCount=0 then
    RaiseGDBException('TTabControlStrings.EndUpdate');
  dec(FUpdateCount);
end;

function TTabControlStrings.IsUpdating: boolean;
begin
  Result:=FUpdateCount>0;
end;

procedure TTabControlStrings.ImageListChange(Sender: TObject);
begin
end;

{ TTabControlNoteBookStrings }

procedure TTabControlNoteBookStrings.NBGetImageIndex(Sender: TObject;
  TheTabIndex: Integer; var ImageIndex: Integer);
begin
  ImageIndex := TabControl.GetImageIndex(TheTabIndex);
end;

procedure TTabControlNoteBookStrings.NBChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  AllowChange:=TabControl.CanChange;
end;

procedure TTabControlNoteBookStrings.NBPageChanged(Sender: TObject);
begin
  TabControl.Change;
end;

function TTabControlNoteBookStrings.Get(Index: Integer): string;
begin
  Result:=FNoteBook.Pages[Index];
end;

function TTabControlNoteBookStrings.GetCount: Integer;
begin
  Result:=FNoteBook.PageCount;
end;

function TTabControlNoteBookStrings.GetObject(Index: Integer): TObject;
begin
  Result:=FNoteBook.Pages.Objects[Index];
end;

procedure TTabControlNoteBookStrings.Put(Index: Integer; const S: string);
begin
  FNoteBook.Pages[Index]:=S;
end;

procedure TTabControlNoteBookStrings.PutObject(Index: Integer; AObject: TObject);
begin
  FNoteBook.Pages.Objects[Index]:=AObject;
end;

procedure TTabControlNoteBookStrings.SetImages(const AValue: TCustomImageList);
begin
  if AValue is TImageList then
    FNoteBook.Images:=TImageList(AValue)
  else
    FNoteBook.Images:=nil;
end;

procedure TTabControlNoteBookStrings.SetUpdateState(Updating: Boolean);
begin
  if Updating then
    FNoteBook.Pages.BeginUpdate
  else
    FNoteBook.Pages.EndUpdate;
end;

procedure TTabControlNoteBookStrings.SetTabHeight(const AValue: Smallint);
begin
  if TabHeight=AValue then exit;
  inherited SetTabHeight(AValue);
  TabControlBoundsChange;
end;

procedure TTabControlNoteBookStrings.SetTabWidth(const AValue: Smallint);
begin
  if TabWidth=AValue then exit;
  inherited SetTabWidth(AValue);
  TabControlBoundsChange;
end;

function TTabControlNoteBookStrings.GetTabIndex: integer;
begin
  Result:=FNoteBook.PageIndex;
end;

procedure TTabControlNoteBookStrings.SetTabIndex(const AValue: integer);
begin
  FNoteBook.PageIndex:=AValue;
end;

constructor TTabControlNoteBookStrings.Create(TheTabControl: TTabControl);
begin
  inherited Create(TheTabControl);
  FNoteBook := TCustomTabControl{%H-}.Create(nil);
  FNoteBook.ControlStyle := FNoteBook.ControlStyle + [csNoDesignSelectable];
  FNoteBook.Parent := TabControl;
  FNoteBook.OnGetImageIndex := @NBGetImageIndex;
  FNoteBook.OnChanging := @NBChanging;
  FNoteBook.OnChange := @NBPageChanged;
  TabControlBoundsChange;
end;

destructor TTabControlNoteBookStrings.Destroy;
begin
  FreeThenNil(FNoteBook);
  inherited Destroy;
end;

procedure TTabControlNoteBookStrings.Clear;
begin
  FNoteBook.Pages.Clear;
end;

procedure TTabControlNoteBookStrings.Delete(Index: Integer);
begin
  FNoteBook.Pages.Delete(Index);
end;

procedure TTabControlNoteBookStrings.Insert(Index: Integer; const S: string);
begin
  FNoteBook.Pages.Insert(Index, S);
end;

function TTabControlNoteBookStrings.GetSize: integer;
begin
  case TabControl.TabPosition of
    tpTop, tpBottom: Result:=FNoteBook.Height;
    tpLeft, tpRight: Result:=FNoteBook.Width;
  end;
end;

procedure TTabControlNoteBookStrings.TabControlBoundsChange;
var
  NewHeight: LongInt;
  NewWidth: LongInt;
begin
  inherited TabControlBoundsChange;

  FNoteBook.TabPosition:=TabControl.TabPosition;

  case TabControl.TabPosition of
  tpTop,tpBottom:
    begin
      NewHeight:=TabHeight;
      if NewHeight<=0 then
        NewHeight:=FNoteBook.GetMinimumTabHeight;
      NewHeight:=Min(TabControl.Height,NewHeight);
      if TabControl.TabPosition=tpTop then
        FNoteBook.SetBounds(0,0,TabControl.Width,NewHeight)
      else
        FNoteBook.SetBounds(0,TabControl.Height-NewHeight,
                            TabControl.Width,NewHeight);
    end;

  tpLeft,tpRight:
    begin
      NewWidth:=TabWidth;
      if NewWidth<=0 then
        NewWidth:=FNoteBook.GetMinimumTabWidth;
      NewWidth:=Min(TabControl.Width,NewWidth);
      if TabControl.TabPosition=tpLeft then
        FNoteBook.SetBounds(0,0,NewWidth,TabControl.Height)
      else
        FNoteBook.SetBounds(TabControl.Width-NewWidth,0,
                            NewWidth,TabControl.Height);
    end;
  end;
end;

function TTabControlNoteBookStrings.IndexOfTabAt(X, Y: Integer): Integer;
begin
  Result:=FNoteBook.TabIndexAtClientPos(Point(X, Y));
end;

{ TTabControl }

procedure TTabControl.AdjustDisplayRect(var ARect: TRect);
begin
  AdjustDisplayRectWithBorder(ARect);
  if TabPosition<>tpTop then
    ARect.Top:=Min(Max(ARect.Top,ARect.Top+BorderWidth),ARect.Bottom);
  if TabPosition<>tpBottom then
    ARect.Bottom:=Max(Min(ARect.Bottom,ARect.Bottom-BorderWidth),ARect.Top);
  if TabPosition<>tpLeft then
    ARect.Left:=Min(Max(ARect.Left,ARect.Left+BorderWidth),ARect.Right);
  if TabPosition<>tpRight then
    ARect.Right:=Max(Min(ARect.Right,ARect.Right-BorderWidth),ARect.Left);
end;

function TTabControl.GetDisplayRect: TRect;
begin
  Result:=ClientRect;
  AdjustDisplayRect(Result);
end;

function TTabControl.GetHotTrack: Boolean;
begin
  Result:=TTabControlStrings(FTabs).HotTrack;
end;

function TTabControl.GetMultiLine: Boolean;
begin
  Result:=TTabControlStrings(FTabs).MultiLine;
end;

function TTabControl.GetMultiSelect: Boolean;
begin
  Result:=TTabControlStrings(FTabs).MultiSelect;
end;

function TTabControl.GetOwnerDraw: Boolean;
begin
  Result:=TTabControlStrings(FTabs).OwnerDraw;
end;

function TTabControl.GetRaggedRight: Boolean;
begin
  Result:=TTabControlStrings(FTabs).RaggedRight;
end;

function TTabControl.GetScrollOpposite: Boolean;
begin
  Result:=TTabControlStrings(FTabs).ScrollOpposite;
end;

function TTabControl.GetTabHeight: Smallint;
begin
  Result:=TTabControlStrings(FTabs).TabHeight;
end;

function TTabControl.GetTabIndex: Integer;
begin
  Result:=TTabControlStrings(FTabs).TabIndex;
end;

function TTabControl.GetTabWidth: Smallint;
begin
  Result:=TTabControlStrings(FTabs).TabWidth;
end;

procedure TTabControl.SetHotTrack(const AValue: Boolean);
begin
  TTabControlStrings(FTabs).HotTrack:=AValue;
end;

procedure TTabControl.SetImages(const AValue: TCustomImageList);
begin
  if FImages = AValue then Exit;
  if FImages <> nil then
    FImages.RemoveFreeNotification(Self);
  FImages := TImageList(AValue);
  if FImages <> nil then
    FImages.FreeNotification(Self);
  TTabControlStrings(FTabs).Images := FImages;
end;

procedure TTabControl.SetMultiLine(const AValue: Boolean);
begin
  TTabControlStrings(FTabs).MultiLine:=AValue;
end;

procedure TTabControl.SetMultiSelect(const AValue: Boolean);
begin
  TTabControlStrings(FTabs).MultiSelect:=AValue;
end;

procedure TTabControl.SetOwnerDraw(const AValue: Boolean);
begin
  TTabControlStrings(FTabs).OwnerDraw:=AValue;
end;

procedure TTabControl.SetRaggedRight(const AValue: Boolean);
begin
  TTabControlStrings(FTabs).RaggedRight:=AValue;
end;

procedure TTabControl.SetScrollOpposite(const AValue: Boolean);
begin
  TTabControlStrings(FTabs).ScrollOpposite:=AValue;
end;

procedure TTabControl.SetStyle(const AValue: TTabStyle);
begin
  if FStyle=AValue then exit;
  FStyle:=AValue;
  // ToDo
end;

procedure TTabControl.SetTabHeight(const AValue: Smallint);
begin
  TTabControlStrings(FTabs).TabHeight:=AValue;
end;

procedure TTabControl.SetTabPosition(const AValue: TTabPosition);
begin
  if FTabPosition=AValue then exit;
  FTabPosition:=AValue;
  TTabControlStrings(FTabs).TabControlBoundsChange;
  ReAlign;
end;

procedure TTabControl.SetTabs(const AValue: TStrings);
begin
  FTabs.Assign(AValue);
end;

procedure TTabControl.SetTabWidth(const AValue: Smallint);
begin
  TTabControlStrings(FTabs).TabWidth:=AValue;
end;

function TTabControl.CanChange: Boolean;
begin
  Result:=true;
  if FTabControlCreating then exit;
  if not IsUpdating and Assigned(FOnChanging) then
    FOnChanging(Self,Result);
end;

function TTabControl.CanShowTab(ATabIndex: Integer): Boolean;
begin
  Result:=true;
end;

procedure TTabControl.Change;
begin
  if FTabControlCreating then exit;
  if IsUpdating then begin
    FOnChangeNeeded:=true;
    exit;
  end else
    FOnChangeNeeded:=false;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{$ifndef LCL_NEW_TABCONTROL}
procedure TTabControl.DrawTab(ATabIndex: Integer; const Rect: TRect;
  Active: Boolean);
begin
  if Assigned(FOnDrawTab) then
    FOnDrawTab(TCustomTabControl(Self),TabIndex,Rect,Active)
  else
    Canvas.FillRect(Rect);
end;
{$endif}

function TTabControl.GetImageIndex(ATabIndex: Integer): Integer;
begin
  Result := ATabIndex;
  if Assigned(FOnGetImageIndex) then
    FOnGetImageIndex(Self, ATabIndex, Result);
end;

procedure TTabControl.Loaded;
begin
  inherited Loaded;
end;

procedure TTabControl.CreateWnd;
begin
  BeginUpdate;
  inherited CreateWnd;
  EndUpdate;
end;

procedure TTabControl.DestroyHandle;
begin
  BeginUpdate;
  inherited DestroyHandle;
  EndUpdate;
end;

procedure TTabControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TTabControl.SetTabIndex(Value: Integer);
begin
  TTabControlStrings(FTabs).TabIndex:=Value;
end;

procedure TTabControl.UpdateTabImages;
begin
  TTabControlStrings(FTabs).UpdateTabImages;
end;

procedure TTabControl.ImageListChange(Sender: TObject);
begin
  TTabControlStrings(FTabs).ImageListChange(Sender);
end;

procedure TTabControl.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
  TTabControlStrings(FTabs).TabControlBoundsChange;
end;

class function TTabControl.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 200;
  Result.CY := 150;
end;

{$ifdef LCL_NEW_TABCONTROL}
procedure TTabControl.PaintWindow(DC: HDC);
var
  DCChanged: boolean;
begin
  DCChanged := (not FCanvas.HandleAllocated) or (FCanvas.Handle <> DC);
  if DCChanged then
    FCanvas.Handle := DC;
  try
    Paint;
  finally
    if DCChanged then FCanvas.Handle := 0;
  end;
end;
{$endif}

procedure TTabControl.Paint;
var
  ARect: TRect;
  TS: TTextStyle;
  Details: TThemedElementDetails;
  lCanvas: TCanvas;
begin
  {$ifdef LCL_NEW_TABCONTROL}
  lCanvas := FCanvas;
  {$else}
  lCanvas := Canvas;
  {$endif}

  //DebugLn(['TTabControl.Paint Bounds=',dbgs(BoundsRect),' ClientRect=',dbgs(ClientRect),' CientOrigin=',dbgs(ClientOrigin)]);
  // clear only display area since button area is painted by another control
  // draw a frame
  ARect := ClientRect;
  AdjustDisplayRectWithBorder(ARect);

  Details := ThemeServices.GetElementDetails(ttPane);
  ThemeServices.DrawElement(lCanvas.Handle, Details, ARect);

  InflateRect(ARect,BorderWidth,BorderWidth);
  lCanvas.Frame3d(ARect, BorderWidth, bvRaised);

  if (csDesigning in ComponentState) and (Caption <> '') then
  begin
    ARect:=GetDisplayRect;
    TS := lCanvas.TextStyle;
    TS.Alignment:=taCenter;
    TS.Layout:= tlCenter;
    TS.Opaque:= false;
    TS.Clipping:= false;
    lCanvas.TextRect(ARect, ARect.Left, ARect.Top, Caption, TS);
  end;
end;

procedure TTabControl.AdjustDisplayRectWithBorder(var ARect: TRect);
var
  TabAreaSize: LongInt;
begin
  TabAreaSize := TTabControlStrings(FTabs).GetSize;
  case TabPosition of
    tpTop:    ARect.Top:=Min(TabAreaSize,ARect.Bottom);
    tpBottom: ARect.Bottom:=Max(ARect.Bottom-TabAreaSize,ARect.Top);
    tpLeft:   ARect.Left:=Min(TabAreaSize,ARect.Right);
    tpRight:  ARect.Right:=Max(ARect.Right-TabAreaSize,ARect.Left);
  end;
end;

function TTabControl.GetTabRectWithBorder: TRect;
var
  TabAreaSize: LongInt;
begin
  Result := ClientRect;
  TabAreaSize := TTabControlStrings(FTabs).GetSize;
  case TabPosition of
    tpTop:    Result.Bottom:=Min(TabAreaSize,Result.Bottom);
    tpBottom: Result.Top:=Max(Result.Bottom-TabAreaSize,Result.Top);
    tpLeft:   Result.Right:=Min(TabAreaSize,Result.Right);
    tpRight:  Result.Left:=Max(Result.Right-TabAreaSize,Result.Left);
  end;
end;

procedure TTabControl.AdjustClientRect(var ARect: TRect);
begin
  AdjustDisplayRect(ARect);
end;

{$ifdef LCL_NEW_TABCONTROL}
class function TTabControl.GetWSComponentClass(ASelf: TLCLComponent
  ): TWSLCLComponentClass;
begin
  Result:= FindWSComponentClass(TCustomControl);
end;

procedure TTabControl.DoCreateWnd;
begin

end;

{$endif}

constructor TTabControl.Create(TheOwner: TComponent);
begin
  FTabControlCreating:=true;
  inherited Create(TheOwner);
  ControlStyle:=ControlStyle+[csAcceptsControls];
  FStyle:=tsTabs;
  FTabPosition:=tpTop;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
  FTabs:=TTabControlNoteBookStrings.Create(Self);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  BorderWidth:=2;
  FTabControlCreating:=false;
  {$ifdef LCL_NEW_TABCONTROL}
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  {$endif}
end;

destructor TTabControl.Destroy;
begin
  BeginUpdate;
  FreeThenNil(FTabs);
  FreeThenNil(FImageChangeLink);
  inherited Destroy;
end;

function TTabControl.IndexOfTabAt(X, Y: Integer): Integer;
begin
  Result:=TTabControlStrings(FTabs).IndexOfTabAt(X,Y);
end;

function TTabControl.GetHitTestInfoAt(X, Y: Integer): THitTests;
begin
  Result:=TTabControlStrings(FTabs).GetHitTestInfoAt(X,Y);
end;

function TTabControl.IndexOfTabWithCaption(const TabCaption: string
  ): Integer;
begin
  Result:=0;
  while Result<Tabs.Count do begin
    if CompareText(Tabs[Result],TabCaption)=0 then exit;
    inc(Result);
  end;
  Result:=-1;
end;

function TTabControl.TabRect(Index: Integer): TRect;
begin
  Result:=TTabControlStrings(FTabs).TabRect(Index);
end;

function TTabControl.RowCount: Integer;
begin
  Result:=TTabControlStrings(FTabs).RowCount;
end;

procedure TTabControl.ScrollTabs(Delta: Integer);
begin
  TTabControlStrings(FTabs).ScrollTabs(Delta);
end;

procedure TTabControl.BeginUpdate;
begin
  if FTabs=nil then exit;
  TTabControlStrings(FTabs).BeginUpdate;
  //debugln('TTabControl.BeginUpdate ',dbgs(IsUpdating));
end;

procedure TTabControl.EndUpdate;
begin
  if FTabs=nil then exit;
  TTabControlStrings(FTabs).EndUpdate;
  //debugln('TTabControl.EndUpdate ',dbgs(IsUpdating));
  if not TTabControlStrings(FTabs).IsUpdating then begin
    if FOnChangeNeeded then Change;
  end;
end;

function TTabControl.IsUpdating: boolean;
begin
  Result:=(FTabs<>nil) and TTabControlStrings(fTabs).IsUpdating;
end;

// included by comctrls.pp

