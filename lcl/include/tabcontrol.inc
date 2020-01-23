{%MainUnit ../comctrls.pp}

{******************************************************************************
                                  TTabControl
 ******************************************************************************

  Author: Mattias Gaertner

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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

{ TNoteBookStringsTabControl }

procedure TNoteBookStringsTabControl.CreateHandle;
begin
  inherited CreateHandle;
  if FHandleCreated <> nil then
    FHandleCreated(self);
end;

procedure TNoteBookStringsTabControl.DoStartDrag(var DragObject: TDragObject);
begin
  if (Parent is TTabControl) then
  begin
    if Assigned(TTabControl(Parent).OnStartDrag) then
      TTabControl(Parent).OnStartDrag(Parent, DragObject);
    if not Assigned(DragObject) then
      DragObject := TDragControlObject.AutoCreate(Parent);
  end;
  inherited DoStartDrag(DragObject);
end;

procedure TNoteBookStringsTabControl.DragDrop(Source: TObject; X, Y: Integer);
begin
  inherited DragDrop(Source, X, Y);
  if (Parent is TTabControl) and Assigned(TTabControl(Parent).OnDragDrop) then
    TTabControl(Parent).OnDragDrop(Parent, Source, X, Y);
end;

procedure TNoteBookStringsTabControl.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);
  if (Parent is TTabControl) and Assigned(TTabControl(Parent).OnDragOver) then
    TTabControl(Parent).OnDragOver(Parent, Source, X, Y, State, Accept);
end;

procedure TNoteBookStringsTabControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Parent is TTabControl) and Assigned(TTabControl(Parent).OnMouseDown) then
    TTabControl(Parent).OnMouseDown(Parent, Button, Shift, X, Y);
end;

procedure TNoteBookStringsTabControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if (Parent is TTabControl) and Assigned(TTabControl(Parent).OnMouseMove) then
    TTabControl(Parent).OnMouseMove(Parent, Shift, X, Y);
end;

procedure TNoteBookStringsTabControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Parent is TTabControl) and Assigned(TTabControl(Parent).OnMouseUp) then
    TTabControl(Parent).OnMouseUp(Parent, Button, Shift, X, Y);
end;

procedure TNoteBookStringsTabControl.MouseEnter;
begin
  inherited MouseEnter;
  if (Parent is TTabControl) and Assigned(TTabControl(Parent).OnMouseEnter) then
    TTabControl(Parent).OnMouseEnter(Parent);
end;

procedure TNoteBookStringsTabControl.MouseLeave;
begin
  inherited MouseLeave;
  if (Parent is TTabControl) and Assigned(TTabControl(Parent).OnMouseLeave) then
    TTabControl(Parent).OnMouseLeave(Parent);
end;

class procedure TNoteBookStringsTabControl.WSRegisterClass;
begin
  inherited WSRegisterClass;
  // TODO:
  //RegisterWSComponent(TNoteBookStringsTabControl, TWSPageControl);
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

procedure TTabControlNoteBookStrings.NBHandleCreated(Sender: TObject);
begin
  if FInHandleCreated then
    exit;
  FInHandleCreated := True;
  TabControlBoundsChange;
  FInHandleCreated := False;
end;

function TTabControlNoteBookStrings.GetTabPosition: TTabPosition;
begin
  Result := FNoteBook.TabPosition;
end;

procedure TTabControlNoteBookStrings.SetTabPosition(AValue: TTabPosition);
begin
  FNoteBook.TabPosition := AValue;
  TabControlBoundsChange;
end;

procedure TTabControlNoteBookStrings.SetStyle(AValue: TTabStyle);
begin
  FNoteBook.Style := AValue;
  TabControlBoundsChange;
end;

function TTabControlNoteBookStrings.GetInternalTabControllClass: TNoteBookStringsTabControlClass;
begin
  Result := TNoteBookStringsTabControl;
end;

function TTabControlNoteBookStrings.GetStyle: TTabStyle;
begin
  Result := FNoteBook.Style;
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

procedure TTabControlNoteBookStrings.SetMultiLine(const AValue: Boolean);
begin
  inherited SetMultiLine(AValue);
  FNoteBook.MultiLine := AValue;
  TabControlBoundsChange;
end;

procedure TTabControlNoteBookStrings.SetUpdateState(Updating: Boolean);
begin
  if Updating then
    FNoteBook.Pages.BeginUpdate
  else
    FNoteBook.Pages.EndUpdate;
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
  FNoteBook := GetInternalTabControllClass.Create(nil);
  FNoteBook.ControlStyle := FNoteBook.ControlStyle + [csNoDesignSelectable];
  FNoteBook.Parent := TabControl;
  FNoteBook.OnGetImageIndex := @NBGetImageIndex;
  FNoteBook.OnChanging := @NBChanging;
  FNoteBook.OnChange := @NBPageChanged;
  TNoteBookStringsTabControl(FNoteBook).FHandleCreated := @NBHandleCreated;
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
  TabControlBoundsChange;
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
      NewHeight:=TabControl.TabHeight;
      if NewHeight<=0 then
        NewHeight:=FNoteBook.GetMinimumTabHeight;
      NewHeight:=Min(TabControl.ClientHeight,NewHeight);
      if TabControl.TabPosition=tpTop then
        FNoteBook.SetBounds(0,0,TabControl.ClientWidth,NewHeight)
      else
        FNoteBook.SetBounds(0,TabControl.ClientHeight-NewHeight,
                            TabControl.ClientWidth,NewHeight);
    end;

  tpLeft,tpRight:
    begin
      NewWidth:=Max(TabControl.TabHeight,FNoteBook.GetMinimumTabWidth);
      NewWidth:=Min(TabControl.Width,NewWidth);
      if TabControl.TabPosition=tpLeft then
        FNoteBook.SetBounds(0,0,NewWidth,TabControl.ClientHeight)
      else
        FNoteBook.SetBounds(TabControl.ClientWidth-NewWidth,0,
                            NewWidth,TabControl.ClientHeight);
    end;
  end;

  TabControl.Invalidate;
end;

function TTabControlNoteBookStrings.IndexOfTabAt(X, Y: Integer): Integer;
begin
  Result:=FNoteBook.IndexOfPageAt(X, Y);
end;

{ TTabControl }

procedure TTabControl.AdjustDisplayRect(var ARect: TRect);
const
  TabControlInternalBorder = 2; // TTabControl paints a border, so limit the children, to be within that border
begin
  AdjustDisplayRectWithBorder(ARect);
  if TabPosition<>tpTop then
    ARect.Top:=Min(Max(ARect.Top,ARect.Top+BorderWidth+TabControlInternalBorder),ARect.Bottom);
  if TabPosition<>tpBottom then
    ARect.Bottom:=Max(Min(ARect.Bottom,ARect.Bottom-BorderWidth-TabControlInternalBorder),ARect.Top);
  if TabPosition<>tpLeft then
    ARect.Left:=Min(Max(ARect.Left,ARect.Left+BorderWidth+TabControlInternalBorder),ARect.Right);
  if TabPosition<>tpRight then
    ARect.Right:=Max(Min(ARect.Right,ARect.Right-BorderWidth-TabControlInternalBorder),ARect.Left);
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

function TTabControl.GetTabIndex: Integer;
begin
  Result:=TTabControlStrings(FTabs).TabIndex;
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

procedure TTabControl.SetStyle(AValue: TTabStyle);
begin
  inherited SetStyle(AValue);
  if FStyle=AValue then exit;
  FStyle:=AValue;
  TTabControlNoteBookStrings(FTabs).Style := AValue;
end;

procedure TTabControl.SetTabHeight(AValue: Smallint);
begin
  if FTabHeight = AValue then exit;
  if not (nbcTabsSizeable in GetCapabilities) then Exit;
  FTabHeight := AValue;
  TTabControlNoteBookStrings(FTabs).NoteBook.TabHeight := AValue;
end;

procedure TTabControl.SetTabPosition(AValue: TTabPosition);
begin
  if FTabPosition=AValue then exit;
  FTabPosition:=AValue;
  TTabControlNoteBookStrings(FTabs).TabPosition := AValue;
  ReAlign;
end;

procedure TTabControl.SetTabs(const AValue: TStrings);
begin
  FTabs.Assign(AValue);
end;

procedure TTabControl.SetTabStop(const AValue: Boolean);
begin
  TTabControlNoteBookStrings(FTabs).NoteBook.TabStop := AValue;
end;

procedure TTabControl.SetTabWidth(AValue: Smallint);
begin
  if FTabWidth = AValue then Exit;
  if not (nbcTabsSizeable in GetCapabilities) then Exit;
  FTabWidth := AValue;
  TTabControlNoteBookStrings(FTabs).NoteBook.TabWidth := AValue;
end;

class procedure TTabControl.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterPropertyToSkip(TTabControl, 'OnDrawTab', 'Property streamed in older Lazarus revision','');
end;

procedure TTabControl.SetOptions(const AValue: TCTabControlOptions);
begin
  inherited SetOptions(AValue);
  //propagate the changes to FTabs.NoteBook, this is needed in TCustomTabControl.SetPageIndex
  //since SetTabIndex eventually does FTabs.NoteBook.SetPageIndex
  TTabControlNoteBookStrings(FTabs).NoteBook.Options := AValue;
end;

procedure TTabControl.AddRemovePageHandle(APage: TCustomPage);
begin
  // There are no pages, don't create a handle
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

function TTabControl.GetImageIndex(ATabIndex: Integer): Integer;
begin
  Result := ATabIndex;
  if Assigned(FOnGetImageIndex) then
    FOnGetImageIndex(Self, ATabIndex, Result);
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

procedure TTabControl.SetDragMode(Value: TDragMode);
begin
  inherited SetDragMode(Value);
  TTabControlNoteBookStrings(FTabs).NoteBook.SetDragMode(Value);
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
  if FTabs <> nil then
    TTabControlStrings(FTabs).TabControlBoundsChange;
end;

class function TTabControl.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 200;
  Result.CY := 150;
end;

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

procedure TTabControl.Paint;
var
  ARect, ARect2: TRect;
  TS: TTextStyle;
  Details: TThemedElementDetails;
  lCanvas: TCanvas;
begin
  lCanvas := FCanvas;

  //DebugLn(['TTabControl.Paint Bounds=',dbgs(BoundsRect),' ClientRect=',dbgs(ClientRect),' CientOrigin=',dbgs(ClientOrigin)]);
  // clear only display area since button area is painted by another control
  // draw a frame
  ARect := ClientRect;
  AdjustDisplayRectWithBorder(ARect);

  Details := ThemeServices.GetElementDetails(ttPane);
  ARect2 := ARect;
  // paint 1 pixel under the header, to avoid painting a closing border
  case TabPosition of
    tpTop:    ARect2.Top    := ARect2.Top    - 1;
    tpBottom: ARect2.Bottom := ARect2.Bottom + 1;
    tpLeft:   ARect2.Left   := ARect2.Left   - 1;
    tpRight:  ARect2.Right  := ARect2.Right  + 1;
  end;
  ThemeServices.DrawElement(lCanvas.Handle, Details, ARect2);

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

function TTabControl.GetTabStop: Boolean;
begin
  Result := TTabControlNoteBookStrings(FTabs).NoteBook.TabStop;
end;

procedure TTabControl.AdjustClientRect(var ARect: TRect);
begin
  AdjustDisplayRect(ARect);
end;

function TTabControl.CreateTabNoteBookStrings: TTabControlNoteBookStrings;
begin
  Result := TTabControlNoteBookStrings.Create(Self);
end;

constructor TTabControl.Create(TheOwner: TComponent);
begin
  FTabControlCreating:=true;
  inherited Create(TheOwner);
  ControlStyle:=ControlStyle+[csAcceptsControls];
  FStyle:=tsTabs;
  FTabPosition:=tpTop;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
  FTabs := CreateTabNoteBookStrings;
  TWinControl(Self).TabStop := False; // workaround, see #30305
  TTabControlNoteBookStrings(FTabs).NoteBook.TabStop := True;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  BorderWidth:=0;
  FTabControlCreating:=false;

  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TTabControl.Destroy;
begin
  BeginUpdate;
  FCanvas.Free;
  FreeThenNil(FTabs);
  FreeThenNil(FImageChangeLink);
  inherited Destroy;
end;

function TTabControl.IndexOfTabAt(X, Y: Integer): Integer;
begin
  Result:=TTabControlStrings(FTabs).IndexOfTabAt(X,Y);
end;

function TTabControl.IndexOfTabAt(P: TPoint): Integer;
begin
  Result:=TTabControlStrings(FTabs).IndexOfTabAt(P.x, P.y);
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

