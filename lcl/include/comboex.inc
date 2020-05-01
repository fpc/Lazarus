{%MainUnit ../comboex.pas}

{*****************************************************************************
                     TCustomComboBoxEx, TCustomCheckComboBox
 *****************************************************************************

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TListControlItem }

constructor TListControlItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FImageIndex:=-1;
end;

{ TListControlItem.Setters }

procedure TListControlItem.SetCaption(const AValue: TTranslateString);
begin
  if FCaption=AValue then exit;
  FCaption:=AValue;
  Changed(False);
end;

procedure TListControlItem.SetImageIndex(AValue: TImageIndex);
begin
  if FImageIndex=AValue then exit;
  FImageIndex:=AValue;
  Changed(False);
end;

{ TComboExItem }

constructor TComboExItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FIndent:=-1;
  FOverlayImageIndex:=-1;
  FSelectedImageIndex:=-1;
end;

destructor TComboExItem.Destroy;
begin
  { normally, Items.Count should be already MenuItems.Count-1 ATM }
  { this solves case when item is not deleted via Collection.Delete(Index) }
  { but directly via Item.Free (exactly what Collection Editor of IDE does) }
  { therefore Notify must be called from here, so count of Items and MenuItems remains same }
  if assigned(Collection) and assigned(Collection.Owner) and
    not (csDestroying in (Collection.Owner as TCustomComboBoxEx).ComponentState)
    and (Collection.Count <= (Collection.Owner as TCustomComboBoxEx).Items.Count)
    then TComboExItems(Collection).Notify(self, cnDeleting);
  inherited Destroy;
end;

{ TComboExItem.Setters }

procedure TComboExItem.SetIndent(AValue: Integer);
begin
  if FIndent=AValue then exit;
  FIndent:=AValue;
  Changed(False);
end;

procedure TComboExItem.SetOverlayImageIndex(AValue: TImageIndex);
begin
  if FOverlayImageIndex=AValue then exit;
  FOverlayImageIndex:=AValue;
  { Changed(False); }
end;

procedure TComboExItem.SetSelectedImageIndex(AValue: TImageIndex);
begin
  if FSelectedImageIndex=AValue then exit;
  FSelectedImageIndex:=AValue;
  Changed(False);
end;

{ TListControlItems }

function TListControlItems.Add: TListControlItem;
begin
  Result:=TListControlItem.Create(self);
end;

function TListControlItems.CompareItems(AItem1, AItem2: TListControlItem): Integer;
begin
  if CaseSensitive
    then Result:=CompareStr((AItem1 as TListControlItem).Caption,
                            (AItem2 as TListControlItem).Caption)
    else Result:=CompareStr(lowercase((AItem1 as TListControlItem).Caption),
                            lowercase((AItem2 as TListControlItem).Caption));
end;

procedure TListControlItems.CustomSort(ACompare: TListItemsCompare);
begin
  if assigned(ACompare) then
    begin
      FCompare:=ACompare;
      Sort;
      FCompare:=nil;
    end;
end;

function TListControlItems.DoCustomSort(AItem1, AItem2: TListControlItem): Integer;
begin
  Result:=FCompare(self, AItem1.Index, AItem2.Index);
end;

function TListControlItems.DoOnCompare(AItem1, AItem2: TListControlItem): Integer;
begin
  Result:=OnCompare(self, AItem1, AItem2);
end;

procedure TListControlItems.Sort;
var pCompareItems: function(AItem1, AItem2: TListControlItem): Integer of object;

  procedure QuickSort(aTop, aBottom: Integer);
  var i, j, aPivot: Integer;
  begin
    repeat
      i:=aTop;
      j:=aBottom;
      aPivot:=(aTop+aBottom) div 2;
      repeat
        while pCompareItems(Items[aPivot], Items[i])>0 do
          inc(i);
        while pCompareItems(Items[aPivot], Items[j])<0 do
          dec(j);
        if i<=j then
          begin
            if i<>j then
              if pCompareItems(Items[i], Items[j])<>0 then Exchange(i, j);
            if aPivot=i
              then aPivot:=j
              else if aPivot=j then aPivot:=i;
            inc(i);
            dec(j);
          end;
      until i>j;
      if aTop<j then QuickSort(aTop, j);
      aTop:=i;
    until i>=aBottom;
  end;

var aID: Integer;
begin
  pCompareItems:=nil;
  if assigned(FCompare)
    then pCompareItems:=@DoCustomSort
    else
    case SortType of
      stData: if assigned(OnCompare) then pCompareItems:=@DoOnCompare;
      stText: pCompareItems:=@CompareItems;
      stBoth: if assigned(OnCompare)
                then pCompareItems:=@DoOnCompare
                else pCompareItems:=@CompareItems;
    end;
  aID:=Items[(Owner as TCustomComboBoxEx).ItemIndex].ID;
  BeginUpdate;
  if assigned(pCompareItems) then QuickSort(0, Count-1);
  (Owner as TCustomComboBoxEx).ItemIndex:=FindItemID(aID).Index;
  EndUpdate;
end;

procedure TListControlItems.Update(AItem: TCollectionItem);
begin
  inherited Update(AItem);
end;

{ TListControlItems.Getters and Setters }

function TListControlItems.GetItems(AIndex: Integer): TListControlItem;
begin
  Result:=GetItem(AIndex) as TListControlItem;
end;

procedure TListControlItems.SetCaseSensitive(AValue: Boolean);
begin
  if FCaseSensitive=AValue then exit;
  FCaseSensitive:=AValue;
end;

procedure TListControlItems.SetSortType(AValue: TListItemsSortType);
begin
  if FSortType=AValue then exit;
  FSortType:=AValue;
  Sort;
end;

{ TComboExItems }

function TComboExItems.Add: TComboExItem;
begin
  Result:=TComboExItem.Create(self);
end;

function TComboExItems.AddItem(const ACaption: string; AImageIndex: SmallInt;
  AOverlayImageIndex: SmallInt; ASelectedImageIndex: SmallInt; AIndent: SmallInt; AData: TCustomData
  ): TComboExItem;
begin
  Result:=Add();
  with Result do
    begin
      Caption:=ACaption;
      Indent:=AIndent;
      ImageIndex:=AImageIndex;
      OverlayImageIndex:=AOverlayImageIndex;
      SelectedImageIndex:=ASelectedImageIndex;
      Data:=AData;
    end;
end;

function TComboExItems.Insert(AIndex: Integer): TComboExItem;
begin
  Result := TComboExItem(inherited Insert(AIndex));
end;

procedure TComboExItems.Notify(Item: TCollectionItem; Action: TCollectionNotification);
var i: Integer;
begin
  inherited Notify(Item, Action);
  case Action of
    cnAdded:
      begin
        FAddingOrDeletingItem:=True;
        with Owner as TCustomComboBoxEx do
          begin
            Items.Add('');
            if not (csLoading in ComponentState) then
              TComboExItem(Item).FCaption:=TComboExItem.cDefCaption+inttostr(Item.ID);
          end;
      end;
    cnDeleting:
      begin
        FAddingOrDeletingItem:=True;
        with Owner as TCustomComboBoxEx do
          begin
            i:=ItemIndex;
            Items.Delete(Item.Index);
            if i<Count then ItemIndex:=i
              else if i>0 then ItemIndex:=i-1;
          end;
      end;
  end;
end;

procedure TComboExItems.Update(Item: TCollectionItem);
var aItemIndex: Integer;
begin
  inherited Update(Item);
  aItemIndex:=(Owner as TCustomComboBoxEx).ItemIndex;
  if not assigned(Item) or ((aItemIndex>=0) and
    (Item=(Owner as TCustomComboBoxEx).ItemsEx[aItemIndex]))
    then (Owner as TCustomComboBoxEx).Invalidate;
  FAddingOrDeletingItem:=False;
end;

{ TComboExItems.Getters and Setters }

function TComboExItems.GetComboItems(AIndex: Integer): TComboExItem;
begin
  Result:=Items[AIndex] as TComboExItem;
end;

{ TCustomComboBoxEx }

constructor TCustomComboBoxEx.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FAutoCompleteOptions:=cDefAutoCompOpts;
  FItemsEx:=TComboExItems.Create(self, TComboExItem);
  FNeedMeasure:=True;
  inherited Style:=csOwnerDrawFixed;
  FStyle:=cDefStyle;
  FStyleEx:=[];
end;

destructor TCustomComboBoxEx.Destroy;
begin
  FreeAndNil(FItemsEx);
  inherited Destroy;
end;

procedure TCustomComboBoxEx.Add(const ACaption: string; AIndent: Integer;
  AImgIdx: TImageIndex; AOverlayImgIdx: TImageIndex; ASelectedImgIdx: TImageIndex);
begin
  Insert(ItemsEx.Count, ACaption, AIndent, AImgIdx, AOverlayImgIdx, ASelectedImgIdx);
end;

function TCustomComboBoxEx.Add: Integer;
begin
  Result:=ItemsEx.Count;
  Insert(Result, TComboExItem.cDefCaption);
end;

procedure TCustomComboBoxEx.AddItem(const Item: String; AnObject: TObject);
begin
  Insert(ItemsEx.Count, Item);
  ItemsEx[ItemsEx.Count].Data:=AnObject;
end;

procedure TCustomComboBoxEx.AssignItemsEx(AItemsEx: TComboExItems);
begin
  ItemsEx.Assign(AItemsEx);
end;

procedure TCustomComboBoxEx.AssignItemsEx(AItems: TStrings);
var i: Integer;
begin
  FItemsEx.BeginUpdate;
  FItemsEx.Clear;
  for i:=0 to AItems.Count-1 do
    ItemsEx.AddItem(AItems[i]);
  FItemsEx.EndUpdate;
end;

procedure TCustomComboBoxEx.Clear;
begin
  FItemsEx.Clear;
end;

procedure TCustomComboBoxEx.CMBiDiModeChanged(var Message: TLMessage);
begin
  inherited CMBiDiModeChanged(Message);
  FRightToLeft:=IsRightToLeft;
  Invalidate;
end;

procedure TCustomComboBoxEx.Delete(AIndex: Integer);
begin
  ItemsEx.Delete(AIndex);
end;

procedure TCustomComboBoxEx.DeleteSelected;
begin
  if ItemIndex>=0 then Delete(ItemIndex);
end;

procedure TCustomComboBoxEx.DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState);
const caThemes: array [Boolean] of TThemedButton = (tbPushButtonDisabled, tbPushButtonNormal);
      cItemIndent: SmallInt = 2;
var aDetail: TThemedElementDetails;
    aDropped: Boolean;
    aEnabled: Boolean;
    aFlags: Cardinal;
    aFocusedEditableMainItemNoDD: Boolean;  { combo has edit-like line edit in csDropDownList (Win) and is closed (not DroppedDown }
    aImgPoint: TPoint;
    aIndent: SmallInt;
    aItemIndex: SmallInt;
    aMainItem: Boolean;
    anyRect: TRect;
    ImagesSize: TSize;
begin  { do not call inherited ! }
  aDropped:=DroppedDown;
  aEnabled:=IsEnabled;
  aMainItem:= (ARect.Left>0);
  {$IF DEFINED(LCLWin32) or DEFINED(LCLWin64)}
  aFocusedEditableMainItemNoDD := (Focused and aMainItem and not aDropped);
  {$ELSE}
  aFocusedEditableMainItemNoDD := False;
  {$ENDIF}
  if aDropped and not aMainItem or aFocusedEditableMainItemNoDD then
    begin
      if not (odSelected in State) then Canvas.Brush.Color:=clWindow;
      Canvas.Brush.Style:=bsSolid;
      Canvas.FillRect(ARect);
    end;
  aDetail:=ThemeServices.GetElementDetails(caThemes[aEnabled]);
  if FNeedMeasure then
    begin
      FTextHeight:=Canvas.TextHeight('ŠjÁÇ');
      FNeedMeasure := False;
    end;
  if not aMainItem
    then aIndent:=TComboExItem(ItemsEx[Index]).Indent
    else aIndent:=-1;
  if aIndent<0 then aIndent:=0;
  inc(aIndent, cItemIndent);
  if assigned(Images) then
    begin
      aItemIndex:=-1;
      ImagesSize := Images.SizeForPPI[ImagesWidth, Font.PixelsPerInch];
      if (aMainItem or (odSelected in State)) and
        ((ItemsEx[Index].SelectedImageIndex>=0) and (ItemsEx[Index].SelectedImageIndex<Images.Count))
        then aItemIndex:=ItemsEx[Index].SelectedImageIndex;
      if aItemIndex<0 then aItemIndex:=ItemsEx[Index].ImageIndex;
      if aItemIndex>=0 then
        begin
          if not FRightToLeft
            then aImgPoint.X:=ARect.Left+aIndent
            else aImgPoint.X:=ARect.Right-aIndent-ImagesSize.cx;
          aImgPoint.Y:=(ARect.Bottom+ARect.Top-ImagesSize.cy) div 2;
          ThemeServices.DrawIcon(Canvas, aDetail, aImgPoint, Images, aItemIndex);
        end;
      inc(aIndent, ImagesSize.cx+2*cItemIndent);
    end;
  Canvas.Brush.Style:=bsClear;
  if (not (odSelected in State) or not aDropped) and not aFocusedEditableMainItemNoDD
    then Canvas.Font.Color:=clWindowText
    else Canvas.Font.Color:=clHighlightText;
  if aFocusedEditableMainItemNoDD then
    begin
      LCLIntf.SetBkColor(Canvas.Handle, ColorToRGB(clBtnFace));
      LCLIntf.DrawFocusRect(Canvas.Handle, aRect);
    end;
  aFlags:=DT_END_ELLIPSIS+DT_VCENTER+DT_SINGLELINE+DT_NOPREFIX;
  if not FRightToLeft then
    begin
      anyRect.Left:=ARect.Left+aIndent;
      anyRect.Right:=ARect.Right;
    end else
    begin
      anyRect.Right:=ARect.Right-aIndent;
      anyRect.Left:=ARect.Left;
      aFlags:=aFlags or DT_RIGHT or DT_RTLREADING;
    end;
  anyRect.Top:=(ARect.Top+ARect.Bottom-FTextHeight) div 2;
  anyRect.Bottom:=anyRect.Top+FTextHeight;
  ThemeServices.DrawText(Canvas, aDetail, ItemsEx[Index].Caption, anyRect, aFlags, 0);
end;

procedure TCustomComboBoxEx.FontChanged(Sender: TObject);
begin
  FNeedMeasure:=True;
  inherited FontChanged(Sender);
end;

procedure TCustomComboBoxEx.InitializeWnd;
begin
  inherited InitializeWnd;
  FRightToLeft:=IsRightToLeft;
end;

procedure TCustomComboBoxEx.Insert(AIndex: Integer; const ACaption: string; AIndent: Integer = -1;
            AImgIdx: TImageIndex = -1; AOverlayImgIdx: TImageIndex = -1; ASelectedImgIdx: TImageIndex = -1);
var aItem: TCollectionItem;
begin
  aItem:=ItemsEx.Insert(AIndex);
  with aItem as TComboExItem do
    begin
      Caption:=ACaption;
      Indent:=AIndent;
      ImageIndex:=AImgIdx;
      OverlayImageIndex:=AOverlayImgIdx;
      SelectedImageIndex:=ASelectedImgIdx;
    end;
end;

procedure TCustomComboBoxEx.SetItemHeight(const AValue: Integer);
begin
  inherited SetItemHeight(AValue);
  FNeedMeasure:=True;
end;

{ TCustomComboBoxEx.Setters }

procedure TCustomComboBoxEx.SetImages(AValue: TCustomImageList);
begin
  if FImages=AValue then exit;
  FImages:=AValue;
  Invalidate;
end;

procedure TCustomComboBoxEx.SetImagesWidth(const aImagesWidth: Integer);
begin
  if FImagesWidth = aImagesWidth then Exit;
  FImagesWidth := aImagesWidth;
  Invalidate;
end;

procedure TCustomComboBoxEx.SetStyle(AValue: TComboBoxExStyle);
begin
  if FStyle=AValue then exit;
  FStyle:=AValue;
end;

procedure TCustomComboBoxEx.SetStyleEx(AValue: TComboBoxExStyles);
begin
  if FStyleEx=AValue then exit;
  FStyleEx:=AValue;
end;

{ TCustomCheckCombo }

constructor TCustomCheckCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TStringList(Items).Duplicates:=dupIgnore;
  Style:=csOwnerDrawFixed;
  FNeedMeasure:=True;
  FRejectToggleOnSelect:=True;
end;

destructor TCustomCheckCombo.Destroy;
begin
  ClearItemStates;
  inherited Destroy;
end;

procedure TCustomCheckCombo.AddItem(const AItem: string; AState: TCheckBoxState; AEnabled: Boolean);
var pItemState: TCheckComboItemState;
begin
  pItemState:=TCheckComboItemState.Create;
  pItemState.State:=aState;
  pItemState.Enabled:=AEnabled;
  pItemState.Data:=nil;
  inherited AddItem(AItem, pItemState);
end;

procedure TCustomCheckCombo.AssignItems(AItems: TStrings);
begin
  ClearItemStates;
  Items.Assign(AItems);
  InitItemStates;
end;

procedure TCustomCheckCombo.CheckAll(AState: TCheckBoxState; AAllowGrayed: Boolean;
  AAllowDisabled: Boolean);
var i: Integer;
begin
  for i:=0 to Items.Count-1 do
  begin
    if (AAllowGrayed or (State[i]<>cbGrayed)) and (AAllowDisabled or ItemEnabled[i])
      then State[i]:=AState;
  end;
end;

procedure TCustomCheckCombo.Clear;
begin
  ClearItemStates;
  inherited Clear;
end;

procedure TCustomCheckCombo.ClearItemStates;
var i: Integer;
begin
  for i:=0 to Items.Count-1 do
  begin
    Items.Objects[i].Free;
    Items.Objects[i]:=nil;
  end;
end;

procedure TCustomCheckCombo.CloseUp;
begin
  FDropped:=False;
  if FRejectDropDown then
  begin
    FRejectDropDown:=False;
    Update;
  end else
    inherited CloseUp;
end;

procedure TCustomCheckCombo.CMBiDiModeChanged(var Message: TLMessage);
begin
  inherited CMBiDiModeChanged(Message);
  FRightToLeft:=IsRightToLeft;
  FNeedMeasure:=True;
  Invalidate;
end;

procedure TCustomCheckCombo.DeleteItem(AIndex: Integer);
begin
  if (AIndex>=0) and (AIndex<Items.Count) then
  begin
    Items.Objects[AIndex].Free;
    Items.Delete(AIndex);
  end;
end;

procedure TCustomCheckCombo.DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState);
                            { Enabled, State, Highlighted }
const caCheckThemes: array [Boolean, TCheckBoxState, Boolean] of TThemedButton =
                           { normal, highlighted }
        (((tbCheckBoxUncheckedDisabled, tbCheckBoxUncheckedDisabled),  { disabled, unchecked }
          (tbCheckBoxCheckedDisabled, tbCheckBoxCheckedDisabled),      { disabled, checked }
          (tbCheckBoxMixedDisabled, tbCheckBoxMixedDisabled)),         { disabled, greyed }
         ((tbCheckBoxUncheckedNormal, tbCheckBoxUncheckedHot),         { enabled, unchecked }
          (tbCheckBoxCheckedNormal, tbCheckBoxCheckedHot),             { enabled, checked }
          (tbCheckBoxMixedNormal, tbCheckBoxMixedHot)));               { enabled, greyed }
      cCheckIndent: SmallInt = 2;
      cTextIndent: SmallInt = 5;
var aDetail: TThemedElementDetails;
    aDropped: Boolean;
    aEnabled: Boolean;
    aFlags: Cardinal;
    aFocusedEditableMainItemNoDD: Boolean;  { combo has edit-like line edit in csDropDownList (Win) and is closed (not DroppedDown }
    aGray: Byte;
    anyRect: TRect;
    aState: TCheckBoxState;
    ItemState: TCheckComboItemState;
begin  { do not call inherited ! }
  ItemState:=TCheckComboItemState(Items.Objects[Index]);
  if not (ItemState is TCheckComboItemState) then
    QueueCheckItemStates;
  aDropped:=DroppedDown;
  if aDropped and FRejectDropDown then
    begin
      DroppedDown:=False;
      exit;  { Exit! }
    end;
  aEnabled:=IsEnabled;
  if not (csDesigning in ComponentState) then
    aEnabled:= (aEnabled and ItemState.Enabled);
  {$IF DEFINED(LCLWin32) or DEFINED(LCLWin64)}
  aFocusedEditableMainItemNoDD := (Focused and (ARect.Left>0) and not aDropped);
  {$ELSE}
  aFocusedEditableMainItemNoDD := False;
  {$ENDIF}
  if (ARect.Left=0) or aFocusedEditableMainItemNoDD then
    begin
      if odSelected in State then
        begin
          if not aEnabled then
            begin
              aGray:=ColorToGray(Canvas.Brush.Color);
              Canvas.Brush.Color:=RGBToColor(aGray, aGray, aGray);
            end;
        end else
        Canvas.Brush.Color:=clWindow;
      Canvas.Brush.Style:=bsSolid;
      Canvas.FillRect(ARect);
    end;
  if not (csDesigning in ComponentState)
    then aState:=ItemState.State
    else aState:=cbUnchecked;
  aDetail:=ThemeServices.GetElementDetails(caCheckThemes
    [aEnabled, aState, not aDropped and FCheckHighlight]);
  if FNeedMeasure then
    begin
      FCheckSize:=ThemeServices.GetDetailSize(aDetail);
      FTextHeight:=Canvas.TextHeight('ŠjÁÇ');
      if not aDropped then
        begin
          if not FRightToLeft then
            begin
              FHiLiteLeft:=-1;
              FHiLiteRight:=ARect.Right;
            end else
            begin
              FHiLiteLeft:=ARect.Left;
              FHiLiteRight:=ARect.Right;
            end;
          FNeedMeasure := False;
        end;
    end;
  if not FRightToLeft
    then anyRect.Left:=ARect.Left+cCheckIndent
    else anyRect.Left:=ARect.Right-cCheckIndent-FCheckSize.cx;
  anyRect.Right:=anyRect.Left+FCheckSize.cx;
  anyRect.Top:=(ARect.Bottom+ARect.Top-FCheckSize.cy) div 2;
  anyRect.Bottom:=anyRect.Top+FCheckSize.cy;
  ThemeServices.DrawElement(Canvas.Handle, aDetail, anyRect);
  Canvas.Brush.Style:=bsClear;
  if (not (odSelected in State) or not aDropped) and not aFocusedEditableMainItemNoDD
    then Canvas.Font.Color:=clWindowText
    else begin
       Canvas.Font.Color:=clHighlightText;
       FHilightedIndex:=Index;
    end;
  if aFocusedEditableMainItemNoDD then
    begin
      LCLIntf.SetBkColor(Canvas.Handle, ColorToRGB(clBtnFace));
      LCLIntf.DrawFocusRect(Canvas.Handle, aRect);
    end;
  aFlags:=DT_END_ELLIPSIS+DT_VCENTER+DT_SINGLELINE+DT_NOPREFIX;
  if not FRightToLeft then
    begin
      anyRect.Left:=ARect.Left+cCheckIndent+FCheckSize.cx+cTextIndent;
      anyRect.Right:=ARect.Right;
    end else
    begin
      anyRect.Right:=anyRect.Left-cTextIndent;
      anyRect.Left:=ARect.Left;
      aFlags:=aFlags or DT_RIGHT or DT_RTLREADING;
    end;
  anyRect.Top:=(ARect.Top+ARect.Bottom-FTextHeight) div 2;
  anyRect.Bottom:=anyRect.Top+FTextHeight;
  ThemeServices.DrawText(Canvas, aDetail, Items[Index], anyRect, aFlags, 0);
end;

procedure TCustomCheckCombo.DropDown;
{$IF DEFINED(LCLWin32) or DEFINED(LCLWin64)}
{$ELSE}
var aCursorPos: TPoint;
    aRect: TRect;
{$ENDIF}
begin
  {$IF DEFINED(LCLWin32) or DEFINED(LCLWin64)}
  FRejectDropDown:=False;
  {$ELSE}
  aCursorPos:=ScreenToControl(Mouse.CursorPos);
  aRect:=Rect(FHiLiteLeft, 0, FHiLiteRight, Height);
  FRejectDropDown:=PtInRect(aRect, aCursorPos);
  {$ENDIF}
  FDropped:=True;
  if not FRejectDropDown then
    begin
      inherited DropDown;
      FRejectToggleOnSelect:=False;
    end else
    if (ItemIndex>=0) and ItemEnabled[ItemIndex] then Toggle(ItemIndex);
end;

procedure TCustomCheckCombo.FontChanged(Sender: TObject);
begin
  FNeedMeasure:=True;
  inherited FontChanged(Sender);
end;

procedure TCustomCheckCombo.InitializeWnd;
begin
  InitItemStates;
  inherited InitializeWnd;
  CheckItemStates;
  FRightToLeft:=IsRightToLeft;
end;

procedure TCustomCheckCombo.InitItemStates;
var i: Integer;
    pItemState: TCheckComboItemState;
begin
  for i:=0 to Items.Count-1 do
    if Items.Objects[i]=nil then begin
      pItemState:=TCheckComboItemState.Create;
      pItemState.Enabled:=True;
      pItemState.State:=cbUnchecked;
      pItemState.Data:=nil;
      Items.Objects[i]:=pItemState;
    end else if not (Items.Objects[i] is TCheckComboItemState) then
      raise Exception.Create(DbgSName(Self)+': Item '+IntToStr(i)+' is not a TCheckComboItemState');
end;

procedure TCustomCheckCombo.CheckItemStates;
var
  i: Integer;
begin
  for i:=0 to Items.Count-1 do
    if not (Items.Objects[i] is TCheckComboItemState) then
      raise Exception.Create(DbgSName(Self)+': Item '+IntToStr(i)+' is not a TCheckComboItemState');
end;

procedure TCustomCheckCombo.QueueCheckItemStates;
begin
  Application.QueueAsyncCall(@AsyncCheckItemStates,0);
end;

procedure TCustomCheckCombo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      if FDropped then
        if (ItemIndex=FHilightedIndex) and ItemEnabled[ItemIndex] then Toggle(ItemIndex);
    VK_SPACE:
      if DroppedDown then
        if (ItemIndex>=0) and ItemEnabled[ItemIndex] then
        begin
          if ItemIndex<>FHilightedIndex then
          begin
            ItemIndex:=FHilightedIndex;
            inherited Select;
          end;
          Toggle(ItemIndex);
          DroppedDown:=False;
        end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TCustomCheckCombo.Loaded;
begin
  inherited Loaded;
  InitItemStates;
end;

procedure TCustomCheckCombo.MouseLeave;
begin
  FCheckHighlight:=False;
  inherited MouseLeave;
end;

procedure TCustomCheckCombo.MouseMove(Shift: TShiftState; X, Y: Integer);
var aHighlight: Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  aHighlight:=((X>FHiLiteLeft) and (X<FHiLiteRight));
  if aHighlight<>FCheckHighlight then
    begin
      FCheckHighlight:=aHighlight;
      Invalidate;
    end;
end;

procedure TCustomCheckCombo.Select;
begin
  inherited Select;
  {$IF DEFINED(LCLWin32) or DEFINED(LCLWin64)}
  if DroppedDown then FRejectToggleOnSelect:=True;
  {$ENDIF}
  if not FRejectToggleOnSelect then
    begin
      if ItemEnabled[ItemIndex] then Toggle(ItemIndex);
      FRejectToggleOnSelect:=True;
    end;
  FDropped:=False;
end;

procedure TCustomCheckCombo.SetItemHeight(const AValue: Integer);
begin
  inherited SetItemHeight(AValue);
  FNeedMeasure:=True;
end;

procedure TCustomCheckCombo.SetItems(const Value: TStrings);
begin
  ClearItemStates;
  inherited SetItems(Value);
  InitItemStates;
end;

procedure TCustomCheckCombo.Toggle(AIndex: Integer);
const caNewStateMap: array [TCheckBoxState, Boolean] of TCheckBoxState =
  { False (AllowGrayed) True }
  ((cbChecked, cbGrayed),       { cbUnchecked }
   (cbUnChecked, cbUnChecked),  { cbChecked }
   (cbChecked, cbChecked));     { cbGrayed }
begin
  State[AIndex]:=caNewStateMap[State[AIndex], AllowGrayed];
end;

{ TCustomCheckCombo.Getters and Setters }

function TCustomCheckCombo.GetChecked(AIndex: Integer): Boolean;
begin
  Result:=(TCheckComboItemState(Items.Objects[AIndex]).State=cbChecked);
end;

procedure TCustomCheckCombo.AsyncCheckItemStates(Data: PtrInt);
begin
  CheckItemStates;
end;

function TCustomCheckCombo.GetCount: Integer;
begin
  Result:=Items.Count;
end;

function TCustomCheckCombo.GetItemEnabled(AIndex: Integer): Boolean;
begin
  Result:=TCheckComboItemState(Items.Objects[AIndex]).Enabled;
end;

function TCustomCheckCombo.GetObject(AIndex: Integer): TObject;
begin
  Result:=TCheckComboItemState(Items.Objects[AIndex]).Data;
end;

function TCustomCheckCombo.GetState(AIndex: Integer): TCheckBoxState;
begin
  Result:=TCheckComboItemState(Items.Objects[AIndex]).State;
end;

procedure TCustomCheckCombo.SetChecked(AIndex: Integer; AValue: Boolean);
begin
  if AValue=(TCheckComboItemState(Items.Objects[AIndex]).State=cbChecked) then exit;
  if AValue
    then TCheckComboItemState(Items.Objects[AIndex]).State:=cbChecked
    else TCheckComboItemState(Items.Objects[AIndex]).State:=cbUnchecked;
  if Assigned(FOnItemChange) then
    FOnItemChange(Self, AIndex);
  if AIndex=ItemIndex then
    Invalidate;
end;

procedure TCustomCheckCombo.SetItemEnabled(AIndex: Integer; AValue: Boolean);
begin
  if TCheckComboItemState(Items.Objects[AIndex]).Enabled=AValue then exit;
  TCheckComboItemState(Items.Objects[AIndex]).Enabled:=AValue;
  if AIndex=ItemIndex then
    Invalidate;
end;

procedure TCustomCheckCombo.SetObject(AIndex: Integer; AValue: TObject);
begin
  TCheckComboItemState(Items.Objects[AIndex]).Data:=AValue;
end;

procedure TCustomCheckCombo.SetState(AIndex: Integer; AValue: TCheckBoxState);
begin
  if TCheckComboItemState(Items.Objects[AIndex]).State=AValue then exit;
  TCheckComboItemState(Items.Objects[AIndex]).State:=AValue;
  if Assigned(FOnItemChange) then
    FOnItemChange(self, AIndex);
  if AIndex=ItemIndex then
    Invalidate;
end;


