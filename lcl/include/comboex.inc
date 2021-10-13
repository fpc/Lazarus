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
  DrawText(Canvas.Handle, PChar(ItemsEx[Index].Caption), Length(ItemsEx[Index].Caption), anyRect, aFlags);
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


