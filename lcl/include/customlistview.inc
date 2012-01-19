{%MainUnit ../comctrls.pp}
{
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

{ TIconOptions }

procedure TIconOptions.SetArrangement(Value: TIconArrangement);
begin
  if FArrangement <> Value then
  begin
    FArrangement := Value;
    if FListView.HandleAllocated then
      TWSCustomListViewClass(FListView.WidgetSetClass).SetIconArrangement(FListView, Arrangement);
  end;
end;

function TIconOptions.GetAutoArrange: Boolean;
begin
  Result := FListView.GetProperty(Ord(lvpAutoArrange));
end;

function TIconOptions.GetWrapText: Boolean;
begin
  Result := FListView.GetProperty(Ord(lvpWrapText));
end;

procedure TIconOptions.SetAutoArrange(Value: Boolean);
begin
  FListView.SetProperty(Ord(lvpAutoArrange), Value);
end;

procedure TIconOptions.SetWrapText(Value: Boolean);
begin
  FListView.SetProperty(Ord(lvpWrapText), Value);
end;

procedure TIconOptions.AssignTo(Dest: TPersistent);
var
  DestOptions: TIconOptions absolute Dest;
begin
  if Dest is TIconOptions then
  begin
    DestOptions.Arrangement := Arrangement;
    DestOptions.AutoArrange := AutoArrange;
    DestOptions.WrapText := WrapText;
  end
  else
    inherited AssignTo(Dest);
end;

function TIconOptions.GetOwner: TPersistent;
begin
  Result := FListView;
end;

constructor TIconOptions.Create(AOwner: TCustomListView);
begin
  inherited Create;
  FListView := AOwner;
  FArrangement := iaTop;
end;

{------------------------------------------------------------------------------
   TCustomListView Constructor
------------------------------------------------------------------------------}
constructor TCustomListView.Create(AOwner: TComponent);
var
  lvil: TListViewImageList;
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csCaptureMouse];
  FAutoSort := True;
  FSortDirection := sdAscending;
  FIconOptions := TIconOptions.Create(Self);
  FColumns := TListColumns.Create(Self);
  FListItems := TListItems.Create(Self);
  BorderStyle := bsSingle;
  FScrollBars := ssBoth;
  FCompStyle := csListView;
  FViewStyle := vsList;
  FSortType := stNone;

  for lvil := Low(TListViewImageList) to High(TListViewImageList) do
  begin
    FImageChangeLinks[lvil] := TChangeLink.Create;
    FImageChangeLinks[lvil].OnChange := @ImageChanged;
  end;
  FHoverTime := -1;
  TabStop := true;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  ParentColor := False;
  Color := {$ifdef UseCLDefault}clDefault{$else}clWindow{$endif};
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FProperties := [lvpColumnClick, lvpHideSelection, lvpShowColumnHeaders, lvpToolTips, lvpWrapText];
  
  FOwnerDataItem := TOwnerDataListItem.Create(FListItems);
end;

{------------------------------------------------------------------------------
   TCustomListView CustomDraw
------------------------------------------------------------------------------}
function TCustomListView.CustomDraw(const ARect: TRect; AStage: TCustomDrawStage): Boolean;
begin
  Result := True;
  if Assigned(FOnCustomDraw) and (AStage = cdPrePaint)
  then FOnCustomDraw(Self, ARect, Result);

  if Assigned(FOnAdvancedCustomDraw)
  then FOnAdvancedCustomDraw(Self, ARect, AStage, Result)
end;

{------------------------------------------------------------------------------}
{   TCustomListView CustomDrawItem                                             }
{------------------------------------------------------------------------------}
function TCustomListView.CustomDrawItem(AItem: TListItem; AState: TCustomDrawState; AStage: TCustomDrawStage): Boolean;
begin
  Result := True;
  if Assigned(FOnCustomDrawItem) and (AStage = cdPrePaint)
  then FOnCustomDrawItem(Self, AItem, AState, Result);
  
  if Assigned(FOnAdvancedCustomDrawItem)
  then FOnAdvancedCustomDrawItem(Self, AItem, AState, AStage, Result);
end;

{------------------------------------------------------------------------------}
{   TCustomListView CustomDrawSubItem                                          }
{------------------------------------------------------------------------------}
function TCustomListView.CustomDrawSubItem(AItem: TListItem; ASubItem: Integer; AState: TCustomDrawState; AStage: TCustomDrawStage): Boolean;
begin
  Result := True;
  if Assigned(FOnCustomDrawSubItem) and (AStage = cdPrePaint)
  then FOnCustomDrawSubItem(Self, AItem, ASubItem, AState, Result);
  
  if Assigned(FOnAdvancedCustomDrawSubItem)
  then FOnAdvancedCustomDrawSubItem(Self, AItem, ASubItem, AState, AStage, Result);

end;

{------------------------------------------------------------------------------}
{   TCustomListView Change                                                     }
{------------------------------------------------------------------------------}
procedure TCustomListView.Change(AItem: TListItem; AChange: Integer);
var
  ItemChange: TItemChange;
begin
  case AChange of
    LVIF_TEXT: ItemChange := ctText;
    LVIF_IMAGE: ItemChange := ctImage;
    LVIF_STATE: ItemChange := ctState;
  else
    Exit;
  end;
  if Assigned(FOnChange)
  then FOnChange(Self, AItem, ItemChange);
end;

{------------------------------------------------------------------------------}
{   TCustomListView ColClick                                                   }
{------------------------------------------------------------------------------}
procedure TCustomListView.ColClick(AColumn: TListColumn);
begin
  if Assigned(FOnColumnClick) and ColumnClick then FOnColumnClick(Self, AColumn);
  // we set autosort after FOnColumnClick, maybe programmer want to
  // stop autosorting after some special column is clicked.
  if FAutoSort then
  begin
    if SortType <> stNone then
    begin
      if AColumn.Index <> SortColumn then
        SortColumn := AColumn.Index
      else
      begin
        // with same column we are changing only direction
        if SortDirection = sdAscending then
          SortDirection := sdDescending
        else
          SortDirection := sdAscending;
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------}
{   TCustomListView CNNotify                                                   }
{------------------------------------------------------------------------------}
procedure TCustomListView.CNNotify(var AMessage: TLMNotify);
var
  nm: PNMListView;
  Item: TListItem;
  n: Integer;
begin
  nm := PNMListView(AMessage.NMHdr);
  // ignore any notifications while initializing items
  if (nm^.iItem >= Items.Count) or
     not (OwnerData or (lisfWSItemsCreated in FListItems.Flags)) then Exit;
  //remark: NMHdr^.code is normally unhanged by the win32 interface, so the others
  //        maps there codes to the of win32
  case AMessage.NMHdr^.code of 
//  HDN_TRACK:
//  NM_CUSTOMDRAW:
      // Custom Drawing is handled direct from the interfaces by IntfCustomDraw
//  LVN_BEGINDRAG:
    LVN_DELETEITEM: begin
      Item := FListItems[nm^.iItem];
      if FSelected = Item then
        InvalidateSelected;
      if Item = nil then Exit; //? nm^.iItem > Items.Count ?
      Exclude(Item.FFlags, lifCreated);
      if not (lifDestroying in Item.FFlags)
      then Item.Delete;
    end;
    LVN_DELETEALLITEMS: begin
      InvalidateSelected;
      for n := FListItems.Count - 1 downto 0 do
      begin
        Item := FListItems[n];
        Exclude(Item.FFlags, lifCreated);
        if not (lifDestroying in Item.FFlags)
        then Item.Delete;
      end;
    end;
//    LVN_GETDISPINFO:
//    LVN_ODCACHEHINT:
//    LVN_ODFINDITEM:
//    LVN_ODSTATECHANGED:
//    LVN_BEGINLABELEDIT:
//    LVN_ENDLABELEDIT:
    LVN_COLUMNCLICK:
    begin
      ColClick(Columns[nm^.iSubItem]);
    end;
    LVN_INSERTITEM: begin
      // don't call insert yet,
      // there is no solution available when we have inserted the item first
      // see delete
      // besides... who's inserting items
    end;
    LVN_ITEMCHANGING: begin
      //Check
    end;
    LVN_ITEMCHANGED:
    begin
      Item := Items[nm^.iItem];
      //DebugLn('TCustomListView.CNNotify Count=',dbgs(Items.Count),' nm^.iItem=',dbgs(nm^.iItem),' destroying=',dbgs(lifDestroying in Item.FFlags));
      if (Item <> nil) and (not OwnerData) and (lifDestroying in Item.FFlags) then
      begin
        if Item=FFocused then
          FFocused:=nil;
        if Item=FSelected then
          InvalidateSelected;
      end else
      begin
        // owner data
        Change(Item, nm^.uChanged);
        if (nm^.uChanged = LVIF_STATE) then
        begin
          // checkbox
          if Checkboxes then
            DoItemChecked(Item);

          // focus
          if (nm^.uOldState and LVIS_FOCUSED) <>
            (nm^.uNewState and LVIS_FOCUSED) then
          begin
            // focus state changed
            if (nm^.uNewState and LVIS_FOCUSED) = 0 then
            begin
              if FFocused = Item then
                FFocused := nil;
            end else
            begin
              FFocused := Item;
            end;
          end;
          // select
          if (nm^.uOldState and LVIS_SELECTED) <>
            (nm^.uNewState and LVIS_SELECTED) then
          begin
            // select state changed
            if (nm^.uNewState and LVIS_SELECTED) = 0 then
            begin
              if not OwnerData and (FSelected = Item) then
                InvalidateSelected
              else if OwnerData and (nm^.iItem=FSelectedIdx) then
              begin
                FSelectedIdx:=-1;
                InvalidateSelected;
              end;
              DoSelectItem(Item, False);
            end else
            begin
              FSelected := Item;
              Include(FFlags,lffSelectedValid);
              if OwnerData then FSelectedIdx:=nm^.iItem;
              //DebugLn('TCustomListView.CNNotify FSelected=',dbgs(FSelected));
              DoSelectItem(Item, True);
            end;
          end;
        end;
      end;
    end;
//    LVN_GETINFOTIP:
//    NM_CLICK:
//    NM_RCLICK:
  end;
end;

procedure TCustomListView.InvalidateSelected;
begin
  FSelected:=nil;
  FSelectedIdx := -1;
  Exclude(FFlags,lffSelectedValid);
end;

{------------------------------------------------------------------------------}
{   TCustomListView IsCustomDrawn                                              }
{------------------------------------------------------------------------------}
function TCustomListView.IsCustomDrawn(ATarget: TCustomDrawTarget; AStage: TCustomDrawStage): Boolean;
begin
  case ATarget of
    dtControl: Result := Assigned(FOnAdvancedCustomDraw)
                      or Assigned(FOnAdvancedCustomDrawItem)
                      or Assigned(FOnAdvancedCustomDrawSubItem);
    dtItem:    Result := Assigned(FOnAdvancedCustomDrawItem)
                      or Assigned(FOnAdvancedCustomDrawSubItem);
    dtSubItem: Result := Assigned(FOnAdvancedCustomDrawSubItem);
  end;
  
  if Result then exit;

  // check the normal events only in the prepaint stage
  if AStage <> cdPrePaint then Exit;

  case ATarget of
    dtControl: Result := Assigned(FOnCustomDraw)
                      or Assigned(FOnCustomDrawItem)
                      or Assigned(FOnCustomDrawSubItem);
    dtItem:    Result := Assigned(FOnCustomDrawItem)
                      or Assigned(FOnCustomDrawSubItem);
    dtSubItem: Result := Assigned(FOnCustomDrawSubItem);
  end;
end;

{------------------------------------------------------------------------------}
{   TCustomListView InitializeWnd                                              }
{------------------------------------------------------------------------------}
procedure TCustomListView.InitializeWnd;
var
  LVC: TWSCustomListViewClass;
  lvil: TListViewImageList;
begin
  inherited InitializeWnd;
  
  LVC := TWSCustomListViewClass(WidgetSetClass);

  // set the style first
  LVC.SetViewStyle(Self, FViewStyle);
  
  // add columns
  FColumns.WSCreateColumns;
  
  // set imagelists and item depending properties
  for lvil := Low(TListViewImageList) to High(TListViewImageList) do
  begin
    if FImages[lvil] <> nil
    then LVC.SetImageList(Self, lvil, FImages[lvil]);
  end;
  LVC.SetScrollBars(Self, FScrollBars);
  LVC.SetViewOrigin(Self, FViewOriginCache) ;
  LVC.SetProperties(Self, FProperties);
  LVC.SetSort(Self, FSortType, FSortColumn, FSortDirection);

  // add items
  if not OwnerData then 
  begin
    FListItems.WSCreateItems;
    // set other properties
    LVC.SetAllocBy(Self, FAllocBy);
  end 
  else 
  begin
    LVC.SetOwnerData(Self, True);
    LVC.SetItemsCount(Self, FListItems.Count);
  end;
  
  LVC.SetDefaultItemHeight(Self, FDefaultItemHeight);
  LVC.SetHotTrackStyles(Self, FHotTrackStyles);
  LVC.SetHoverTime(Self, FHoverTime);

  if FSelected <> nil 
  then LVC.ItemSetState(Self, FSelected.Index, FSelected, lisSelected, True);
  if FFocused <> nil
  then LVC.ItemSetState(Self, FFocused.Index, FFocused, lisFocused, True);
end;

{------------------------------------------------------------------------------}
{   TCustomListView DoDeletion                                                 }
{------------------------------------------------------------------------------}
procedure TCustomListView.DoDeletion(AItem: TListItem);
begin
  if Assigned(FOnDeletion) then FOnDeletion(Self, AItem);
end;

{------------------------------------------------------------------------------}
{   TCustomListView DoInsert                                                   }
{------------------------------------------------------------------------------}
procedure TCustomListView.DoInsert(AItem: TListItem);
begin
  if Assigned(FOnInsert) then FOnInsert(Self, AItem);
end;

{------------------------------------------------------------------------------}
{   TCustomListView DoItemChecked                                              }
{------------------------------------------------------------------------------}
procedure TCustomListView.DoItemChecked(AItem: TListItem);
var
  B: Boolean;
begin
  if (not HandleAllocated) or (csLoading in ComponentState) then exit;
  B := TWSCustomListViewClass(WidgetSetClass).ItemGetChecked(Self,
    AItem.Index, AItem);
  if B <> AItem.GetCheckedInternal then
  begin
    AItem.Checked := B;
    if Assigned(FOnItemChecked) then
      FOnItemChecked(Self, AItem);
  end;
end;

{------------------------------------------------------------------------------}
{   TCustomListView DoSelectItem                                               }
{------------------------------------------------------------------------------}
procedure TCustomListView.DoSelectItem(AItem: TListItem; ASelected: Boolean);
begin
  AItem.Selected:=ASelected;
  if Assigned(FOnSelectItem) and
    ([lffItemsMoving, lffItemsSorting] * FFlags = []) then
      FOnSelectItem(Self, AItem, ASelected);
end;

{------------------------------------------------------------------------------}
{   TCustomListView ItemDeleted                                                }
{------------------------------------------------------------------------------}
procedure TCustomListView.ItemDeleted(const AItem: TListItem);  //called by TListItems
begin
  //DebugLn('TCustomListView.ItemDeleted ',dbgs(AItem),' FSelected=',dbgs(FSelected));
  if FSelected = AItem then InvalidateSelected;
  if FFocused = AItem then FFocused := nil;
  if csDestroying in Componentstate then Exit;
  DoDeletion(AItem);
end;

{------------------------------------------------------------------------------}
{   TCustomListView ItemInserted                                               }
{------------------------------------------------------------------------------}
procedure TCustomListView.ItemInserted(const AItem: TListItem);
begin
  if csDestroying in Componentstate then Exit;
  DoInsert(AItem);
end;

class procedure TCustomListView.WSRegisterClass;
begin
  RegisterPropertyToSkip(Self, 'ItemIndex', 'Property streamed in older Lazarus revision', '');
  RegisterPropertyToSkip(Self, 'BevelKind', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TListItem, 'OverlayIndex', 'VCL compatibility property', '');
  inherited WSRegisterClass;
  RegisterCustomListView;
end;

class function TCustomListView.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 250;
  Result.CY := 150;
end;

{------------------------------------------------------------------------------}
{   TCustomListView SetItems                                                   }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetItems(const AValue : TListItems);
begin
end;

{------------------------------------------------------------------------------}
{   TCustomListView SetItemVisible                                             }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetItemVisible(const AValue : TListItem;
                                                     const APartialOK: Boolean);
begin
  if (not HandleAllocated) or (csLoading in ComponentState) then exit;
  TWSCustomListViewClass(WidgetSetClass).ItemShow(
                                        Self, AValue.Index, AValue, APartialOK);
end;

{------------------------------------------------------------------------------}
{   TCustomListView Delete                                                     }
{------------------------------------------------------------------------------}
procedure TCustomListView.Delete(Item : TListItem);
begin
end;

{------------------------------------------------------------------------------}
{   TCustomListView InsertItem                                                 }
{------------------------------------------------------------------------------}
procedure TCustomListView.InsertItem(Item : TListItem);
begin
end;

function TCustomListView.IntfCustomDraw(ATarget: TCustomDrawTarget; AStage: TCustomDrawStage; AItem, ASubItem: Integer; AState: TCustomDrawState; const ARect: PRect): TCustomDrawResult;
begin
  Result := [];
  
  // in the prepaint stage, return the notifications we want
  if AStage = cdPrePaint
  then begin
    case ATarget of
      dtControl: begin
        if IsCustomDrawn(dtItem, cdPrePaint) or IsCustomDrawn(dtItem, cdPreErase)
        then Include(Result, cdrNotifyItemDraw);

        if IsCustomDrawn(dtItem, cdPostPaint)
        then Include(Result, cdrNotifyPostPaint);

        if IsCustomDrawn(dtItem, cdPostErase)
        then Include(Result, cdrNotifyPostErase);

        if IsCustomDrawn(dtSubItem, cdPrePaint)
        then Include(Result, cdrNotifySubitemDraw);
      end;
      dtItem: begin
        if IsCustomDrawn(dtItem, cdPostPaint)
        then Include(Result, cdrNotifyPostPaint);

        if IsCustomDrawn(dtItem, cdPostErase)
        then Include(Result, cdrNotifyPostErase);

        if IsCustomDrawn(dtSubItem, cdPrePaint)
        then Include(Result, cdrNotifySubitemDraw);
      end;
      dtSubItem: begin
        if IsCustomDrawn(dtSubItem, cdPostPaint)
        then Include(Result, cdrNotifyPostPaint);

        if IsCustomDrawn(dtSubItem, cdPostErase)
        then Include(Result, cdrNotifyPostErase);
      end;
    end;
  end;

  if not IsCustomDrawn(ATarget, AStage) then Exit;


  case ATarget of
    dtControl: if CustomDraw(ARect^, AStage) then Exit;
    dtItem:    if CustomDrawItem(Items[AItem], AState, AStage) then Exit;
    dtSubItem: if CustomDrawSubItem(Items[AItem], ASubItem, AState, AStage) then Exit;
  end;

  // if we are here, a custom step returned false, so no default drawing
  if AStage = cdPrePaint
  then Result := [cdrSkipDefault];
end;

function TCustomListView.GetUpdateCount: Integer;
begin
  Result := FUpdateCount;
end;

procedure TCustomListView.DoGetOwnerData(Item: TListItem); 
begin
  if Assigned(OnData) then OnData(Self, Item);
end;

{------------------------------------------------------------------------------}
{   TCustomListView SetColumns                                                 }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetColumns(const AValue: TListColumns);
begin
  if AValue=FColumns then exit;
  BeginUpdate;
  FColumns.Assign(AValue);
  EndUpdate;
  if ([csDesigning,csLoading,csReading]*ComponentState=[csDesigning]) then
    OwnerFormDesignerModified(Self);
end;


{------------------------------------------------------------------------------}
{   TCustomListView SetViewOrigin                                              }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetViewOrigin(AValue: TPoint);
begin
  if AValue.X < 0 then AValue.X := 0;
  if AValue.Y < 0 then AValue.Y := 0;
  if HandleAllocated
  then begin
    TWSCustomListViewClass(WidgetSetClass).SetViewOrigin(Self, AValue);
  end
  else begin
    FViewOriginCache := AValue;
  end;
end;

{------------------------------------------------------------------------------}
{   TCustomListView SetViewStyle                                               }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetViewStyle(const AValue: TViewStyle);
begin
  if FViewStyle = AValue then Exit;
  FViewStyle := AValue;
  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).SetViewStyle(Self, AValue);
end;

{------------------------------------------------------------------------------}
{   TCustomListView SetSortType                                                }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetSortType(const AValue: TSortType);
begin
  if FSortType = AValue then Exit;
  FSortType := AValue;
  Sort;
end;

{------------------------------------------------------------------------------}
{   TCustomListView SetSortColumn                                              }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetSortColumn(const AValue : Integer);
begin
  if FSortColumn = AValue then Exit;
  FSortColumn := AValue;
  Sort;
end;

procedure TCustomListView.SetSortDirection(const AValue: TSortDirection);
begin
  if FSortDirection=AValue then exit;
  FSortDirection:=AValue;
  Sort;
end;

function CompareItems(Item1, Item2: Pointer): Integer;
var
  Str1: String;
  Str2: String;
  ListView: TCustomListView;
begin
  Result := 0;
  ListView := TListItem(Item1).Owner.Owner;
  if Assigned(ListView.FOnCompare) then
    ListView.FOnCompare(ListView, TListItem(Item1), TListItem(Item2),0 ,Result)
  else
  begin
    if ListView.SortType in [stData] then
      Result := CompareValue(PtrUInt(TListItem(Item1).Data), PtrUInt(TListItem(Item2).Data))
    else
    begin
      if ListView.FSortColumn = 0 then
      begin
        Str1 := TListItem(Item1).Caption;
        Str2 := TListItem(Item2).Caption;
      end else
      begin
        if ListView.FSortColumn <= TListItem(Item1).SubItems.Count then
          Str1 := TListItem(Item1).SubItems.Strings[ListView.FSortColumn-1]
        else
          Str1 := '';
        if ListView.FSortColumn <= TListItem(Item2).SubItems.Count then
          Str2 := TListItem(Item2).SubItems.Strings[ListView.FSortColumn-1]
        else
          Str2 := '';
      end;
      Result := AnsiCompareText(Str1, Str2);
    end;
    if ListView.SortDirection = sdDescending then
      Result := -Result;
  end;
end;

{------------------------------------------------------------------------------}
{   TCustomListView Sort                                                       }
{------------------------------------------------------------------------------}
procedure TCustomListView.Sort;
var
  FSavedSelection: TFPList;
  FSavedFocused: TListItem;
  i: Integer;
  AItemIndex: Integer;
begin
  if FSortType = stNone then exit;
  if FListItems.Count < 2 then Exit;
  if lffPreparingSorting in FFlags then
    exit;

  if HandleAllocated then
  begin
    Include(FFlags, lffItemsSorting);
    FSavedSelection := TFPList.Create;
    try
      if (ItemIndex >= 0) then
        FSavedFocused := Items[ItemIndex]
      else
        FSavedFocused := nil;
      if Assigned(Selected) then
      begin
        FSavedSelection.Add(Selected);
        if MultiSelect then
        begin
          for i := 0 to Items.Count-1 do
          begin
            if Items[i].Selected and (Items[i] <> Selected) then
              FSavedSelection.Add(Items[i]);
          end;
        end;
      end;

      FListItems.FItems.Sort(@CompareItems);
      TWSCustomListViewClass(WidgetSetClass).SetSort(Self, FSortType,
        FSortColumn, FSortDirection);

      if (FSavedSelection.Count > 0) or Assigned(FSavedFocused) then
      begin
        Selected := nil; // unselect all

        if FSavedFocused <> nil then
          TWSCustomListViewClass(WidgetSetClass).ItemSetState(Self,
            FSavedFocused.Index, FSavedFocused, lisFocused, True);

        for i := FSavedSelection.Count - 1 downto 0 do
        begin
          AItemIndex := Items.IndexOf(TListItem(FSavedSelection.Items[i]));
          if AItemIndex <> -1 then
            TWSCustomListViewClass(WidgetSetClass).ItemSetState(Self,
              AItemIndex, Items[AItemIndex], lisSelected, True);
        end;
      end;
    finally
      FreeThenNil(FSavedSelection);
      Exclude(FFlags, lffItemsSorting);
    end;
  end else
    FListItems.FItems.Sort(@CompareItems);
end;

{------------------------------------------------------------------------------}
{   TCustomListView Destructor                                                 }
{------------------------------------------------------------------------------}
destructor TCustomListView.Destroy;
var
  lvil: TListViewImageList;
begin
  // Better destroy the wincontrol (=widget) first. So wo don't have to delete
  // all items/columns and we won't get notifications for each.
  FreeAndNil(FCanvas);
  inherited Destroy;
  FreeAndNil(FColumns);
  for lvil := Low(TListViewImageList) to High(TListViewImageList) do
    FreeAndNil(FImageChangeLinks[lvil]);
  FreeAndNil(FOwnerDataItem);
  FreeAndNil(FListItems);
  FreeAndNil(FIconOptions);
end;

function TCustomListView.AlphaSort: Boolean;
begin
  Result := False;
  Include(FFlags, lffPreparingSorting);
  // always reset direction, so sort triggers later !
  SortDirection := sdDescending;
  SortType := stText;
  SortColumn := 0;
  Exclude(FFlags, lffPreparingSorting);
  // now trigger sort when all rules are applied
  SortDirection := sdAscending;
  Result := True;
end;

{------------------------------------------------------------------------------
   TCustomListView DestroyWnd
   Params: None
   Result: none

   Frees the canvas
 ------------------------------------------------------------------------------}
procedure TCustomListView.DestroyWnd; 
begin
  if FCanvas<>nil then
    TControlCanvas(FCanvas).FreeHandle;
  inherited DestroyWnd;
end;

procedure TCustomListView.BeginAutoDrag;
begin
  BeginDrag(False);
end;

{------------------------------------------------------------------------------
   TCustomListView BeginUpdate
   Params: None
   Result: none

   Increases the update count. Use this procedure before any big change, so that
   the interface will not show any single step.
 ------------------------------------------------------------------------------}
procedure TCustomListView.BeginUpdate;
begin
  Inc(FUpdateCount);
  if (FUpdateCount = 1) and HandleAllocated
  then TWSCustomListViewClass(WidgetSetClass).BeginUpdate(Self);
end;

procedure TCustomListView.Clear;
begin
  FListItems.Clear;
end;

{------------------------------------------------------------------------------}
{   TCustomListView EndUpdate                                               }
{------------------------------------------------------------------------------}
procedure TCustomListView.EndUpdate;
begin
  if FUpdateCount <= 0
  then RaiseGDBException('TCustomListView.EndUpdate FUpdateCount=0');
  
  Dec(FUpdateCount);
  if (FUpdateCount = 0) and HandleAllocated
  then TWSCustomListViewClass(WidgetSetClass).EndUpdate(Self);
end;

procedure TCustomListView.Repaint;
begin
  if OwnerData then
    // the last cached item might be left updated, because OnData isn't called!
    FOwnerDataItem.SetDataIndex(-1);
  inherited Repaint;
end;

procedure TCustomListView.FinalizeWnd;
begin
  // store origin
  FViewOriginCache := TWSCustomListViewClass(WidgetSetClass).GetViewOrigin(Self);
  if not OwnerData then
    FListItems.DoFinalizeWnd;
  inherited FinalizeWnd;
end;

function TCustomListView.FindCaption(StartIndex: Integer; Value: string;
  Partial, Inclusive, Wrap: Boolean; PartStart: Boolean = True): TListItem;
begin
  Result := FListItems.FindCaption(StartIndex, Value, Partial, Inclusive, Wrap);
end;

function TCustomListView.FindData(StartIndex: Integer; Value: Pointer;
  Inclusive, Wrap: Boolean): TListItem;
begin
  Result := FListItems.FindData(StartIndex, Value, Inclusive, Wrap);
end;

function TCustomListView.GetBoundingRect: TRect;
begin
  if not HandleAllocated
  then Result := Rect(0,0,0,0)
  else Result := TWSCustomListViewClass(WidgetSetClass).GetBoundingRect(Self);
end;

function TCustomListView.GetColumnCount: Integer;
begin
  Result := FColumns.Count;
end;

function TCustomListView.GetColumnFromIndex(AIndex: Integer): TListColumn;
begin
  Result := FColumns[AIndex];
end;

function TCustomListView.GetDropTarget: TListItem;
var
  idx: Integer;
begin
  if not HandleAllocated
  then idx := -1
  else idx := TWSCustomListViewClass(WidgetSetClass).GetDropTarget(Self);
  if idx = -1
  then Result := nil
  else Result := FListItems[idx];
end;

function TCustomListView.GetFocused: TListItem;
begin
  Result := FFocused;
end;

function TCustomListView.GetImageList(const ALvilOrd: Integer): TCustomImageList;
begin
  Result := FImages[TListViewImageList(ALvilOrd)];
end;

function TCustomListView.GetHoverTime: Integer;
begin
  if HandleAllocated
  then Result := TWSCustomListViewClass(WidgetSetClass).GetHoverTime(Self)
  else Result := FHoverTime;
end;

function TCustomListView.GetItemIndex: Integer;
begin
  Result := -1;
  if not OwnerData then begin
    if Selected = nil then Exit;
    Result := Selected.Index
  end else
    Result := FSelectedIdx;
end;

function TCustomListView.GetHitTestInfoAt(X, Y: Integer): THitTests;
begin
  Result := [];
  if HandleAllocated then
    Result := TWSCustomListViewClass(WidgetSetClass).GetHitTestInfoAt( Self, X, Y );
end;

function TCustomListView.GetItemAt(x,y: Integer): TListItem;
var 
  Item: Integer;
begin      
  Result := nil; 
  if HandleAllocated
  then begin
    Item := TWSCustomListViewClass(WidgetSetClass).GetItemAt(Self,x,y);
    if Item <> -1 
    then Result := Items[Item];
  end;
end;

function TCustomListView.GetProperty(const ALvpOrd: Integer): Boolean;
begin
  Result := (TListViewProperty(ALvpOrd) in FProperties);
end;

function TCustomListView.GetSelCount: Integer;
var
  i: integer;
begin
  if HandleAllocated
  then Result := TWSCustomListViewClass(WidgetSetClass).GetSelCount(Self)
  else
  begin
    Result := 0;
    for i := 0 to Items.Count - 1 do
      if Items[i].Selected then
        inc(Result);
  end;
end;

{------------------------------------------------------------------------------
   TCustomListView GetSelection
------------------------------------------------------------------------------}
function TCustomListView.GetSelection: TListItem;
var
  i: Integer;
begin
  if not OwnerData then
  begin
    {according to Delphi docs
     we always must return first selected item, not the last selected one
     see issue #16773}
    if not (lffSelectedValid in FFlags) or MultiSelect then
    begin
      FSelected := nil;
      for i := 0 to Items.Count - 1 do
      begin
        if Items[i].Selected then
        begin
          FSelected := Items[i];
          break;
        end;
      end;
      Include(FFlags, lffSelectedValid);
    end;
    Result := FSelected;
  end else begin
    if FSelectedIdx>=0 then begin
      FOwnerDataItem.SetDataIndex(FSelectedIdx);
      Result:=FOwnerDataItem;
    end else
      Result:=nil;
  end;
end;

function TCustomListView.GetTopItem: TListItem;
var
  idx: Integer;
begin
  if ViewStyle in [vsSmallIcon, vsIcon]
  then idx := -1
  else idx := TWSCustomListViewClass(WidgetSetClass).GetTopItem(Self);
  if idx = -1
  then Result := nil
  else Result := FListItems[idx];
end;

{------------------------------------------------------------------------------}
{   TCustomListView GetViewOrigin                                              }
{------------------------------------------------------------------------------}
function TCustomListView.GetViewOrigin: TPoint;
begin
  if HandleAllocated
  then begin
    Result := TWSCustomListViewClass(WidgetSetClass).GetViewOrigin(Self);
  end
  else begin
    Result := FViewOriginCache;
  end;
end;


function TCustomListView.GetVisibleRowCount: Integer;
begin
  if ViewStyle in [vsReport, vsList]
  then Result := TWSCustomListViewClass(WidgetSetClass).GetVisibleRowCount(Self)
  else Result := 0;
end;

procedure TCustomListView.SetAllocBy(const AValue: Integer);
begin
  if FAllocBy = AValue then Exit;
  FAllocBy := AValue;
  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).SetAllocBy(Self, AValue);
end;

procedure TCustomListView.SetDefaultItemHeight(AValue: Integer);
begin
  if AValue <=0 then AValue := 20;
  if AValue = FDefaultItemHeight then Exit;
  FDefaultItemHeight := AValue;
  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).SetDefaultItemHeight(Self, AValue);
end;

procedure TCustomListView.SetDropTarget(const AValue: TListItem);
begin
  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).ItemSetState(Self, AValue.Index, AValue, lisDropTarget, True);
end;

procedure TCustomListView.SetFocused(const AValue: TListItem);
begin
  if FFocused = AValue then exit;
  FFocused := AValue;
  if not HandleAllocated then
    Exit;
  if FFocused <> nil then
    TWSCustomListViewClass(WidgetSetClass).ItemSetState(Self, FFocused.Index, FFocused, lisFocused, True);
end;

procedure TCustomListView.SetHotTrackStyles(const AValue: TListHotTrackStyles);
begin
  if FHotTrackStyles = AValue then Exit;
  FHotTrackStyles := AValue;
  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).SetHotTrackStyles(Self, AValue);
end;

procedure TCustomListView.SetHoverTime(const AValue: Integer);
begin
  if FHoverTime = AValue then Exit;
  FHoverTime := AValue;
  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).SetHoverTime(Self, FHoverTime);
end;

procedure TCustomListView.SetIconOptions(const AValue: TIconOptions);
begin
  FIconOptions.Assign(AValue);
end;

procedure TCustomListView.SetImageList(const ALvilOrd: Integer; const AValue: TCustomImageList);
var
  lvil: TListViewImageList;
begin
  lvil := TListViewImageList(ALvilOrd);

  if FImages[lvil] = AValue then Exit;

  if FImages[lvil] <> nil
  then FImages[lvil].UnregisterChanges(FImageChangeLinks[lvil]);

  FImages[lvil] := AValue;

  if FImages[lvil] <> nil
  then begin
    FImages[lvil].RegisterChanges(FImageChangeLinks[lvil]);
    FImages[lvil].FreeNotification(self);
  end;

  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).SetImageList(Self, lvil, AValue);
end;

procedure TCustomListView.SetItemIndex(const AValue: Integer);
begin
  if (AValue < -1) or (AValue >= Items.Count) then
    raise Exception.CreateFmt(rsListIndexExceedsBounds,[AValue]);
  if AValue = -1 then
    Selected := nil
  else
    Selected := Items.Item[AValue];
end;

{------------------------------------------------------------------------------
   TCustomListView SetSelection
------------------------------------------------------------------------------}
procedure TCustomListView.SetSelection(const AValue: TListItem);
var
  i: integer;
begin
  if (AValue<>nil) and (AValue.ListView<>Self) then
    raise Exception.Create('Item does not belong to this listview');
  if FSelected = AValue then Exit;
  //DebugLn('TCustomListView.SetSelection FSelected=',dbgs(FSelected));
  if AValue = nil then begin
    if MultiSelect then begin
      BeginUpdate;
      try
        for i:=0 to Items.Count-1 do
          with Items[i] do
            if Selected then
              Selected:=False;
      finally
        EndUpdate;
      end;
    end
    else
      FSelected.Selected := False;
    FSelected := nil;
    Include(FFlags,lffSelectedValid);
  end
  else begin
    FSelected := AValue;
    if HandleAllocated then
      TWSCustomListViewClass(WidgetSetClass).ItemSetState(Self, FSelected.Index,
                                                      FSelected, lisSelected, True);
  end;
end;

procedure TCustomListView.SetOwnerData(const AValue: Boolean);
begin
  if FOwnerData=AValue then exit;
  FOwnerData:=AValue;
  FOwnerDataItem.SetOwner(nil);
  Items.Free;
  if AValue then
  begin
    FSelectedIdx:=-1;
    FListItems:=TOwnerDataListItems.Create(Self);
  end else
    FListItems:=TListItems.Create(Self);

  if HandleAllocated then 
    TWSCustomListViewClass(WidgetSetClass).SetOwnerData(Self, AValue);
  FOwnerDataItem.SetOwner(FListItems);
end;

procedure TCustomListView.SetProperty(const ALvpOrd: Integer;
  const AIsSet: Boolean);
var
  AProp: TListViewProperty;
begin
  AProp := TListViewProperty(ALvpOrd);
  if (AProp in FProperties) = AIsSet then Exit;

  if AIsSet
  then Include(FProperties, AProp)
  else Exclude(FProperties, AProp);

  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).SetProperty(Self, AProp, AIsSet);
end;

procedure TCustomListView.ImageChanged(Sender : TObject);
begin
  if csDestroying in ComponentState Then Exit;
// TODO: move Imagelist to interface, image changes can be update there
//  if FUpdateCount>0 then
//    Include(FStates,lvUpdateNeeded)
//  else begin
//    //image changed so redraw it all....
//    UpdateProperties;
//  end;
end;

procedure TCustomListView.Loaded;
begin
  // create interface columns if needed
  if HandleAllocated then
    FColumns.WSCreateColumns;
  inherited Loaded;
end;

procedure TCustomListView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then begin
    if AComponent = LargeImages then LargeImages := nil;
    if AComponent = SmallImages then SmallImages := nil;
    if AComponent = StateImages then StateImages := nil;
  end;
end;

procedure TCustomListView.SetScrollBars(const AValue: TScrollStyle);
begin
  if (FScrollBars = AValue) then exit;
  FScrollBars := AValue;
  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).SetScrollBars(Self, AValue);
  UpdateScrollBars;
end;

procedure TCustomListView.UpdateScrollbars;
begin
  // this needs to be done in the widgetset
  DebugLn('TODO: TCustomListView.UpdateScrollbars');
  exit;

  if not HandleAllocated then exit;
end;

