
{------------------------------------------------------------------------------}
{   TCustomListView Constructor                                                           }
{------------------------------------------------------------------------------}
constructor TCustomListView.Create(Aowner: TComponent);
begin
  Inherited Create(AOwner);

  //ParentWindow := TScrolledwindow.Create(AOwner);
  //ParentWindow.Show;
  //fix this call it is a hack

  Columns := TViewColumns.Create(self);
  Columns.OnChange := @ColumnsChanged;
  FListItems := TListITems.Create(self);
  FBorderStyle := bsSingle;
  FScrollBars:=ssBoth;
  FImages := NIL;
  fCompStyle := csListView;
  fViewStyle := vsList;
  fSorted := False;
  fSortColumn := 0;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageChanged;
  Setbounds(2,2,300,300);
end;


{------------------------------------------------------------------------------}
{   TCustomListView ColumnsChanged                                             }
{------------------------------------------------------------------------------}
Procedure TCustomListView.ColumnsChanged(Sender : TObject);
Begin
  //notify the interface....
  RecreateWnd;
  CNSendMessage(LM_SETPROPERTIES,self,nil);
End;

{------------------------------------------------------------------------------}
{   TCustomListView ItemChanged                                             }
{------------------------------------------------------------------------------}
Procedure TCustomListView.ItemChanged(Index : Integer);  //called by TListItems
Begin
  //notify the interface....
  CNSendMessage(LM_LV_CHANGEITEM,self,@Index);
End;

{------------------------------------------------------------------------------}
{   TCustomListView ItemChanged                                             }
{------------------------------------------------------------------------------}
Procedure TCustomListView.ItemDeleted(Index : Integer);  //called by TListItems
Begin
  //notify the interface....
  CNSendMessage(LM_LV_DELETEITEM,self,@Index);
End;



{------------------------------------------------------------------------------}
{   TCustomListView ItemAdded                                             }
{------------------------------------------------------------------------------}
Procedure TCustomListView.ItemAdded;
Begin
  //notify the interface....
  CNSendMessage(LM_LV_ADDITEM,self,nil);
End;

{------------------------------------------------------------------------------}
{   TCustomListView SetItems                                                   }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetItems(Value : TListItems);
begin
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

{------------------------------------------------------------------------------}
{   TCustomListView SetViewStyle                                               }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetViewStyle(Value : TViewStyle);
begin
  if FViewStyle = Value then Exit;
  FViewStyle := Value;
  CNSendMessage(LM_SETPROPERTIES,self,nil);
end;


{------------------------------------------------------------------------------}
{   TCustomListView SetSorted                                                  }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetSorted(Value : Boolean);
begin
  if FSorted = Value then Exit;
  FSorted := Value;
  CNSendMessage(LM_SETPROPERTIES,self,nil);
end;

{------------------------------------------------------------------------------}
{   TCustomListView SetSortColumn                                              }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetSortColumn(Value : Integer);
begin
  if FSortColumn = Value then Exit;
  FSortColumn := Value;
  CNSendMessage(LM_SETPROPERTIES,self,nil);
end;




{------------------------------------------------------------------------------}
{   TCustomListView Destructor                                                 }
{------------------------------------------------------------------------------}
destructor TCustomListView.Destroy;
begin
  Columns.Free;
  FImageChangeLink.Free;
  FListItems.Free;
  inherited Destroy;
end;

procedure TCustomListView.SetMultiSelect(const AValue: Boolean);
begin
  if FMultiSelect <> AValue then
     Begin
       FMultiSelect := AValue;
       CNSendMessage(LM_SETPROPERTIES,self,nil);
     end;
     
end;

procedure TCustomListView.SetImages(const AValue: TCustomImageList);
begin
    if AValue <> FImages then
     Begin
        if FImages <> nil then
           FImages.UnregisterChanges(FImageChangeLink);

           FImages := AValue;

        if FImages <> nil then
           Begin
             FImages.RegisterChanges(FImageChangeLink);
             FImages.FreeNotification(self);
           end;
        CNSendMessage(LM_SETPROPERTIES,self,nil);
     end;
end;

Procedure TCustomListView.ImageChanged(Sender : TObject);
begin
  //image changed so redraw it all....
        CNSendMessage(LM_SETPROPERTIES,self,nil);
end;

procedure TCustomListView.SetScrollBars(const Value: TScrollStyle);
begin
  if (FScrollBars <> Value) then
  begin
    FScrollBars := Value;
    RecreateWnd;
    UpdateScrollBars;
  end;
end;

procedure TCustomListView.SetScrolledLeft(AValue: integer);
begin
  if AValue<0 then AValue:=0;
  if AValue=FScrolledLeft then exit;
//temp  if AValue>GetMaxScrollLeft then AValue:=GetMaxScrollLeft;
  if AValue=FScrolledLeft then exit;
  FScrolledLeft:=AValue;
//  Include(FStates,tvsScrollbarChanged);
  Invalidate;
end;

procedure TCustomListView.SetScrolledTop(AValue: integer);
begin
  if FScrolledTop=AValue then exit;
  if AValue<0 then AValue:=0;
//temp  if AValue>GetMaxScrollTop then AValue:=GetMaxScrollTop;
  if AValue=FScrolledTop then exit;
  FScrolledTop:=AValue;
//  FStates:=FStates+[tvsTopItemNeedsUpdate,tvsBottomItemNeedsUpdate,
//                    tvsScrollbarChanged];
  Invalidate;
end;

procedure TCustomListView.UpdateScrollbars;
var
  ScrollInfo: TScrollInfo;
begin
  if not HandleAllocated then exit
  else
  begin
  //  Exclude(FStates,tvsScrollbarChanged);
    if fScrollBars <> ssNone then begin
      ScrollInfo.cbSize := SizeOf(ScrollInfo);
      ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
      ScrollInfo.nTrackPos := 0;
      if fScrollBars in [ssBoth, ssHorizontal] then begin
        // horizontal scrollbar
        ScrollInfo.nMin := 0;
        ScrollInfo.nPage := (ClientWidth-ScrollBarWidth)-2*BorderWidth;
        if ScrollInfo.nPage<1 then ScrollInfo.nPage:=1;
        ScrollInfo.nMax := {GetMaxScrollLeft+}ScrollInfo.nPage;
        if ScrollInfo.nMax<1 then ScrollInfo.nMax:=1;
        ScrollInfo.nPos := FScrolledLeft;
        if not CompareMem(@ScrollInfo,@FLastHorzScrollInfo,SizeOf(TScrollInfo))
        then begin
          FLastHorzScrollInfo:=ScrollInfo;
          SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
          ShowScrollBar(Handle,SB_HORZ,True);
        end;
      end;
      if fScrollBars in [ssBoth, ssVertical] then begin
        // vertical scrollbar
        ScrollInfo.nMin := 0;
        ScrollInfo.nPage := (ClientHeight-ScrollBarWidth)-FDefItemHeight;
        if ScrollInfo.nPage<1 then ScrollInfo.nPage:=1;
        ScrollInfo.nMax := {GetMaxScrollTop+}ScrollInfo.nPage;
        if ScrollInfo.nMax<1 then ScrollInfo.nMax:=1;
        ScrollInfo.nTrackPos := 0;
        ScrollInfo.nPos := FScrolledTop;
        if not CompareMem(@ScrollInfo,@FLastVertScrollInfo,SizeOf(TScrollInfo))
        then begin
          FLastVertScrollInfo:=ScrollInfo;
          SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
          ShowScrollBar(Handle,SB_VERT,True);
        end;
      end;
    end;
  end;
end;

procedure TCustomListView.WMHScroll(var Msg: TLMScroll);
begin
  case Msg.ScrollCode of
      // Scrolls to start / end of the text
    SB_LEFT:       ScrolledLeft := 0;
    SB_RIGHT:      ScrolledLeft := width{GetMaxScrollLeft};
      // Scrolls one line left / right
    SB_LINERIGHT:  ScrolledLeft := ScrolledLeft + FDefItemHeight div 2;
    SB_LINELEFT:   ScrolledLeft := ScrolledLeft - FDefItemHeight div 2;
      // Scrolls one page of lines left / right
    SB_PAGERIGHT:  ScrolledLeft := ScrolledLeft + (ClientHeight-ScrollBarWidth)
                                       - FDefItemHeight;
    SB_PAGELEFT:   ScrolledLeft := ScrolledLeft - (ClientHeight-ScrollBarWidth)
                                       + FDefItemHeight;
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK: ScrolledLeft := Msg.Pos;
      // Ends scrolling
    SB_ENDSCROLL: ;
  end;
end;

procedure TCustomListView.WMVScroll(var Msg: TLMScroll);
begin
  case Msg.ScrollCode of
      // Scrolls to start / end of the text
    SB_TOP:        ScrolledTop := 0;
    SB_BOTTOM:     ScrolledTop := Height{GetMaxScrollTop};
      // Scrolls one line up / down
    SB_LINEDOWN:   ScrolledTop := ScrolledTop + FDefItemHeight div 2;
    SB_LINEUP:     ScrolledTop := ScrolledTop - FDefItemHeight div 2;
      // Scrolls one page of lines up / down
    SB_PAGEDOWN:   ScrolledTop := ScrolledTop + (ClientHeight-ScrollBarWidth)
                                     - FDefItemHeight;
    SB_PAGEUP:     ScrolledTop := ScrolledTop - (ClientHeight-ScrollBarWidth)
                                     + FDefItemHeight;
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK: ScrolledTop := Msg.Pos;
      // Ends scrolling
    SB_ENDSCROLL: ;
  end;
end;

Function TCustomListView.GetMaxScrolledLeft : Integer;
begin
result := 0;
end;

Function TCustomListView.GetMaxScrolledTop : Integer;
begin
 Result := 0;
end;

procedure TCustomListView.SetDefaultItemHeight(AValue: integer);
begin
  if AValue<=0 then AValue:=20;
  if AValue=FDefItemHeight then exit;
  FDefItemHeight:=AValue;
//  Include(FStates,tvsTopsNeedsUpdate);
  Invalidate;

end;



