// included by comctrls.pp

{------------------------------------------------------------------------------}
{   TCustomListView Constructor                                                           }
{------------------------------------------------------------------------------}
constructor TCustomListView.Create(Aowner: TComponent);
begin
  inherited Create(AOwner);
  FColumns := TListColumns.Create(self);
  FListItems := TListItems.Create(self);
  FBorderStyle := bsSingle;
  FScrollBars := ssBoth;
  FSmallImages := nil;
  FCompStyle := csListView;
  FViewStyle := vsList;
  FSortType := stNone;
  FSortColumn := 0;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageChanged;
  Setbounds(2,2,300,300);
end;


{------------------------------------------------------------------------------}
{   TCustomListView ColumnsChanged                                             }
{------------------------------------------------------------------------------}
Procedure TCustomListView.ColumnsChanged;
Begin
  if csDestroying in Componentstate Then Exit;
//TODO: Optimize implementation by invoking individual upadates instead of
//      recreating window
  //notify the interface....
  RecreateWnd;
  CNSendMessage(LM_SETPROPERTIES,self,nil);
End;

{------------------------------------------------------------------------------}
{   TCustomListView ItemChanged                                             }
{------------------------------------------------------------------------------}
Procedure TCustomListView.ItemChanged(Index : Integer);  //called by TListItems
Begin
  if csDestroying in Componentstate Then Exit;
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
procedure TCustomListView.SetItems(const AValue : TListItems);
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
{   TCustomListView SetColumns                                                 }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetColumns(const AValue: TListColumns);
begin
  FColumns.Assign(AValue);
end;
         
{------------------------------------------------------------------------------}
{   TCustomListView SetViewStyle                                               }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetViewStyle(const AValue: TViewStyle);
begin
  if FViewStyle = AValue then Exit;
  FViewStyle := AValue;
  CNSendMessage(LM_SETPROPERTIES,self,nil);
end;


{------------------------------------------------------------------------------}
{   TCustomListView SetSortType                                                  }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetSortType(const AValue: TSortType);
begin
  if FSortType = AValue then Exit;
  FSortType := AValue;
  CNSendMessage(LM_SETPROPERTIES,self,nil);
end;

{------------------------------------------------------------------------------}
{   TCustomListView SetSortColumn                                              }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetSortColumn(const AValue : Integer);
begin
  if FSortColumn = AValue then Exit;
  FSortColumn := AValue;
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

{------------------------------------------------------------------------------}
{   TCustomListView GetSelection                                                 }
{------------------------------------------------------------------------------}
function TCustomListView.GetSelection: TListItem;
begin
  Result := nil;
end;

{------------------------------------------------------------------------------}
{   TCustomListView SetSelection                                                 }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetSelection(const AValue: TListItem);
begin
end;

procedure TCustomListView.SetMultiSelect(const AValue: Boolean);
begin
  if FMultiSelect <> AValue then
     Begin
       FMultiSelect := AValue;
       CNSendMessage(LM_SETPROPERTIES,self,nil);
     end;
     
end;

procedure TCustomListView.SetSmallImages(const AValue: TCustomImageList);
begin
    if AValue <> FSmallImages then
     Begin
        if FSmallImages <> nil then
           FSmallImages.UnregisterChanges(FImageChangeLink);

           FSmallImages := AValue;

        if FSmallImages <> nil then
           Begin
             FSmallImages.RegisterChanges(FImageChangeLink);
             FSmallImages.FreeNotification(self);
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

// included by comctrls.pp

{ =============================================================================

  $Log$
  Revision 1.15  2002/03/23 15:49:22  lazarus
  MWE: Fixed more compatebility issues (Sort, SelectedItem)

  Revision 1.14  2002/03/14 23:25:52  lazarus
  MG: fixed TBevel.Create and TListView.Destroy

  Revision 1.13  2002/03/12 23:55:37  lazarus
  MWE:
    * More delphi compatibility added/updated to TListView
    * Introduced TDebugger.locals
    * Moved breakpoints dialog to debugger dir
    * Changed breakpoints dialog to read from resource

}

 

