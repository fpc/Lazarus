{%MainUnit ../comctrls.pp}

{******************************************************************************
                                  TTabControl
 ******************************************************************************

  Author: Mattias Gaertner

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

}

{$IFDEF EnableTabControl}

{ TTabControlStrings }

constructor TTabControlStrings.Create(TheTabControl: TCustomTabControl);
begin
  inherited Create;
  FTabControl:=TheTabControl;
end;

procedure TTabControlStrings.TabControlBoundsChange;
begin

end;

{ TTabControlNoteBookStrings }

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
  Result:=FNoteBook.Pages.GetObject(Index);
end;

procedure TTabControlNoteBookStrings.Put(Index: Integer; const S: string);
begin
  FNoteBook.Pages.Put(Index, S);
end;

procedure TTabControlNoteBookStrings.PutObject(Index: Integer; AObject: TObject
  );
begin
  FNoteBook.Pages.PutObject(Index, AObject);
end;

procedure TTabControlNoteBookStrings.SetUpdateState(Updating: Boolean);
begin
  FNoteBook.Pages.SetUpdateState(Updating);
end;

function TTabControlNoteBookStrings.GetTabIndex: integer;
begin
  Result:=FNoteBook.PageIndex;
end;

procedure TTabControlNoteBookStrings.SetTabIndex(const AValue: integer);
begin
  FNoteBook.PageIndex:=AValue;
end;

constructor TTabControlNoteBookStrings.Create(TheTabControl: TCustomTabControl);
begin
  inherited Create(TheTabControl);
  FNoteBook:=TNoteBook.Create(nil);
  FNoteBook.Parent:=TabControl;
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
  // ToDo
  Result:=FNoteBook.Height;
end;

procedure TTabControlNoteBookStrings.TabControlBoundsChange;
begin
  inherited TabControlBoundsChange;
  FNoteBook.SetBounds(0,0,TabControl.Width,30);
end;

{ TCustomTabControl }

function TCustomTabControl.GetDisplayRect: TRect;
var
  TabAreaSize: LongInt;
begin
  // ToDo
  Result:=ClientRect;
  TabAreaSize:=TTabControlStrings(FTabs).GetSize;
  Result.Top:=Min(TabAreaSize,Result.Bottom);
end;

function TCustomTabControl.GetTabIndex: Integer;
begin
  Result:=TTabControlStrings(FTabs).TabIndex;
end;

procedure TCustomTabControl.SetHotTrack(const AValue: Boolean);
begin
  if FHotTrack=AValue then exit;
  FHotTrack:=AValue;
  // ToDo
end;

procedure TCustomTabControl.SetImages(const AValue: TCustomImageList);
begin
  if FImages=AValue then exit;
  FImages:=AValue;
  // ToDo
end;

procedure TCustomTabControl.SetMultiLine(const AValue: Boolean);
begin
  if FMultiLine=AValue then exit;
  FMultiLine:=AValue;
  // ToDo
end;

procedure TCustomTabControl.SetMultiSelect(const AValue: Boolean);
begin
  if FMultiSelect=AValue then exit;
  FMultiSelect:=AValue;
  // ToDo
end;

procedure TCustomTabControl.SetOwnerDraw(const AValue: Boolean);
begin
  if FOwnerDraw=AValue then exit;
  FOwnerDraw:=AValue;
  // ToDo
end;

procedure TCustomTabControl.SetRaggedRight(const AValue: Boolean);
begin
  if FRaggedRight=AValue then exit;
  FRaggedRight:=AValue;
  // ToDo
end;

procedure TCustomTabControl.SetScrollOpposite(const AValue: Boolean);
begin
  if FScrollOpposite=AValue then exit;
  FScrollOpposite:=AValue;
  // ToDo
end;

procedure TCustomTabControl.SetStyle(const AValue: TTabStyle);
begin
  if FStyle=AValue then exit;
  FStyle:=AValue;
  // ToDo
end;

procedure TCustomTabControl.SetTabHeight(const AValue: Smallint);
begin
  if FTabHeight=AValue then exit;
  FTabHeight:=AValue;
  // ToDo
end;

procedure TCustomTabControl.SetTabPosition(const AValue: TTabPosition);
begin
  if FTabPosition=AValue then exit;
  FTabPosition:=AValue;
  // ToDo
end;

procedure TCustomTabControl.SetTabs(const AValue: TStrings);
begin
  FTabs.Assign(AValue);
end;

procedure TCustomTabControl.SetTabWidth(const AValue: Smallint);
begin
  if FTabWidth=AValue then exit;
  FTabWidth:=AValue;
  // ToDo
end;

function TCustomTabControl.CanChange: Boolean;
begin
  Result:=true;
  if Assigned(FOnChanging) then FOnChanging(Self,Result);
end;

function TCustomTabControl.CanShowTab(TabIndex: Integer): Boolean;
begin
  Result:=true;
end;

procedure TCustomTabControl.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomTabControl.DrawTab(TabIndex: Integer; const Rect: TRect;
  Active: Boolean);
begin
  // ToDo
end;

function TCustomTabControl.GetImageIndex(TabIndex: Integer): Integer;
begin
  Result:=TabIndex;
  if Assigned(FOnGetImageIndex) then
    FOnGetImageIndex(Self,TabIndex,Result);
end;

procedure TCustomTabControl.Loaded;
begin
  inherited Loaded;
end;

procedure TCustomTabControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TCustomTabControl.SetTabIndex(Value: Integer);
begin
  TTabControlStrings(FTabs).TabIndex:=Value;
end;

procedure TCustomTabControl.UpdateTabImages;
begin
  // ToDo
end;

procedure TCustomTabControl.ImageListChange(Sender: TObject);
begin
  // ToDo
end;

procedure TCustomTabControl.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
  TTabControlStrings(FTabs).TabControlBoundsChange;
end;

constructor TCustomTabControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FHotTrack:=false;
  FMultiLine:=false;
  FMultiSelect:=false;
  FOwnerDraw:=false;
  FRaggedRight:=false;
  FScrollOpposite:=false;
  FStyle:=tsTabs;
  FTabHeight:=0;
  FTabIndex:=-1;
  FTabPosition:=tpTop;
  FTabWidth:=0;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
  FTabs:=TTabControlNoteBookStrings.Create(Self);
  SetInitialBounds(0,0,200,150);
end;

destructor TCustomTabControl.Destroy;
begin
  FreeThenNil(FTabs);
  FreeThenNil(FImageChangeLink);
  inherited Destroy;
end;

function TCustomTabControl.IndexOfTabAt(X, Y: Integer): Integer;
begin
  Result:=0;
  // ToDo
end;

function TCustomTabControl.GetHitTestInfoAt(X, Y: Integer): THitTests;
begin
  Result:=[];
  // ToDo
end;

function TCustomTabControl.TabRect(Index: Integer): TRect;
begin
  FillChar(Result,SizeOf(Result),0);
  // ToDo
end;

function TCustomTabControl.RowCount: Integer;
begin
  Result:=1;
  // ToDo
end;

procedure TCustomTabControl.ScrollTabs(Delta: Integer);
begin
  // ToDo
end;

{$ENDIF}


// included by comctrls.pp

{ =============================================================================

  $Log$
  Revision 1.3  2004/09/09 22:00:37  mattias
  started TTabControlNotebookStrings

  Revision 1.2  2004/09/09 09:35:44  mattias
  renamed customradiogroup.inc to radiogroup.inc

  Revision 1.1  2004/09/08 22:59:54  mattias
  started TTabControl

}

