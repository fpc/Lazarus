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

type
  TTabControlStrings = class(TStrings)
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    TabControl: TCustomTabControl;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;


{ TCustomTabControl }

function TCustomTabControl.GetDisplayRect: TRect;
begin
  Result:=ClientRect;
end;

function TCustomTabControl.GetTabIndex: Integer;
begin
  Result:=FTabIndex;
  // ToDo
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
  if FTabs=AValue then exit;
  FTabs:=AValue;
  // ToDo
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
  if TabIndex=Value then exit;
  FTabIndex:=Value;
end;

procedure TCustomTabControl.UpdateTabImages;
begin
  // ToDo
end;

procedure TCustomTabControl.ImageListChange(Sender: TObject);
begin
  // ToDo
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
  SetInitialBounds(0,0,200,150);
end;

destructor TCustomTabControl.Destroy;
begin
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
  Revision 1.2  2004/09/09 09:35:44  mattias
  renamed customradiogroup.inc to radiogroup.inc

  Revision 1.1  2004/09/08 22:59:54  mattias
  started TTabControl

}

