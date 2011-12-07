{%MainUnit ../comctrls.pp}

{******************************************************************************
                                  TPageControl
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

{ TPageControl }

function TPageControl.GetActivePageIndex: Integer;
begin
  Result:=inherited PageIndex;
end;

function TPageControl.GetActiveTabSheet: TTabSheet;
begin
  Result:=TTabSheet(inherited ActivePageComponent);
end;

function TPageControl.GetTabSheet(Index: Integer): TTabSheet;
begin
  Result:=TTabSheet(inherited Page[Index]);
end;

procedure TPageControl.SetActivePageIndex(const AValue: Integer);
begin
  inherited PageIndex:=AValue;
end;

procedure TPageControl.SetActiveTabSheet(const AValue: TTabSheet);
begin
  ActivePageComponent := AValue;
end;

function TPageControl.FindPageWithDockClient(Client: TControl): TTabSheet;
var
  i: integer;
begin
  for i := 0 to PageCount - 1 do
    if Pages[i] = Client.Parent then
    begin
      Result := Pages[i];
      exit;
    end;
  Result := nil;
end;

class procedure TPageControl.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterPageControl;
end;

procedure TPageControl.DoAddDockClient(Client: TControl; const ARect: TRect);
var
  TabSheet: TTabSheet;
begin
  // Client cannot be added to TPageControl itself but new TabSheet should be
  // added and client placed onto it
  TabSheet := TTabSheet.Create(Self);
  TabSheet.Caption := GetDockCaption(Client);
  try
    TabSheet.PageControl := Self;
    Client.Parent := TabSheet;
    // delphi compatible behaviour => align to client
    Client.Align := alClient;
  except
    FreeAndNil(TabSheet);
  end;
end;

procedure TPageControl.DockOver(Source: TDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  P: TPoint;
begin
  P := Parent.ClientToScreen(Point(Left, Top));
  Source.DockRect := Rect(P.X, P.Y, P.X + Width, P.Y + Height);
  DoDockOver(Source, X, Y, State, Accept);
end;

procedure TPageControl.DoRemoveDockClient(Client: TControl);
begin
  // we cannot search for client page here since Client.Parent
  // is changed at moment => search for page before parent change and free here
  FreeAndNil(FPageToUndock);
end;

function TPageControl.DoUndockClientMsg(NewTarget, Client: TControl): boolean;
begin
  FPageToUndock := FindPageWithDockClient(Client);
  Result := inherited DoUndockClientMsg(NewTarget, Client);
end;

function TPageControl.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  Result:=(ChildClass<>nil) and (ChildClass.InheritsFrom(PageClass));
  if Widgetset.GetLCLCapability(lcAllowChildControlsInNativeControls) = LCL_CAPABILITY_YES then Result := True;
end;

constructor TPageControl.Create(TheOwner: TComponent);
begin
  PageClass:=TTabSheet;
  inherited Create(TheOwner);
end;

function TPageControl.FindNextPage(CurPage: TTabSheet; GoForward,
  CheckTabVisible: Boolean): TTabSheet;
var
  I, StartIndex: Integer;
begin
  Result := nil;
  if PageCount = 0 then
    exit;
  StartIndex := IndexOf(CurPage);
  if StartIndex < 0 then
    if GoForward then
      StartIndex := PageCount - 1
    else
      StartIndex := 0;
  i := StartIndex;
  repeat
    if GoForward then
    begin
      Inc(i);
      if i = PageCount then
        i := 0;
    end else
    begin
      if i = 0 then
        i := PageCount;
      Dec(I);
    end;
    if not CheckTabVisible or Pages[i].TabVisible then
    begin
      Result := Pages[i];
      exit;
    end;
  until i = StartIndex;
end;

procedure TPageControl.SelectNextPage(GoForward: Boolean);
begin
  SelectNextPage(GoForward,true);
end;

procedure TPageControl.SelectNextPage(GoForward: Boolean;
  CheckTabVisible: Boolean);
var
  NextPage: TTabSheet;
begin
  NextPage:=FindNextPage(ActivePage,GoForward,CheckTabVisible);
  if NextPage<>nil then ActivePage:=NextPage;
end;

// Convenience routine, to make the TPageControl more intuitive to use
// A Lazarus addition
function TPageControl.AddTabSheet: TTabSheet;
begin
  Result := TTabSheet.Create(Self);
  Result.PageControl := Self;
end;


// included by comctrls.pp
