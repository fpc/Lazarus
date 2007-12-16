{%MainUnit ../comctrls.pp}

{******************************************************************************
                                  TPageControl
 ******************************************************************************

  Author: Mattias Gaertner

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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

function TPageControl.GetTabIndex: Integer;
begin
  Result:=inherited PageIndex;
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
  ActivePageComponent:=AValue;
end;

procedure TPageControl.SetTabIndex(const AValue: Integer);
begin
  inherited PageIndex:=Avalue;
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

procedure TPageControl.DockDrop(DockObject: TDragDockObject; X, Y: Integer);
var DockObjectVisible: Boolean;
  FNewDockTabSheet: TTabSheet;
begin
  FNewDockTabSheet := TTabSheet.Create(Self);
  FNewDockTabSheet.Parent := Self;
  try
    FNewDockTabSheet.PageControl := Self;
    if DockObject.Control is TCustomForm then
      FNewDockTabSheet.Caption := TCustomForm(DockObject.Control).Caption;
    DockObject.Control.Dock(FNewDockTabSheet, DockObject.DockRect);
    DockObject.Control.Align := alClient;
  except
    FNewDockTabSheet.Free;
    raise;
  end;
  DockObjectVisible := DockObject.Control.Visible;
  FNewDockTabSheet.TabVisible := DockObjectVisible;
  if DockObjectVisible
  then ActivePage := FNewDockTabSheet;
end;


// included by comctrls.pp
