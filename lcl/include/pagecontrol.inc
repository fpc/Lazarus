// included by comctrls.pp

{******************************************************************************
                                  TPageControl
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

  Abstract:
    TPageControl for LCL

  ToDo:
    - all
}

{ TPageControl }

procedure TPageControl.ChangeActivePage(Page: TTabSheet);
begin

end;

procedure TPageControl.DeleteTab(Page: TTabSheet; Index: Integer);
begin

end;

function TPageControl.GetActivePageIndex: Integer;
begin
  Result:=-1;
end;

function TPageControl.GetPage(Index: Integer): TTabSheet;
begin
  Result:=nil;
end;

function TPageControl.GetPageCount: Integer;
begin
  Result:=0;
end;

function TPageControl.GetTabIndex: Integer;
begin
  Result:=-1;
end;

procedure TPageControl.InsertPage(Page: TTabSheet);
begin

end;

procedure TPageControl.InsertTab(Page: TTabSheet);
begin

end;

procedure TPageControl.MoveTab(CurIndex, NewIndex: Integer);
begin

end;

procedure TPageControl.RemovePage(Page: TTabSheet);
begin

end;

procedure TPageControl.SetActivePage(Page: TTabSheet);
begin

end;

procedure TPageControl.SetActivePageIndex(const Value: Integer);
begin

end;

procedure TPageControl.SetImages(const AValue: TImageList);
begin
  if FImages=AValue then exit;
  FImages:=AValue;
end;

procedure TPageControl.SetTabIndex(const AValue: Integer);
begin

end;

function TPageControl.CanShowTab(TheTabIndex: Integer): Boolean;
begin
  Result:=false;
end;

procedure TPageControl.Change;
begin

end;

procedure TPageControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  inherited GetChildren(Proc, Root);
end;

function TPageControl.GetImageIndex(TheTabIndex: Integer): Integer;
begin
  Result:=-1;
end;

procedure TPageControl.Loaded;
begin
  inherited Loaded;
end;

procedure TPageControl.SetChildOrder(Child: TComponent; Order: Integer);
begin
  inherited SetChildOrder(Child, Order);
end;

procedure TPageControl.ShowControl(AControl: TControl);
begin
  inherited ShowControl(AControl);
end;

procedure TPageControl.UpdateActivePage;
begin

end;

constructor TPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TPageControl.Destroy;
begin
  inherited Destroy;
end;

function TPageControl.FindNextPage(CurPage: TTabSheet; GoForward,
  CheckTabVisible: Boolean): TTabSheet;
begin
  Result:=nil;
end;

procedure TPageControl.SelectNextPage(GoForward: Boolean);
begin
  SelectNextPage(GoForward,true);
end;

procedure TPageControl.SelectNextPage(GoForward: Boolean;
  CheckTabVisible: Boolean);
begin

end;

// included by comctrls.pp

