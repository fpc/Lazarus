{%MainUnit ../comctrls.pp}

{******************************************************************************
                                  TTabControl
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

{ TTabPage }

type
  TTabPage = class(TCustomPage)
  protected
    FIndex: integer;
    FTabs: TCustomTabControl;
    function GetTabVisible: Boolean; override; //assure always visible
    function GetPageIndex: integer; override;
    procedure SetPageIndex(AValue: Integer); override;
  public
    function IsControlVisible: Boolean; override;
    function VisibleIndex: integer; override;
  end;

function TTabPage.GetTabVisible: Boolean;
begin
  Result := True;
end;

function TTabPage.GetPageIndex: integer;
begin
  Result := FIndex;
end;

procedure TTabPage.SetPageIndex(AValue: Integer);
begin
  FIndex := AValue;
end;

function TTabPage.IsControlVisible: Boolean;
begin
  Result := True;
end;

function TTabPage.VisibleIndex: integer;
begin
  Result := FIndex; //assume always visible
end;

{ TTabPages }

type
  TTabPages = class(TStringList)
  protected
    FPage: TTabPage;
    FTabs: TTabControl;
  public
    constructor Create(ATabCtl: TTabControl);
    function Add(const S: string): Integer; override;
    procedure Delete(Index: Integer); override;
  end;

constructor TTabPages.Create(ATabCtl: TTabControl);
begin
  inherited Create;
  FTabs := ATabCtl;
  FPage := TTabPage.Create(ATabCtl);
  FPage.Visible := False;
end;

function TTabPages.Add(const S: string): Integer;
begin
  Result:=inherited Add(S);
  FTabs.InsertPage(nil, Result);
end;

procedure TTabPages.Delete(Index: Integer);
begin
  inherited Delete(Index);
  FTabs.PageRemoved(Index);
end;

{ TTabs }

constructor TTabControl.Create(TheOwner: TComponent);
begin
  FUnPaged:=True;
  FAccess := TTabPages.Create(Self); //specialization required for notifications
  TTabPages(FAccess).FTabs := Self;
  inherited Create(TheOwner);
end;

procedure TTabControl.DoChange;
var
  i: integer;
  p: TObject;
begin
  //inherited DoChange;
  if Assigned(OnPageChanged) then
    OnPageChanged(Self)
  else begin
  //emulate page switch
    for i := 0 to TabCount - 1 do begin
      p := Tabs.Objects[i];
      if p is TControl then
        TControl(p).Visible := i = TabIndex;
    end;
  end;
end;

function TTabControl.GetPage(AIndex: Integer): TCustomPage;
begin
  Result := TTabPages(Tabs).FPage;
  Result.PageIndex := AIndex;
  Result.Caption := FAccess.Strings[AIndex];
end;

function TTabControl.IndexOf(APage: TPersistent): integer;
begin
  if APage is TTabPage then begin
    Result := TTabPage(APage).FIndex;
  end else
    Result := Tabs.IndexOfObject(APage);
end;

function TTabControl.IndexOfTabWithCaption(const TabCaption: string): Integer;
begin
  Result := Tabs.IndexOf(TabCaption);
end;

procedure TTabControl.InsertPage(APage: TCustomPage; Index: Integer);
begin
//A page has been added to Pages - notify widgetset
//TWSCustomNotebookClass(WidgetSetClass).AddPage(Self, APage, APage.VisibleIndex);
  if HandleAllocated and (not (csLoading in ComponentState)) then begin
  //if first page, make it current
    if APage = nil then
      APage := Page[Index];
    TWSCustomTabControlClass(WidgetSetClass).AddPage(Self, APage, Index);
    if PageIndex <> Index then begin //???
      //DoSendPageIndex;
      PageIndex:=Index;
    end;
  end;
end;

procedure TTabControl.RemovePage(Index: Integer);
begin
//A page has been removed from Pages - notify widgetset
//TWSCustomNotebookClass(WidgetSetClass).RemovePage(Self, APage.VisibleIndex);
  //if False then inherited RemovePage(Index);
  if HandleAllocated and not (csDestroying in ComponentState) then begin
  //select next visible page
    TWSCustomTabControlClass(WidgetSetClass).RemovePage(Self, Index);
    if FPageIndex >= Index then
      Dec(FPageIndex);
  end;
end;

// included by comctrls.pp

