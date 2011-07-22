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
  PageClass:=TTabSheet;
  inherited Create(TheOwner);
end;

// included by comctrls.pp

