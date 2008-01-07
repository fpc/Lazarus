{%MainUnit ../comctrls.pp}

{******************************************************************************
                                  TTabSheet
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

{ TTabSheet }

function TTabSheet.GetTabIndex: Integer;
begin
  Result:=PageIndex;
end;

function TTabSheet.GetPageControl: TPageControl;
begin
  if (Parent is TPageControl) then
    Result:=TPageControl(Parent)
  else
    Result:=nil;
end;

procedure TTabSheet.SetPageControl(APageControl: TPageControl);
begin
  if PageControl=APageControl then exit;
  Parent:=APageControl;
end;

procedure TTabSheet.SetTabIndex(const AValue: Integer);
begin
  PageIndex:=AValue;
end;

constructor TTabSheet.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ImageIndex:=0;
end;

destructor TTabSheet.Destroy;
begin
  if (ControlCount = 1) and (Controls[0].HostDockSite <> nil) then
    Controls[0].HostDockSite := nil;
  inherited Destroy;
end;

// included by comctrls.pp
