{%MainUnit ../comctrls.pp}

{******************************************************************************
                                  TTabSheet
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

{ TTabSheet }

function TTabSheet.GetTabIndex: Integer;
var
  i: Integer;
begin
  if not TabVisible then
    Result := -1
  else
  begin
    Result := 0;
    for i := 0 to PageIndex - 1 do
      if PageControl.Pages[i].TabVisible then
        inc(Result);
  end;
end;

function TTabSheet.GetPageControl: TPageControl;
begin
  if (Parent is TPageControl) then
    Result := TPageControl(Parent)
  else
    Result := nil;
end;

procedure TTabSheet.SetPageControl(APageControl: TPageControl);
begin
  if PageControl=APageControl then exit;
  Parent:=APageControl;
end;

class procedure TTabSheet.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterTabSheet;
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
