{%MainUnit ../forms.pp}

{******************************************************************************
                                  TMonitor
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

}

{ TMonitor }

function TMonitor.GetInfo(out Info: TMonitorInfo): Boolean;
begin
  Info.cbSize := SizeOf(TMonitorInfo);
  Result := GetMonitorInfo(Handle, @Info);
end;

function TMonitor.GetLeft: Integer;
begin
  with GetBoundsRect do
    Result := Left;
end;

function TMonitor.GetPixelsPerInch: Integer;
var
  X, Y: UINT;
begin
  if WidgetSet.GetDpiForMonitor(FHandle, MDT_EFFECTIVE_DPI, X, Y)=S_OK then
    Result := X
  else
    Result := Screen.PixelsPerInch;
end;

function TMonitor.GetHeight: Integer;
begin
  with GetBoundsRect do
    Result := Bottom - Top;
end;

function TMonitor.GetTop: Integer;
begin
  with GetBoundsRect do
    Result := Top;
end;

function TMonitor.GetWidth: Integer;
begin
  with GetBoundsRect do
    Result := Right - Left;
end;

function TMonitor.GetBoundsRect: TRect;
var
  Info: TMonitorInfo;
begin
  if GetInfo(Info) then
    Result := Info.rcMonitor
  else
    Result := Rect(0, 0, 0, 0);
end;

function TMonitor.GetWorkareaRect: TRect;
var
  Info: TMonitorInfo;
begin
  if GetInfo(Info) then
    Result := Info.rcWork
  else
    Result := Rect(0, 0, 0, 0);
end;

function TMonitor.GetPrimary: Boolean;
var
  Info: TMonitorInfo;
begin
  Result := GetInfo(Info) and (Info.dwFlags and MONITORINFOF_PRIMARY <> 0)
end;

{ TMonitorList }

function TMonitorList.GetItem(AIndex: Integer): TMonitor;
begin
  Result := TMonitor(inherited Get(AIndex));
end;

procedure TMonitorList.SetItem(AIndex: Integer; const AValue: TMonitor);
begin
  inherited Put(AIndex, AValue)
end;

procedure TMonitorList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    TMonitor(Ptr).Free;
end;

// included by forms.pp
