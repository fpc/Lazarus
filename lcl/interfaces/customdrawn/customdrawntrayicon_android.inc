{%mainunit carbonwsextctrls.pas}

class function TCDWSCustomTrayIcon.Hide(const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  Result := False;
end;

class function TCDWSCustomTrayIcon.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  Result := False;
//  Result := True;
end;

{*******************************************************************
*  TCDWSCustomTrayIcon.InternalUpdate ()
*
*  DESCRIPTION:    Makes modifications to the Icon while running
*                  i.e. without hiding it and showing again
*******************************************************************}
class procedure TCDWSCustomTrayIcon.InternalUpdate(const ATrayIcon: TCustomTrayIcon);
begin
end;

class function TCDWSCustomTrayIcon.ShowBalloonHint(
  const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  Result := False;
end;

class function TCDWSCustomTrayIcon.GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint;
begin
  Result := Point(0, 0);
end;
