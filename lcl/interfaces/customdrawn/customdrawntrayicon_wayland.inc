{%mainunit customdrawnwsextctrls.pas}

{ TCDWSCustomTrayIcon -- Wayland.
  FIXME: Wayland has no native tray-icon protocol. The conventional
  workaround is the DBus StatusNotifierItem (KDE) spec, which is out
  of scope for this initial host. Everything below is a no-op. }

class function TCDWSCustomTrayIcon.Hide(const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  { FIXME: stub. }
  Result := True;
end;

class function TCDWSCustomTrayIcon.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  { FIXME: stub -- would need a DBus StatusNotifierItem implementation. }
  Result := False;
end;

class procedure TCDWSCustomTrayIcon.InternalUpdate(const ATrayIcon: TCustomTrayIcon);
begin
  { FIXME: stub. }
end;

class function TCDWSCustomTrayIcon.ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  { FIXME: stub. Could use libnotify / org.freedesktop.Notifications. }
  Result := False;
end;

class function TCDWSCustomTrayIcon.GetPosition(const ATrayIcon: TCustomTrayIcon): Types.TPoint;
begin
  { FIXME: stub -- no tray, no position. }
  Result := Types.Point(0, 0);
end;
