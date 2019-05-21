unit ceUtils;

{$mode objfpc}{$H+}

interface

uses
  Graphics, Classes, SysUtils, Controls;

const
  DEFAULT_DROPDOWN_COUNT = 32;

procedure BoldHeaders(AControl: TControl);

implementation

uses
  TypInfo, StdCtrls, ComCtrls;

procedure BoldGroup(AControl: TWinControl);
var
  i: Integer;
  propinfo: PPropInfo;
  cntrl: TControl;
  fnt: TFont;
begin
  for i:=0 to AControl.ControlCount-1 do begin
    cntrl := AControl.Controls[i];
    propinfo := GetPropInfo(cntrl, 'ParentFont');
    if propinfo <> nil then
      SetOrdProp(cntrl, propinfo, Longint(false));
    propinfo := GetPropInfo(cntrl, 'Font');
    if propinfo <> nil then begin
      fnt := TFont(GetObjectProp(cntrl, 'Font'));
      fnt.Style := [];
      SetObjectProp(cntrl, 'Font', fnt);
    end;
  end;
  AControl.Font.Style := [fsBold];
end;

{ Requests painting of the headers of TCustomGroupbox descendants (TGroupbox,
  TRadiogroup, TCheckgroup) in bold. To be called from form or frame after
  construction with self as parameter. }
procedure BoldHeaders(AControl: TControl);
var
  i: Integer;
begin
  if (AControl is TToolbar) then
    // skip all the toolbuttons
  else
  if (AControl is TCustomGroupbox) then
    BoldGroup(TCustomGroupBox(AControl))
  else
    for i:=0 to AControl.ComponentCount-1 do
      if AControl.Components[i] is TControl then
        BoldHeaders(TControl(AControl.Components[i]))
end;


end.

