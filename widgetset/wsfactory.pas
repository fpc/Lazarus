unit WSFactory;

{$mode objfpc}{$H+}

interface
uses
  Classes, Controls, ImgList,
  WSLCLClasses, WSControls, WSImgList;

// imglist
procedure RegisterCustomImageList; external name 'WSRegisterCustomImageList';
// controls
procedure RegisterDragImageList; external name 'WSRegisterDragImageList';
procedure RegisterControl; external name 'WSRegisterControl';
procedure RegisterWinControl; external name 'WSRegisterWinControl';
procedure RegisterGraphicControl; external name 'WSRegisterGraphicControl';
procedure RegisterCustomControl; external name 'WSRegisterCustomControl';
procedure RegisterImageList; external name 'WSRegisterImageList';
// comctrls
procedure RegisterStatusBar; external name 'WSRegisterStatusBar';

// defaults
// imglist
procedure DefRegisterCustomImageList;
// controls
procedure DefRegiterDragImageList;
procedure DefRegisterControl;
procedure DefRegisterWinControl;
procedure DefRegisterGraphicControl;
procedure DefRegisterCustomControl;
procedure DefRegisterImageList;
// comctrls
procedure DefRegisterStatusBar;

implementation

const
  HasRegisteredCustomImageList: Boolean = False;
  HasRegisteredDragImageList: Boolean = False;
  HasRegisteredControl: Boolean = False;
  HasRegisteredWinControl: Boolean = False;

procedure DefRegisterCustomImageList;
begin
  if not HasRegisteredCustomImageList then
    RegisterWSComponent(TCustomImageList, TWSCustomImageList);
  HasRegisteredCustomImageList := True;
end;

procedure DefRegiterDragImageList;
begin
  if not HasRegisteredDragImageList then
    RegisterWSComponent(TDragImageList, TWSDragImageList);
  HasRegisteredDragImageList := True;
end;

procedure DefRegisterControl;
begin
  if not HasRegisteredControl then
    RegisterWSComponent(TControl, TWSControl);
  HasRegisteredControl := True;
end;

procedure DefRegisterWinControl;
begin
  if not HasRegisteredWinControl then
    RegisterWSComponent(TWinControl, TWSWinControl);
  HasRegisteredWinControl := True;
end;

procedure DefRegisterGraphicControl;
begin
//  RegisterWSComponent(TGraphicControl, TWSGraphicControl);
end;

procedure DefRegisterCustomControl;
begin
//  RegisterWSComponent(TCustomControl, TWSCustomControl);
end;

procedure DefRegisterImageList;
begin
//  RegisterWSComponent(TImageList, TWSImageList);
end;

procedure DefRegisterStatusBar;
begin
//  RegisterWSComponent(TStatusBar, TWSStatusBar);
end;

end.

