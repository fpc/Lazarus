unit WSFactory;

{$mode objfpc}{$H+}

interface
uses
  Classes, Controls, ImgList,
  WSLCLClasses, WSControls, WSImgList;

// imglist
procedure RegisterCustomImageList;
// controls
procedure RegisterDragImageList;
procedure RegisterControl;
procedure RegisterWinControl;
procedure RegisterGraphicControl;
procedure RegisterCustomControl;
procedure RegisterImageList;
// comctrls
procedure RegisterStatusBar;

implementation

// imglist
function WSRegisterCustomImageList: Boolean; external name 'WSRegisterCustomImageList';
// controls
function WSRegisterDragImageList: Boolean;   external name 'WSRegisterDragImageList';
function WSRegisterControl: Boolean;         external name 'WSRegisterControl';
function WSRegisterWinControl: Boolean;      external name 'WSRegisterWinControl';
function WSRegisterGraphicControl: Boolean;  external name 'WSRegisterGraphicControl';
function WSRegisterCustomControl: Boolean;   external name 'WSRegisterCustomControl';
function WSRegisterImageList: Boolean;       external name 'WSRegisterImageList';
// comctrls
function WSRegisterStatusBar: Boolean;       external name 'WSRegisterStatusBar';

procedure RegisterCustomImageList;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterCustomImageList then
    RegisterWSComponent(TCustomImageList, TWSCustomImageList);
  Done := true;
end;

procedure RegisterDragImageList;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterDragImageList then
    RegisterWSComponent(TDragImageList, TWSDragImageList);
  Done := true;
end;

procedure RegisterControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterControl then
    RegisterWSComponent(TControl, TWSControl);
  Done := true;
end;

procedure RegisterWinControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterWinControl then
    RegisterWSComponent(TWinControl, TWSWinControl);
  Done := true;
end;

procedure RegisterGraphicControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterGraphicControl;
//  if not WSRegisterGraphicControl then
//    RegisterWSComponent(TGraphicControl, TWSGraphicControl);
  Done := true;
end;

procedure RegisterCustomControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomControl;
//  if not WSRegisterCustomControl then
//    RegisterWSComponent(TCustomControl, TWSCustomControl);
  Done := true;
end;

procedure RegisterImageList;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterImageList;
//  if not WSRegisterImageList then
//    RegisterWSComponent(TImageList, TWSImageList);
  Done := true;
end;

procedure RegisterStatusBar;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterStatusBar;
//  if not WSRegisterStatusBar then
//    RegisterWSComponent(TStatusBar, TWSStatusBar);
  Done := true;
end;

end.

