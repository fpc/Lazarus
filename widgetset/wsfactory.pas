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
// comctrls
procedure RegisterStatusBar;
procedure RegisterTabSheet;
procedure RegisterPageControl;
procedure RegisterCustomListView;
procedure RegisterCustomProgressBar;
procedure RegisterCustomUpDown;
procedure RegisterCustomToolButton;
procedure RegisterToolBar;
procedure RegisterCustomTrackBar;
procedure RegisterCustomTreeView;
// calender
procedure RegisterCustomCalendar;

implementation

// imglist
function WSRegisterCustomImageList: Boolean; external name 'WSRegisterCustomImageList';
// controls
function WSRegisterDragImageList: Boolean;   external name 'WSRegisterDragImageList';
function WSRegisterControl: Boolean;         external name 'WSRegisterControl';
function WSRegisterWinControl: Boolean;      external name 'WSRegisterWinControl';
function WSRegisterGraphicControl: Boolean;  external name 'WSRegisterGraphicControl';
function WSRegisterCustomControl: Boolean;   external name 'WSRegisterCustomControl';
// comctrls
function WSRegisterStatusBar: Boolean;         external name 'WSRegisterStatusBar';
function WSRegisterTabSheet: Boolean;          external name 'WSRegisterTabSheet';
function WSRegisterPageControl: Boolean;       external name 'WSRegisterPageControl';
function WSRegisterCustomListView: Boolean;    external name 'WSRegisterCustomListView';
function WSRegisterCustomProgressBar: Boolean; external name 'WSRegisterCustomProgressBar';
function WSRegisterCustomUpDown: Boolean;      external name 'WSRegisterCustomUpDown';
function WSRegisterCustomToolButton: Boolean;  external name 'WSRegisterCustomToolButton';
function WSRegisterToolBar: Boolean;           external name 'WSRegisterToolBar';
function WSRegisterCustomTrackBar: Boolean;    external name 'WSRegisterCustomTrackBar';
function WSRegisterCustomTreeView: Boolean;    external name 'WSRegisterCustomTreeView';
// calender
function WSRegisterCustomCalendar: Boolean;    external name 'WSRegisterCustomCalendar';

procedure RegisterCustomImageList;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterCustomImageList then
    RegisterWSComponent(TCustomImageList, TWSCustomImageList);
  Done := True;
end;

procedure RegisterDragImageList;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterDragImageList then
    RegisterWSComponent(TDragImageList, TWSDragImageList);
  Done := True;
end;

procedure RegisterControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterControl then
    RegisterWSComponent(TControl, TWSControl);
  Done := True;
end;

procedure RegisterWinControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterWinControl then
    RegisterWSComponent(TWinControl, TWSWinControl);
  Done := True;
end;

procedure RegisterGraphicControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterGraphicControl;
//  if not WSRegisterGraphicControl then
//    RegisterWSComponent(TGraphicControl, TWSGraphicControl);
  Done := True;
end;

procedure RegisterCustomControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomControl;
//  if not WSRegisterCustomControl then
//    RegisterWSComponent(TCustomControl, TWSCustomControl);
  Done := True;
end;

procedure RegisterStatusBar;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterStatusBar;
//  if not WSRegisterStatusBar then
//    RegisterWSComponent(TStatusBar, TWSStatusBar);
  Done := True;
end;

procedure RegisterTabSheet;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterTabSheet;
//  if not WSRegisterTabSheet then
//    RegisterWSComponent(TTabSheet, TWSTabSheet)
  Done := True;
end;

procedure RegisterPageControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterPageControl;
//  if not WSRegisterPageControl then
//    RegisterWSComponent(TPageControl, TWSPageControl);
  Done := True;
end;

procedure RegisterCustomListView;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomListView;
//  if not WSRegisterCustomListView then
//    RegisterWSComponent(TCustomListView, TWSCustomListView);
  Done := True;
end;

procedure RegisterCustomProgressBar;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomProgressBar;
//  if not WSRegisterCustomProgressBar then
//    RegisterWSComponent(TCustomProgressBar, TWSCustomProgressBar);
  Done := True;
end;

procedure RegisterCustomUpDown;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomUpDown;
//  if not WSRegisterCustomUpDown then
//    RegisterWSComponent(TCustomUpDown, TWSCustomUpDown);
  Done := True;
end;

procedure RegisterCustomToolButton;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomToolButton;
//  if not WSRegisterCustomToolButton then
//    RegisterWSComponent(TCustomToolButton, TWSToolButton);
  Done := True;
end;

procedure RegisterToolBar;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterToolBar;
//  if not WSRegisterToolBar then
//    RegisterWSComponent(TToolBar, TWSToolBar);
  Done := True;
end;

procedure RegisterCustomTrackBar;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomTrackBar;
//  if not WSRegisterCustomTrackBar then
//    RegisterWSComponent(TCustomTrackBar, TWSCustomTrackBar);
  Done := True;
end;

procedure RegisterCustomTreeView;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterStatusBar;
//  if not WSRegisterStatusBar then
//    RegisterWSComponent(TCustomTreeView, TWSCustomTreeView);
  Done := True;
end;

procedure RegisterCustomCalendar;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomCalendar;
//  if not WSRegisterCustomCalendar then
//    RegisterWSComponent(TCustomCalendar, TWSCustomCalendar);
  Done := True;
end;

end.

