unit Win32WSFactory;

{$mode objfpc}{$H+}

interface
uses
  Classes, Controls, ComCtrls, ImgList, Calendar,
  WSLCLClasses, WSFactory;

implementation
uses
  WSComCtrls,
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as possible circles,
// uncomment only those units with implementation
////////////////////////////////////////////////////
// Win32WSActnList,
  Win32WSArrow,
  Win32WSButtons,
  Win32WSCalendar,
  Win32WSCheckLst,
  Win32WSComCtrls,
  Win32WSControls,
// Win32WSDbCtrls,
// Win32WSDBGrids,
  Win32WSDialogs,
// Win32WSDirSel,
// Win32WSEditBtn,
  Win32WSExtCtrls,
  Win32WSExtDlgs,
// Win32WSFileCtrl,
  Win32WSForms,
  Win32WSGrids,
  Win32WSImgList,
// Win32WSMaskEdit,
  Win32WSMenus,
  Win32WSPairSplitter,
  Win32WSSpin,
  Win32WSStdCtrls;

// imglist
function RegisterCustomImageList: Boolean; alias : 'WSRegisterCustomImageList';
begin
  RegisterWSComponent(TCustomImageList, TWin32WSCustomImageList);
  Result := True;
end;

// controls
function RegisterDragImageList: Boolean; alias : 'WSRegisterDragImageList';
begin
  RegisterWSComponent(TDragImageList, TWin32WSDragImageList);
  Result := True;
end;

function RegisterControl: Boolean; alias : 'WSRegisterControl';
begin
  RegisterWSComponent(TControl, TWin32WSControl);
  Result := True;
end;

function RegisterWinControl: Boolean; alias : 'WSRegisterWinControl';
begin
  RegisterWSComponent(TWinControl, TWin32WSWinControl);
  Result := True;
end;

function RegisterGraphicControl: Boolean; alias : 'WSRegisterGraphicControl';
begin
  Result := False;
end;

function RegisterCustomControl: Boolean; alias : 'WSRegisterCustomControl';
begin
  Result := False;
end;

// comctrls
function RegisterStatusBar: Boolean; alias : 'WSRegisterStatusBar';
begin
  RegisterWSComponent(TStatusBar, TWin32WSStatusBar);
  Result := True;
end;

function RegisterTabSheet: Boolean; alias : 'WSRegisterTabSheet';
begin
  Result := False;
end;

function RegisterPageControl: Boolean; alias : 'WSRegisterPageControl';
begin
  Result := False;
end;

function RegisterCustomListView: Boolean; alias : 'WSRegisterCustomListView';
begin
  RegisterWSComponent(TCustomListView, TWin32WSCustomListView);
  Result := True;
end;

function RegisterCustomProgressBar: Boolean; alias : 'WSRegisterCustomProgressBar';
begin
  RegisterWSComponent(TCustomProgressBar, TWin32WSProgressBar);
  Result := True;
end;

function RegisterCustomUpDown: Boolean; alias : 'WSRegisterCustomUpDown';
begin
  Result := False;
end;

function RegisterCustomToolButton: Boolean; alias : 'WSRegisterCustomToolButton';
begin
  Result := False;
end;

function RegisterToolBar: Boolean; alias : 'WSRegisterToolBar';
begin
  Result := False;
end;

function RegisterCustomTrackBar: Boolean; alias : 'WSRegisterCustomTrackBar';
begin
  RegisterWSComponent(TCustomTrackBar, TWin32WSTrackBar);
  Result := True;
end;

function RegisterCustomTreeView: Boolean; alias : 'WSRegisterCustomTreeView';
begin
  Result := False;
end;

// calendar
function RegisterCustomCalendar: Boolean; alias : 'WSRegisterCustomCalendar';
begin
  RegisterWSComponent(TCustomCalendar, TWin32WSCustomCalendar);
  Result := True;
end;

end.
