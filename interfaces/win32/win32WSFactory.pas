unit Win32WSFactory;

{$mode objfpc}{$H+}

interface
uses
  Classes, Controls, ComCtrls, ImgList,
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

const
  HasRegisteredDragImageList: Boolean = False;
  HasRegisteredControl: Boolean = False;
  HasRegisteredWinControl: Boolean = False;
  HasRegisteredStatusBar: Boolean = False;

// imglist
function WSRegisterCustomImageList: Boolean; alias : 'WSRegisterCustomImageList';
begin
  RegisterWSComponent(TCustomImageList, TWin32WSCustomImageList);
  Result := True;
end;

// controls
procedure RegisterDragImageList; alias : 'WSRegisterDragImageList';
begin
  if not HasRegisteredDragImageList then
    RegisterWSComponent(TDragImageList, TWin32WSDragImageList);
  HasRegisteredDragImageList := True;
end;

procedure RegisterControl; alias : 'WSRegisterControl';
begin
  if not HasRegisteredControl then
    RegisterWSComponent(TControl, TWin32WSControl);
  HasRegisteredControl := True;
end;

procedure RegisterWinControl; alias : 'WSRegisterWinControl';
begin
  if not HasRegisteredWinControl then
    RegisterWSComponent(TWinControl, TWin32WSWinControl);
  HasRegisteredWinControl := True;
end;

procedure RegisterGraphicControl; alias : 'WSRegisterGraphicControl';
begin
  DefRegisterGraphicControl;
end;

procedure RegisterCustomControl; alias : 'WSRegisterCustomControl';
begin
  DefRegisterCustomControl;
end;

procedure RegisterImageList; alias : 'WSRegisterImageList';
begin
  DefRegisterImageList;
end;

// comctrls
procedure RegisterStatusBar; alias : 'WSRegisterStatusBar';
begin
  if not HasRegisteredStatusBar then
    RegisterWSComponent(TStatusBar, TWin32WSStatusBar);
  HasRegisteredStatusBar := True;
end;

end.
