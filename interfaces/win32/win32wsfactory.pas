unit Win32WSFactory;

{$mode objfpc}{$H+}

interface
uses
  Classes, Controls, ComCtrls, ImgList, Calendar, StdCtrls, Arrow, Spin,
  Dialogs, ExtCtrls, ExtDlgs, Buttons, CheckLst, Forms, Grids, Menus,
  WSLCLClasses;

implementation
uses
  Win32WSArrow,
  Win32WSButtons,
  Win32WSCalendar,
  Win32WSCheckLst,
  Win32WSComCtrls,
  Win32WSControls,
  Win32WSDialogs,
  Win32WSExtCtrls,
  Win32WSExtDlgs,
  Win32WSForms,
  Win32WSGrids,
  Win32WSImgList,
  Win32WSMenus,
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

// dialogs
function RegisterCommonDialog: Boolean; alias : 'WSRegisterCommonDialog';
begin
  RegisterWSComponent(TCommonDialog, TWin32WSCommonDialog);
  Result := True;
end;

function RegisterFileDialog: Boolean; alias : 'WSRegisterFileDialog';
begin
//  RegisterWSComponent(TFileDialog, TWin32WSFileDialog);
  Result := False;
end;

function RegisterOpenDialog: Boolean; alias : 'WSRegisterOpenDialog';
begin
  RegisterWSComponent(TOpenDialog, TWin32WSOpenDialog);
  Result := True;
end;

function RegisterSaveDialog: Boolean; alias : 'WSRegisterSaveDialog';
begin
  RegisterWSComponent(TSaveDialog, TWin32WSSaveDialog);
  Result := True;
end;

function RegisterSelectDirectoryDialog: Boolean; alias : 'WSRegisterSelectDirectoryDialog';
begin
  RegisterWSComponent(TSelectDirectoryDialog, TWin32WSSelectDirectoryDialog);
  Result := True;
end;

function RegisterColorDialog: Boolean; alias : 'WSRegisterColorDialog';
begin
  RegisterWSComponent(TColorDialog, TWin32WSColorDialog);
  Result := True;
end;

function RegisterColorButton: Boolean; alias : 'WSRegisterColorButton';
begin
//  RegisterWSComponent(TColorButton, TWin32WSColorButton);
  Result := False;
end;

function RegisterFontDialog: Boolean; alias : 'WSRegisterFontDialog';
begin
  RegisterWSComponent(TFontDialog, TWin32WSFontDialog);
  Result := True;
end;

// StdCtrls
function RegisterCustomScrollBar: Boolean; alias : 'WSRegisterCustomScrollBar';
begin
  RegisterWSComponent(TCustomScrollBar, TWin32WSScrollBar);
  Result := True;
end;

function RegisterCustomGroupBox: Boolean; alias : 'WSRegisterCustomGroupBox';
begin
  RegisterWSComponent(TCustomGroupBox, TWin32WSCustomGroupBox);
  Result := True;
end;

function RegisterCustomComboBox: Boolean; alias : 'WSRegisterCustomComboBox';
begin
  RegisterWSComponent(TCustomComboBox, TWin32WSCustomComboBox);
  Result := True;
end;

function RegisterCustomListBox: Boolean; alias : 'WSRegisterCustomListBox';
begin
  RegisterWSComponent(TCustomListBox, TWin32WSCustomListBox);
  Result := True;
end;

function RegisterCustomEdit: Boolean; alias : 'WSRegisterCustomEdit';
begin
  RegisterWSComponent(TCustomEdit, TWin32WSCustomEdit);
  Result := True;
end;

function RegisterCustomMemo: Boolean; alias : 'WSRegisterCustomMemo';
begin
  RegisterWSComponent(TCustomMemo, TWin32WSCustomMemo);
  Result := True;
end;

function RegisterButtonControl: Boolean; alias : 'WSRegisterButtonControl';
begin
  RegisterWSComponent(TButtonControl, TWin32WSButtonControl);
  Result := True;
end;

function RegisterCustomButton: Boolean; alias : 'WSRegisterCustomButton';
begin
  RegisterWSComponent(TCustomButton, TWin32WSButton);
  Result := True;
end;

function RegisterCustomCheckBox: Boolean; alias : 'WSRegisterCustomCheckBox';
begin
  RegisterWSComponent(TCustomCheckBox, TWin32WSCustomCheckBox);
  Result := True;
end;

function RegisterToggleBox: Boolean; alias : 'WSRegisterToggleBox';
begin
  RegisterWSComponent(TToggleBox, TWin32WSToggleBox);
  Result := True;
end;

function RegisterRadioButton: Boolean; alias : 'WSRegisterRadioButton';
begin
  RegisterWSComponent(TRadioButton, TWin32WSRadioButton);
  Result := True;
end;

function RegisterCustomStaticText: Boolean; alias : 'WSRegisterCustomStaticText';
begin
  RegisterWSComponent(TCustomStaticText, TWin32WSCustomStaticText);
  Result := True;
end;

function RegisterCustomLabel: Boolean; alias : 'WSRegisterCustomLabel';
begin
  Result := False;
end;

// extctrls
function RegisterCustomPage: Boolean; alias : 'WSRegisterCustomPage';
begin
  RegisterWSComponent(TCustomPage, TWin32WSCustomPage);
  Result := True;
end;

function RegisterCustomNotebook: Boolean; alias : 'WSRegisterCustomNotebook';
begin
  RegisterWSComponent(TCustomNotebook, TWin32WSCustomNotebook);
  Result := True;
end;

function RegisterShape: Boolean; alias : 'WSRegisterShape';
begin
  Result := False;
end;

function RegisterCustomSplitter: Boolean; alias : 'WSRegisterCustomSplitter';
begin
  Result := False;
end;

function RegisterPaintBox: Boolean; alias : 'WSRegisterPaintBox';
begin
  Result := False;
end;

function RegisterCustomImage: Boolean; alias : 'WSRegisterCustomImage';
begin
  Result := False;
end;

function RegisterBevel: Boolean; alias : 'WSRegisterBevel';
begin
  Result := False;
end;

function RegisterCustomRadioGroup: Boolean; alias : 'WSRegisterCustomRadioGroup';
begin
  Result := False;
end;

function RegisterCustomCheckGroup: Boolean; alias : 'WSRegisterCustomCheckGroup';
begin
  Result := False;
end;

function RegisterCustomLabeledEdit: Boolean; alias : 'WSRegisterCustomLabeledEdit';
begin
  Result := False;
end;

function RegisterCustomPanel: Boolean; alias : 'WSRegisterCustomPanel';
begin
  RegisterWSComponent(TCustomPanel, TWin32WSCustomPanel);
  Result := True;
end;

function RegisterCustomTrayIcon: Boolean; alias : 'WSRegisterCustomTrayIcon';
begin
  RegisterWSComponent(TCustomTrayIcon, TWin32WSCustomTrayIcon);
  Result := True;
end;

//ExtDlgs
function RegisterPreviewFileControl: Boolean; alias : 'WSRegisterPreviewFileControl';
begin
  RegisterWSComponent(TPreviewFileControl, TWin32WSPreviewFileControl);
  Result := True;
end;

function RegisterPreviewFileDialog: Boolean; alias : 'WSRegisterPreviewFileDialog';
begin
  Result := False;
end;

function RegisterOpenPictureDialog: Boolean; alias : 'WSRegisterOpenPictureDialog';
begin
  RegisterWSComponent(TOpenPictureDialog, TWin32WSOpenPictureDialog);
  Result := True;
end;

function RegisterSavePictureDialog: Boolean; alias : 'WSRegisterSavePictureDialog';
begin
  RegisterWSComponent(TSavePictureDialog, TWin32WSSaveDialog);
  Result := True;
end;

function RegisterCalculatorDialog: Boolean; alias : 'WSRegisterCalculatorDialog';
begin
  Result := False;
end;

function RegisterCalculatorForm: Boolean; alias : 'WSRegisterCalculatorForm';
begin
  Result := False;
end;

(*function RegisterCalendarDialogForm: Boolean; alias : 'WSRegisterCalendarDialogForm';
begin
//  RegisterWSComponent(TCalendarDialogForm, TWin32WSCalendarDialogForm);
  Result := False;
end;*)

function RegisterCalendarDialog: Boolean; alias : 'WSRegisterCalendarDialog';
begin
  Result := False;
end;

// Buttons
function RegisterCustomBitBtn: Boolean; alias : 'WSRegisterCustomBitBtn';
begin
  RegisterWSComponent(TCustomBitBtn, TWin32WSBitBtn);
  Result := True;
end;

function RegisterCustomSpeedButton: Boolean; alias : 'WSRegisterCustomSpeedButton';
begin
  Result := False;
end;

// Arrow
function RegisterArrow: Boolean; alias : 'WSRegisterArrow';
begin
  RegisterWSComponent(TArrow, TWin32WSArrow);
  Result := True;
end;

// CheckLst
function RegisterCustomCheckListBox: Boolean; alias : 'WSRegisterCustomCheckListBox';
begin
  RegisterWSComponent(TCustomCheckListBox, TWin32WSCustomCheckListBox);
  Result := True;
end;

// Forms
function RegisterScrollingWinControl: Boolean; alias : 'WSRegisterScrollingWinControl';
begin
  RegisterWSComponent(TScrollingWinControl, TWin32WSScrollingWinControl);
  Result := True;
end;

function RegisterScrollBox: Boolean; alias : 'WSRegisterScrollBox';
begin
  RegisterWSComponent(TScrollBox, TWin32WSScrollBox);
  Result := True;
end;

function RegisterCustomFrame: Boolean; alias : 'WSRegisterCustomFrame';
begin
  Result := False;
end;

function RegisterCustomForm: Boolean; alias : 'WSRegisterCustomForm';
begin
  RegisterWSComponent(TCustomForm, TWin32WSCustomForm);
  Result := True;
end;

function RegisterHintWindow: Boolean; alias : 'WSRegisterHintWindow';
begin
  RegisterWSComponent(THintWindow, TWin32WSHintWindow);
  Result := True;
end;

function RegisterCustomGrid: Boolean; alias : 'WSRegisterCustomGrid';
begin
  RegisterWSComponent(TCustomGrid, TWin32WSCustomGrid);
  Result := True;
end;

function RegisterMenuItem: Boolean; alias : 'WSRegisterMenuItem';
begin
  RegisterWSComponent(TMenuItem, TWin32WSMenuItem);
  Result := True;
end;

function RegisterMenu: Boolean; alias : 'WSRegisterMenu';
begin
  RegisterWSComponent(TMenu, TWin32WSMenu);
  Result := True;
end;

function RegisterMainMenu: Boolean; alias : 'WSRegisterMainMenu';
begin
  Result := False;
end;

function RegisterPopupMenu: Boolean; alias : 'WSRegisterPopupMenu';
begin
  RegisterWSComponent(TPopupMenu, TWin32WSPopupMenu);
  Result := True;
end;

function RegisterPairSplitterSide: Boolean; alias : 'WSRegisterPairSplitterSide';
begin
  Result := False;
end;

function RegisterCustomPairSplitter: Boolean; alias : 'WSRegisterCustomPairSplitter';
begin
  Result := False;
end;

function RegisterCustomFloatSpinEdit: Boolean; alias : 'WSRegisterCustomFloatSpinEdit';
begin
  RegisterWSComponent(TCustomFloatSpinEdit, TWin32WSCustomFloatSpinEdit);
  Result := True;
end;

function RegisterCustomRubberBand: Boolean; alias : 'WSRegisterCustomRubberBand';
begin
  Result := False;
end;

end.
