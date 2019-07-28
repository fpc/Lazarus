unit CocoaWSFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, ComCtrls, StdCtrls, Spin, PairSplitter,
  Dialogs, ExtCtrls, Buttons, CheckLst, Forms, Menus, Calendar,
  WSLCLClasses,
  CocoaWSCommon,
  CocoaWSButtons,
  CocoaWSExtCtrls,
  CocoaWSForms,
  CocoaWSMenus,
  CocoaWSStdCtrls,
  CocoaWSComCtrls,
  CocoaWSDialogs,
  CocoaWSSpin,
  CocoaWSCheckLst,
  CocoaWSDatePicker;

// imglist
function RegisterCustomImageListResolution: Boolean;
// controls
function RegisterDragImageListResolution: Boolean;
function RegisterLazAccessibleObject: Boolean;
function RegisterControl: Boolean;
function RegisterWinControl: Boolean;
function RegisterGraphicControl: Boolean;
function RegisterCustomControl: Boolean;
// comctrls
function RegisterStatusBar: Boolean;
function RegisterTabSheet: Boolean;
function RegisterPageControl: Boolean;
function RegisterCustomListView: Boolean;
function RegisterCustomProgressBar: Boolean;
function RegisterCustomUpDown: Boolean;
function RegisterCustomToolButton: Boolean;
function RegisterToolBar: Boolean;
function RegisterCustomTrackBar: Boolean;
function RegisterCustomTreeView: Boolean;
// calendar
function RegisterCustomCalendar: Boolean;
// dialogs
function RegisterCommonDialog: Boolean;
function RegisterFileDialog: Boolean;
function RegisterOpenDialog: Boolean;
function RegisterSaveDialog: Boolean;
function RegisterSelectDirectoryDialog: Boolean;
function RegisterColorDialog: Boolean;
function RegisterColorButton: Boolean;
function RegisterFontDialog: Boolean;
// StdCtrls
function RegisterCustomScrollBar: Boolean;
function RegisterCustomGroupBox: Boolean;
function RegisterCustomComboBox: Boolean;
function RegisterCustomListBox: Boolean;
function RegisterCustomEdit: Boolean;
function RegisterCustomMemo: Boolean;
function RegisterButtonControl: Boolean;
function RegisterCustomButton: Boolean;
function RegisterCustomCheckBox: Boolean;
function RegisterToggleBox: Boolean;
function RegisterRadioButton: Boolean;
function RegisterCustomStaticText: Boolean;
function RegisterCustomLabel: Boolean;
// extctrls
function RegisterCustomPage: Boolean;
function RegisterCustomNotebook: Boolean;
function RegisterShape: Boolean;
function RegisterCustomSplitter: Boolean;
function RegisterPaintBox: Boolean;
function RegisterCustomImage: Boolean;
function RegisterBevel: Boolean;
function RegisterCustomRadioGroup: Boolean;
function RegisterCustomCheckGroup: Boolean;
function RegisterCustomLabeledEdit: Boolean;
function RegisterCustomPanel: Boolean;
function RegisterCustomTrayIcon: Boolean;
//ExtDlgs
function RegisterPreviewFileControl: Boolean;
function RegisterPreviewFileDialog: Boolean;
function RegisterOpenPictureDialog: Boolean;
function RegisterSavePictureDialog: Boolean;
function RegisterCalculatorDialog: Boolean;
function RegisterCalculatorForm: Boolean;
function RegisterCalendarDialog: Boolean;
// Buttons
function RegisterCustomBitBtn: Boolean;
function RegisterCustomSpeedButton: Boolean;
// CheckLst
function RegisterCustomCheckListBox: Boolean;
// Forms
function RegisterScrollingWinControl: Boolean;
function RegisterScrollBox: Boolean;
function RegisterCustomFrame: Boolean;
function RegisterCustomForm: Boolean;
function RegisterHintWindow: Boolean;
function RegisterCustomGrid: Boolean;
function RegisterMenuItem: Boolean;
function RegisterMenu: Boolean;
function RegisterMainMenu: Boolean;
function RegisterPopupMenu: Boolean;
function RegisterPairSplitterSide: Boolean;
function RegisterCustomPairSplitter: Boolean;
function RegisterCustomFloatSpinEdit: Boolean;
function RegisterCustomRubberBand: Boolean;
// LazDeviceAPIs
function RegisterLazDeviceAPIs: Boolean;

implementation



// imglist
function RegisterCustomImageListResolution: Boolean; alias : 'WSRegisterCustomImageListResolution';
begin
  Result := False;
end;

// controls
function RegisterDragImageListResolution: Boolean; alias : 'WSRegisterDragImageListResolution';
begin
  Result := False;
end;

function RegisterLazAccessibleObject: Boolean; alias : 'WSRegisterLazAccessibleObject';
begin
//      RegisterWSLazAccessibleObject(TGtk2WSLazAccessibleObject);
//      Result := True;
  Result := False;
end;

function RegisterControl: Boolean; alias : 'WSRegisterControl';
begin
  RegisterWSComponent(TControl, TCocoaWSControl);
  Result := True;
end;

function RegisterWinControl: Boolean; alias : 'WSRegisterWinControl';
begin
  RegisterWSComponent(TWinControl, TCocoaWSWinControl);
  Result := True;
end;

function RegisterGraphicControl: Boolean; alias : 'WSRegisterGraphicControl';
begin
  Result := False;
end;

function RegisterCustomControl: Boolean; alias : 'WSRegisterCustomControl';
begin
  RegisterWSComponent(TCustomControl, TCocoaWSCustomControl);
  Result := True;
end;

// comctrls
function RegisterStatusBar: Boolean; alias : 'WSRegisterStatusBar';
begin
  RegisterWSComponent(TStatusBar, TCocoaWSStatusBar);
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
  RegisterWSComponent(TCustomListView, TCocoaWSCustomListView);
  Result := True;
end;

function RegisterCustomProgressBar: Boolean; alias : 'WSRegisterCustomProgressBar';
begin
  RegisterWSComponent(TCustomProgressBar, TCocoaWSProgressBar);
  Result := True;
end;

function RegisterCustomUpDown: Boolean; alias : 'WSRegisterCustomUpDown';
begin
  RegisterWSComponent(TCustomUpDown, TCocoaWSCustomUpDown);
  Result := True;
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
  RegisterWSComponent(TCustomTrackBar, TCocoaWSTrackBar);
  Result := True;
end;

function RegisterCustomTreeView: Boolean; alias : 'WSRegisterCustomTreeView';
begin
  Result := False;
end;

// calendar
function RegisterCustomCalendar: Boolean; alias : 'WSRegisterCustomCalendar';
begin
  RegisterWSComponent(TCustomCalendar, TCocoaWSCustomCalendar);
  Result := True;
end;

// dialogs
function RegisterCommonDialog: Boolean; alias : 'WSRegisterCommonDialog';
begin
  Result := False;
end;

function RegisterFileDialog: Boolean; alias : 'WSRegisterFileDialog';
begin
  RegisterWSComponent(TFileDialog, TCocoaWSFileDialog);
  Result := True;
end;

function RegisterOpenDialog: Boolean; alias : 'WSRegisterOpenDialog';
begin
  Result := False;
end;

function RegisterSaveDialog: Boolean; alias : 'WSRegisterSaveDialog';
begin
  Result := False;
end;

function RegisterSelectDirectoryDialog: Boolean; alias : 'WSRegisterSelectDirectoryDialog';
begin
  Result := False;
end;

function RegisterColorDialog: Boolean; alias : 'WSRegisterColorDialog';
begin
  RegisterWSComponent(TColorDialog, TCocoaWSColorDialog);
  Result := True;
end;

function RegisterColorButton: Boolean; alias : 'WSRegisterColorButton';
begin
  Result := False;
end;

function RegisterFontDialog: Boolean; alias : 'WSRegisterFontDialog';
begin
  RegisterWSComponent(TFontDialog, TCocoaWSFontDialog);
  Result := False;
end;

// StdCtrls
function RegisterCustomScrollBar: Boolean; alias : 'WSRegisterCustomScrollBar';
begin
  RegisterWSComponent(TCustomScrollBar, TCocoaWSScrollBar);
  Result := True;
end;

function RegisterCustomGroupBox: Boolean; alias : 'WSRegisterCustomGroupBox';
begin
  RegisterWSComponent(TCustomGroupBox, TCocoaWSCustomGroupBox);
  Result := True;
end;

function RegisterCustomComboBox: Boolean; alias : 'WSRegisterCustomComboBox';
begin
  RegisterWSComponent(TCustomComboBox, TCocoaWSCustomComboBox);
  Result := True;
end;

function RegisterCustomListBox: Boolean; alias : 'WSRegisterCustomListBox';
begin
  RegisterWSComponent(TCustomListBox, TCocoaWSCustomListBox);
  Result := True;
end;

function RegisterCustomEdit: Boolean; alias : 'WSRegisterCustomEdit';
begin
  RegisterWSComponent(TCustomEdit, TCocoaWSCustomEdit);
  Result := True;
end;

function RegisterCustomMemo: Boolean; alias : 'WSRegisterCustomMemo';
begin
  RegisterWSComponent(TCustomMemo, TCocoaWSCustomMemo);
  Result := True;
end;

function RegisterButtonControl: Boolean; alias : 'WSRegisterButtonControl';
begin
  Result := False;
end;

function RegisterCustomButton: Boolean; alias : 'WSRegisterCustomButton';
begin
  RegisterWSComponent(TCustomButton, TCocoaWSButton);
  Result := True;
end;

function RegisterCustomCheckBox: Boolean; alias : 'WSRegisterCustomCheckBox';
begin
  RegisterWSComponent(TCustomCheckBox, TCocoaWSCustomCheckBox);
  Result := True;
end;

function RegisterToggleBox: Boolean; alias : 'WSRegisterToggleBox';
begin
  RegisterWSComponent(TToggleBox, TCocoaWSToggleBox);
  Result := True;
end;

function RegisterRadioButton: Boolean; alias : 'WSRegisterRadioButton';
begin
  RegisterWSComponent(TRadioButton, TCocoaWSRadioButton);
  Result := False;
end;

function RegisterCustomStaticText: Boolean; alias : 'WSRegisterCustomStaticText';
begin
  RegisterWSComponent(TCustomStaticText, TCocoaWSCustomStaticText);
  Result := True;
end;

function RegisterCustomLabel: Boolean; alias : 'WSRegisterCustomLabel';
begin
  Result := False;
end;

// extctrls
function RegisterCustomPage: Boolean; alias : 'WSRegisterCustomPage';
begin
  RegisterWSComponent(TCustomPage, TCocoaWSCustomPage);
  Result := True;
end;

function RegisterCustomNotebook: Boolean; alias : 'WSRegisterCustomNotebook';
begin
  RegisterWSComponent(TCustomTabControl, TCocoaWSCustomTabControl);
  Result := True;
end;

function RegisterShape: Boolean; alias : 'WSRegisterShape';
begin
  Result := False;
end;

function RegisterCustomSplitter: Boolean; alias : 'WSRegisterCustomSplitter';
begin
  RegisterWSComponent(TCustomSplitter, TCocoaWSCustomSplitter);
  Result := True;
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
  Result := False;
end;

function RegisterCustomTrayIcon: Boolean; alias : 'WSRegisterCustomTrayIcon';
begin
  RegisterWSComponent(TCustomTrayIcon, TCocoaWSCustomTrayIcon);
  Result := True;
end;

//ExtDlgs
function RegisterPreviewFileControl: Boolean; alias : 'WSRegisterPreviewFileControl';
begin
  Result := False;
end;

function RegisterPreviewFileDialog: Boolean; alias : 'WSRegisterPreviewFileDialog';
begin
  Result := False;
end;

function RegisterOpenPictureDialog: Boolean; alias : 'WSRegisterOpenPictureDialog';
begin
  Result := False;
end;

function RegisterSavePictureDialog: Boolean; alias : 'WSRegisterSavePictureDialog';
begin
  Result := False;
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
//  RegisterWSComponent(TCalendarDialogForm, TCarbonWSCalendarDialogForm);
  Result := False;
end;*)

function RegisterCalendarDialog: Boolean; alias : 'WSRegisterCalendarDialog';
begin
  Result := False;
end;

// Buttons
function RegisterCustomBitBtn: Boolean; alias : 'WSRegisterCustomBitBtn';
begin
  RegisterWSComponent(TCustomBitBtn, TCocoaWSBitBtn);
  Result := True;
end;

function RegisterCustomSpeedButton: Boolean; alias : 'WSRegisterCustomSpeedButton';
begin
  Result := False;
end;

// CheckLst
function RegisterCustomCheckListBox: Boolean; alias : 'WSRegisterCustomCheckListBox';
begin
  RegisterWSComponent(TCustomCheckListBox, TCocoaWSCustomCheckListBox);
  Result := True;
end;

// Forms
function RegisterScrollingWinControl: Boolean; alias : 'WSRegisterScrollingWinControl';
begin
  RegisterWSComponent(TScrollingWinControl, TCocoaWSScrollingWinControl);
  Result := True;
end;

function RegisterScrollBox: Boolean; alias : 'WSRegisterScrollBox';
begin
  Result := False;
end;

function RegisterCustomFrame: Boolean; alias : 'WSRegisterCustomFrame';
begin
  Result := False;
end;

function RegisterCustomForm: Boolean; alias : 'WSRegisterCustomForm';
begin
  RegisterWSComponent(TCustomForm, TCocoaWSCustomForm);
  Result := True;
end;

function RegisterHintWindow: Boolean; alias : 'WSRegisterHintWindow';
begin
   RegisterWSComponent(THintWindow, TCocoaWSHintWindow);
   Result := True;
end;

// Grids
function RegisterCustomGrid: Boolean; alias : 'WSRegisterCustomGrid';
begin
  Result := False;
end;

// Menus
function RegisterMenuItem: Boolean; alias : 'WSRegisterMenuItem';
begin
  RegisterWSComponent(TMenuItem, TCocoaWSMenuItem);
  Result := True;
end;

function RegisterMenu: Boolean; alias : 'WSRegisterMenu';
begin
  RegisterWSComponent(TMenu, TCocoaWSMenu);
  Result := True;
end;

function RegisterMainMenu: Boolean; alias : 'WSRegisterMainMenu';
begin
  RegisterWSComponent(TMainMenu, TCocoaWSMainMenu);
  Result := True;
end;

function RegisterPopupMenu: Boolean; alias : 'WSRegisterPopupMenu';
begin
  RegisterWSComponent(TPopupMenu, TCocoaWSPopupMenu);
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

// Spin
function RegisterCustomFloatSpinEdit: Boolean; alias : 'WSRegisterCustomFloatSpinEdit';
begin
  RegisterWSComponent(TCustomFloatSpinEdit, TCocoaWSCustomFloatSpinEdit);
  Result := True;
end;

// RubberBand
function RegisterCustomRubberBand: Boolean; alias : 'WSRegisterCustomRubberBand';
begin
  Result := False;
end;

function RegisterLazDeviceAPIs: Boolean; alias : 'WSRegisterLazDeviceAPIs';
begin
  //RegisterWSLazDeviceAPIs(TCDWSLazDeviceAPIs);
  Result := False;
end;

end.

