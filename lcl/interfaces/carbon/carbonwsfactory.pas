unit CarbonWSFactory;

{$mode objfpc}{$H+}

interface
uses
  Classes, Controls, ComCtrls, StdCtrls, Arrow, Spin, PairSplitter,
  Dialogs, ExtCtrls, Buttons, CheckLst, Forms, Menus, Calendar,
  WSLCLClasses,

  // Carbon units
  CarbonWSArrow, CarbonWSButtons, CarbonWSCheckLst, CarbonWSComCtrls, CarbonWSControls,
  CarbonWSDialogs, CarbonWSExtCtrls, CarbonWSForms, CarbonWSMenus, CarbonWSPairSplitter,
  CarbonWSSpin, CarbonWSStdCtrls, CarbonWSCalendar;

// imglist
function RegisterCustomImageList: Boolean;
// controls
function RegisterDragImageList: Boolean;
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
// Arrow
function RegisterArrow: Boolean;
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

implementation

// imglist
function RegisterCustomImageList: Boolean; alias : 'WSRegisterCustomImageList';
begin
  Result := False;
end;

// controls
function RegisterDragImageList: Boolean; alias : 'WSRegisterDragImageList';
begin
  Result := False;
end;

function RegisterControl: Boolean; alias : 'WSRegisterControl';
begin
  Result := False;
end;

function RegisterWinControl: Boolean; alias : 'WSRegisterWinControl';
begin
  RegisterWSComponent(TWinControl, TCarbonWSWinControl);
  Result := True;
end;

function RegisterGraphicControl: Boolean; alias : 'WSRegisterGraphicControl';
begin
  Result := False;
end;

function RegisterCustomControl: Boolean; alias : 'WSRegisterCustomControl';
begin
  RegisterWSComponent(TCustomControl, TCarbonWSCustomControl);
  Result := True;
end;

// comctrls
function RegisterStatusBar: Boolean; alias : 'WSRegisterStatusBar';
begin
  RegisterWSComponent(TStatusBar, TCarbonWSStatusBar);
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
  RegisterWSComponent(TCustomListView, TCarbonWSCustomListView);
  Result := True;
end;

function RegisterCustomProgressBar: Boolean; alias : 'WSRegisterCustomProgressBar';
begin
  RegisterWSComponent(TCustomProgressBar, TCarbonWSProgressBar);
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
  RegisterWSComponent(TToolbar, TCarbonWSToolBar);
  Result := True;
end;

function RegisterCustomTrackBar: Boolean; alias : 'WSRegisterCustomTrackBar';
begin
  RegisterWSComponent(TCustomTrackBar, TCarbonWSTrackBar);
  Result := True;
end;

function RegisterCustomTreeView: Boolean; alias : 'WSRegisterCustomTreeView';
begin
  Result := False;
end;

// calendar
function RegisterCustomCalendar: Boolean; alias : 'WSRegisterCustomCalendar';
begin
  RegisterWSComponent(TCustomCalendar, TCarbonWSCustomCalendar);
  Result := True;
end;

// dialogs
function RegisterCommonDialog: Boolean; alias : 'WSRegisterCommonDialog';
begin
  Result := False;
end;

function RegisterFileDialog: Boolean; alias : 'WSRegisterFileDialog';
begin
  RegisterWSComponent(TFileDialog, TCarbonWSFileDialog);
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
  RegisterWSComponent(TColorDialog, TCarbonWSColorDialog);
  Result := True;
end;

function RegisterColorButton: Boolean; alias : 'WSRegisterColorButton';
begin
  Result := False;
end;

function RegisterFontDialog: Boolean; alias : 'WSRegisterFontDialog';
begin
  RegisterWSComponent(TFontDialog, TCarbonWSFontDialog);
  Result := True;
end;

// StdCtrls
function RegisterCustomScrollBar: Boolean; alias : 'WSRegisterCustomScrollBar';
begin
  RegisterWSComponent(TCustomScrollBar, TCarbonWSScrollBar);
  Result := True;
end;

function RegisterCustomGroupBox: Boolean; alias : 'WSRegisterCustomGroupBox';
begin
  RegisterWSComponent(TCustomGroupBox, TCarbonWSCustomGroupBox);
  Result := True;
end;

function RegisterCustomComboBox: Boolean; alias : 'WSRegisterCustomComboBox';
begin
  RegisterWSComponent(TCustomComboBox, TCarbonWSCustomComboBox);
  Result := True;
end;

function RegisterCustomListBox: Boolean; alias : 'WSRegisterCustomListBox';
begin
  RegisterWSComponent(TCustomListBox, TCarbonWSCustomListBox);
  Result := True;
end;

function RegisterCustomEdit: Boolean; alias : 'WSRegisterCustomEdit';
begin
  RegisterWSComponent(TCustomEdit, TCarbonWSCustomEdit);
  Result := True;
end;

function RegisterCustomMemo: Boolean; alias : 'WSRegisterCustomMemo';
begin
  RegisterWSComponent(TCustomMemo, TCarbonWSCustomMemo);
  Result := True;
end;

function RegisterButtonControl: Boolean; alias : 'WSRegisterButtonControl';
begin
  Result := False;
end;

function RegisterCustomButton: Boolean; alias : 'WSRegisterCustomButton';
begin
  RegisterWSComponent(TCustomButton, TCarbonWSButton);
  Result := True;
end;

function RegisterCustomCheckBox: Boolean; alias : 'WSRegisterCustomCheckBox';
begin
  RegisterWSComponent(TCustomCheckBox, TCarbonWSCustomCheckBox);
  Result := True;
end;

function RegisterToggleBox: Boolean; alias : 'WSRegisterToggleBox';
begin
  RegisterWSComponent(TToggleBox, TCarbonWSToggleBox);
  Result := True;
end;

function RegisterRadioButton: Boolean; alias : 'WSRegisterRadioButton';
begin
  RegisterWSComponent(TRadioButton, TCarbonWSRadioButton);
  Result := True;
end;

function RegisterCustomStaticText: Boolean; alias : 'WSRegisterCustomStaticText';
begin
  RegisterWSComponent(TCustomStaticText, TCarbonWSCustomStaticText);
  Result := True;
end;

function RegisterCustomLabel: Boolean; alias : 'WSRegisterCustomLabel';
begin
  Result := False;
end;

// extctrls
function RegisterCustomPage: Boolean; alias : 'WSRegisterCustomPage';
begin
  RegisterWSComponent(TCustomPage, TCarbonWSCustomPage);
  Result := True;
end;

function RegisterCustomNotebook: Boolean; alias : 'WSRegisterCustomNotebook';
begin
  RegisterWSComponent(TCustomTabControl, TCarbonWSCustomNotebook);
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
  RegisterWSComponent(TCustomPanel, TCarbonWSCustomPanel);
  Result := True;
end;

function RegisterCustomTrayIcon: Boolean; alias : 'WSRegisterCustomTrayIcon';
begin
  RegisterWSComponent(TCustomTrayIcon, TCarbonWSCustomTrayIcon);
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
  RegisterWSComponent(TCustomBitBtn, TCarbonWSBitBtn);
  Result := True;
end;

function RegisterCustomSpeedButton: Boolean; alias : 'WSRegisterCustomSpeedButton';
begin
  RegisterWSComponent(TCustomSpeedButton, TCarbonWSSpeedButton);
  Result := True;
end;

// Arrow
function RegisterArrow: Boolean; alias : 'WSRegisterArrow';
begin
  RegisterWSComponent(TArrow, TCarbonWSArrow);
  Result := True;
end;

// CheckLst
function RegisterCustomCheckListBox: Boolean; alias : 'WSRegisterCustomCheckListBox';
begin
  RegisterWSComponent(TCustomCheckListBox, TCarbonWSCustomCheckListBox);
  Result := True;
end;

// Forms
function RegisterScrollingWinControl: Boolean; alias : 'WSRegisterScrollingWinControl';
begin
  RegisterWSComponent(TScrollingWinControl, TCarbonWSScrollingWinControl);
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
  RegisterWSComponent(TCustomForm, TCarbonWSCustomForm);
  Result := True;
end;

function RegisterHintWindow: Boolean; alias : 'WSRegisterHintWindow';
begin
  RegisterWSComponent(THintWindow, TCarbonWSHintWindow);
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
  RegisterWSComponent(TMenuItem, TCarbonWSMenuItem);
  Result := True;
end;

function RegisterMenu: Boolean; alias : 'WSRegisterMenu';
begin
  RegisterWSComponent(TMenu, TCarbonWSMenu);
  Result := True;
end;

function RegisterMainMenu: Boolean; alias : 'WSRegisterMainMenu';
begin
  Result := False;
end;

function RegisterPopupMenu: Boolean; alias : 'WSRegisterPopupMenu';
begin
  RegisterWSComponent(TPopupMenu, TCarbonWSPopupMenu);
  Result := True;
end;

function RegisterPairSplitterSide: Boolean; alias : 'WSRegisterPairSplitterSide';
begin
  RegisterWSComponent(TPairSplitterSide, TCarbonWSPairSplitterSide);
  Result := True;
end;

function RegisterCustomPairSplitter: Boolean; alias : 'WSRegisterCustomPairSplitter';
begin
  RegisterWSComponent(TCustomPairSplitter, TCarbonWSCustomPairSplitter);
  Result := True;
end;

// Spin
function RegisterCustomFloatSpinEdit: Boolean; alias : 'WSRegisterCustomFloatSpinEdit';
begin
  RegisterWSComponent(TCustomFloatSpinEdit, TCarbonWSCustomFloatSpinEdit);
  Result := True;
end;

// RubberBand
function RegisterCustomRubberBand: Boolean; alias : 'WSRegisterCustomRubberBand';
begin
  Result := False;
end;

end.
