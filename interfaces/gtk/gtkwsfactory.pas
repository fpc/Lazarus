unit GtkWSFactory;

{$mode objfpc}{$H+}

interface
uses
  Classes, Controls, ComCtrls, ImgList, Calendar, StdCtrls, Arrow, Spin,
  Dialogs, ExtCtrls, ExtDlgs, Buttons, CheckLst, Forms, Grids, Menus,
  WSLCLClasses;

implementation
uses
  GtkWSArrow,
  GtkWSButtons,
  GtkWSCalendar,
  GtkWSCheckLst,
  GtkWSComCtrls,
  GtkWSControls,
  GtkWSDialogs,
  GtkWSExtCtrls,
  GtkWSExtDlgs,
  GtkWSForms,
  GtkWSGrids,
  GtkWSImgList,
  GtkWSMenus,
  GtkWSSpin,
  GtkWSStdCtrls;

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
  Result := False;
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
  Result := False;
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
  Result := False;
end;

function RegisterCustomProgressBar: Boolean; alias : 'WSRegisterCustomProgressBar';
begin
  Result := False;
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
  Result := False;
end;

function RegisterCustomTreeView: Boolean; alias : 'WSRegisterCustomTreeView';
begin
  Result := False;
end;

// calendar
function RegisterCustomCalendar: Boolean; alias : 'WSRegisterCustomCalendar';
begin
  Result := False;
end;

// dialogs
function RegisterCommonDialog: Boolean; alias : 'WSRegisterCommonDialog';
begin
  Result := False;
end;

function RegisterFileDialog: Boolean; alias : 'WSRegisterFileDialog';
begin
//  Result := False;
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
  Result := False;
end;

function RegisterColorButton: Boolean; alias : 'WSRegisterColorButton';
begin
//  Result := False;
end;

function RegisterFontDialog: Boolean; alias : 'WSRegisterFontDialog';
begin
  Result := False;
end;

// StdCtrls
function RegisterCustomScrollBar: Boolean; alias : 'WSRegisterCustomScrollBar';
begin
  Result := False;
end;

function RegisterCustomGroupBox: Boolean; alias : 'WSRegisterCustomGroupBox';
begin
  Result := False;
end;

function RegisterCustomComboBox: Boolean; alias : 'WSRegisterCustomComboBox';
begin
  Result := False;
end;

function RegisterCustomListBox: Boolean; alias : 'WSRegisterCustomListBox';
begin
  Result := False;
end;

function RegisterCustomEdit: Boolean; alias : 'WSRegisterCustomEdit';
begin
  Result := False;
end;

function RegisterCustomMemo: Boolean; alias : 'WSRegisterCustomMemo';
begin
  Result := False;
end;

function RegisterButtonControl: Boolean; alias : 'WSRegisterButtonControl';
begin
  Result := False;
end;

function RegisterCustomButton: Boolean; alias : 'WSRegisterCustomButton';
begin
  Result := False;
end;

function RegisterCustomCheckBox: Boolean; alias : 'WSRegisterCustomCheckBox';
begin
  Result := False;
end;

function RegisterToggleBox: Boolean; alias : 'WSRegisterToggleBox';
begin
  Result := False;
end;

function RegisterRadioButton: Boolean; alias : 'WSRegisterRadioButton';
begin
  Result := False;
end;

function RegisterCustomStaticText: Boolean; alias : 'WSRegisterCustomStaticText';
begin
  Result := False;
end;

function RegisterCustomLabel: Boolean; alias : 'WSRegisterCustomLabel';
begin
  Result := False;
end;

// extctrls
function RegisterCustomPage: Boolean; alias : 'WSRegisterCustomPage';
begin
  Result := False;
end;

function RegisterCustomNotebook: Boolean; alias : 'WSRegisterCustomNotebook';
begin
  Result := False;
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
  Result := False;
end;

function RegisterCustomTrayIcon: Boolean; alias : 'WSRegisterCustomTrayIcon';
begin
  Result := False;
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
//  Result := False;
end;*)

function RegisterCalendarDialog: Boolean; alias : 'WSRegisterCalendarDialog';
begin
  Result := False;
end;

// Buttons
function RegisterCustomBitBtn: Boolean; alias : 'WSRegisterCustomBitBtn';
begin
  Result := False;
end;

function RegisterCustomSpeedButton: Boolean; alias : 'WSRegisterCustomSpeedButton';
begin
  Result := False;
end;

// Arrow
function RegisterArrow: Boolean; alias : 'WSRegisterArrow';
begin
  Result := False;
end;

// CheckLst
function RegisterCustomCheckListBox: Boolean; alias : 'WSRegisterCustomCheckListBox';
begin
  Result := False;
end;

// Forms
function RegisterScrollingWinControl: Boolean; alias : 'WSRegisterScrollingWinControl';
begin
  Result := False;
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
  Result := False;
end;

function RegisterHintWindow: Boolean; alias : 'WSRegisterHintWindow';
begin
  Result := False;
end;

function RegisterCustomGrid: Boolean; alias : 'WSRegisterCustomGrid';
begin
  Result := False;
end;

function RegisterMenuItem: Boolean; alias : 'WSRegisterMenuItem';
begin
  Result := False;
end;

function RegisterMenu: Boolean; alias : 'WSRegisterMenu';
begin
  Result := False;
end;

function RegisterMainMenu: Boolean; alias : 'WSRegisterMainMenu';
begin
  Result := False;
end;

function RegisterPopupMenu: Boolean; alias : 'WSRegisterPopupMenu';
begin
  Result := False;
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
  Result := False;
end;

function RegisterCustomRubberBand: Boolean; alias : 'WSRegisterCustomRubberBand';
begin
  Result := False;
end;

end.
