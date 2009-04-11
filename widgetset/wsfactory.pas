unit WSFactory;

{$mode objfpc}{$H+}

interface
uses
  Classes, Controls, ImgList, Grids, PairSplitter, Spin,
  WSLCLClasses, WSControls, WSImgList, WSGrids, WSPairSplitter, WSSpin;

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
// dialogs
procedure RegisterCommonDialog;
procedure RegisterFileDialog;
procedure RegisterOpenDialog;
procedure RegisterSaveDialog;
procedure RegisterSelectDirectoryDialog;
procedure RegisterColorDialog;
procedure RegisterColorButton;
procedure RegisterFontDialog;
// StdCtrls
procedure RegisterCustomScrollBar;
procedure RegisterCustomGroupBox;
procedure RegisterCustomComboBox;
procedure RegisterCustomListBox;
procedure RegisterCustomEdit;
procedure RegisterCustomMemo;
procedure RegisterButtonControl;
procedure RegisterCustomButton;
procedure RegisterCustomCheckBox;
procedure RegisterToggleBox;
procedure RegisterRadioButton;
procedure RegisterCustomStaticText;
procedure RegisterCustomLabel;
// extctrls
procedure RegisterCustomPage;
procedure RegisterCustomNotebook;
procedure RegisterShape;
procedure RegisterCustomSplitter;
procedure RegisterPaintBox;
procedure RegisterCustomImage;
procedure RegisterBevel;
procedure RegisterCustomRadioGroup;
procedure RegisterCustomCheckGroup;
procedure RegisterCustomLabeledEdit;
procedure RegisterCustomPanel;
procedure RegisterCustomTrayIcon;
//ExtDlgs
procedure RegisterPreviewFileControl;
procedure RegisterPreviewFileDialog;
procedure RegisterOpenPictureDialog;
procedure RegisterSavePictureDialog;
procedure RegisterCalculatorDialog;
procedure RegisterCalculatorForm;
//procedure RegisterCalendarDialogForm;
procedure RegisterCalendarDialog;
// Buttons
procedure RegisterCustomBitBtn;
procedure RegisterCustomSpeedButton;
// Arrow
procedure RegisterArrow;
// CheckLst
procedure RegisterCustomCheckListBox;
// Forms
procedure RegisterScrollingWinControl;
procedure RegisterScrollBox;
procedure RegisterCustomFrame;
procedure RegisterCustomForm;
procedure RegisterHintWindow;
// Grids
procedure RegisterCustomGrid;
// Menus
procedure RegisterMenuItem;
procedure RegisterMenu;
procedure RegisterMainMenu;
procedure RegisterPopupMenu;
// PairSplitter
procedure RegisterPairSplitterSide;
procedure RegisterCustomPairSplitter;
// Spin
procedure RegisterCustomFloatSpinEdit;
// RubberBand
procedure RegisterCustomRubberBand;

implementation
uses
  Dialogs, ExtCtrls, ExtDlgs,
  WSDialogs, WSExtCtrls, WSExtDlgs;

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
// dialogs
function WSRegisterCommonDialog: Boolean;      external name 'WSRegisterCommonDialog';
function WSRegisterFileDialog: Boolean;        external name 'WSRegisterFileDialog';
function WSRegisterOpenDialog: Boolean;        external name 'WSRegisterOpenDialog';
function WSRegisterSaveDialog: Boolean;        external name 'WSRegisterSaveDialog';
function WSRegisterSelectDirectoryDialog: Boolean; external name 'WSRegisterSelectDirectoryDialog';
function WSRegisterColorDialog: Boolean;       external name 'WSRegisterColorDialog';
function WSRegisterColorButton: Boolean;       external name 'WSRegisterColorButton';
function WSRegisterFontDialog: Boolean;        external name 'WSRegisterFontDialog';
// StdCtrls
function WSRegisterCustomScrollBar: Boolean;   external name 'WSRegisterCustomScrollBar';
function WSRegisterCustomGroupBox: Boolean;    external name 'WSRegisterCustomGroupBox';
function WSRegisterCustomComboBox: Boolean;    external name 'WSRegisterCustomComboBox';
function WSRegisterCustomListBox: Boolean;     external name 'WSRegisterCustomListBox';
function WSRegisterCustomEdit: Boolean;        external name 'WSRegisterCustomEdit';
function WSRegisterCustomMemo: Boolean;        external name 'WSRegisterCustomMemo';
function WSRegisterButtonControl: Boolean;     external name 'WSRegisterButtonControl';
function WSRegisterCustomButton: Boolean;      external name 'WSRegisterCustomButton';
function WSRegisterCustomCheckBox: Boolean;    external name 'WSRegisterCustomCheckBox';
function WSRegisterToggleBox: Boolean;         external name 'WSRegisterToggleBox';
function WSRegisterRadioButton: Boolean;       external name 'WSRegisterRadioButton';
function WSRegisterCustomStaticText: Boolean;  external name 'WSRegisterCustomStaticText';
function WSRegisterCustomLabel: Boolean;       external name 'WSRegisterCustomLabel';
// extctrls
function WSRegisterCustomPage: Boolean;        external name 'WSRegisterCustomPage';
function WSRegisterCustomNotebook: Boolean;    external name 'WSRegisterCustomNotebook';
function WSRegisterShape: Boolean;             external name 'WSRegisterShape';
function WSRegisterCustomSplitter: Boolean;    external name 'WSRegisterCustomSplitter';
function WSRegisterPaintBox: Boolean;          external name 'WSRegisterPaintBox';
function WSRegisterCustomImage: Boolean;       external name 'WSRegisterCustomImage';
function WSRegisterBevel: Boolean;             external name 'WSRegisterBevel';
function WSRegisterCustomRadioGroup: Boolean;  external name 'WSRegisterCustomRadioGroup';
function WSRegisterCustomCheckGroup: Boolean;  external name 'WSRegisterCustomCheckGroup';
function WSRegisterCustomLabeledEdit: Boolean; external name 'WSRegisterCustomLabeledEdit';
function WSRegisterCustomPanel: Boolean;       external name 'WSRegisterCustomPanel';
function WSRegisterCustomTrayIcon: Boolean;    external name 'WSRegisterCustomTrayIcon';
//ExtDlgs
function WSRegisterPreviewFileControl: Boolean; external name 'WSRegisterPreviewFileControl';
function WSRegisterPreviewFileDialog: Boolean; external name 'WSRegisterPreviewFileDialog';
function WSRegisterOpenPictureDialog: Boolean; external name 'WSRegisterOpenPictureDialog';
function WSRegisterSavePictureDialog: Boolean; external name 'WSRegisterSavePictureDialog';
function WSRegisterCalculatorDialog: Boolean;  external name 'WSRegisterCalculatorDialog';
function WSRegisterCalculatorForm: Boolean;    external name 'WSRegisterCalculatorForm';
//function WSRegisterCalendarDialogForm: Boolean; external name 'WSRegisterCalendarDialogForm';
function WSRegisterCalendarDialog: Boolean;    external name 'WSRegisterCalendarDialog';
// Buttons
function WSRegisterCustomBitBtn: Boolean;      external name 'WSRegisterCustomBitBtn';
function WSRegisterCustomSpeedButton: Boolean; external name 'WSRegisterCustomSpeedButton';
// Arrow
function WSRegisterArrow: Boolean;             external name 'WSRegisterArrow';
// CheckLst
function WSRegisterCustomCheckListBox: Boolean; external name 'WSRegisterCustomCheckListBox';
// Forms
function WSRegisterScrollingWinControl: Boolean;external name 'WSRegisterScrollingWinControl';
function WSRegisterScrollBox: Boolean;          external name 'WSRegisterScrollBox';
function WSRegisterCustomFrame: Boolean;        external name 'WSRegisterCustomFrame';
function WSRegisterCustomForm: Boolean;         external name 'WSRegisterCustomForm';
function WSRegisterHintWindow: Boolean;         external name 'WSRegisterHintWindow';
// Grids
function WSRegisterCustomGrid: Boolean;         external name 'WSRegisterCustomGrid';
// Menus
function WSRegisterMenuItem: Boolean;           external name 'WSRegisterMenuItem';
function WSRegisterMenu: Boolean;               external name 'WSRegisterMenu';
function WSRegisterMainMenu: Boolean;           external name 'WSRegisterMainMenu';
function WSRegisterPopupMenu: Boolean;          external name 'WSRegisterPopupMenu';
// PairSplitter
function WSRegisterPairSplitterSide: Boolean;   external name 'WSRegisterPairSplitterSide';
function WSRegisterCustomPairSplitter: Boolean; external name 'WSRegisterCustomPairSplitter';
// Spin
function WSRegisterCustomFloatSpinEdit: Boolean;external name 'WSRegisterCustomFloatSpinEdit';
// RubberBand
function WSRegisterCustomRubberBand: Boolean;   external name 'WSRegisterCustomRubberBand';

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

// dialogs
procedure RegisterCommonDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterCommonDialog then
    RegisterWSComponent(TCommonDialog, TWSCommonDialog);
  Done := True;
end;

procedure RegisterFileDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterFileDialog;
//  if not WSRegisterFileDialog then
//    RegisterWSComponent(TFileDialog, TWSFileDialog);
  Done := True;
end;

procedure RegisterOpenDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterOpenDialog;
//  if not WSRegisterOpenDialog then
//    RegisterWSComponent(TOpenDialog, TWSOpenDialog);
  Done := True;
end;

procedure RegisterSaveDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterSaveDialog;
//  if not WSRegisterSaveDialog then
//    RegisterWSComponent(TSaveDialog, TWSSaveDialog);
  Done := True;
end;

procedure RegisterSelectDirectoryDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterSelectDirectoryDialog;
//  if not WSRegisterSelectDirectoryDialog then
//    RegisterWSComponent(TSelectDirectoryDialog, TWSSelectDirectoryDialog);
  Done := True;
end;

procedure RegisterColorDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterColorDialog;
//  if not WSRegisterColorDialog then
//    RegisterWSComponent(TColorDialog, TWSColorDialog);
  Done := True;
end;

procedure RegisterColorButton;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterColorButton;
//  if not WSRegisterColorButton then
//    RegisterWSComponent(TColorButton, TWSColorButton);
  Done := True;
end;

procedure RegisterFontDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterFontDialog;
//  if not WSRegisterFontDialog then
//    RegisterWSComponent(TFontDialog, TWSFontDialog);
  Done := True;
end;

// SdtCtrls
procedure RegisterCustomScrollBar;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomScrollBar;
//  if not WSRegisterCustomScrollBar then
//    RegisterWSComponent(TCustomScrollBar, TWSCustomScrollBar);
  Done := True;
end;

procedure RegisterCustomGroupBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomGroupBox;
//  if not WSRegisterCustomGroupBox then
//    RegisterWSComponent(TCustomGroupBox, TWSCustomGroupBox);
  Done := True;
end;

procedure RegisterCustomComboBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomComboBox;
//  if not WSRegisterCustomComboBox then
//    RegisterWSComponent(TCustomComboBox, TWSCustomComboBox);
  Done := True;
end;

procedure RegisterCustomListBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomListBox;
//  if not WSRegisterCustomListBox then
//    RegisterWSComponent(TCustomListBox, TWSCustomListBox);
  Done := True;
end;

procedure RegisterCustomEdit;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomEdit;
//  if not WSRegisterCustomEdit then
//    RegisterWSComponent(TCustomEdit, TWSCustomEdit);
  Done := True;
end;

procedure RegisterCustomMemo;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomMemo;
//  if not WSRegisterCustomMemo then
//    RegisterWSComponent(TCustomMemo, TWSCustomMemo);
  Done := True;
end;

procedure RegisterButtonControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterButtonControl;
//  if not WSRegisterButtonControl then
//    RegisterWSComponent(TButtonControl, TWSButtonControl);
  Done := True;
end;

procedure RegisterCustomButton;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomButton;
//  if not WSRegisterCustomButton then
//    RegisterWSComponent(TCustomButton, TWSButton);
  Done := True;
end;

procedure RegisterCustomCheckBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomCheckBox;
//  if not WSRegisterCustomCheckBox then
//    RegisterWSComponent(TCustomCheckBox, TWSCustomCheckBox);
  Done := True;
end;

procedure RegisterToggleBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterToggleBox;
//  if not WSRegisterToggleBox then
//    RegisterWSComponent(TToggleBox, TWSToggleBox);
  Done := True;
end;

procedure RegisterRadioButton;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterRadioButton;
//  if not WSRegisterRadioButton then
//    RegisterWSComponent(TRadioButton, TWSRadioButton);
  Done := True;
end;

procedure RegisterCustomStaticText;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomStaticText;
//  if not WSRegisterCustomStaticText then
//    RegisterWSComponent(TCustomStaticText, TWSCustomStaticText);
  Done := True;
end;

procedure RegisterCustomLabel;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomLabel;
//  if not WSRegisterCustomLabel then
//    RegisterWSComponent(TCustomLabel, TWSCustomLabel);
  Done := True;
end;

// extctrls
procedure RegisterCustomPage;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomPage;
//  if not WSRegisterCustomPage then
//    RegisterWSComponent(TCustomPage, TWSCustomPage);
  Done := True;
end;

procedure RegisterCustomNotebook;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterCustomNotebook then
    RegisterWSComponent(TCustomNotebook, TWSCustomNotebook);
  Done := True;
end;

procedure RegisterShape;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterShape;
//  if not WSRegisterShape then
//    RegisterWSComponent(TShape, TWSShape);
  Done := True;
end;

procedure RegisterCustomSplitter;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomSplitter;
//  if not WSRegisterCustomSplitter then
//    RegisterWSComponent(TCustomSplitter, TWSCustomSplitter);
  Done := True;
end;

procedure RegisterPaintBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterPaintBox;
//  if not WSRegisterPaintBox then
//    RegisterWSComponent(TPaintBox, TWSPaintBox);
  Done := True;
end;

procedure RegisterCustomImage;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomImage;
//  if not WSRegisterCustomImage then
//    RegisterWSComponent(TCustomImage, TWSCustomImage);
  Done := True;
end;

procedure RegisterBevel;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterBevel;
//  if not WSRegisterBevel then
//    RegisterWSComponent(TBevel, TWSBevel);
  Done := True;
end;

procedure RegisterCustomRadioGroup;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomRadioGroup;
//  if not WSRegisterCustomRadioGroup then
//    RegisterWSComponent(TCustomRadioGroup, TWSCustomRadioGroup);
  Done := True;
end;

procedure RegisterCustomCheckGroup;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomCheckGroup;
//  if not WSRegisterCustomCheckGroup then
//    RegisterWSComponent(TCustomCheckGroup, TWSCustomCheckGroup);
  Done := True;
end;

procedure RegisterCustomLabeledEdit;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomLabeledEdit;
//  if not WSRegisterCustomLabeledEdit then
//    RegisterWSComponent(TCustomLabeledEdit, TWSCustomLabeledEdit);
  Done := True;
end;

procedure RegisterCustomPanel;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomPanel;
//  if not WSRegisterCustomPanel then
//    RegisterWSComponent(TCustomPanel, TWSCustomPanel);
  Done := True;
end;

procedure RegisterCustomTrayIcon;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterCustomTrayIcon then
    RegisterWSComponent(TCustomTrayIcon, TWSCustomTrayIcon);
  Done := True;
end;

// ExtDlgs
procedure RegisterPreviewFileControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterPreviewFileControl;
//  if not WSRegisterPreviewFileControl then
//    RegisterWSComponent(TPreviewFileControl, TWSPreviewFileControl);
  Done := True;
end;

procedure RegisterPreviewFileDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterPreviewFileDialog;
//  if not WSRegisterPreviewFileDialog then
//    RegisterWSComponent(TPreviewFileDialog, TWSPreviewFileDialog);
  Done := True;
end;

procedure RegisterOpenPictureDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterOpenPictureDialog;
//  if not WSRegisterOpenPictureDialog then
//    RegisterWSComponent(TOpenPictureDialog, TWSOpenPictureDialog);
  Done := True;
end;

procedure RegisterSavePictureDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterSavePictureDialog;
//  if not WSRegisterSavePictureDialog then
//    RegisterWSComponent(TSavePictureDialog, TWSSavePictureDialog);
  Done := True;
end;

procedure RegisterCalculatorDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCalculatorDialog;
//  if not WSRegisterCalculatorDialog then
//    RegisterWSComponent(TCalculatorDialog, TWSCalculatorDialog);
  Done := True;
end;

procedure RegisterCalculatorForm;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCalculatorForm;
//  if not WSRegisterCalculatorForm then
//    RegisterWSComponent(TCalculatorForm, TWSCalculatorForm);
  Done := True;
end;

(*procedure RegisterCalendarDialogForm;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCalendarDialogForm;
//  if not WSRegisterCalendarDialogForm then
//    RegisterWSComponent(TCalendarDialogForm, TWSCalendarDialogForm);
  Done := True;
end;*)

procedure RegisterCalendarDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCalendarDialog;
//  if not WSRegisterCalendarDialog then
//    RegisterWSComponent(TCalendarDialog, TWSCalendarDialog);
  Done := True;
end;

procedure RegisterCustomBitBtn;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomBitBtn;
//  if not WSRegisterCustomBitBtn then
//    RegisterWSComponent(TCustomBitBtn, TWSBitBtn);
  Done := True;
end;

procedure RegisterCustomSpeedButton;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomSpeedButton;
//  if not WSRegisterCustomSpeedButton then
//    RegisterWSComponent(TCustomSpeedButton, TWSSpeedButton);
  Done := True;
end;

procedure RegisterArrow;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterArrow;
//  if not WSRegisterArrow then
//    RegisterWSComponent(TArrow, TWSArrow);
  Done := True;
end;

procedure RegisterCustomCheckListBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomCheckListBox;
//  if not WSRegisterCustomCheckListBox then
//    RegisterWSComponent(TCustomCheckListBox, TWSCustomCheckListBox);
  Done := True;
end;

procedure RegisterScrollingWinControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterScrollingWinControl;
//  if not WSRegisterScrollingWinControl then
//    RegisterWSComponent(TScrollingWinControl, TWSScrollingWinControl);
  Done := True;
end;

procedure RegisterScrollBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterScrollBox;
//  if not WSRegisterScrollBox then
//    RegisterWSComponent(TScrollBox, TWSScrollBox);
  Done := True;
end;

procedure RegisterCustomFrame;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomFrame;
//  if not WSRegisterCustomFrame then
//    RegisterWSComponent(TCustomFrame, TWSCustomFrame);
  Done := True;
end;

procedure RegisterCustomForm;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomForm;
//  if not WSRegisterCustomForm then
//    RegisterWSComponent(TCustomForm, TWSCustomForm);
  Done := True;
end;

procedure RegisterHintWindow;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterHintWindow;
//  if not WSRegisterHintWindow then
//    RegisterWSComponent(THintWindow, TWSHintWindow);
  Done := True;
end;

procedure RegisterCustomGrid;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterCustomGrid then
    RegisterWSComponent(TCustomGrid, TWSCustomGrid);
  Done := True;
end;

procedure RegisterMenuItem;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterMenuItem;
//  if not WSRegisterMenuItem then
//    RegisterWSComponent(TMenuItem, TWSMenuItem);
  Done := True;
end;

procedure RegisterMenu;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterMenu;
//  if not WSRegisterMenu then
//    RegisterWSComponent(TMenu, TWSMenu);
  Done := True;
end;

procedure RegisterMainMenu;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterMainMenu;
//  if not WSRegisterMainMenu then
//    RegisterWSComponent(TMainMenu, TWSMainMenu);
  Done := True;
end;

procedure RegisterPopupMenu;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterPopupMenu;
//  if not WSRegisterPopupMenu then
//    RegisterWSComponent(TPopupMenu, TWSPopupMenu);
  Done := True;
end;

procedure RegisterPairSplitterSide;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterPairSplitterSide;
//  if not WSRegisterPairSplitterSide then
//    RegisterWSComponent(TPairSplitterSide, TWSPairSplitterSide);
  Done := True;
end;

procedure RegisterCustomPairSplitter;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterCustomPairSplitter then
    RegisterWSComponent(TCustomPairSplitter, TWSCustomPairSplitter);
  Done := True;
end;

procedure RegisterCustomFloatSpinEdit;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterCustomFloatSpinEdit then
    RegisterWSComponent(TCustomFloatSpinEdit, TWSCustomFloatSpinEdit);
  Done := True;
end;

procedure RegisterCustomRubberBand;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomRubberBand;
//  if not WSRegisterCustomRubberBand then
//    RegisterWSComponent(TCustomRubberBand, TWSCustomRubberBand);
  Done := True;
end;

end.

