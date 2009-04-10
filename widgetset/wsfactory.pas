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
// dialogs
procedure RegisterCommonDialog;
procedure RegisterFileDialog;
procedure RegisterOpenDialog;
procedure RegisterSaveDialog;
procedure RegisterSelectDirectoryDialog;
procedure RegisterColorDialog;
procedure RegisterColorButton;
procedure RegisterFontDialog;
// dirsel
//procedure RegisterDirSelDlg;
// StdCtrls
procedure RegisterScrollBar;
procedure RegisterCustomGroupBox;
procedure RegisterGroupBox;
procedure RegisterCustomComboBox;
procedure RegisterComboBox;
procedure RegisterCustomListBox;
procedure RegisterListBox;
procedure RegisterCustomEdit;
procedure RegisterCustomMemo;
procedure RegisterEdit;
procedure RegisterMemo;
procedure RegisterButtonControl;
procedure RegisterCustomButton;
procedure RegisterCustomCheckBox;
procedure RegisterCheckBox;
procedure RegisterToggleBox;
procedure RegisterRadioButton;
procedure RegisterCustomStaticText;
procedure RegisterStaticText;
procedure RegisterLabel;

// extctrls
procedure RegisterCustomPage;
procedure RegisterCustomNotebook;
procedure RegisterPage;
procedure RegisterNotebook;
procedure RegisterShape;
procedure RegisterCustomSplitter;
procedure RegisterSplitter;
procedure RegisterPaintBox;
procedure RegisterCustomImage;
procedure RegisterImage;
procedure RegisterBevel;
procedure RegisterCustomRadioGroup;
procedure RegisterRadioGroup;
procedure RegisterCustomCheckGroup;
procedure RegisterCheckGroup;
procedure RegisterCustomLabeledEdit;
procedure RegisterLabeledEdit;
procedure RegisterCustomPanel;
procedure RegisterPanel;
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


implementation
uses
  Dialogs, ExtCtrls, ExtDlgs,
  WSDialogs, WSExtCtrls,WSExtDlgs;

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
// dirsel
//function WSRegisterDirSelDlg: Boolean;         external name 'WSRegisterDirSelDlg';
// StdCtrls
function WSRegisterScrollBar: Boolean;         external name 'WSRegisterScrollBar';
function WSRegisterCustomGroupBox: Boolean;    external name 'WSRegisterCustomGroupBox';
function WSRegisterGroupBox: Boolean;          external name 'WSRegisterGroupBox';
function WSRegisterCustomComboBox: Boolean;    external name 'WSRegisterCustomComboBox';
function WSRegisterComboBox: Boolean;          external name 'WSRegisterComboBox';
function WSRegisterCustomListBox: Boolean;     external name 'WSRegisterCustomListBox';
function WSRegisterListBox: Boolean;           external name 'WSRegisterListBox';
function WSRegisterCustomEdit: Boolean;        external name 'WSRegisterCustomEdit';
function WSRegisterCustomMemo: Boolean;        external name 'WSRegisterCustomMemo';
function WSRegisterEdit: Boolean;              external name 'WSRegisterEdit';
function WSRegisterMemo: Boolean;              external name 'WSRegisterMemo';
function WSRegisterButtonControl: Boolean;     external name 'WSRegisterButtonControl';
function WSRegisterCustomButton: Boolean;      external name 'WSRegisterCustomButton';
function WSRegisterCustomCheckBox: Boolean;    external name 'WSRegisterCustomCheckBox';
function WSRegisterCheckBox: Boolean;          external name 'WSRegisterCheckBox';
function WSRegisterToggleBox: Boolean;         external name 'WSRegisterToggleBox';
function WSRegisterRadioButton: Boolean;       external name 'WSRegisterRadioButton';
function WSRegisterCustomStaticText: Boolean;  external name 'WSRegisterCustomStaticText';
function WSRegisterStaticText: Boolean;        external name 'WSRegisterStaticText';
function WSRegisterLabel: Boolean;             external name 'WSRegisterLabel';
// extctrls
function WSRegisterCustomPage: Boolean;        external name 'WSRegisterCustomPage';
function WSRegisterCustomNotebook: Boolean;    external name 'WSRegisterCustomNotebook';
function WSRegisterPage: Boolean;              external name 'WSRegisterPage';
function WSRegisterNotebook: Boolean;          external name 'WSRegisterNotebook';
function WSRegisterShape: Boolean;             external name 'WSRegisterShape';
function WSRegisterCustomSplitter: Boolean;    external name 'WSRegisterCustomSplitter';
function WSRegisterSplitter: Boolean;          external name 'WSRegisterSplitter';
function WSRegisterPaintBox: Boolean;          external name 'WSRegisterPaintBox';
function WSRegisterCustomImage: Boolean;       external name 'WSRegisterCustomImage';
function WSRegisterImage: Boolean;             external name 'WSRegisterImage';
function WSRegisterBevel: Boolean;             external name 'WSRegisterBevel';
function WSRegisterCustomRadioGroup: Boolean;  external name 'WSRegisterCustomRadioGroup';
function WSRegisterRadioGroup: Boolean;        external name 'WSRegisterRadioGroup';
function WSRegisterCustomCheckGroup: Boolean;  external name 'WSRegisterCustomCheckGroup';
function WSRegisterCheckGroup: Boolean;        external name 'WSRegisterCheckGroup';
function WSRegisterCustomLabeledEdit: Boolean; external name 'WSRegisterCustomLabeledEdit';
function WSRegisterLabeledEdit: Boolean;       external name 'WSRegisterLabeledEdit';
function WSRegisterCustomPanel: Boolean;       external name 'WSRegisterCustomPanel';
function WSRegisterPanel: Boolean;             external name 'WSRegisterPanel';
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
procedure RegisterScrollBar;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterScrollBar;
//  if not WSRegisterScrollBar then
//    RegisterWSComponent(TScrollBar, TWSScrollBar);
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

procedure RegisterGroupBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterGroupBox;
//  if not WSRegisterGroupBox then
//    RegisterWSComponent(TGroupBox, TWSGroupBox);
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

procedure RegisterComboBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterComboBox;
//  if not WSRegisterComboBox then
//    RegisterWSComponent(TComboBox, TWSComboBox);
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

procedure RegisterListBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterListBox;
//  if not WSRegisterListBox then
//    RegisterWSComponent(TListBox, TWSListBox);
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

procedure RegisterEdit;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterEdit;
//  if not WSRegisterEdit then
//    RegisterWSComponent(TEdit, TWSEdit);
  Done := True;
end;

procedure RegisterMemo;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterMemo;
//  if not WSRegisterMemo then
//    RegisterWSComponent(TMemo, TWSMemo);
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


procedure RegisterCheckBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCheckBox;
//  if not WSRegisterCheckBox then
//    RegisterWSComponent(TCheckBox, TWSCheckBox);
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

procedure RegisterStaticText;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterStaticText;
//  if not WSRegisterStaticText then
//    RegisterWSComponent(TStaticText, TWSStaticText);
  Done := True;
end;

procedure RegisterLabel;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterLabel;
//  if not WSRegisterLabel then
//    RegisterWSComponent(TLabel, TWSLabel);
  Done := True;
end;


// dirsel
(*procedure RegisterDirSelDlg;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterDirSelDlg;
  Done := True;
end;*)

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

procedure RegisterPage;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterPage;
//  if not WSRegisterPage then
//    RegisterWSComponent(TPage, TWSPage);
  Done := True;
end;

procedure RegisterNotebook;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterNotebook;
//  if not WSRegisterNotebook then
//    RegisterWSComponent(TNotebook, TWSNotebook);
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

procedure RegisterSplitter;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterSplitter;
//  if not WSRegisterSplitter then
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

procedure RegisterImage;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterImage;
//  if not WSRegisterImage then
//    RegisterWSComponent(TImage, TWSImage);
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

procedure RegisterRadioGroup;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterRadioGroup;
//  if not WSRegisterRadioGroup then
//    RegisterWSComponent(TRadioGroup, TWSRadioGroup);
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

procedure RegisterCheckGroup;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCheckGroup;
//  if not WSRegisterCheckGroup then
//    RegisterWSComponent(TCheckGroup, TWSCheckGroup);
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

procedure RegisterLabeledEdit;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterLabeledEdit;
//  if not WSRegisterLabeledEdit then
//    RegisterWSComponent(TLabeledEdit, TWSLabeledEdit);
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

procedure RegisterPanel;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterPanel;
//  if not WSRegisterPanel then
//    RegisterWSComponent(TPanel, TWSPanel);
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

end.

