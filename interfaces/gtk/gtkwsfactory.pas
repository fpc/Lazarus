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
//  RegisterWSComponent(TImageList, TGtkWSImageList);
//  RegisterWSComponent(TCustomImageList, TGtkWSCustomImageList);
  Result := False;
end;

// controls
function RegisterDragImageList: Boolean; alias : 'WSRegisterDragImageList';
begin
  RegisterWSComponent(TDragImageList, TGtkWSDragImageList);
  Result := True;
end;

function RegisterControl: Boolean; alias : 'WSRegisterControl';
begin
//  RegisterWSComponent(TControl, TGtkWSControl);
  Result := False;
end;

function RegisterWinControl: Boolean; alias : 'WSRegisterWinControl';
begin
  RegisterWSComponent(TWinControl, TGtkWSWinControl, TGtkPrivateWidget);
  Result := True;
end;

function RegisterGraphicControl: Boolean; alias : 'WSRegisterGraphicControl';
begin
//  RegisterWSComponent(TGraphicControl, TGtkWSGraphicControl);
  Result := False;
end;

function RegisterCustomControl: Boolean; alias : 'WSRegisterCustomControl';
begin
//  RegisterWSComponent(TCustomControl, TGtkWSCustomControl);
  Result := False;
end;

// comctrls
function RegisterStatusBar: Boolean; alias : 'WSRegisterStatusBar';
begin
  RegisterWSComponent(TStatusBar, TGtkWSStatusBar);
  Result := True;
end;

function RegisterTabSheet: Boolean; alias : 'WSRegisterTabSheet';
begin
//  RegisterWSComponent(TCustomTabSheet, TGtkWSTabSheet);
  Result := False;
end;

function RegisterPageControl: Boolean; alias : 'WSRegisterPageControl';
begin
//  RegisterWSComponent(TCustomPageControl, TGtkWSPageControl);
  Result := False;
end;

function RegisterCustomListView: Boolean; alias : 'WSRegisterCustomListView';
begin
{$IFDEF GTK1}
//  RegisterWSComponent(TCustomListView, TGtkWSListView);
  RegisterWSComponent(TCustomListView, TGtkWSCustomListView, TGtkPrivateScrolling);
  Result := True;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function RegisterCustomProgressBar: Boolean; alias : 'WSRegisterCustomProgressBar';
begin
{$IFDEF GTK1}
  RegisterWSComponent(TCustomProgressBar, TGtkWSProgressBar);
  Result := True;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function RegisterCustomUpDown: Boolean; alias : 'WSRegisterCustomUpDown';
begin
//  RegisterWSComponent(TCustomUpDown, TGtkWSCustomUpDown);
//  RegisterWSComponent(TCustomUpDown, TGtkWSUpDown);
  Result := False;
end;

function RegisterCustomToolButton: Boolean; alias : 'WSRegisterCustomToolButton';
begin
//  RegisterWSComponent(TCustomToolButton, TGtkWSToolButton);
  Result := False;
end;

function RegisterToolBar: Boolean; alias : 'WSRegisterToolBar';
begin
  RegisterWSComponent(TToolBar, TGtkWSToolBar);
  Result := True;
end;

function RegisterCustomTrackBar: Boolean; alias : 'WSRegisterCustomTrackBar';
begin
  RegisterWSComponent(TCustomTrackBar, TGtkWSTrackBar);
  Result := True;
end;

function RegisterCustomTreeView: Boolean; alias : 'WSRegisterCustomTreeView';
begin
//  RegisterWSComponent(TCustomTreeView, TGtkWSCustomTreeView);
//  RegisterWSComponent(TCustomTreeView, TGtkWSTreeView);
  Result := False;
end;

// calendar
function RegisterCustomCalendar: Boolean; alias : 'WSRegisterCustomCalendar';
begin
  RegisterWSComponent(TCustomCalendar, TGtkWSCustomCalendar);
  Result := True;
end;

// dialogs
function RegisterCommonDialog: Boolean; alias : 'WSRegisterCommonDialog';
begin
  RegisterWSComponent(TCommonDialog, TGtkWSCommonDialog);
  Result := True;
end;

function RegisterFileDialog: Boolean; alias : 'WSRegisterFileDialog';
begin
//  RegisterWSComponent(TFileDialog, TGtkWSFileDialog);
  Result := False;
end;

function RegisterOpenDialog: Boolean; alias : 'WSRegisterOpenDialog';
begin
  RegisterWSComponent(TOpenDialog, TGtkWSOpenDialog);
  Result := True;
end;

function RegisterSaveDialog: Boolean; alias : 'WSRegisterSaveDialog';
begin
//  RegisterWSComponent(TSaveDialog, TGtkWSSaveDialog);
  Result := False;
end;

function RegisterSelectDirectoryDialog: Boolean; alias : 'WSRegisterSelectDirectoryDialog';
begin
//  RegisterWSComponent(TSelectDirectoryDialog, TGtkWSSelectDirectoryDialog);
  Result := False;
end;

function RegisterColorDialog: Boolean; alias : 'WSRegisterColorDialog';
begin
  RegisterWSComponent(TColorDialog, TGtkWSColorDialog);
  Result := True;
end;

function RegisterColorButton: Boolean; alias : 'WSRegisterColorButton';
begin
//  RegisterWSComponent(TColorButton, TGtkWSColorButton);
  Result := False;
end;

function RegisterFontDialog: Boolean; alias : 'WSRegisterFontDialog';
begin
  RegisterWSComponent(TFontDialog, TGtkWSFontDialog);
  Result := True;
end;

// StdCtrls
function RegisterCustomScrollBar: Boolean; alias : 'WSRegisterCustomScrollBar';
begin
  RegisterWSComponent(TScrollBar, TGtkWSScrollBar);
  Result := True;
end;

function RegisterCustomGroupBox: Boolean; alias : 'WSRegisterCustomGroupBox';
begin
  RegisterWSComponent(TCustomGroupBox, TGtkWSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TGtkWSGroupBox);
  Result := True;
end;

function RegisterCustomComboBox: Boolean; alias : 'WSRegisterCustomComboBox';
begin
  RegisterWSComponent(TCustomComboBox, TGtkWSCustomComboBox);
//  RegisterWSComponent(TComboBox, TGtkWSComboBox);
  Result := True;
end;

function RegisterCustomListBox: Boolean; alias : 'WSRegisterCustomListBox';
begin
//  RegisterWSComponent(TListBox, TGtkWSListBox);
{$ifdef gtk1}
  RegisterWSComponent(TCustomListBox, TGtkWSCustomListBox, TGtk1PrivateList);
  Result := True;
{$else}
  Result := False;
{$endif}
end;

function RegisterCustomEdit: Boolean; alias : 'WSRegisterCustomEdit';
begin
  RegisterWSComponent(TCustomEdit, TGtkWSCustomEdit, TGtkPrivateEntry);
  Result := True;
end;

function RegisterCustomMemo: Boolean; alias : 'WSRegisterCustomMemo';
begin
  RegisterWSComponent(TCustomMemo, TGtkWSCustomMemo, TGtkPrivateScrolling);
  Result := True;
end;

function RegisterButtonControl: Boolean; alias : 'WSRegisterButtonControl';
begin
//  RegisterWSComponent(TButtonControl, TGtkWSButtonControl);
  Result := False;
end;

function RegisterCustomButton: Boolean; alias : 'WSRegisterCustomButton';
begin
{$ifdef gtk1}
  RegisterWSComponent(TCustomButton, TGtkWSButton, TGtk1PrivateButton);
{$else}
  RegisterWSComponent(TCustomButton, TGtkWSButton, TGtk2PrivateButton);
{$endif}
  Result := True;
end;

function RegisterCustomCheckBox: Boolean; alias : 'WSRegisterCustomCheckBox';
begin
  RegisterWSComponent(TCustomCheckBox, TGtkWSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TGtkWSCheckBox);
  Result := False;
end;

function RegisterToggleBox: Boolean; alias : 'WSRegisterToggleBox';
begin
  RegisterWSComponent(TToggleBox, TGtkWSToggleBox);
  Result := False;
end;

function RegisterRadioButton: Boolean; alias : 'WSRegisterRadioButton';
begin
  RegisterWSComponent(TRadioButton, TGtkWSRadioButton);
  Result := False;
end;

function RegisterCustomStaticText: Boolean; alias : 'WSRegisterCustomStaticText';
begin
//  RegisterWSComponent(TStaticText, TGtkWSStaticText);
  RegisterWSComponent(TCustomStaticText, TGtkWSCustomStaticText);
  Result := False;
end;

function RegisterCustomLabel: Boolean; alias : 'WSRegisterCustomLabel';
begin
  Result := False;
end;

// extctrls
function RegisterCustomPage: Boolean; alias : 'WSRegisterCustomPage';
begin
//  RegisterWSComponent(TPage, TGtkWSPage);
  RegisterWSComponent(TCustomPage, TGtkWSCustomPage);
  Result := True;
end;

function RegisterCustomNotebook: Boolean; alias : 'WSRegisterCustomNotebook';
begin
//  RegisterWSComponent(TNotebook, TGtkWSNotebook);
{$IFDEF GTK1}
  RegisterWSComponent(TCustomNotebook, TGtkWSCustomNotebook, TGtk1PrivateNotebook);
  Result := True;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function RegisterShape: Boolean; alias : 'WSRegisterShape';
begin
//  RegisterWSComponent(TShape, TGtkWSShape);
  Result := False;
end;

function RegisterCustomSplitter: Boolean; alias : 'WSRegisterCustomSplitter';
begin
//  RegisterWSComponent(TSplitter, TGtkWSSplitter);
//  RegisterWSComponent(TCustomSplitter, TGtkWSCustomSplitter);
  Result := False;
end;

function RegisterPaintBox: Boolean; alias : 'WSRegisterPaintBox';
begin
//  RegisterWSComponent(TPaintBox, TGtkWSPaintBox);
  Result := False;
end;

function RegisterCustomImage: Boolean; alias : 'WSRegisterCustomImage';
begin
//  RegisterWSComponent(TCustomImage, TGtkWSCustomImage);
//  RegisterWSComponent(TImage, TGtkWSImage);
  Result := False;
end;

function RegisterBevel: Boolean; alias : 'WSRegisterBevel';
begin
//  RegisterWSComponent(TBevel, TGtkWSBevel);
  Result := False;
end;

function RegisterCustomRadioGroup: Boolean; alias : 'WSRegisterCustomRadioGroup';
begin
//  RegisterWSComponent(TCustomRadioGroup, TGtkWSCustomRadioGroup);
//  RegisterWSComponent(TRadioGroup, TGtkWSRadioGroup);
  Result := False;
end;

function RegisterCustomCheckGroup: Boolean; alias : 'WSRegisterCustomCheckGroup';
begin
//  RegisterWSComponent(TCustomCheckGroup, TGtkWSCustomCheckGroup);
//  RegisterWSComponent(TCheckGroup, TGtkWSCheckGroup);
  Result := False;
end;

function RegisterCustomLabeledEdit: Boolean; alias : 'WSRegisterCustomLabeledEdit';
begin
//  RegisterWSComponent(TCustomLabeledEdit, TGtkWSCustomLabeledEdit);
//  RegisterWSComponent(TLabeledEdit, TGtkWSLabeledEdit);
  Result := False;
end;

function RegisterCustomPanel: Boolean; alias : 'WSRegisterCustomPanel';
begin
//  RegisterWSComponent(TCustomPanel, TGtkWSCustomPanel);
//  RegisterWSComponent(TPanel, TGtkWSPanel);
  Result := False;
end;

function RegisterCustomTrayIcon: Boolean; alias : 'WSRegisterCustomTrayIcon';
begin
{$IFDEF GTK1}
  RegisterWSComponent(TCustomTrayIcon, TGtkWSCustomTrayIcon);
  Result := True;
{$ELSE}
  Result := False;
{$ENDIF}
end;

//ExtDlgs
function RegisterPreviewFileControl: Boolean; alias : 'WSRegisterPreviewFileControl';
begin
  RegisterWSComponent(TPreviewFileControl, TGtkWSPreviewFileControl);
  Result := True;
end;

function RegisterPreviewFileDialog: Boolean; alias : 'WSRegisterPreviewFileDialog';
begin
//  RegisterWSComponent(TPreviewFileDialog, TGtkWSPreviewFileDialog);
  Result := False;
end;

function RegisterOpenPictureDialog: Boolean; alias : 'WSRegisterOpenPictureDialog';
begin
//  RegisterWSComponent(TOpenPictureDialog, TGtkWSOpenPictureDialog);
  Result := False;
end;

function RegisterSavePictureDialog: Boolean; alias : 'WSRegisterSavePictureDialog';
begin
//  RegisterWSComponent(TSavePictureDialog, TGtkWSSavePictureDialog);
  Result := False;
end;

function RegisterCalculatorDialog: Boolean; alias : 'WSRegisterCalculatorDialog';
begin
//  RegisterWSComponent(TCalculatorDialog, TGtkWSCalculatorDialog);
  Result := False;
end;

function RegisterCalculatorForm: Boolean; alias : 'WSRegisterCalculatorForm';
begin
//  RegisterWSComponent(TCalculatorForm, TGtkWSCalculatorForm);
  Result := False;
end;

(*function RegisterCalendarDialogForm: Boolean; alias : 'WSRegisterCalendarDialogForm';
begin
//  RegisterWSComponent(TCalendarDialogForm, TGtkWSCalendarDialogForm);
  Result := False;
end;*)

function RegisterCalendarDialog: Boolean; alias : 'WSRegisterCalendarDialog';
begin
//  RegisterWSComponent(TCalendarDialog, TGtkWSCalendarDialog);
  Result := False;
end;

// Buttons
function RegisterCustomBitBtn: Boolean; alias : 'WSRegisterCustomBitBtn';
begin
{$ifdef gtk1}
  RegisterWSComponent(TCustomBitBtn, TGtkWSBitBtn, TGtk1PrivateButton); // register it to fallback to default
{$else}
  RegisterWSComponent(TCustomBitBtn, TGtkWSBitBtn, TGtk2PrivateButton); // register it to fallback to default
{$endif}
  Result := True;
end;

function RegisterCustomSpeedButton: Boolean; alias : 'WSRegisterCustomSpeedButton';
begin
  Result := False;
end;

// Arrow
function RegisterArrow: Boolean; alias : 'WSRegisterArrow';
begin
  RegisterWSComponent(TArrow, TGtkWSArrow);
  Result := True;
end;

// CheckLst
function RegisterCustomCheckListBox: Boolean; alias : 'WSRegisterCustomCheckListBox';
begin
  RegisterWSComponent(TCustomCheckListBox, TGtkWSCustomCheckListBox);
  Result := True;
end;

// Forms
function RegisterScrollingWinControl: Boolean; alias : 'WSRegisterScrollingWinControl';
begin
  RegisterWSComponent(TScrollingWinControl, TGtkWSScrollingWinControl, TGtkPrivateScrollingWinControl);
  Result := True;
end;

function RegisterScrollBox: Boolean; alias : 'WSRegisterScrollBox';
begin
//  RegisterWSComponent(TScrollBox, TGtkWSScrollBox);
  Result := False;
end;

function RegisterCustomFrame: Boolean; alias : 'WSRegisterCustomFrame';
begin
//  RegisterWSComponent(TCustomFrame, TGtkWSCustomFrame);
//  RegisterWSComponent(TFrame, TGtkWSFrame);
  Result := False;
end;

function RegisterCustomForm: Boolean; alias : 'WSRegisterCustomForm';
begin
  RegisterWSComponent(TCustomForm, TGtkWSCustomForm);
//  RegisterWSComponent(TForm, TGtkWSForm);
  Result := True;
end;

function RegisterHintWindow: Boolean; alias : 'WSRegisterHintWindow';
begin
  RegisterWSComponent(THintWindow, TGtkWSHintWindow);
  Result := True;
end;

function RegisterCustomGrid: Boolean; alias : 'WSRegisterCustomGrid';
begin
  RegisterWSComponent(TCustomGrid, TGtkWSCustomGrid);
  Result := True;
end;

function RegisterMenuItem: Boolean; alias : 'WSRegisterMenuItem';
begin
{$IFDEF GTK1}
  RegisterWSComponent(TMenuItem, TGtkWSMenuItem);
  Result := True;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function RegisterMenu: Boolean; alias : 'WSRegisterMenu';
begin
  RegisterWSComponent(TMenu, TGtkWSMenu);
  Result := True;
end;

function RegisterMainMenu: Boolean; alias : 'WSRegisterMainMenu';
begin
//  RegisterWSComponent(TMainMenu, TGtkWSMainMenu);
  Result := False;
end;

function RegisterPopupMenu: Boolean; alias : 'WSRegisterPopupMenu';
begin
  RegisterWSComponent(TPopupMenu, TGtkWSPopupMenu);
  Result := True;
end;

function RegisterPairSplitterSide: Boolean; alias : 'WSRegisterPairSplitterSide';
begin
  RegisterWSComponent(TPairSplitterSide, TGtkWSPairSplitterSide);
  Result := True;
end;

function RegisterCustomPairSplitter: Boolean; alias : 'WSRegisterCustomPairSplitter';
begin
  RegisterWSComponent(TCustomPairSplitter, TGtkWSCustomPairSplitter, TGtkPrivatePaned);
  Result := True;
end;

function RegisterCustomFloatSpinEdit: Boolean; alias : 'WSRegisterCustomFloatSpinEdit';
begin
  RegisterWSComponent(TCustomFloatSpinEdit, TGtkWSCustomFloatSpinEdit);
//  RegisterWSComponent(TFloatSpinEdit, TGtkWSFloatSpinEdit);
  Result := True;
end;

function RegisterCustomRubberBand: Boolean; alias : 'WSRegisterCustomRubberBand';
begin
  Result := False;
end;

end.

(* // Unused calls

//  RegisterWSComponent(TdbGrid, TGtkWSdbGrid);
//  RegisterWSComponent(TCustomDbGrid, TGtkWSCustomDbGrid);

//  RegisterWSComponent(TCustomEditButton, TGtkWSCustomEditButton);
//  RegisterWSComponent(TEditButton, TGtkWSEditButton);
//  RegisterWSComponent(TFileNameEdit, TGtkWSFileNameEdit);
//  RegisterWSComponent(TDirectoryEdit, TGtkWSDirectoryEdit);
//  RegisterWSComponent(TDateEdit, TGtkWSDateEdit);
//  RegisterWSComponent(TCalcEdit, TGtkWSCalcEdit);

//  RegisterWSComponent(TScreen, TGtkWSScreen);
//  RegisterWSComponent(TApplicationProperties, TGtkWSApplicationProperties);

//  RegisterWSComponent(TToolWindow, TGtkWSToolWindow);

//  RegisterWSComponent(TCustomActionList, TGtkWSCustomActionList);
//  RegisterWSComponent(TActionList, TGtkWSActionList);

//  RegisterWSComponent(TDBEdit, TGtkWSDBEdit);
//  RegisterWSComponent(TDBText, TGtkWSDBText);
//  RegisterWSComponent(TDBListBox, TGtkWSDBListBox);
//  RegisterWSComponent(TDBRadioGroup, TGtkWSDBRadioGroup);
//  RegisterWSComponent(TDBCheckBox, TGtkWSDBCheckBox);
//  RegisterWSComponent(TDBComboBox, TGtkWSDBComboBox);
//  RegisterWSComponent(TDBMemo, TGtkWSDBMemo);
//  RegisterWSComponent(TDBGroupBox, TGtkWSDBGroupBox);
//  RegisterWSComponent(TDBImage, TGtkWSDBImage);
//  RegisterWSComponent(TDBCalendar, TGtkWSDBCalendar);
//  RegisterWSComponent(TDBCustomNavigator, TGtkWSDBCustomNavigator);
//  RegisterWSComponent(TDBNavButton, TGtkWSDBNavButton);
//  RegisterWSComponent(TDBNavigator, TGtkWSDBNavigator);

//  RegisterWSComponent(TCustomFileListBox, TGtkWSCustomFileListBox);
//  RegisterWSComponent(TFileListBox, TGtkWSFileListBox);

//  RegisterWSComponent(TCustomMaskEdit, TGtkWSCustomMaskEdit);
//  RegisterWSComponent(TMaskEdit, TGtkWSMaskEdit);

*)
