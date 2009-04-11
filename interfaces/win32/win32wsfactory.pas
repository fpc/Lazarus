unit Win32WSFactory;

{$mode objfpc}{$H+}

interface
uses
  Classes, Controls, ComCtrls, ImgList, Calendar, StdCtrls,
  Dialogs, ExtCtrls, ExtDlgs, Buttons,
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

// dialogs
function WSRegisterCommonDialog: Boolean;      alias : 'WSRegisterCommonDialog';
begin
  RegisterWSComponent(TCommonDialog, TWin32WSCommonDialog);
  Result := True;
end;

function WSRegisterFileDialog: Boolean;        alias : 'WSRegisterFileDialog';
begin
//  RegisterWSComponent(TFileDialog, TWin32WSFileDialog);
  Result := False;
end;

function WSRegisterOpenDialog: Boolean;        alias : 'WSRegisterOpenDialog';
begin
  RegisterWSComponent(TOpenDialog, TWin32WSOpenDialog);
  Result := True;
end;

function WSRegisterSaveDialog: Boolean;        alias : 'WSRegisterSaveDialog';
begin
  RegisterWSComponent(TSaveDialog, TWin32WSSaveDialog);
  Result := True;
end;

function WSRegisterSelectDirectoryDialog: Boolean; alias : 'WSRegisterSelectDirectoryDialog';
begin
  RegisterWSComponent(TSelectDirectoryDialog, TWin32WSSelectDirectoryDialog);
  Result := True;
end;

function WSRegisterColorDialog: Boolean;       alias : 'WSRegisterColorDialog';
begin
  RegisterWSComponent(TColorDialog, TWin32WSColorDialog);
  Result := True;
end;

function WSRegisterColorButton: Boolean;       alias : 'WSRegisterColorButton';
begin
//  RegisterWSComponent(TColorButton, TWin32WSColorButton);
  Result := False;
end;

function WSRegisterFontDialog: Boolean;        alias : 'WSRegisterFontDialog';
begin
  RegisterWSComponent(TFontDialog, TWin32WSFontDialog);
  Result := True;
end;

// StdCtrls
function WSRegisterScrollBar: Boolean;    alias : 'WSRegisterScrollBar';
begin
  RegisterWSComponent(TScrollBar, TWin32WSScrollBar);
  Result := True;
end;

function WSRegisterCustomGroupBox: Boolean;    alias : 'WSRegisterCustomGroupBox';
begin
  RegisterWSComponent(TCustomGroupBox, TWin32WSCustomGroupBox);
  Result := True;
end;

function WSRegisterGroupBox: Boolean;    alias : 'WSRegisterGroupBox';
begin
//  RegisterWSComponent(TGroupBox, TWin32WSGroupBox);
  Result := False;
end;

function WSRegisterCustomComboBox: Boolean;    alias : 'WSRegisterCustomComboBox';
begin
  RegisterWSComponent(TCustomComboBox, TWin32WSCustomComboBox);
  Result := True;
end;

function WSRegisterComboBox: Boolean;    alias : 'WSRegisterComboBox';
begin
//  RegisterWSComponent(TComboBox, TWin32WSComboBox);
  Result := False;
end;

function WSRegisterCustomListBox: Boolean;    alias : 'WSRegisterCustomListBox';
begin
  RegisterWSComponent(TCustomListBox, TWin32WSCustomListBox);
  Result := True;
end;

function WSRegisterListBox: Boolean;    alias : 'WSRegisterListBox';
begin
//  RegisterWSComponent(TListBox, TWin32WSListBox);
  Result := False;
end;

function WSRegisterCustomEdit: Boolean;    alias : 'WSRegisterCustomEdit';
begin
  RegisterWSComponent(TCustomEdit, TWin32WSCustomEdit);
  Result := True;
end;

function WSRegisterCustomMemo: Boolean;    alias : 'WSRegisterCustomMemo';
begin
  RegisterWSComponent(TCustomMemo, TWin32WSCustomMemo);
  Result := True;
end;

function WSRegisterEdit: Boolean;    alias : 'WSRegisterEdit';
begin
//  RegisterWSComponent(TEdit, TWin32WSEdit);
  Result := False;
end;

function WSRegisterMemo: Boolean;    alias : 'WSRegisterMemo';
begin
//  RegisterWSComponent(TMemo, TWin32WSMemo);
  Result := False;
end;

function WSRegisterButtonControl: Boolean;    alias : 'WSRegisterButtonControl';
begin
  RegisterWSComponent(TButtonControl, TWin32WSButtonControl);
  Result := True;
end;

function WSRegisterCustomButton: Boolean;    alias : 'WSRegisterCustomButton';
begin
  RegisterWSComponent(TCustomButton, TWin32WSButton);
  Result := True;
end;

function WSRegisterCustomCheckBox: Boolean;    alias : 'WSRegisterCustomCheckBox';
begin
  RegisterWSComponent(TCustomCheckBox, TWin32WSCustomCheckBox);
  Result := True;
end;

function WSRegisterCheckBox: Boolean;    alias : 'WSRegisterCheckBox';
begin
//  RegisterWSComponent(TCheckBox, TWin32WSCheckBox);
  Result := False;
end;

function WSRegisterToggleBox: Boolean;    alias : 'WSRegisterToggleBox';
begin
  RegisterWSComponent(TToggleBox, TWin32WSToggleBox);
  Result := True;
end;

function WSRegisterRadioButton: Boolean;    alias : 'WSRegisterRadioButton';
begin
  RegisterWSComponent(TRadioButton, TWin32WSRadioButton);
  Result := True;
end;

function WSRegisterCustomStaticText: Boolean;    alias : 'WSRegisterCustomStaticText';
begin
  RegisterWSComponent(TCustomStaticText, TWin32WSCustomStaticText);
  Result := True;
end;

function WSRegisterStaticText: Boolean;    alias : 'WSRegisterStaticText';
begin
//  RegisterWSComponent(TStaticText, TWin32WSStaticText);
  Result := False;
end;

function WSRegisterLabel: Boolean;    alias : 'WSRegisterLabel';
begin
//  RegisterWSComponent(TLabel, );
  Result := False;
end;


// extctrls
function WSRegisterCustomPage: Boolean;        alias : 'WSRegisterCustomPage';
begin
  RegisterWSComponent(TCustomPage, TWin32WSCustomPage);
  Result := True;
end;

function WSRegisterCustomNotebook: Boolean;    alias : 'WSRegisterCustomNotebook';
begin
  RegisterWSComponent(TCustomNotebook, TWin32WSCustomNotebook);
  Result := True;
end;

function WSRegisterPage: Boolean;              alias : 'WSRegisterPage';
begin
//  RegisterWSComponent(TPage, TWin32WSPage);
  Result := False;
end;

function WSRegisterNotebook: Boolean;          alias : 'WSRegisterNotebook';
begin
//  RegisterWSComponent(TNotebook, TWin32WSNotebook);
  Result := False;
end;

function WSRegisterShape: Boolean;             alias : 'WSRegisterShape';
begin
//  RegisterWSComponent(TShape, TWin32WSShape);
  Result := False;
end;

function WSRegisterCustomSplitter: Boolean;    alias : 'WSRegisterCustomSplitter';
begin
//  RegisterWSComponent(TCustomSplitter, TWin32WSCustomSplitter);
  Result := False;
end;

function WSRegisterSplitter: Boolean;          alias : 'WSRegisterSplitter';
begin
//  RegisterWSComponent(TSplitter, TWin32WSSplitter);
  Result := False;
end;

function WSRegisterPaintBox: Boolean;          alias : 'WSRegisterPaintBox';
begin
//  RegisterWSComponent(TPaintBox, TWin32WSPaintBox);
  Result := False;
end;

function WSRegisterCustomImage: Boolean;       alias : 'WSRegisterCustomImage';
begin
//  RegisterWSComponent(TCustomImage, TWin32WSCustomImage);
  Result := False;
end;

function WSRegisterImage: Boolean;             alias : 'WSRegisterImage';
begin
//  RegisterWSComponent(TImage, TWin32WSImage);
  Result := False;
end;

function WSRegisterBevel: Boolean;             alias : 'WSRegisterBevel';
begin
//  RegisterWSComponent(TBevel, TWin32WSBevel);
  Result := False;
end;

function WSRegisterCustomRadioGroup: Boolean;  alias : 'WSRegisterCustomRadioGroup';
begin
//  RegisterWSComponent(TCustomRadioGroup, TWin32WSCustomRadioGroup);
  Result := False;
end;

function WSRegisterRadioGroup: Boolean;        alias : 'WSRegisterRadioGroup';
begin
//  RegisterWSComponent(TRadioGroup, TWin32WSRadioGroup);
  Result := False;
end;

function WSRegisterCustomCheckGroup: Boolean;  alias : 'WSRegisterCustomCheckGroup';
begin
//  RegisterWSComponent(TCustomCheckGroup, TWin32WSCustomCheckGroup);
  Result := False;
end;

function WSRegisterCheckGroup: Boolean;        alias : 'WSRegisterCheckGroup';
begin
//  RegisterWSComponent(TCheckGroup, TWin32WSCheckGroup);
  Result := False;
end;

function WSRegisterCustomLabeledEdit: Boolean; alias : 'WSRegisterCustomLabeledEdit';
begin
//  RegisterWSComponent(TCustomLabeledEdit, TWin32WSCustomLabeledEdit);
  Result := False;
end;

function WSRegisterLabeledEdit: Boolean;       alias : 'WSRegisterLabeledEdit';
begin
//  RegisterWSComponent(TLabeledEdit, TWin32WSLabeledEdit);
  Result := False;
end;

function WSRegisterCustomPanel: Boolean;       alias : 'WSRegisterCustomPanel';
begin
  RegisterWSComponent(TCustomPanel, TWin32WSCustomPanel);
  Result := True;
end;

function WSRegisterPanel: Boolean;             alias : 'WSRegisterPanel';
begin
//  RegisterWSComponent(TPanel, TWin32WSPanel);
  Result := False;
end;

function WSRegisterCustomTrayIcon: Boolean;    alias : 'WSRegisterCustomTrayIcon';
begin
  RegisterWSComponent(TCustomTrayIcon, TWin32WSCustomTrayIcon);
  Result := True;
end;

//ExtDlgs
function WSRegisterPreviewFileControl: Boolean; alias : 'WSRegisterPreviewFileControl';
begin
  RegisterWSComponent(TPreviewFileControl, TWin32WSPreviewFileControl);
  Result := True;
end;

function WSRegisterPreviewFileDialog: Boolean; alias : 'WSRegisterPreviewFileDialog';
begin
//  RegisterWSComponent(TPreviewFileDialog, TWin32WSPreviewFileDialog);
  Result := False;
end;

function WSRegisterOpenPictureDialog: Boolean; alias : 'WSRegisterOpenPictureDialog';
begin
  RegisterWSComponent(TOpenPictureDialog, TWin32WSOpenPictureDialog);
  Result := True;
end;

function WSRegisterSavePictureDialog: Boolean; alias : 'WSRegisterSavePictureDialog';
begin
  RegisterWSComponent(TSavePictureDialog, TWin32WSSaveDialog);
  Result := True;
end;

function WSRegisterCalculatorDialog: Boolean;  alias : 'WSRegisterCalculatorDialog';
begin
//  RegisterWSComponent(TCalculatorDialog, TWin32WSCalculatorDialog);
  Result := False;
end;

function WSRegisterCalculatorForm: Boolean;    alias : 'WSRegisterCalculatorForm';
begin
//  RegisterWSComponent(TCalculatorForm, TWin32WSCalculatorForm);
  Result := False;
end;

(*function WSRegisterCalendarDialogForm: Boolean; alias : 'WSRegisterCalendarDialogForm';
begin
//  RegisterWSComponent(TCalendarDialogForm, TWin32WSCalendarDialogForm);
  Result := False;
end;*)

function WSRegisterCalendarDialog: Boolean;    alias : 'WSRegisterCalendarDialog';
begin
//  RegisterWSComponent(TCalendarDialog, TWin32WSCalendarDialog);
  Result := False;
end;

// Buttons
function WSRegisterCustomBitBtn: Boolean;      alias : 'WSRegisterCustomBitBtn';
begin
  RegisterWSComponent(TCustomBitBtn, TWin32WSBitBtn);
  Result := True;
end;

function WSRegisterCustomSpeedButton: Boolean; alias : 'WSRegisterCustomSpeedButton';
begin
//  RegisterWSComponent(TCustomSpeedButton, TWin32WSSpeedButton);
  Result := False;
end;

end.
