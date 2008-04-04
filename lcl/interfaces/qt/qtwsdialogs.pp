{ $Id$}
{
 *****************************************************************************
 *                              QtWSDialogs.pp                               * 
 *                              --------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit QtWSDialogs;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Libs
  qt4,
  qtobjects, qtwidgets, qtproc,
  // RTL + LCL
  SysUtils, Classes, LCLType, Dialogs, Controls, Forms, Graphics,
  // Widgetset
  WSDialogs, WSLCLClasses;

type

  { TQtWSCommonDialog }

  TQtWSCommonDialog = class(TWSCommonDialog)
  private
  protected
    class function GetDialogParent(const ACommonDialog: TCommonDialog): QWidgetH;
  public
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TQtWSFileDialog }

  TQtWSFileDialog = class(TWSFileDialog)
  private
  protected
    class function GetQtFilterString(const AFileDialog: TFileDialog): WideString;
    class procedure UpdateProperties(const AFileDialog: TFileDialog; QtFileDialog: TQtFileDialog);
  public
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TQtWSOpenDialog }

  TQtWSOpenDialog = class(TWSOpenDialog)
  private
  protected
  public
  end;

  { TQtWSSaveDialog }

  TQtWSSaveDialog = class(TWSSaveDialog)
  private
  protected
  public
  end;

  { TQtWSSelectDirectoryDialog }

  TQtWSSelectDirectoryDialog = class(TWSSelectDirectoryDialog)
  private
  protected
    class procedure UpdateProperties(const AFileDialog: TSelectDirectoryDialog; QtFileDialog: TQtFileDialog);
  public
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TQtWSColorDialog }

  TQtWSColorDialog = class(TWSColorDialog)
  private
  protected
  public
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TQtWSColorButton }

  TQtWSColorButton = class(TWSColorButton)
  private
  protected
  public
  end;

  { TQtWSFontDialog }

  TQtWSFontDialog = class(TWSFontDialog)
  private
  protected
  public
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;


implementation

const
  QtDialogCodeToModalResultMap: array[QDialogDialogCode] of TModalResult =
  (
{QDialogRejected} mrCancel,
{QDialogAccepted} mrOk
  );
  
{ TQtWSCommonDialog }

class function TQtWSCommonDialog.GetDialogParent(const ACommonDialog: TCommonDialog): QWidgetH;
begin
  if ACommonDialog.Owner is TWinControl then
    Result := TQtWidget(TWinControl(ACommonDialog.Owner).Handle).Widget
  else
  if Assigned(Application.MainForm) then
    Result := TQtWidget(Application.MainForm.Handle).Widget
  else
    Result := nil;
end;

{------------------------------------------------------------------------------
  Function: TQtWSCommonDialog.CreateHandle
  Params:  None
  Returns: Nothing

  Dummy handle creator. On Qt we don´t need a Handle for common dialogs
 ------------------------------------------------------------------------------}
class function TQtWSCommonDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
begin
  Result := THandle(TQtDialog.Create(ACommonDialog, GetDialogParent(ACommonDialog)));
  TQtDialog(Result).AttachEvents;
end;

{------------------------------------------------------------------------------
  Function: TQtWSCommonDialog.DestroyHandle
  Params:  None
  Returns: Nothing

  Dummy handle destructor. On Qt we don´t need a Handle for common dialogs
 ------------------------------------------------------------------------------}
class procedure TQtWSCommonDialog.DestroyHandle(const ACommonDialog: TCommonDialog);
begin
  if ACommonDialog.HandleAllocated then
    TQtDialog(ACommonDialog.Handle).Release;
end;

class procedure TQtWSCommonDialog.ShowModal(const ACommonDialog: TCommonDialog);
begin
  TQtDialog(ACommonDialog.Handle).exec;
end;

{ TQtWSFileDialog }

class function TQtWSFileDialog.GetQtFilterString(const AFileDialog: TFileDialog): WideString;

  function GetExtensionString(ASource: String; AStart, ALength: Integer): String; inline;
  begin
    // replace *.ext1, *.ext2 by *.ext1 *.ext2
    Result := '(' + StringReplace(Copy(ASource, AStart, ALength), ';', ' ', [rfReplaceAll]) + ')';
  end;
  
var
  TmpFilter, strExtensions: string;
  ParserState, Position, i: Integer;
begin
    {------------------------------------------------------------------------------
    This is a parser that converts LCL filter strings to Qt filter strings

    The parses states are:

    0 - Initial state, is reading a string to be displayed on the filter
    1 - Is reading the extensions to be filtered
    2 - Reached the end of extensions text, now it will write

    A LCL filter string looks like this:

    Text files (*.txt *.pas)|*.txt *.pas|Binaries (*.exe)|*.exe

    And a Qt filter string looks like this

    Text files (*.txt *.pas)
    Binaries (*.exe)

    The following LCL filter simply cannot be represented under Qt, because Qt
   always appends a string with the extensions on the combo box

    Text files|*.txt *.pas|Binaries|*.exe

    To solve this this algorithm will try to find (*.txt) or similar on the display text
   and will remove it. This algorithm is far from perfect and may cause trouble on some
   special cases, but should work 99% of the time.
   ------------------------------------------------------------------------------}

  ParserState := 0;
  Position := 1;
  TmpFilter := '';
  
  for i := 1 to Length(AFileDialog.Filter) do
  begin
    if Copy(AFileDialog.Filter, i, 1) = '|' then
    begin
      ParserState := ParserState + 1;

      if ParserState = 1 then
        TmpFilter := TmpFilter + Copy(AFileDialog.Filter, Position, i - Position)
      else
      if ParserState = 2 then
      begin
        strExtensions := GetExtensionString(AFileDialog.Filter, Position, i - Position);

        if Pos(strExtensions, TmpFilter) = 0 then
          TmpFilter := TmpFilter + ' ' + strExtensions;

        TmpFilter := TmpFilter + ';;';

        ParserState := 0;
      end;

      if i <> Length(AFileDialog.Filter) then
        Position := i + 1;
    end;
  end;

  strExtensions := GetExtensionString(AFileDialog.Filter, Position, i + 1 - Position);

  if Pos(strExtensions, TmpFilter) = 0 then
    TmpFilter := TmpFilter + ' ' + strExtensions;
    
  if (AFileDialog is TSaveDialog) and (trim(TmpFilter)='()') then
    Result := ''
  else
    Result := GetUtf8String(TmpFilter);
end;

class procedure TQtWSFileDialog.UpdateProperties(
  const AFileDialog: TFileDialog; QtFileDialog: TQtFileDialog);
var
  ATitle: WideString;
begin
  ATitle := GetUtf8String(AFileDialog.Title);
  QtFileDialog.setWindowTitle(@ATitle);
  QtFileDialog.setDirectory(GetUtf8String(AFileDialog.InitialDir));
  QtFileDialog.setFilter(GetQtFilterString(AFileDialog));
  QtFileDialog.setConfirmOverwrite(ofOverwritePrompt in TOpenDialog(AFileDialog).Options);
  QtFileDialog.setReadOnly(ofReadOnly in TOpenDialog(AFileDialog).Options);
  QtFileDialog.setSizeGripEnabled(ofEnableSizing in TOpenDialog(AFileDialog).Options);

  if ofViewDetail in TOpenDialog(AFileDialog).Options then
    QtFileDialog.setViewMode(QFileDialogDetail)
  else
    QtFileDialog.setViewMode(QFileDialogList);
    
  if ofAllowMultiSelect in TOpenDialog(AFileDialog).Options then
    QtFileDialog.setFileMode(QFileDialogExistingFiles)
  else
  if ofFileMustExist in TOpenDialog(AFileDialog).Options then
    QtFileDialog.setFileMode(QFileDialogExistingFile)
  else
    QtFileDialog.setFileMode(QFileDialogAnyFile);
end;

class function TQtWSFileDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
var
  FileDialog: TQtFileDialog;
begin
  FileDialog := TQtFileDialog.Create(ACommonDialog, TQtWSCommonDialog.GetDialogParent(ACommonDialog));
  FileDialog.AttachEvents;
  
  Result := THandle(FileDialog);
end;

{------------------------------------------------------------------------------
  Function: TQtWSFileDialog.ShowModal
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSFileDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  {selectedFilter, }ReturnText,
  saveFileName, saveTitle, saveFilter: WideString;
  FileDialog: TFileDialog;
  ReturnList: QStringListH;
  i: integer;
  QtFileDialog: TQtFileDialog;
begin
  {------------------------------------------------------------------------------
    Initialization of variables
   ------------------------------------------------------------------------------}
  ReturnText := '';
  //selectedFilter := '';
  saveFileName := '';
  saveTitle := '';

  FileDialog := TFileDialog(ACommonDialog);
  QtFileDialog := TQtFileDialog(FileDialog.Handle);
  
  UpdateProperties(FileDialog, QtFileDialog);

  {------------------------------------------------------------------------------
    Code to call the dialog
   ------------------------------------------------------------------------------}
  if FileDialog is TSaveDialog then
    QtFileDialog.setAcceptMode(QFileDialogAcceptSave)
  else
  if FileDialog is TOpenDialog then
    QtFileDialog.setAcceptMode(QFileDialogAcceptOpen)
  else
  if ACommonDialog is TSelectDirectoryDialog then
    QtFileDialog.setFileMode(QFileDialogDirectoryOnly);

  if ACommonDialog is TSaveDialog then
  begin
    saveFilter := GetQtFilterString(TSaveDialog(ACommonDialog));
    saveFileName := GetUtf8String(FileDialog.InitialDir+FileDialog.Filename);
    saveTitle := GetUTF8String(FileDialog.Title);
    QFileDialog_getSaveFileName(@ReturnText, QWidget_parentWidget(QtFileDialog.Widget), @SaveTitle, @saveFileName, @saveFilter, {selectedFilter} nil, 0);
    if ReturnText <> '' then
    begin
      FileDialog.FileName := UTF8Encode(ReturnText);
      FileDialog.UserChoice := mrOK;
    end else
      FileDialog.UserChoice := mrCancel;
  end else
  begin
    FileDialog.UserChoice := QtDialogCodeToModalResultMap[QDialogDialogCode(QtFileDialog.exec)];
    ReturnList := QStringList_create;
    try
      QtFileDialog.selectedFiles(ReturnList);
      for i := 0 to QStringList_size(ReturnList) - 1 do
      begin
        QStringList_at(ReturnList, @ReturnText, i);
        FileDialog.Files.Add(UTF8Encode(ReturnText));
        if i = 0 then
          FileDialog.FileName := UTF8Encode(ReturnText);
      end;
      ReturnText := FileDialog.Files.Text;
    finally
      QStringList_destroy(ReturnList);
    end;
  end;
end;

{ TQtWSSelectDirectoryDialog }

class procedure TQtWSSelectDirectoryDialog.UpdateProperties(
  const AFileDialog: TSelectDirectoryDialog; QtFileDialog: TQtFileDialog);
var
  ATitle: WideString;
begin
  ATitle := GetUtf8String(AFileDialog.Title);
  QtFileDialog.setWindowTitle(@ATitle);
  QtFileDialog.setDirectory(GetUtf8String(AFileDialog.InitialDir));
  QtFileDialog.setSizeGripEnabled(ofEnableSizing in TSelectDirectoryDialog(AFileDialog).Options);

  if ofViewDetail in TSelectDirectoryDialog(AFileDialog).Options then
    QtFileDialog.setViewMode(QFileDialogDetail)
  else
    QtFileDialog.setViewMode(QFileDialogList);

  QtFileDialog.setFileMode(QFileDialogDirectoryOnly);

end;

class function TQtWSSelectDirectoryDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
var
  FileDialog: TQtFileDialog;
begin
  FileDialog := TQtFileDialog.Create(ACommonDialog, TQtWSCommonDialog.GetDialogParent(ACommonDialog));
  FileDialog.AttachEvents;

  Result := THandle(FileDialog);
end;

{------------------------------------------------------------------------------
  Function: TQtWSSelectDirectoryDialog.ShowModal
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSSelectDirectoryDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  ReturnText, saveFileName, saveTitle: WideString;
  FileDialog: TSelectDirectoryDialog;
  QtFileDialog: TQtFileDialog;
begin
  {------------------------------------------------------------------------------
    Initialization of variables
   ------------------------------------------------------------------------------}
  ReturnText := '';
  saveFileName := '';
  saveTitle := '';

  FileDialog := TSelectDirectoryDialog(ACommonDialog);
  QtFileDialog := TQtFileDialog(FileDialog.Handle);

  UpdateProperties(FileDialog, QtFileDialog);

  {------------------------------------------------------------------------------
    Code to call the dialog
   ------------------------------------------------------------------------------}

  saveTitle := GetUTF8String(FileDialog.Title);
  saveFileName := GetUtf8String(FileDialog.InitialDir);
  QFileDialog_getExistingDirectory(@ReturnText, QWidget_parentWidget(QtFileDialog.Widget), @SaveTitle, @saveFileName);
  if ReturnText <> '' then
  begin
    FileDialog.FileName := UTF8Encode(ReturnText);
    FileDialog.UserChoice := mrOK;
  end else
    FileDialog.UserChoice := mrCancel;
end;

{ TQtWSColorDialog }

class function TQtWSColorDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
begin
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: TQtWSColorDialog.ShowModal
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSColorDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  AColor: TColor;
  AQColor: TQColor;
  AQtColor: QColorH;
  ARgb: QRgb;
  ReturnBool: Boolean;
begin
  AColor := ColorToRgb(TColorDialog(ACommonDialog).Color);
  AQColor.Alpha := $FFFF;
  AQColor.ColorSpec := 1;
  AQColor.Pad := 0;
  ColorRefToTQColor(AColor, AQColor);
  AQtColor := QColor_create(PQColor(@AQColor));
  ARgb := QColor_rgba(AQtColor);

  ARgb := QColorDialog_getRgba(ARgb, @ReturnBool,
    TQtWSCommonDialog.GetDialogParent(ACommonDialog));

  QColor_fromRgba(PQColor(AQtColor), ARgb);
  try
    QColor_toRgb(AQtColor, @AQColor);
    TQColorToColorRef(AQColor, AColor);
    TColorDialog(ACommonDialog).Color := AColor;
  finally
    QColor_destroy(AQtColor);
  end;

  if ReturnBool then
    ACommonDialog.UserChoice := mrOk
  else
    ACommonDialog.UserChoice := mrCancel;
end;

{ TQtWSFontDialog }

class function TQtWSFontDialog.CreateHandle(const ACommonDialog: TCommonDialog
  ): THandle;
begin
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: TQtWSFontDialog.ShowModal
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSFontDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  ReturnFont, CurrentFont: QFontH;
  ReturnBool: Boolean;
  Str: WideString;
begin
  {------------------------------------------------------------------------------
    Code to call the dialog
   ------------------------------------------------------------------------------}
  CurrentFont := TQtFont(TFontDialog(ACommonDialog).Font.Reference.Handle).Widget;

  ReturnFont := QFont_create;
  try
    QFontDialog_getFont(ReturnFont, @ReturnBool, CurrentFont,
      TQtWSCommonDialog.GetDialogParent(ACommonDialog));
   
    QFont_family(ReturnFont, @Str);
    TFontDialog(ACommonDialog).Font.Name := UTF8Encode(Str);
   
    if QFont_pixelSize(ReturnFont) = -1 then
      TFontDialog(ACommonDialog).Font.Size := QFont_pointSize(ReturnFont)
    else
      TFontDialog(ACommonDialog).Font.Height := QFont_pixelSize(ReturnFont);
      
    TFontDialog(ACommonDialog).Font.Style := [];
   
   if QFont_bold(ReturnFont) then
     TFontDialog(ACommonDialog).Font.Style := TFontDialog(ACommonDialog).Font.Style + [fsBold];
   
   if QFont_italic(ReturnFont) then
     TFontDialog(ACommonDialog).Font.Style := TFontDialog(ACommonDialog).Font.Style + [fsItalic];
   
   if QFont_strikeOut(ReturnFont) then
     TFontDialog(ACommonDialog).Font.Style := TFontDialog(ACommonDialog).Font.Style + [fsStrikeOut];
   
   if QFont_underline(ReturnFont) then
     TFontDialog(ACommonDialog).Font.Style := TFontDialog(ACommonDialog).Font.Style + [fsUnderline];
   
   if QFont_fixedPitch(ReturnFont) then
     TFontDialog(ACommonDialog).Font.Pitch := fpFixed
   else
     TFontDialog(ACommonDialog).Font.Pitch := fpDefault;
   
  finally
    QFont_destroy(ReturnFont);
  end;

  if ReturnBool then
    ACommonDialog.UserChoice := mrOk
  else
    ACommonDialog.UserChoice := mrCancel;
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCommonDialog, TQtWSCommonDialog);
  RegisterWSComponent(TFileDialog, TQtWSFileDialog);
//  RegisterWSComponent(TOpenDialog, TQtWSOpenDialog);
//  RegisterWSComponent(TSaveDialog, TQtWSSaveDialog);
  RegisterWSComponent(TSelectDirectoryDialog, TQtWSSelectDirectoryDialog);
  RegisterWSComponent(TColorDialog, TQtWSColorDialog);
//  RegisterWSComponent(TColorButton, TQtWSColorButton);
  RegisterWSComponent(TFontDialog, TQtWSFontDialog);
////////////////////////////////////////////////////
end.
