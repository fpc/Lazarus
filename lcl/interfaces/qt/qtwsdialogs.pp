{ $Id$}
{
 *****************************************************************************
 *                              QtWSDialogs.pp                               * 
 *                              --------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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
  SysUtils, Classes, LCLType, LazUTF8, LazFileUtils, Dialogs, Controls, Forms, Graphics,
  // Widgetset
  WSDialogs, WSLCLClasses;

type

  { TQtWSCommonDialog }

  TQtWSCommonDialog = class(TWSCommonDialog)
  private
  protected
    class function GetDialogParent(const ACommonDialog: TCommonDialog): QWidgetH;
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TQtWSFileDialog }

  TQtWSFileDialog = class(TWSFileDialog)
  private
  protected
    class function GetQtFilterString(const AFileDialog: TFileDialog;
      var ASelectedFilter: WideString): WideString;
    class procedure UpdateProperties(const AFileDialog: TFileDialog; QtFileDialog: TQtFileDialog);
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TQtWSOpenDialog }

  TQtWSOpenDialog = class(TWSOpenDialog)
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class function QueryWSEventCapabilities(const ACommonDialog: TCommonDialog): TCDWSEventCapabilities; override;
  end;

  { TQtWSSaveDialog }

  TQtWSSaveDialog = class(TWSSaveDialog)
  published
    class function QueryWSEventCapabilities(const ACommonDialog: TCommonDialog): TCDWSEventCapabilities; override;
  end;

  { TQtWSSelectDirectoryDialog }

  TQtWSSelectDirectoryDialog = class(TWSSelectDirectoryDialog)
  protected
    class procedure UpdateProperties(const AFileDialog: TSelectDirectoryDialog; QtFileDialog: TQtFileDialog);
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
    class function QueryWSEventCapabilities(const ACommonDialog: TCommonDialog): TCDWSEventCapabilities; override;
  end;

  { TQtWSColorDialog }

  TQtWSColorDialog = class(TWSColorDialog)
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
    class function QueryWSEventCapabilities(const ACommonDialog: TCommonDialog): TCDWSEventCapabilities; override;
  end;

  { TQtWSColorButton }

  TQtWSColorButton = class(TWSColorButton)
  published
  end;

  { TQtWSFontDialog }

  TQtWSFontDialog = class(TWSFontDialog)
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
    class function QueryWSEventCapabilities(const ACommonDialog: TCommonDialog): TCDWSEventCapabilities; override;
  end;


implementation
uses ExtDlgs, qtint;
{$ifndef QT_NATIVE_DIALOGS}
const
  QtDialogCodeToModalResultMap: array[QDialogDialogCode] of TModalResult =
  (
{QDialogRejected} mrCancel,
{QDialogAccepted} mrOk
  );


{$endif}

{ TQtWSSaveDialog }

class function TQtWSSaveDialog.QueryWSEventCapabilities(
  const ACommonDialog: TCommonDialog): TCDWSEventCapabilities;
begin
  Result := [cdecWSNoCanCloseSupport];
end;

{ TQtWSCommonDialog }

class function TQtWSCommonDialog.GetDialogParent(const ACommonDialog: TCommonDialog): QWidgetH;
begin
  if ACommonDialog.Owner is TWinControl then
    Result := TQtWidget(TWinControl(ACommonDialog.Owner).Handle).Widget
  else
  if (QtWidgetSet.GetActiveWindow <> 0) then
    Result := TQtWidget(QtWidgetSet.GetActiveWindow).Widget
  else
  if Assigned(Application.MainForm) and Application.MainForm.Visible then
    Result := TQtWidget(Application.MainForm.Handle).Widget
  else
    Result := nil;
  {$IFDEF MSWINDOWS}
  if Assigned(Result) then
  begin
    if (QWidget_windowFlags(Result) and QtTool <> 0) or
      (QWidget_windowFlags(Result) and QtDrawer <> 0) or
      (QWidget_windowFlags(Result) and QtSheet <> 0) or
      (QWidget_windowFlags(Result) and QtPopup <> 0) then
      Result := QApplication_desktop;
  end;
  {$ENDIF}
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

class function TQtWSFileDialog.GetQtFilterString(const AFileDialog: TFileDialog;
  var ASelectedFilter: WideString): WideString;

  function GetExtensionString(ASource: String; AStart, ALength: Integer): String; inline;
  begin
    // replace *.ext1, *.ext2 by *.ext1 *.ext2
    Result := '(' + StringReplace(Copy(ASource, AStart, ALength), ';', ' ', [rfReplaceAll]) + ')';
  end;
  
var
  TmpFilter, strExtensions, DialogFilter: string;
  ParserState, Position, i: Integer;
  List: TStrings;
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
  TmpFilter := AFileDialog.Filter;
  DialogFilter := AFileDialog.Filter;
  ASelectedFilter := '';

  {we must remove all brackets since qt-45 doesn't like brackets
   outside filters,so our eg. Pascal source (*.pas;*.pp) | *.pas;*.pp
   becomes invalid after filters processing.}
  TmpFilter := StringReplace(TmpFilter,'(','',[rfReplaceAll]);
  TmpFilter := StringReplace(TmpFilter,')','',[rfReplaceAll]);
  DialogFilter := TmpFilter;

  TmpFilter := '';

  List := TStringList.Create;
  try
    for i := 1 to Length(DialogFilter) do
    begin
      if Copy(DialogFilter, i, 1) = '|' then
      begin
        ParserState := ParserState + 1;

        if ParserState = 1 then
        begin
          List.Add(Copy(DialogFilter, Position, i - Position));
          TmpFilter := TmpFilter + Copy(DialogFilter, Position, i - Position);
        end else
        if ParserState = 2 then
        begin
          strExtensions := GetExtensionString(DialogFilter, Position, i - Position);

          if Pos(strExtensions, TmpFilter) = 0 then
          begin
            if List.Count > 0 then
              List.Strings[List.Count - 1] := List.Strings[List.Count - 1] +' '+ strExtensions;
            TmpFilter := TmpFilter + ' ' + strExtensions;
          end;

          TmpFilter := TmpFilter + ';;';

          ParserState := 0;
        end;

        if i <> Length(DialogFilter) then
          Position := i + 1;
      end;
    end;

    strExtensions := GetExtensionString(DialogFilter, Position, i + 1 - Position);

    if Pos(strExtensions, TmpFilter) = 0 then
    begin
      if List.Count > 0 then
        List.Strings[List.Count - 1] := List.Strings[List.Count - 1] +' '+ strExtensions;
      TmpFilter := TmpFilter + ' ' + strExtensions;
    end;

    // Remember that AFileDialog.FilterIndex is a 1-based index and that
    // List has a zero-based index
    if (AFileDialog.FilterIndex > 0) and (List.Count >= AFileDialog.FilterIndex) then
      ASelectedFilter := List{%H-}.Strings[AFileDialog.FilterIndex - 1]
    else
    if (List.Count > 0) then
      ASelectedFilter := List{%H-}.Strings[0];

  finally
    List.Free;
  end;
    
  if (AFileDialog is TSaveDialog) and (trim(TmpFilter)='()') then
    Result := ''
  else
    Result := {%H-}TmpFilter;
end;

class procedure TQtWSFileDialog.UpdateProperties(
  const AFileDialog: TFileDialog; QtFileDialog: TQtFileDialog);
var
  ATitle: WideString;
  {$ifndef QT_NATIVE_DIALOGS}
  AInitDir: WideString;
  {$ENDIF}
  S: String;
begin
  ATitle := AFileDialog{%H-}.Title;
  QtFileDialog.setWindowTitle(@ATitle);

  {$ifndef QT_NATIVE_DIALOGS}
  S := AFileDialog.InitialDir;
  if UTF8Pos('$HOME', S) > 0 then
  begin
    {$IFDEF MSWINDOWS}
    AInitDir := {%H-}GetEnvironmentVariableUTF8('HOMEDRIVE') +
                {%H-}GetEnvironmentVariableUTF8('HOMEPATH');
    {$ELSE}
    AInitDir := {%H-}GetEnvironmentVariableUTF8('HOME');
    {$ENDIF}
    S := StringReplace(S,'$HOME', UTF8Encode(AInitDir),[rfReplaceAll]);
  end else
  if (S = '') then
    S := GetCurrentDirUTF8;
  if not DirectoryExistsUTF8(S) then
    S := GetCurrentDirUTF8;
  QtFileDialog.setDirectory(S{%H-});
  {$else}
  S := AFileDialog.InitialDir;
  if S = '' then
    S := GetCurrentDirUTF8;
  if DirectoryExistsUTF8(S) then
    QtFileDialog.setDirectory(S{%H-});
  {$endif}

  QtFileDialog.setHistory(AFileDialog.HistoryList);
  QtFileDialog.setFilter(GetQtFilterString(AFileDialog, ATitle));
  QtFileDialog.setSelectedFilter(ATitle);
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

  if (AFileDialog.FileName <> '') and
    not DirectoryExistsUTF8(AFileDialog.FileName) then
  begin
    ATitle := AFileDialog{%H-}.FileName;
    if (AFileDialog is TSaveDialog) or FileExistsUTF8(AFileDialog.FileName) then
      QFileDialog_selectFile(QFileDialogH(QtFileDialog.Widget), @ATitle);
    {$ifndef QT_NATIVE_DIALOGS}
    if (AFileDialog is TOpenPictureDialog) then
      TQtFilePreviewDialog(QtFileDialog).CurrentChangedEvent(@ATitle);
    {$ENDIF}
  end;
  {$ifndef QT_NATIVE_DIALOGS}
  // set kbd shortcuts in case when we are not native dialog.
  QtFileDialog.setShortcuts(AFileDialog is TOpenDialog);
  {$endif}
end;

class function TQtWSFileDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
var
  FileDialog: TQtFileDialog;
begin
  FileDialog := TQtFileDialog.Create(ACommonDialog, TQtWSCommonDialog.GetDialogParent(ACommonDialog));
  {$ifdef darwin}
  QWidget_setWindowFlags(FileDialog.Widget, QtDialog or QtWindowSystemMenuHint or QtCustomizeWindowHint);
  {$endif}

  {$note WE MUST USE NON NATIVE DIALOGS HERE, OTHERWISE NO SIGNALS #16532.}
  QFileDialog_setOption(QFileDialogH(FileDialog.Widget),
    QFileDialogDontUseNativeDialog, True);

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
  ReturnText: WideString;
  s: string;
  FileDialog: TFileDialog;
  ReturnList: QStringListH;
  i: integer;
  QtFileDialog: TQtFileDialog;
  {$ifdef QT_NATIVE_DIALOGS}
  selectedFilter, saveFileName, saveFilter, saveTitle: WideString;
  sDir: String;
  Flags: Cardinal;
  {$endif}
  ActiveWin: HWND;
begin
  {------------------------------------------------------------------------------
    Initialization of variables
   ------------------------------------------------------------------------------}
  ReturnText := '';

  FileDialog := TFileDialog(ACommonDialog);
  QtFileDialog := TQtFileDialog(FileDialog.Handle);

  UpdateProperties(FileDialog, QtFileDialog);

  {$ifdef QT_NATIVE_DIALOGS}
  sDir := '';
  if UTF8Pos('$HOME', FileDialog.InitialDir) > 0 then
  begin
    sDir := FileDialog.InitialDir;
    {$IFDEF MSWINDOWS}
    saveFileName := {%H-}GetEnvironmentVariableUTF8('HOMEDRIVE') +
                    {%H-}GetEnvironmentVariableUTF8('HOMEPATH');
    {$ELSE}
    saveFileName := {%H-}GetEnvironmentVariableUTF8('HOME');
    {$ENDIF}
    sDir := StringReplace(sDir,'$HOME', UTF8Encode(saveFileName),[rfReplaceAll]);
    saveFileName := sDir + PathDelim {%H-}+ ExtractFileName(FileDialog.Filename);
  end;
  {$endif}

  {------------------------------------------------------------------------------
    Code to call the dialog
   ------------------------------------------------------------------------------}
  if (FileDialog is TSaveDialog) or (FileDialog is TSavePictureDialog) then
    QtFileDialog.setAcceptMode(QFileDialogAcceptSave)
  else
  if (FileDialog is TOpenDialog) or (FileDialog is TOpenPictureDialog) then
    QtFileDialog.setAcceptMode(QFileDialogAcceptOpen)
  else
  if ACommonDialog is TSelectDirectoryDialog then
    QtFileDialog.setFileMode(QFileDialogDirectoryOnly);

  ActiveWin := QtWidgetSet.GetActiveWindow;
  if ACommonDialog is TSaveDialog then
  begin
    {$ifdef QT_NATIVE_DIALOGS}
    selectedFilter:='';
    saveFilter := GetQtFilterString(TSaveDialog(ACommonDialog), selectedFilter);
    if sDir = '' then
      sDir := FileDialog.InitialDir;
    if (SDir <> '') and (SDir[length(SDir)] <> PathDelim) then
      SDir := SDir + PathDelim;
    if (FileDialog.FileName <> '') and
      (ExtractFileName(FileDialog.FileName) <> FileDialog.FileName) then
        saveFileName := FileDialog{%H-}.Filename
    else
      saveFileName := SDir{%H-}+FileDialog.Filename;
    saveTitle := FileDialog{%H-}.Title;

    Flags := 0;
    if not (ofOverwritePrompt in TSaveDialog(FileDialog).Options) then
      Flags := Flags or QFileDialogDontConfirmOverwrite;
    {$IFDEF HASX11}
    Clipboard.BeginX11SelectionLock;
    try
    {$ENDIF}
      QFileDialog_getSaveFileName(@ReturnText,
        QWidget_parentWidget(QtFileDialog.Widget), @SaveTitle, @saveFileName,
        @saveFilter, @selectedFilter, Flags);
    {$IFDEF HASX11}
    finally
      Clipboard.EndX11SelectionLock;
    end;
    {$ENDIF}

    if ReturnText <> '' then
    begin
      {$ifdef MSWINDOWS}
      s := ReturnText;
      s := StringReplace(s, '/','\', [rfReplaceAll]);
      FileDialog.FileName := s;
      {$else}
      FileDialog.FileName := ReturnText{%H-};
      {$endif}
      FileDialog.UserChoice := mrOK;
    end else
      FileDialog.UserChoice := mrCancel;
    {$else}

    QFileDialog_setOption(QFileDialogH(QtFileDialog.Widget),
      QFileDialogDontConfirmOverwrite,
      not (ofOverwritePrompt in TSaveDialog(FileDialog).Options));

    FileDialog.UserChoice := QtDialogCodeToModalResultMap[QDialogDialogCode(QtFileDialog.exec)];
    ReturnList := QStringList_create;
    try
      QtFileDialog.selectedFiles(ReturnList);
      FileDialog.Files.Clear;
      for i := 0 to QStringList_size(ReturnList) - 1 do
      begin
        QStringList_at(ReturnList, @ReturnText, i);
        s := {%H-}ReturnText;
        {$ifdef MSWINDOWS}
        s := StringReplace(s, '/','\', [rfReplaceAll]);
        {$endif}
        FileDialog.Files.Add(s);
        if i = 0 then
          FileDialog.FileName := s;
      end;
      ReturnText := FileDialog{%H-}.Files.Text;
    finally
      QStringList_destroy(ReturnList);
    end;
    {$endif}
  end else
  begin
    {$ifdef QT_NATIVE_DIALOGS}
    saveFilter := GetQtFilterString(TOpenDialog(ACommonDialog), selectedFilter);
    if sDir = '' then
      sDir := FileDialog.InitialDir;
    if (SDir <> '') and (SDir[length(SDir)] <> PathDelim) then
      SDir := SDir + PathDelim;
    if (FileDialog.FileName <> '') and
      (ExtractFileName(FileDialog.FileName) <> FileDialog.FileName) then
        saveFileName := FileDialog{%H-}.Filename
    else
      saveFileName := SDir{%H-}+FileDialog.Filename;
    saveTitle := FileDialog{%H-}.Title;

    Flags := 0;
    if (ofReadOnly in TOpenDialog(FileDialog).Options) then
      Flags := Flags or QFileDialogReadOnly;

    if (ofAllowMultiSelect in TOpenDialog(FileDialog).Options) then
    begin
      ReturnText := '';
      ReturnList := QStringList_create;
      {$IFDEF HASX11}
      Clipboard.BeginX11SelectionLock;
      {$ENDIF}
      try
        QFileDialog_getOpenFileNames(ReturnList,
          QWidget_parentWidget(QtFileDialog.Widget), @SaveTitle, @saveFileName,
          @saveFilter, @selectedFilter, Flags);
        FileDialog.Files.Clear;
        for i := 0 to QStringList_size(ReturnList) - 1 do
        begin
          QStringList_at(ReturnList, @ReturnText, i);
          {$ifdef MSWINDOWS}
          s := ReturnText;
          s := StringReplace(s, '/','\', [rfReplaceAll]);
          FileDialog.Files.Add(s);
          if i = 0 then
            FileDialog.FileName := s;
          {$else}
          FileDialog.Files.Add(ReturnText{%H-});
          if i = 0 then
            FileDialog.FileName := ReturnText{%H-};
          {$endif}
        end;
        {assign to ReturnText first filename}
        if QStringList_size(ReturnList) > 0 then
          QStringList_at(ReturnList, @ReturnText, 0);

      finally
        QStringList_destroy(ReturnList);
        {$IFDEF HASX11}
        Clipboard.EndX11SelectionLock;
        {$ENDIF}
      end;
    end else
    begin
      {$IFDEF HASX11}
      Clipboard.BeginX11SelectionLock;
      try
      {$ENDIF}
        QFileDialog_getOpenFileName(@ReturnText,
          QWidget_parentWidget(QtFileDialog.Widget), @SaveTitle, @saveFileName,
          @saveFilter, @selectedFilter, Flags);
      {$IFDEF HASX11}
      finally
        Clipboard.EndX11SelectionLock;
      end;
      {$ENDIF}
    end;

    if ReturnText <> '' then
    begin
      s := {%H-}ReturnText;
      {$ifdef MSWINDOWS}
      s := StringReplace(s, '/','\', [rfReplaceAll]);
      {$endif}
      FileDialog.FileName := s;
      FileDialog.UserChoice := mrOK;
    end else
      FileDialog.UserChoice := mrCancel;
    {$else}
    FileDialog.UserChoice := QtDialogCodeToModalResultMap[QDialogDialogCode(QtFileDialog.exec)];
    ReturnList := QStringList_create;
    try
      QtFileDialog.selectedFiles(ReturnList);
      FileDialog.Files.Clear;
      for i := 0 to QStringList_size(ReturnList) - 1 do
      begin
        QStringList_at(ReturnList, @ReturnText, i);
        {$ifdef MSWINDOWS}
        s := ReturnText;
        s := StringReplace(s, '/','\', [rfReplaceAll]);
        FileDialog.Files.Add(s);
        if i = 0 then
          FileDialog.FileName := s;
        {$else}
        FileDialog.Files.Add(ReturnText{%H-});
        if i = 0 then
          FileDialog.FileName := ReturnText{%H-};
        {$endif}
      end;
      ReturnText := FileDialog{%H-}.Files.Text;
    finally
      QStringList_destroy(ReturnList);
    end;
    {$endif}
  end;
  if ActiveWin <> 0 then
  begin
    if QtWidgetSet.IsValidHandle(ActiveWin) then
      QtWidgetSet.SetActiveWindow(ActiveWin);
  end;
end;

{ TQtWSOpenDialog }

class function TQtWSOpenDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
var
  FileDialog: TQtFilePreviewDialog;
begin
  if (ACommonDialog is TPreviewFileDialog) then
  begin
    FileDialog := TQtFilePreviewDialog.Create(ACommonDialog, TQtWSCommonDialog.GetDialogParent(ACommonDialog));
    {$ifdef darwin}
    QWidget_setWindowFlags(FileDialog.Widget, QtDialog or QtWindowSystemMenuHint or QtCustomizeWindowHint);
    {$endif}
    {. $note WE MUST USE NON NATIVE DIALOGS HERE, OTHERWISE NO SIGNALS #16532.}
    QFileDialog_setOption(QFileDialogH(FileDialog.Widget),
      QFileDialogDontUseNativeDialog, True);
    {$ifndef QT_NATIVE_DIALOGS}
    FileDialog.initializePreview(TPreviewFileDialog(ACommonDialog).PreviewFileControl);
    {$endif}
    FileDialog.AttachEvents;

    Result := THandle(FileDialog);
  end else
    Result := TQtWSFileDialog.CreateHandle(ACommonDialog);
end;

class function TQtWSOpenDialog.QueryWSEventCapabilities(
  const ACommonDialog: TCommonDialog): TCDWSEventCapabilities;
begin
  Result := [cdecWSNoCanCloseSupport];
end;

{ TQtWSSelectDirectoryDialog }

class procedure TQtWSSelectDirectoryDialog.UpdateProperties(
  const AFileDialog: TSelectDirectoryDialog; QtFileDialog: TQtFileDialog);
var
  ATitle: WideString;
  {$ifndef QT_NATIVE_DIALOGS}
  AInitDir: WideString;
  {$endif}
  S: String;
begin
  ATitle := AFileDialog{%H-}.Title;
  QtFileDialog.setWindowTitle(@ATitle);
  {$ifndef QT_NATIVE_DIALOGS}
  S := AFileDialog.InitialDir;
  if UTF8Pos('$HOME', AFileDialog.InitialDir) > 0 then
  begin
    {$IFDEF MSWINDOWS}
    AInitDir := {%H-}GetEnvironmentVariableUTF8('HOMEDRIVE') +
                {%H-}GetEnvironmentVariableUTF8('HOMEPATH');
    {$ELSE}
    AInitDir := {%H-}GetEnvironmentVariableUTF8('HOME');
    {$ENDIF}
    S := StringReplace(S,'$HOME', UTF8Encode(AInitDir),[rfReplaceAll]);
  end;
  if not DirectoryExistsUTF8(S) then
    S := GetCurrentDirUTF8;
  QtFileDialog.setDirectory(S{%H-});
  {$else}
  S := AFileDialog.InitialDir;
  if not DirectoryExistsUTF8(S) then
    S := GetCurrentDirUTF8;
  QtFileDialog.setDirectory(S{%H-});
  {$endif}
  QtFileDialog.setSizeGripEnabled(ofEnableSizing in TSelectDirectoryDialog(AFileDialog).Options);

  if ofViewDetail in TSelectDirectoryDialog(AFileDialog).Options then
    QtFileDialog.setViewMode(QFileDialogDetail)
  else
    QtFileDialog.setViewMode(QFileDialogList);
  {$ifndef QT_NATIVE_DIALOGS}
  // set kbd shortcuts in case when we are not native dialog.
  QtFileDialog.setShortcuts(False);
  {$endif}
end;

class function TQtWSSelectDirectoryDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
var
  FileDialog: TQtFileDialog;
begin
  FileDialog := TQtFileDialog.Create(ACommonDialog, TQtWSCommonDialog.GetDialogParent(ACommonDialog));

  {$ifdef darwin}
  QWidget_setWindowFlags(FileDialog.Widget, QtDialog or
    QtWindowSystemMenuHint or QtCustomizeWindowHint);
  {$endif}

  {$note qt-4.5.0,qt-4.5.1 currently supports macosx only.}
  QFileDialog_setOption(QFileDialogH(FileDialog.Widget),
    QFileDialogDontUseNativeDialog, True);

  FileDialog.setFileMode(QFileDialogDirectoryOnly);

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
  ReturnText: WideString;
  {$ifdef QT_NATIVE_DIALOGS}
  saveFileName: WideString;
  saveTitle: WideString;
  {$endif}
  FileDialog: TSelectDirectoryDialog;
  QtFileDialog: TQtFileDialog;
  {$ifndef QT_NATIVE_DIALOGS}
  ReturnList: QStringListH;
  i: Integer;
  {$endif}
  s: string;
begin
  {------------------------------------------------------------------------------
    Initialization of variables
   ------------------------------------------------------------------------------}
  ReturnText := '';

  FileDialog := TSelectDirectoryDialog(ACommonDialog);
  QtFileDialog := TQtFileDialog(FileDialog.Handle);

  UpdateProperties(FileDialog, QtFileDialog);

  {------------------------------------------------------------------------------
    Code to call the dialog
   ------------------------------------------------------------------------------}
  {$ifdef QT_NATIVE_DIALOGS}
  saveTitle := FileDialog{%H-}.Title;
  // saveFileName := FileDialog.InitialDir;
  if UTF8Pos('$HOME', FileDialog.InitialDir) > 0 then
  begin
    s := FileDialog.InitialDir;
    {$IFDEF MSWINDOWS}
    saveFileName := {%H-}GetEnvironmentVariableUTF8('HOMEDRIVE') +
                    {%H-}GetEnvironmentVariableUTF8('HOMEPATH');
    {$ELSE}
    saveFileName := {%H-}GetEnvironmentVariableUTF8('HOME');
    {$ENDIF}
    s := StringReplace(S,'$HOME', UTF8Encode(saveFileName),[rfReplaceAll]);
    saveFileName := {%H-}s;
  end else
    saveFileName := FileDialog{%H-}.InitialDir;

  QFileDialog_getExistingDirectory(@ReturnText,
    QWidget_parentWidget(QtFileDialog.Widget), @SaveTitle, @saveFileName);
  if ReturnText <> '' then
  begin
    {$ifdef MSWINDOWS}
    s := ReturnText;
    s := StringReplace(s, '/','\', [rfReplaceAll]);
    FileDialog.FileName := s;
    {$else}
    FileDialog.FileName := ReturnText{%H-};
    {$endif}
    FileDialog.UserChoice := mrOK;
  end else
    FileDialog.UserChoice := mrCancel;
  {$else}
  FileDialog.UserChoice := QtDialogCodeToModalResultMap[QDialogDialogCode(QtFileDialog.exec)];
  ReturnList := QStringList_create;
  try
    QtFileDialog.selectedFiles(ReturnList);
    FileDialog.Files.Clear;
    for i := 0 to QStringList_size(ReturnList) - 1 do
    begin
      QStringList_at(ReturnList, @ReturnText, i);
      {$ifdef MSWINDOWS}
      s := ReturnText;
      s := StringReplace(s, '/','\', [rfReplaceAll]);
      FileDialog.Files.Add(s);
      if i = 0 then
        FileDialog.FileName := s;
      {$else}
      FileDialog.Files.Add(ReturnText{%H-});
      if i = 0 then
        FileDialog.FileName := ReturnText{%H-};
      {$endif}
    end;
    ReturnText := FileDialog{%H-}.Files.Text;
  finally
    QStringList_destroy(ReturnList);
  end;
  {$endif}
end;

class function TQtWSSelectDirectoryDialog.QueryWSEventCapabilities(
  const ACommonDialog: TCommonDialog): TCDWSEventCapabilities;
begin
  Result := [cdecWSNoCanCloseSupport];
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
  AColor: TColorRef;
  AQColor: TQColor;
  AQtColor: QColorH;
  ARgb: QRgb;
  ReturnBool: Boolean;
  ColorDialog: TColorDialog absolute ACommonDialog;
  {$IFDEF HASX11}
  AWND: HWND;
  {$ENDIF}


  procedure FillCustomColors;
  var
    i, AIndex, CustomColorCount: integer;
    AColor: TColor;
  begin
    CustomColorCount := QColorDialog_customCount();
    for i := 0 to ColorDialog.CustomColors.Count - 1 do
      if ExtractColorIndexAndColor(ColorDialog.CustomColors, i, AIndex, AColor) then
        if AIndex < CustomColorCount then
          QColorDialog_setCustomColor(AIndex, QRgb(AColor));
  end;

begin
  AColor := ColorToRgb(ColorDialog.Color);
  AQColor.Alpha := $FFFF;
  AQColor.ColorSpec := 1;
  AQColor.Pad := 0;
  ColorRefToTQColor(AColor, AQColor);
  AQtColor := QColor_create(PQColor(@AQColor));
  ARgb := QColor_rgba(AQtColor);
  FillCustomColors;

  ARgb := QColorDialog_getRgba(ARgb, @ReturnBool,
    TQtWSCommonDialog.GetDialogParent(ACommonDialog));

  QColor_fromRgba(PQColor(AQtColor), ARgb);
  try
    QColor_toRgb(AQtColor, @AQColor);
    TQColorToColorRef(AQColor, AColor);
    ColorDialog.Color := TColor(AColor);
  finally
    QColor_destroy(AQtColor);
  end;

  if ReturnBool then
    ACommonDialog.UserChoice := mrOk
  else
    ACommonDialog.UserChoice := mrCancel;
  {$IFDEF HASX11}
  if (QtWidgetSet.WindowManagerName = 'xfwm4') and (QApplication_activeModalWidget() <> nil) then
  begin
    AWND := HwndFromWidgetH(QApplication_activeModalWidget());
    if (AWND <> 0) and (X11GetActivewindow <> TQtWidget(AWND).Widget) then
      X11Raise(QWidget_winID(TQtWidget(AWND).Widget));
  end;
  {$ENDIF}
  //Since TQtWSColorDialog.CreateHandle returns 0, in TCommonDialog.Close DoClose will not be called,
  //so call it from here
  ACommonDialog.DoClose;
end;

class function TQtWSColorDialog.QueryWSEventCapabilities(
  const ACommonDialog: TCommonDialog): TCDWSEventCapabilities;
begin
  Result := [cdecWSPerformsDoClose, cdecWSNoCanCloseSupport];
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
  {$IFDEF HASX11}
  AWND: HWND;
  {$ENDIF}
begin
  {------------------------------------------------------------------------------
    Code to call the dialog
   ------------------------------------------------------------------------------}
  CurrentFont := TQtFont(TFontDialog(ACommonDialog).Font.Reference.Handle).FHandle;

  ReturnFont := QFont_create;
  try
    QFontDialog_getFont(ReturnFont, @ReturnBool, CurrentFont,
      TQtWSCommonDialog.GetDialogParent(ACommonDialog));
   
    QFont_family(ReturnFont, @Str);
    TFontDialog(ACommonDialog).Font.Name := Str{%H-};
   
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
  {$IFDEF HASX11}
  if (QtWidgetSet.WindowManagerName = 'xfwm4') and (QApplication_activeModalWidget() <> nil) then
  begin
    AWND := HwndFromWidgetH(QApplication_activeModalWidget());
    if (AWND <> 0) and (X11GetActivewindow <> TQtWidget(AWND).Widget) then
      X11Raise(QWidget_winID(TQtWidget(AWND).Widget));
  end;
  {$ENDIF}
  //Since TQtWSFontDialog.CreateHandle returns 0, in TCommonDialog.Close DoClose will not be called,
  //so call it from here
  ACommonDialog.DoClose;
end;

class function TQtWSFontDialog.QueryWSEventCapabilities(
  const ACommonDialog: TCommonDialog): TCDWSEventCapabilities;
begin
  Result := [cdecWSPerformsDoClose, cdecWSNoCanCloseSupport];
end;

end.
