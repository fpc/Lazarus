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

{$mode delphi}{$H+}

interface

uses
  // Libs
{$ifdef USE_QT_4_3}
  qt43,
{$else}
  qt4,
{$endif}
  qtobjects, qtwidgets,
  // RTL + LCL
  SysUtils, Classes, LCLType, Dialogs, Controls, Forms, Graphics,
  // Widgetset
  WSDialogs, WSLCLClasses;

type

  { TQtWSCommonDialog }

  TQtWSCommonDialog = class(TWSCommonDialog)
  private
  protected
  public
    class function  CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); override;
  end;

  { TQtWSFileDialog }

  TQtWSFileDialog = class(TWSFileDialog)
  private
  protected
  public
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
  public
  end;

  { TQtWSColorDialog }

  TQtWSColorDialog = class(TWSColorDialog)
  private
  protected
  public
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
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;


implementation

{ TQtWSCommonDialog }

{------------------------------------------------------------------------------
  Function: TQtWSCommonDialog.CreateHandle
  Params:  None
  Returns: Nothing

  Dummy handle creator. On Qt we don´t need a Handle for common dialogs
 ------------------------------------------------------------------------------}
class function TQtWSCommonDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
begin
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: TQtWSCommonDialog.DestroyHandle
  Params:  None
  Returns: Nothing

  Dummy handle destructor. On Qt we don´t need a Handle for common dialogs
 ------------------------------------------------------------------------------}
class procedure TQtWSCommonDialog.DestroyHandle(const ACommonDialog: TCommonDialog);
begin

end;

{ TQtWSFileDialog }

{------------------------------------------------------------------------------
  Function: TQtWSFileDialog.ShowModal
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSFileDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  Caption, Dir, Filter, selectedFilter, ReturnText: WideString;
  TmpFilter, strExtensions: string;
  FileDialog: TFileDialog;
  options: QFileDialogOptions;
  Parent: QWidgetH;
  ReturnList: QStringListH;
  ParserState, Position, i: Integer;
begin
  {------------------------------------------------------------------------------
    Initialization of variables
   ------------------------------------------------------------------------------}
  ReturnText := '';
  TmpFilter := '';
  selectedFilter := '';

  {------------------------------------------------------------------------------
    Initialization of the dialog fields
   ------------------------------------------------------------------------------}
  if ACommonDialog.Owner is TWinControl then
   Parent := TQtWidget(TWinControl(ACommonDialog.Owner).Handle).Widget
  else if Assigned(Application.MainForm) then
   Parent := TQtWidget(Application.MainForm.Handle).Widget
  else Parent := nil;

  Caption := UTF8Decode(ACommonDialog.Title);

  FileDialog := TFileDialog(ACommonDialog);

  Dir := UTF8Decode(FileDialog.InitialDir);

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

  for i := 1 to Length(FileDialog.Filter) do
  begin
    if Copy(FileDialog.Filter, i, 1) = '|' then
    begin
      ParserState := ParserState + 1;

      if ParserState = 1 then
       TmpFilter := TmpFilter + Copy(FileDialog.Filter, Position, i - Position)
      else if ParserState = 2 then
      begin
        strExtensions := '(' + Copy(FileDialog.Filter, Position, i - Position) + ')';

        if Pos(strExtensions, TmpFilter) = 0 then TmpFilter := TmpFilter + ' ' + strExtensions;

        TmpFilter := TmpFilter + ';;';

        ParserState := 0;
      end;

      if i <> Length(FileDialog.Filter) then Position := i + 1;
    end;
  end;

  strExtensions := '(' + Copy(FileDialog.Filter, Position, i + 1 - Position) + ')';

  if Pos(strExtensions, TmpFilter) = 0 then TmpFilter := TmpFilter + ' ' + strExtensions;

  {$ifdef VerboseQt}
    WriteLn('[TQtWSCommonDialog.ShowModal] Parsed Filter: ', TmpFilter);
  {$endif}

  Filter := UTF8Decode(TmpFilter);

  {------------------------------------------------------------------------------
    Qt doesn´t have most of the dialog options available on LCL
   ------------------------------------------------------------------------------}

  options := 0;

  {------------------------------------------------------------------------------
    Code to call the dialog
   ------------------------------------------------------------------------------}
  if ACommonDialog is TSaveDialog then
  begin
    if ofOverwritePrompt in TSaveDialog(ACommonDialog).Options then
     options := options or QFileDialogDontConfirmOverwrite;

    Dir := Dir + ExtractFileName(FileDialog.FileName);

    QFileDialog_getSaveFileName(@ReturnText, Parent, @Caption, @Dir, @Filter, @selectedFilter, options);

    if ReturnText = '' then ACommonDialog.UserChoice := mrCancel
    else ACommonDialog.UserChoice := mrOK;

    FileDialog.FileName := UTF8Encode(ReturnText);
  end
  else if ACommonDialog is TOpenDialog then
  begin
    if ofAllowMultiSelect in TOpenDialog(ACommonDialog).Options then
    begin
      ReturnList := QStringList_create;
      try
      
        QFileDialog_getOpenFileNames(ReturnList, Parent, @Caption, @Dir, @Filter, @selectedFilter, options);

        for i := 0 to QStringList_size(ReturnList) - 1 do
        begin
          QStringList_at(ReturnList, @ReturnText, i);
          FileDialog.Files.Add(UTF8Encode(ReturnText));
        end;
        
        ReturnText := FileDialog.Files.Text;
        
      finally
        QStringList_destroy(ReturnList);
      end;
    end
    else
    begin
      QFileDialog_getOpenFileName(@ReturnText, Parent, @Caption, @Dir, @Filter, @selectedFilter, options);

      FileDialog.FileName := UTF8Encode(ReturnText);
    end;

    if ReturnText = '' then ACommonDialog.UserChoice := mrCancel
    else ACommonDialog.UserChoice := mrOK;
  end
  else if ACommonDialog is TSelectDirectoryDialog then
  begin
    QFileDialog_getExistingDirectory(@ReturnText, Parent, @Caption, @Dir);

    if ReturnText = '' then ACommonDialog.UserChoice := mrCancel
    else ACommonDialog.UserChoice := mrOK;
  end;
end;

{ TQtWSColorDialog }

{------------------------------------------------------------------------------
  Function: TQtWSColorDialog.ShowModal
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSColorDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  AColor: TQColor;
  ARefColor: TColor;
  Parent: QWidgetH;
  ARgb: QRgb;
  ReturnBool: Boolean;
  AQtColor: QColorH;
begin

  if ACommonDialog.Owner is TWinControl then
    Parent := TQtWidget(TWinControl(ACommonDialog.Owner).Handle).Widget
  else if Assigned(Application.MainForm) then
    Parent := TQtWidget(Application.MainForm.Handle).Widget
  else Parent := nil;

  ARefColor:= ColorToRgb(TColorDialog(ACommonDialog).Color);

  ARgb := QColorDialog_getRgba(QRgb(ARefColor), @ReturnBool, Parent);

  AQtColor := QColor_create(ARgb);
  try
    QColor_toRgb(AQtColor, @AColor);
    TQColorToColorRef(AColor, ARefColor);
    TColorDialog(ACommonDialog).Color := ARefColor;
  finally
    QColor_destroy(AQtColor);
  end;

  if ReturnBool then ACommonDialog.UserChoice := mrOk
  else ACommonDialog.UserChoice := mrCancel;
end;

{ TQtWSFontDialog }

{------------------------------------------------------------------------------
  Function: TQtWSFontDialog.ShowModal
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSFontDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  Parent: QWidgetH;
  ReturnFont, CurrentFont: QFontH;
  ReturnBool: Boolean;
  Str: WideString;
begin
  {------------------------------------------------------------------------------
    Initialization of options
   ------------------------------------------------------------------------------}
  if ACommonDialog.Owner is TWinControl then
    Parent := TQtWidget(TWinControl(ACommonDialog.Owner).Handle).Widget
  else if Assigned(Application.MainForm) then
    Parent := TQtWidget(Application.MainForm.Handle).Widget
  else Parent := nil;

  {------------------------------------------------------------------------------
    Code to call the dialog
   ------------------------------------------------------------------------------}
  CurrentFont := TQtFont(TFontDialog(ACommonDialog).Font.Handle).Widget;

  ReturnFont := QFont_create;
  try
    QFontDialog_getFont(ReturnFont, @ReturnBool, CurrentFont, Parent);
   
    QFont_family(ReturnFont, @Str);
    TFontDialog(ACommonDialog).Font.Name := UTF8Encode(Str);
   
    TFontDialog(ACommonDialog).Font.Height := QFont_pointSize(ReturnFont);
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

  if ReturnBool then ACommonDialog.UserChoice := mrOk
  else ACommonDialog.UserChoice := mrCancel;
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
//  RegisterWSComponent(TSelectDirectoryDialog, TQtWSSelectDirectoryDialog);
  RegisterWSComponent(TColorDialog, TQtWSColorDialog);
//  RegisterWSComponent(TColorButton, TQtWSColorButton);
  RegisterWSComponent(TFontDialog, TQtWSFontDialog);
////////////////////////////////////////////////////
end.
