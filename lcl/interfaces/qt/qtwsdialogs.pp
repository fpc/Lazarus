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
  qt4, qtobjects,
  // LCL
  SysUtils, Classes, Dialogs, Controls,
  // Widgetset
  WSDialogs, WSLCLClasses;

type

  { TQtWSCommonDialog }

  TQtWSCommonDialog = class(TWSCommonDialog)
  private
  protected
  public
    class function  CreateHandle(const ACommonDialog: TCommonDialog): integer; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); override;
  end;

  { TQtWSFileDialog }

  TQtWSFileDialog = class(TWSFileDialog)
  private
  protected
  public
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
  end;


implementation

{ TQtWSCommonDialog }

class function TQtWSCommonDialog.CreateHandle(const ACommonDialog: TCommonDialog): integer;
begin
  Result := 1000;
end;

class procedure TQtWSCommonDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  Caption, Dir, Filter, selectedFilter, ReturnText: WideString;
  FileDialog: TFileDialog;
  options: QFileDialogOptions;
  Parent: QWidgetH;
  ReturnList: QStringListH;
  ParserState, Position, i: Integer;
  Strings: TStringList;
  ReturnFont, CurrentFont: QFontH;
  ReturnBool: Boolean;
begin
  {------------------------------------------------------------------------------
    Initialization of the dialog´s options
   ------------------------------------------------------------------------------}

  Parent := QWidgetH(TWinControl(ACommonDialog.Owner).Handle);

  ReturnText := '';
  
  Caption := UTF8Decode(ACommonDialog.Title);

  if ACommonDialog is TFileDialog then
  begin
    FileDialog := TFileDialog(ACommonDialog);

    Dir := UTF8Decode(FileDialog.InitialDir);

    Filter := UTF8Decode(FileDialog.Filter);

    {------------------------------------------------------------------------------
      This is a parser that converts LCL filter strings to Qt filter strings
      
      A LCL filter string looks like this:

      Text files|*.txt *.pas|Binaries|*.exe
      
      And a Qt filter string looks like this:
      
      Text files (*.txt *.pas)
      Binaries (*.exe)
     ------------------------------------------------------------------------------}

    ParserState := 0;
    Position := 1;
      
    for i := 1 to Length(FileDialog.Filter) do
    begin
      if Copy(FileDialog.Filter, i, 1) = '|' then
      begin
        ParserState := ParserState + 1;

        if ParserState = 1 then
         Filter := Filter + Copy(FileDialog.Filter, Position, i - Position) + ' '
        else if ParserState = 2 then
        begin
          Filter := Filter + '(' + Copy(FileDialog.Filter, Position, i - Position) + ')' + LineEnding;
          ParserState := 0;
        end;

        if i <> Length(FileDialog.Filter) then Position := i + 1;
      end;
    end;
    
    Filter := Filter + '(' + Copy(FileDialog.Filter, Position, i + 1 - Position) + ')';

    {------------------------------------------------------------------------------
      Sets the selected filter
     ------------------------------------------------------------------------------}

    Strings := TStringList.Create;
    try
      Strings.Text := Filter;
      
      if FileDialog.FilterIndex < Strings.Count then
       selectedFilter := Strings.Strings[FileDialog.FilterIndex];
    finally
      Strings.Free;
    end;

    {------------------------------------------------------------------------------
      Qt doesn´t have most of the dialog options available on LCL
     ------------------------------------------------------------------------------}

    options := 0;
  end;
  
  {------------------------------------------------------------------------------
    Code to call the dialog
   ------------------------------------------------------------------------------}
  if ACommonDialog is TOpenDialog then
  begin
    if ofAllowMultiSelect in TOpenDialog(ACommonDialog).Options then
     QFileDialog_getOpenFileNames(ReturnList, Parent, @Caption, @Dir, @Filter, @selectedFilter, options)
    else
     QFileDialog_getOpenFileName(@ReturnText, Parent, @Caption, @Dir, @Filter, @selectedFilter, options);

    FileDialog.FileName := UTF8Encode(ReturnText);

    if ReturnText = '' then ACommonDialog.UserChoice := mrCancel
    else ACommonDialog.UserChoice := mrOK;
  end
  else if ACommonDialog is TSaveDialog then
  begin
    if ofOverwritePrompt in TSaveDialog(ACommonDialog).Options then
     options := options or QFileDialogDontConfirmOverwrite;
    
    QFileDialog_getSaveFileName(@ReturnText, Parent, @Caption, @Dir, @Filter, @selectedFilter, options);

    if ReturnText = '' then ACommonDialog.UserChoice := mrCancel
    else ACommonDialog.UserChoice := mrOK;
  end
  else if ACommonDialog is TSelectDirectoryDialog then
  begin
    QFileDialog_getExistingDirectory(@ReturnText, Parent, @Caption, @Dir);

    if ReturnText = '' then ACommonDialog.UserChoice := mrCancel
    else ACommonDialog.UserChoice := mrOK;
  end
  else if ACommonDialog is TColorDialog then
  begin
  end
  else if ACommonDialog is TFontDialog then
  begin
    CurrentFont := TQtFont(TFontDialog(ACommonDialog).Font.Handle).Widget;
  
    QFontDialog_getFont(ReturnFont, @ReturnBool, CurrentFont, Parent);

    if ReturnBool then ACommonDialog.UserChoice := mrOk
    else ACommonDialog.UserChoice := mrCancel;
  end;
end;

class procedure TQtWSCommonDialog.DestroyHandle(const ACommonDialog: TCommonDialog);
begin

end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCommonDialog, TQtWSCommonDialog);
//  RegisterWSComponent(TFileDialog, TQtWSFileDialog);
//  RegisterWSComponent(TOpenDialog, TQtWSOpenDialog);
//  RegisterWSComponent(TSaveDialog, TQtWSSaveDialog);
//  RegisterWSComponent(TSelectDirectoryDialog, TQtWSSelectDirectoryDialog);
//  RegisterWSComponent(TColorDialog, TQtWSColorDialog);
//  RegisterWSComponent(TColorButton, TQtWSColorButton);
//  RegisterWSComponent(TFontDialog, TQtWSFontDialog);
////////////////////////////////////////////////////
end.
