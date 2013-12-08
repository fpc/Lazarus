{
 *****************************************************************************
 *                              CocoaWSDialogs.pp                           *
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
unit CocoaWSDialogs;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface


uses
  // RTL,FCL
  MacOSAll, CocoaAll, Classes,
  // LCL
  Controls, StrUtils, SysUtils, Forms, Dialogs, Graphics, Masks,
  LCLType, LMessages, LCLProc,
  // Widgetset
  WSForms, WSLCLClasses, WSProc, WSDialogs, LCLMessageGlue,
  // LCL Cocoa
  CocoaPrivate, CocoaUtils, CocoaWSCommon, CocoaWSStdCtrls;

type

  { TCocoaWSCommonDialog }

  TCocoaWSCommonDialog = class(TWSCommonDialog)
  published
  end;

  { TCocoaWSFileDialog }

  TCocoaWSFileDialog = class(TWSFileDialog)
  published
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TCocoaWSOpenDialog }

  TCocoaWSOpenDialog = class(TWSOpenDialog)
  published
  end;

  { TCocoaWSSaveDialog }

  TCocoaWSSaveDialog = class(TWSSaveDialog)
  published
  end;

  { TCocoaWSSelectDirectoryDialog }

  TCocoaWSSelectDirectoryDialog = class(TWSSelectDirectoryDialog)
  published
  end;

  { TCocoaWSColorDialog }

  TCocoaWSColorDialog = class(TWSColorDialog)
  published
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TCocoaWSColorButton }

  TCocoaWSColorButton = class(TWSColorButton)
  published
  end;

  { TCocoaWSFontDialog }

  TCocoaWSFontDialog = class(TWSFontDialog)
  published
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;


implementation



{ TCocoaWSFileDialog }


{------------------------------------------------------------------------------
  Method:  TCocoaWSFileDialog.ShowModal
  Params:  ACommonDialog - LCL common dialog

 ------------------------------------------------------------------------------}
class procedure TCocoaWSFileDialog.ShowModal(const ACommonDialog: TCommonDialog);
 {
  Called by Execute method of TOpenDialog, TSaveDialog and TSelectDirectoryDialog.
 }
var
  FileDialog: TFileDialog;
  i, m: integer;
  openDlg: NSOpenPanel;
  saveDlg: NSSavePanel;
  nsfilter: NSMutableArray;
  Filters: TStringList;
  ParsedFilter: TParseStringList;
  filterext: string;
  Masks: TParseStringList;
  Extensions: TParseStringList;
  extension: string;
  LocalPool: NSAutoReleasePool;
begin

  {$IFDEF VerboseWSClass}
  DebugLn('TCocoaWSFileDialog.ShowModal for ' + ACommonDialog.Name);
  {$ENDIF}

  LocalPool := NSAutoReleasePool.alloc.init;

  FileDialog := ACommonDialog as TFileDialog;

  Filters := TStringList.Create;

  // Cocoa doesn't supports a filter list selector like we know from windows. So we add all the masks into one filter list.

  ParsedFilter := TParseStringList.Create(FileDialog.Filter, '|');

  for i := 1 to ParsedFilter.Count div 2 do
  begin
    filterext := ParsedFilter[i * 2 - 1];
    Masks := TParseStringList.Create(filterext, ';');
    for m := 0 to Masks.Count - 1 do
    begin
      if Masks[m] = '*.*' then
        continue;

      Extensions := TParseStringList.Create(Masks[m], '.');

      if Extensions.Count > 0 then
        extension := Extensions[Extensions.Count - 1]
      else
        extension := Masks[m];

      Filters.Add(lowercase(extension));
      Filters.Add(uppercase(extension));
      debugln('Filters: ' + extension);
      Extensions.Free;
    end;

    Masks.Free;
  end;

  ParsedFilter.Free;


  nsfilter := nil;
  if Filters.Count > 0 then
  begin
    nsfilter := NSMutableArray.alloc.init;
    for i := 0 to Filters.Count - 1 do
      nsfilter.addObject(NSStringUtf8(Filters.Strings[i]));
  end;

  Filters.Free;

  FileDialog.UserChoice := mrCancel;

  //todo: Options

  if FileDialog.FCompStyle = csOpenFileDialog then
  begin
    openDlg := NSOpenPanel.openPanel;
    openDlg.setAllowsMultipleSelection(ofAllowMultiSelect in
      TOpenDialog(FileDialog).Options);
    openDlg.setCanChooseFiles(True);
    openDlg.setTitle(NSStringUtf8(FileDialog.Title));
    openDlg.setAllowedFileTypes(nsfilter);
    openDlg.setDirectoryURL(NSURL.fileURLWithPath(NSStringUtf8(FileDialog.InitialDir)));

    if openDlg.runModal = NSOKButton then
    begin
      FileDialog.FileName := NSStringToString(openDlg.URL.path);
      FileDialog.Files.Clear;
      for i := 0 to openDlg.filenames.Count - 1 do
        FileDialog.Files.Add(NSStringToString(
          NSURL(openDlg.URLs.objectAtIndex(i)).path));
      FileDialog.UserChoice := mrOk;
    end;

  end
  else
  if FileDialog.FCompStyle = csSaveFileDialog then
  begin
    saveDlg := NSSavePanel.savePanel;
    saveDlg.setCanCreateDirectories(True);
    saveDlg.setTitle(NSStringUtf8(FileDialog.Title));
    saveDlg.setAllowedFileTypes(nsfilter);
    saveDlg.setDirectoryURL(NSURL.fileURLWithPath(
      NSStringUtf8(FileDialog.InitialDir)));
    saveDlg.setNameFieldStringValue(NSStringUtf8(FileDialog.FileName));
    if saveDlg.runModal = NSOKButton then
    begin
      FileDialog.FileName := NSStringToString(saveDlg.URL.path);
      FileDialog.Files.Clear;
      FileDialog.UserChoice := mrOk;
    end;
  end;

  // release everything
  LocalPool.Release;

end;  {TCocoaWSFileDialog.ShowModal}

{ TCocoaWSColorDialog }

{------------------------------------------------------------------------------
  Method:  TCocoaWSColorDialog.ShowModal
  Params:  ACommonDialog - LCL color dialog

  Shows Cocoa interface color picker
 ------------------------------------------------------------------------------}
class procedure TCocoaWSColorDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  ColorDialog: TColorDialog;
begin
  {$IFDEF VerboseWSClass}
  DebugLn('TCocoaWSColorDialog.ShowModal for ' + ACommonDialog.Name);
  {$ENDIF}

  ACommonDialog.UserChoice := mrCancel;
  ColorDialog := ACommonDialog as TColorDialog;
end;


{ TCocoaWSFontDialog }


{------------------------------------------------------------------------------
  Method:  TCocoaWSFontDialog.ShowModal
  Params:  ACommonDialog - LCL font dialog

  Shows Cocoa interface font panel
 ------------------------------------------------------------------------------}
class procedure TCocoaWSFontDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  AFontDialog: TFontDialog;
begin
  {$IFDEF VerboseWSClass}
  DebugLn('TCocoaWSFontDialog.ShowModal for ' + ACommonDialog.Name);
  {$ENDIF}

  AFontDialog := ACommonDialog as TFontDialog;
  AFontDialog.UserChoice := mrCancel;
end;

end.
