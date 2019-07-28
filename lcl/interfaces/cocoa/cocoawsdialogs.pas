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
{$modeswitch objectivec2}
{$include cocoadefines.inc}

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
  CocoaPrivate, CocoaUtils, CocoaWSCommon, CocoaWSStdCtrls, CocoaGDIObjects
  ,Cocoa_Extra;

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

  TColorPanelDelegate = objcclass(NSObject, NSWindowDelegateProtocol)
  public
    colorPanel: NSColorPanel;
    ColorDialog: TColorDialog;
    DontPickColorOnClose: Boolean;
    // NSWindowDelegateProtocol
    procedure windowWillClose(notification: NSNotification); message 'windowWillClose:';
    //
    procedure doPickColor; message 'doPickColor';
    procedure pickColor; message 'pickColor'; // button action
    procedure exit; message 'exit'; // button action
  end;

  TFontPanelDelegate = objcclass(NSObject, NSWindowDelegateProtocol)
  public
    FontPanel: NSFontPanel;
    FontDialog: TFontDialog;
    DontSelectFontOnClose: Boolean;
    // NSWindowDelegateProtocol
    procedure windowWillClose(notification: NSNotification); message 'windowWillClose:';
    //
    procedure doSelectFont; message 'doSelectFont';
    procedure selectFont; message 'selectFont'; // button action
    procedure exit; message 'exit'; // button action
  end;

  { TCocoaFilterComboBox }

  TCocoaFilterComboBox = objcclass(NSPopUpButton, NSOpenSavePanelDelegateProtocol)
  private
    class procedure DoParseFilters(AFileDialog: TFileDialog; AOutput: TStringList); message 'DoParseFilters:AOutput:';
    class function locateNSPanelBrowser(AView: NSView; AClass: Pobjc_class): NSView; message 'locateNSPanelBrowser:AClass:';
    class procedure reloadNSPanelBrowser(APanel: NSSavePanel; AIsOpenDialog: Boolean); message 'reloadNSPanelBrowser:AIsOpenDialog:';
  public
    Owner: TFileDialog;
    DialogHandle: NSSavePanel;
    IsOpenDialog: Boolean;
    Filters: TStringList; // filled by updateFilterList()
    NSFilters: NSMutableArray;
    lastSelectedItemIndex: Integer; // -1 means invalid or none selected
    procedure updateFilterList(); message 'updateFilterList';
    procedure setDialogFilter(ASelectedFilterIndex: Integer); message 'setDialogFilter:';
    procedure comboboxAction(sender: id); message 'comboboxAction:';
    // NSOpenSavePanelDelegateProtocol
    function panel_shouldEnableURL(sender: id; url: NSURL): Boolean; message 'panel:shouldEnableURL:';
  end;

implementation

// API irony.
// In LCL the base dialog is TOpenDialog (savedialog inherits from it)
// In Cocoa the base dialog is SaveDialog (opendialog inherites from it)
procedure UpdateOptions(src: TOpenDialog; dst: NSSavePanel);
begin
  dst.setShowsHiddenFiles( ofForceShowHidden in src.Options );
end;

procedure UpdateOptions(src: TFileDialog; dst: NSSavePanel);
begin
  if (src is TOpenDialog) then
    UpdateOptions(TOpenDialog(src), dst);
end;

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
  FileDialog: TFileDialog absolute ACommonDialog;
  i: integer;
  openDlg: NSOpenPanel;
  saveDlg: NSSavePanel;
  InitName, InitDir: string;
  LocalPool: NSAutoReleasePool;
  // filter accessory view
  accessoryView: NSView;
  lFilter: TCocoaFilterComboBox;

  // setup panel and its accessory view
  procedure CreateAccessoryView(AOpenOwner: NSOpenPanel; ASaveOwner: NSSavePanel);
  const
    INT_MIN_ACCESSORYVIEW_WIDTH = 300;
    OFS_HRZ = 10;
  var
    lRect: NSRect;
    lText: NSTextField;
    lTextStr: NSString;
    lDialogView: NSView;
    lAccessoryWidth: Integer = INT_MIN_ACCESSORYVIEW_WIDTH;
    w: Integer;
    nw: Integer;
    fw: Integer;
  begin
    // check if the accessory is necessary
    if FileDialog.Filter = '' then Exit;

    // try to obtain the dialog size
    lDialogView := NSView(ASaveOwner.contentView);
    if lDialogView <> nil then
    begin
      if lDialogView.frame.size.width > INT_MIN_ACCESSORYVIEW_WIDTH then
        lAccessoryWidth := Round(lDialogView.frame.size.width);
    end;
    lRect := GetNSRect(0, 0, lAccessoryWidth, 30);
    accessoryView := NSView.alloc.initWithFrame(lRect);

    // "Format:" label
    lText := NSTextField.alloc.initWithFrame(NSNullRect);
    {$ifdef BOOLFIX}
    lText.setBezeled_(Ord(False));
    lText.setDrawsBackground_(Ord(False));
    lText.setEditable_(Ord(False));
    lText.setSelectable_(Ord(False));
    {$else}
    lText.setBezeled(False);
    lText.setDrawsBackground(False);
    lText.setEditable(False);
    lText.setSelectable(False);
    {$endif}
    lTextStr := NSStringUTF8('Format:');
    lText.setStringValue(lTextStr);
    lText.sizeToFit;

    // Combobox
    lFilter := TCocoaFilterComboBox.alloc.initWithFrame(NSNullRect);
    lFilter.IsOpenDialog := AOpenOwner <> nil;
    if lFilter.IsOpenDialog then
      lFilter.DialogHandle := AOpenOwner
    else
      lFilter.DialogHandle := ASaveOwner;
    lFilter.Owner := FileDialog;
    lFilter.setTarget(lFilter);
    lFilter.setAction(objcselector('comboboxAction:'));
    lFilter.updateFilterList();
    if FileDialog.FilterIndex <= 0 then
      lFilter.lastSelectedItemIndex := 0
    else
      lFilter.lastSelectedItemIndex := FileDialog.FilterIndex-1;
    lFilter.setDialogFilter(lFilter.lastSelectedItemIndex);
    lFilter.sizeToFit;
    lFilter.setAutoresizingMask(NSViewWidthSizable);
    if FileDialog.FilterIndex>0 then
      lFilter.selectItemAtIndex(FileDialog.FilterIndex-1);

    // Trying to put controls into the center of the Acc-view
    //  Label must fit in full. Whatever is left is for filter
    w:=lAccessoryWidth - OFS_HRZ - OFS_HRZ;
    fw:=Round(lFilter.frame.size.width);
    nw:=Round(lText.frame.size.width + fw + OFS_HRZ);
    if nw>w then begin
      dec(fw, nw-w);
      nw:=w;
    end;

    lText.setFrame(  NSMakeRect(
       Round((w-nw) / 2+OFS_HRZ)
       , 0
       , lText.frame.size.width
       , lFilter.frame.size.height
    ));

    lFilter.setFrame( NSMakeRect(
       lText.frame.origin.x+lText.frame.size.width+OFS_HRZ
       ,4
       ,fw
       ,lFilter.frame.size.height
      ));

    accessoryView.addSubview(lText.autorelease);
    accessoryView.addSubview(lFilter.autorelease);

    lFilter.setAutoresizingMask(NSViewWidthSizable);

    lFilter.DialogHandle.setAccessoryView(accessoryView.autorelease);
    lFilter.DialogHandle.setDelegate(lFilter);
  end;

begin
  {$IFDEF VerboseWSClass}
  DebugLn('TCocoaWSFileDialog.ShowModal for ' + ACommonDialog.Name);
  {$ENDIF}
  lFilter := nil;

  LocalPool := NSAutoReleasePool.alloc.init;

  // two sources for init dir
  InitName := ExtractFileName(FileDialog.FileName);
  InitDir := FileDialog.InitialDir;
  if InitDir = '' then
    InitDir := ExtractFileDir(FileDialog.FileName);

  // Cocoa doesn't supports a filter list selector like we know from windows natively
  // So we need to create our own accessory view

  FileDialog.UserChoice := mrCancel;

  //todo: Options
  if (FileDialog.FCompStyle = csOpenFileDialog) or
     (FileDialog.FCompStyle = csPreviewFileDialog) or
    (FileDialog is TSelectDirectoryDialog)
    then
  begin
    openDlg := NSOpenPanel.openPanel;
    openDlg.setAllowsMultipleSelection(ofAllowMultiSelect in
      TOpenDialog(FileDialog).Options);
    if (FileDialog is TSelectDirectoryDialog) then
    begin
      openDlg.setCanChooseDirectories(True);
      openDlg.setCanChooseFiles(False);
      openDlg.setAccessoryView(nil);
    end
    else
    begin
      openDlg.setCanChooseFiles(True);
      openDlg.setCanChooseDirectories(False);
      // accessory view
      CreateAccessoryView(openDlg, openDlg);
    end;
    openDlg.setTitle(NSStringUtf8(FileDialog.Title));
    openDlg.setDirectoryURL(NSURL.fileURLWithPath(NSStringUtf8(InitDir)));
    UpdateOptions(FileDialog, openDlg);

    if openDlg.runModal = NSOKButton then
    begin
      FileDialog.FileName := NSStringToString(openDlg.URL.path);
      FileDialog.Files.Clear;
      for i := 0 to openDlg.filenames.Count - 1 do
        FileDialog.Files.Add(NSStringToString(
          NSURL(openDlg.URLs.objectAtIndex(i)).path));
      FileDialog.UserChoice := mrOk;
      if lFilter <> nil then
        FileDialog.FilterIndex := lFilter.lastSelectedItemIndex+1;
    end;
  end
  else if FileDialog.FCompStyle = csSaveFileDialog then
  begin
    saveDlg := NSSavePanel.savePanel;
    saveDlg.setCanCreateDirectories(True);
    saveDlg.setTitle(NSStringUtf8(FileDialog.Title));
    saveDlg.setDirectoryURL(NSURL.fileURLWithPath(NSStringUtf8(InitDir)));
    saveDlg.setNameFieldStringValue(NSStringUtf8(InitName));
    UpdateOptions(FileDialog, saveDlg);
    // accessory view
    CreateAccessoryView(nil, saveDlg);

    if saveDlg.runModal = NSOKButton then
    begin
      FileDialog.FileName := NSStringToString(saveDlg.URL.path);
      FileDialog.Files.Clear;
      FileDialog.UserChoice := mrOk;
      if lFilter <> nil then
        FileDialog.FilterIndex := lFilter.lastSelectedItemIndex+1;
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
  colorDelegate: TColorPanelDelegate;
  ColorDialog: TColorDialog absolute ACommonDialog;
  colorPanel: NSColorPanel;
  // accessory view
  accessoryView: NSView;
  lRect: NSRect;
  okButton, cancelButton: NSButton;
begin
  {$IFDEF VerboseWSClass}
  DebugLn('TCocoaWSColorDialog.ShowModal for ' + ACommonDialog.Name);
  {$ENDIF}

  ACommonDialog.UserChoice := mrCancel;

  colorPanel := NSColorPanel.sharedColorPanel();
  colorPanel.setColor(ColorToNSColor(ColorDialog.Color));

  colorDelegate := TColorPanelDelegate.alloc.init();
  colorDelegate.colorPanel := colorPanel;
  colorDelegate.ColorDialog := ColorDialog;

  // setup panel and its accessory view
  lRect := GetNSRect(0, 0, 220, 30);
  accessoryView := NSView.alloc.initWithFrame(lRect);

  lRect := GetNSRect(110, 4, 110-8, 24);
  okButton := NSButton.alloc.initWithFrame(lRect);
  okButton.setButtonType(NSMomentaryPushInButton);
  okButton.setBezelStyle(NSRoundedBezelStyle);
  okButton.setTitle(NSStringUtf8('Pick'));
  okButton.setAction(objcselector('pickColor'));
  okButton.setTarget(colorDelegate);

  lRect := GetNSRect(8, 4, 110-8, 24);
  cancelButton := NSButton.alloc.initWithFrame(lRect);
  cancelButton.setButtonType(NSMomentaryPushInButton);
  cancelButton.setBezelStyle(NSRoundedBezelStyle);
  cancelButton.setTitle(NSStringUtf8('Cancel'));
  cancelButton.SetAction(objcselector('exit'));
  cancelButton.setTarget(colorDelegate);

  accessoryView.addSubview(okButton.autorelease);
  accessoryView.addSubview(cancelButton.autorelease);

  colorPanel.setDelegate(colorDelegate);
  colorPanel.setAccessoryView(accessoryView.autorelease);
  colorPanel.setShowsAlpha(True);
  colorPanel.setDefaultButtonCell(okButton.cell);

  // load user settings
  (*NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];
  NSString *color = [defaults stringForKey:@"startColor"];
  if (color != nil) {
    [panel setColor:[NSColor colorFromHex:color]];
  }
  [panel setMode:[defaults integerForKey:@"mode"]]; // will be 0 if not set, wich is NSGrayModeColorPanel
*)

  // show panel
  colorPanel.makeKeyAndOrderFront(colorDelegate);
  NSApp.runModalForWindow(colorPanel);
end;

{ TCocoaWSFontDialog }

{------------------------------------------------------------------------------
  Method:  TCocoaWSFontDialog.ShowModal
  Params:  ACommonDialog - LCL font dialog

  Shows Cocoa interface font panel
 ------------------------------------------------------------------------------}
class procedure TCocoaWSFontDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  FontDialog: TFontDialog absolute ACommonDialog;
  FontDelegate: TFontPanelDelegate;
  FontPanel: NSFontPanel;
  inFont: TCocoaFont;
  // accessory view
  accessoryView: NSView;
  lRect: NSRect;
  okButton, cancelButton: NSButton;
begin
  {$IFDEF VerboseWSClass}
  DebugLn('TCocoaWSFontDialog.ShowModal for ' + ACommonDialog.Name);
  {$ENDIF}

  ACommonDialog.UserChoice := mrCancel;

  fontPanel := NSFontPanel.sharedFontPanel();
  inFont := TCocoaFont(FontDialog.Font.Handle);
  fontPanel.setPanelFont_isMultiple(inFont.Font, False);

  FontDelegate := TFontPanelDelegate.alloc.init();
  FontDelegate.FontPanel := FontPanel;
  FontDelegate.FontDialog := FontDialog;

  // setup panel and its accessory view
  lRect := GetNSRect(0, 0, 220, 30);
  accessoryView := NSView.alloc.initWithFrame(lRect);

  lRect := GetNSRect(110, 4, 110-8, 24);
  okButton := NSButton.alloc.initWithFrame(lRect);
  okButton.setButtonType(NSMomentaryPushInButton);
  okButton.setBezelStyle(NSRoundedBezelStyle);
  okButton.setTitle(NSStringUtf8('Select'));
  okButton.setAction(objcselector('selectFont'));
  okButton.setTarget(FontDelegate);

  lRect := GetNSRect(8, 4, 110-8, 24);
  cancelButton := NSButton.alloc.initWithFrame(lRect);
  cancelButton.setButtonType(NSMomentaryPushInButton);
  cancelButton.setBezelStyle(NSRoundedBezelStyle);
  cancelButton.setTitle(NSStringUtf8('Cancel'));
  cancelButton.SetAction(objcselector('exit'));
  cancelButton.setTarget(FontDelegate);

  accessoryView.addSubview(okButton.autorelease);
  accessoryView.addSubview(cancelButton.autorelease);

  fontPanel.setDelegate(FontDelegate);
  fontPanel.setAccessoryView(accessoryView.autorelease);
  fontPanel.setDefaultButtonCell(okButton.cell);

  // load user settings
  (*NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];
  NSString *color = [defaults stringForKey:@"startColor"];
  if (color != nil) {
    [panel setColor:[NSColor colorFromHex:color]];
  }
  [panel setMode:[defaults integerForKey:@"mode"]]; // will be 0 if not set, wich is NSGrayModeColorPanel
*)

  // show panel
  FontPanel.makeKeyAndOrderFront(FontDelegate);
  NSApp.runModalForWindow(FontPanel);
end;

{ TColorPanelDelegate }

procedure TColorPanelDelegate.windowWillClose(notification: NSNotification);
begin
  if not DontPickColorOnClose then
  begin
    ColorDialog.UserChoice := mrOk;
    doPickColor();
  end;
  NSApp.stopModal();
end;

procedure TColorPanelDelegate.doPickColor();
begin
  ColorDialog.Color := NSColorToRGB(colorPanel.color);
end;

procedure TColorPanelDelegate.pickColor();
begin
  ColorDialog.UserChoice := mrCancel;
  DontPickColorOnClose := True;
  doPickColor();
  exit();
end;

procedure TColorPanelDelegate.exit();
begin
  ColorDialog.UserChoice := mrOk;
  DontPickColorOnClose := True;
  colorPanel.close();
end;

{ TFontPanelDelegate }

procedure TFontPanelDelegate.windowWillClose(notification: NSNotification);
begin
  if not DontSelectFontOnClose then
  begin
    FontDialog.UserChoice := mrOk;
    doSelectFont();
  end;
  NSApp.stopModal();
end;

procedure TFontPanelDelegate.doSelectFont();
var
  oldHandle, newHandle: TCocoaFont;
  oldFont, newFont: NSFont;
begin
  oldHandle := TCocoaFont(FontDialog.Font.Handle);
  oldFont := oldHandle.Font;
  //oldFont := NSFont.fontWithName_size(NSFont.systemFontOfSize(0).fontDescriptor.postscriptName, 0);
  newFont := FontPanel.panelConvertFont(oldFont);
  newHandle := TCocoaFont.Create(newFont);
  FontDialog.Font.Handle := HFONT(newHandle);
end;

procedure TFontPanelDelegate.selectFont();
begin
  FontDialog.UserChoice := mrCancel;
  DontSelectFontOnClose := True;
  doSelectFont();
  exit();
end;

procedure TFontPanelDelegate.exit();
begin
  FontDialog.UserChoice := mrOk;
  DontSelectFontOnClose := True;
  FontPanel.close();
end;

{ TCocoaFilterComboBox }

class procedure TCocoaFilterComboBox.DoParseFilters(AFileDialog: TFileDialog; AOutput: TStringList);
var
  lFilterParser, lExtParser: TParseStringList;
  Masks: TParseStringList;
  lFilterCounter, m: Integer;
  lFilterName, filterext, lCurExtension: String;
  lExtensions: TStringList;
  i: Integer;
begin
  lFilterParser := TParseStringList.Create(AFileDialog.Filter, '|');
  try
    lFilterCounter := 0;
    while lFilterCounter < lFilterParser.Count - 1 do
    begin
      lFilterName := lFilterParser[lFilterCounter];
      filterext := lFilterParser[lFilterCounter+1];
      Masks := TParseStringList.Create(filterext, ';');
      try
        lExtensions := TStringList.Create;
        for m := 0 to Masks.Count - 1 do
        begin
          if Masks[m] = '*.*' then
            continue;

          i:=Pos('.',Masks[m]);
          // ignore anything before the first dot (see #32069)
          // storing the extension with leading '.'
          // the dot is used later with file name comparison
          if i>0 then
            lCurExtension := System.Copy(Masks[m], i, length(Masks[m]))
          else
            lCurExtension := '.'+Masks[m];

          lExtensions.Add(lowercase(lCurExtension));
        end;
        AOutput.AddObject(lFilterName, lExtensions);
      finally
        Masks.Free;
      end;

      Inc(lFilterCounter, 2);
    end;
  finally
    lFilterParser.Free;
  end;
end;

class function TCocoaFilterComboBox.locateNSPanelBrowser(AView: NSView; AClass: Pobjc_class): NSView;
var
  lSubview: NSView;
begin
  Result := nil;
  for lSubView in AView.subviews do
  begin
    //lCurClass := lCurClass + '/' + NSStringToString(lSubview.className);
    if lSubview.isKindOfClass_(AClass) then
    begin
      Exit(lSubview);
    end;
    if lSubview.subviews.count > 0 then
    begin
      Result := locateNSPanelBrowser(lSubview, AClass);
    end;
    if Result <> nil then Exit;
  end;
  Result := nil;
end;

class procedure TCocoaFilterComboBox.reloadNSPanelBrowser(APanel: NSSavePanel; AIsOpenDialog: Boolean);
var
  lBrowser: NSBrowser;
  panelSelectionPath: NSArray;
begin
  //obtain browser
  if AIsOpenDialog then
  begin
    lBrowser := NSBrowser(locateNSPanelBrowser(APanel.contentView, NSBrowser));
    if lBrowser = nil then Exit;

    //reload browser
    panelSelectionPath := lBrowser.selectionIndexPaths; //otherwise the panel return the wrong urls
    if lBrowser.lastColumn > 0 then
    begin
      lBrowser.reloadColumn(lBrowser.lastColumn-1);
    end;
    lBrowser.reloadColumn(lBrowser.lastColumn);
    lBrowser.setSelectionIndexPaths(panelSelectionPath);
  end;
end;

// This will recreate the contents of the filter combobox and fill the cache
// which is utilized when a particular filter is selected (Filters: TStringList)
procedure TCocoaFilterComboBox.updateFilterList();
var
  nsstr: NSString;
  i: Integer;
  lItems: array of NSMenuItem;
begin
  if Filters = nil then
  begin
    Filters := TStringList.Create;
    Filters.OwnsObjects := True;
  end;
  Filters.Clear;
  DoParseFilters(Owner, Filters);

  // Now update the combobox
  removeAllItems();
  // Adding an item with its final name will cause it to be deleted,
  // so we need to first add all items with unique names, and then
  // rename all of them, see bug 30847
  SetLength(lItems, Filters.Count);
  for i := 0 to Filters.Count-1 do
  begin
    nsstr := NSStringUtf8(Format('unique_item_%d', [i]));
    addItemWithTitle(nsstr);
    lItems[i] := lastItem;
    nsstr.release;
  end;
  for i := 0 to Filters.Count-1 do
  begin
    nsstr := NSStringUtf8(Filters.Strings[i]);
    lItems[i].setTitle(nsstr);
    nsstr.release;
  end;
  SetLength(lItems, 0);

  // reset the selected item
  selectItemAtIndex(lastSelectedItemIndex);
end;

// Generates NSFilters from Filters, for the currently selected combobox index
procedure TCocoaFilterComboBox.setDialogFilter(ASelectedFilterIndex: Integer);
var
  lCurFilter: TStringList;
  ns: NSString;
  i, lOSVer: Integer;
  lStr: String;
begin
  if Filters = nil then Exit;

  if NSFilters = nil then
  begin
    NSFilters := NSMutableArray.alloc.init;
  end
  else
  begin
    NSFilters.removeAllObjects();
  end;

  if (Filters.Count > 0) and (ASelectedFilterIndex >= 0) and
   (ASelectedFilterIndex < Filters.Count) then
  begin
    lCurFilter := TStringList(Filters.Objects[ASelectedFilterIndex]);
    for i := 0 to lCurFilter.Count - 1 do
    begin
      lStr := lCurFilter.Strings[i];
      ns := NSStringUtf8(lStr);
      NSFilters.addObject(ns);
      ns.release;
    end;
  end;

  DialogHandle.validateVisibleColumns();
  // work around for bug in validateVisibleColumns() in Mavericks till 10.10.2
  // see https://bugs.freepascal.org/view.php?id=28687
  lOSVer := GetMacOSXVersion();
  if (lOSVer >= $090000)
     and (lOSVer <= $0A0A02)
     and (NSAppKitVersionNumber >= NSAppKitVersionNumber10_7) then
  begin
    // won't work on 10.6
    reloadNSPanelBrowser(DialogHandle, IsOpenDialog);
  end;

  // Felipe: setAllowedFileTypes generates misterious crashes on dialog close after combobox change if uncommented :(
  // DialogHandle.setAllowedFileTypes(NSFilters);
end;

procedure TCocoaFilterComboBox.comboboxAction(sender: id);
begin
  if (indexOfSelectedItem <> lastSelectedItemIndex) then
    setDialogFilter(indexOfSelectedItem);
  lastSelectedItemIndex := indexOfSelectedItem;
end;

function TCocoaFilterComboBox.panel_shouldEnableURL(sender: id; url: NSURL): Boolean;
var
  lPath, lExt, lCurExt: NSString;
  lExtStr, lCurExtStr: String;
  i: Integer;
  lIsDirectory: Boolean = True;
  item: NSMenuItem;
begin
  // if there are no filters, accept everything
  if NSFilters.count = 0 then Exit(True);

  Result := False;
  lPath := url.path;

  // always allow selecting dirs, otherwise you can't change the directory
  NSFileManager.defaultManager.fileExistsAtPath_isDirectory(lPath, @lIsDirectory);
  if lIsDirectory then Exit(True);

  for i := 0 to NSFilters.count - 1 do
  begin
    lCurExt := NSString(NSFilters.objectAtIndex(i));
    // Match *.* to everything
    if lCurExt.compare( NSString.stringWithUTF8String('.*') ) = NSOrderedSame then
      Exit(True);

    lExt := lPath.substringFromIndex( lPath.length - lCurExt.length );
    if lExt.caseInsensitiveCompare(lCurExt) = NSOrderedSame then Exit(True);
  end;
end;

end.
