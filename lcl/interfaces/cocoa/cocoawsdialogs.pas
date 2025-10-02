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
  MacOSAll,
  CocoaAll, Classes,
  // LCL
  Controls, SysUtils, Forms, Dialogs, Graphics, Masks,
  LCLType, LCLProc, LCLStrConsts,
  // Widgetset
  WSLCLClasses, WSDialogs,
  // LCL Cocoa
  CocoaConst, CocoaUtils, CocoaGDIObjects, Cocoa_Extra, CocoaMenus;

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

  { TColorPanelDelegate }

  TColorPanelDelegate = objcclass(NSObject, NSWindowDelegateProtocol)
  public
    colorPanel: NSColorPanel;
    ColorDialog: TColorDialog;
    didPickColor: Boolean;
    // NSWindowDelegateProtocol
    procedure windowWillClose(notification: NSNotification); message 'windowWillClose:';
    //
    procedure doPickColor; message 'doPickColor';
    procedure pickColor; message 'pickColor'; // button action
    procedure exit; message 'exit'; // button action
  end;

  { TFontPanelDelegate }

  TFontPanelDelegate = objcclass(NSObject, NSWindowDelegateProtocol)
  public
    FontPanel: NSFontPanel;
    FontDialog: TFontDialog;
    DontSelectFontOnClose: Boolean;
    FontAttr : NSMutableDictionary;

    function init: id; override;
    procedure dealloc; override;

    // NSWindowDelegateProtocol
    procedure windowWillClose(notification: NSNotification); message 'windowWillClose:';
    //
    procedure doSelectFont; message 'doSelectFont';
    procedure selectFont; message 'selectFont'; // button action
    procedure exit; message 'exit'; // button action

    procedure changeAttributes (sender: id); message 'changeAttributes:';
    procedure changeFont (sender: id); override;

    function validModesForFontPanel(afontPanel: NSFontPanel): NSUInteger; override;
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
    class function alloc: id; override;
    procedure dealloc; override;
    procedure updateFilterList(); message 'updateFilterList';
    function setDialogFilter(ASelectedFilterIndex: Integer): Integer; message 'setDialogFilter:';
    procedure comboboxAction(sender: id); message 'comboboxAction:';
    // NSOpenSavePanelDelegateProtocol
    function panel_shouldEnableURL(sender: id; url: NSURL): LCLObjCBoolean; message 'panel:shouldEnableURL:';
  end;

var
  // if set to "true", then OpenDialog would create UTI identifiers
  // for filtering files. As a result a broaded set of files might be available
  // than specified in the extensions filter.
  // "false" is forces the dialog to use panel_shouldEnableURL. It's a slower
  // check (due to security implications), but it's LCL compatible
  CocoaUseUTIFilter : Boolean = false;

procedure FontToDict(src: TFont; dst: NSMutableDictionary);
procedure DictToFont(src: NSDictionary; dst: TFont);
function DictToCocoaFontStyle(src: NSDictionary): TCocoaFontStyle;

implementation

// API irony.
// In LCL the base dialog is TOpenDialog (savedialog inherits from it)
// In Cocoa the base dialog is SaveDialog (opendialog inherites from it)
procedure UpdateOptions(src: TOpenDialog; dst: NSSavePanel);
begin
  dst.setShowsHiddenFiles( ofForceShowHidden in src.Options );
  if (dst.respondsToSelector(objcselector('setShowsTagField:'))) then
    dst.setShowsTagField(false);
end;

procedure UpdateOptions(src: TFileDialog; dst: NSSavePanel);
begin
  if (src is TOpenDialog) then
    UpdateOptions(TOpenDialog(src), dst);
end;

type

  { TOpenSaveDelegate }

  TOpenSaveDelegate = objcclass(NSObject, NSOpenSavePanelDelegateProtocol)
    FileDialog: TFileDialog;
    OpenDialog: TOpenDialog;
    selUrl: NSURL;
    filter: NSOpenSavePanelDelegateProtocol;
    procedure dealloc; override;
    procedure panel_didChangeToDirectoryURL(sender: id; url: NSURL);
    function panel_userEnteredFilename_confirmed(sender: id; filename: NSString; okFlag: LCLObjCBoolean): NSString;
    procedure panel_willExpand(sender: id; expanding: LCLObjCBoolean);
    procedure panelSelectionDidChange(sender: id);
  end;

  // Having path_shouldEnableURL is causing a slowness on a file selection
  // Just having the method declared already introduces a lag in the file selection
  TOpenSaveDelegateWithFilter = objcclass(TOpenSaveDelegate, NSOpenSavePanelDelegateProtocol)
    function panel_shouldEnableURL(sender: id; url: NSURL): LCLObjCBoolean;
  end;

{ TOpenSaveDelegate }

procedure TOpenSaveDelegate.dealloc;
begin
  if Assigned(selUrl) then selURL.release;
  inherited dealloc;
end;

function TOpenSaveDelegateWithFilter.panel_shouldEnableURL(sender: id; url: NSURL
  ): LCLObjCBoolean;
begin
  if Assigned(filter) then
    Result := filter.panel_shouldEnableURL(sender, url)
  else
    Result := true;
end;

procedure TOpenSaveDelegate.panel_didChangeToDirectoryURL(sender: id; url: NSURL);
begin
  if Assigned(OpenDialog) then
    OpenDialog.DoFolderChange;
end;

function TOpenSaveDelegate.panel_userEnteredFilename_confirmed(sender: id;
  filename: NSString; okFlag: LCLObjCBoolean): NSString;
begin
  Result := filename;
end;

procedure TOpenSaveDelegate.panel_willExpand(sender: id; expanding: LCLObjCBoolean);
begin

end;

procedure TOpenSaveDelegate.panelSelectionDidChange(sender: id);
var
  sp : NSSavePanel;
  ch : Boolean;     // set to true, if actually getting a new file name
begin
  // it only matters for Open or Save dialogs
  if not Assigned(OpenDialog) then Exit;

  sp := NSSavePanel(sender);
  ch := false;
  if not Assigned(sp.URL) then begin
    if Assigned(selUrl) then
    begin
      selURL.release;
      selURL := nil;
    end;
    ch := true;
  end
  else if not Assigned(selUrl) then
  begin
    ch := true;
    selURL := NSURL(sp.URL.copy)
  end
  else begin
    ch := not selURL.isEqualTo(sp.URL);
    if ch then
    begin
      selURL.release;
      selURL := sp.URL.copy;
    end;
  end;

  if ch then
  begin
    OpenDialog.FileName := NSStringToString(sp.URL.path);
    OpenDialog.DoSelectionChange;
  end;
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
  callback: TOpenSaveDelegate;

  isMenuOn: Boolean;

  oldEditMenu: NSMenuItem = nil;
  editMenuIndex: NSInteger = -1;

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
      if (NSAppkitVersionNumber >= NSAppKitVersionNumber11_0) then
        // starting with Big Sur, the dialog retains the last openned size
        // causing the width to be increased on every openning of the dialog
        // we'd simply force the lAccessoryWidth to start with the minimum width
        lAccessoryWidth := INT_MIN_ACCESSORYVIEW_WIDTH
      else if lDialogView.frame.size.width > INT_MIN_ACCESSORYVIEW_WIDTH then
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
    lTextStr := NSStringUTF8(rsMacOSFileFormat);
    lText.setStringValue(lTextStr);
    lText.setFont(NSFont.systemFontOfSize(NSFont.systemFontSizeForControlSize(NSRegularControlSize)));
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
    lFilter.lastSelectedItemIndex := lFilter.setDialogFilter(lFilter.lastSelectedItemIndex);
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

  class procedure ReplaceEditMenu();
  var
    mainMenu: NSMenu;
    editMenuTitle: NSString;
  begin
    mainMenu:= NSApplication(NSApp).mainMenu;
    if NOT Assigned(mainMenu) or (mainMenu.numberOfItems=0) then
      Exit;

    oldEditMenu:= FindEditMenu(mainMenu, CocoaConst.NSSTR_EDIT_MENU);
    if Assigned(oldEditMenu) then begin
      editMenuIndex:= mainMenu.indexOfItem(oldEditMenu);
      oldEditMenu.retain;
      mainMenu.removeItemAtIndex(editMenuIndex);
      editMenuTitle:= oldEditMenu.title;
    end else begin
      editMenuIndex:= mainMenu.numberOfItems;
      editMenuTitle:= CocoaConst.NSSTR_EDIT_MENU;
    end;

    AttachEditMenu( mainMenu, editMenuIndex, editMenuTitle );
  end;

  class procedure RestoreEditMenu();
  var
    mainMenu: NSMenu;
  begin
    mainMenu:= NSApplication(NSApp).mainMenu;
    if editMenuIndex > 0 then
      mainMenu.removeItemAtIndex(editMenuIndex);
    if Assigned(oldEditMenu) then begin
      mainMenu.insertItem_atIndex(oldEditMenu, editMenuIndex);
      oldEditMenu.release;
    end;
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
      openDlg.setCanCreateDirectories(True);
      openDlg.setAccessoryView(nil);
    end
    else
    begin
      openDlg.setCanChooseFiles(True);
      openDlg.setCanChooseDirectories(False);
      // accessory view
      CreateAccessoryView(openDlg, openDlg);
    end;
    saveDlg := openDlg;
  end
  else if FileDialog.FCompStyle = csSaveFileDialog then
  begin
    saveDlg := NSSavePanel.savePanel;
    saveDlg.setCanCreateDirectories(True);
    saveDlg.setNameFieldStringValue(NSStringUtf8(InitName));
    // accessory view
    CreateAccessoryView(nil, saveDlg);
    openDlg := nil;
  end;

  saveDlg.retain; // this is for OSX 10.6 (and we don't use ARC either)

  if not Assigned(lFilter) or (CocoaUseUTIFilter) then
    callback:=TOpenSaveDelegate.alloc
  else begin
    callback:=TOpenSaveDelegateWithFilter.alloc;
  end;
  callback.filter := lFilter;
  callback := callback.init;
  callback.autorelease;
  callback.FileDialog := FileDialog;
  if FileDialog is TOpenDialog then
    callback.OpenDialog := TOpenDialog(FileDialog);
  saveDlg.setDelegate(callback);
  saveDlg.setTitle(NSStringUtf8(FileDialog.Title));
  saveDlg.setDirectoryURL(NSURL.fileURLWithPath(NSStringUtf8(InitDir)));
  UpdateOptions(FileDialog, saveDlg);

  isMenuOn := ToggleAppMenu(false);
  ReplaceEditMenu();

  try
    if saveDlg.runModal = NSOKButton then
    begin
      FileDialog.FileName := NSStringToString(saveDlg.URL.path);
      FileDialog.Files.Clear;

      if Assigned(openDlg) then
        for i := 0 to openDlg.URLs.Count - 1 do
          FileDialog.Files.Add(NSStringToString(
            NSURL(openDlg.URLs.objectAtIndex(i)).path));

      FileDialog.UserChoice := mrOk;
      if lFilter <> nil then
        FileDialog.FilterIndex := lFilter.lastSelectedItemIndex+1;
    end;
    FileDialog.DoClose;

    // release everything
    saveDlg.release;
    LocalPool.Release;
  finally
    RestoreEditMenu();
    ToggleAppMenu(isMenuOn);
  end;

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

  isMenuOn: Boolean;
begin
  {$IFDEF VerboseWSClass}
  DebugLn('TCocoaWSColorDialog.ShowModal for ' + ACommonDialog.Name);
  {$ENDIF}

  ACommonDialog.UserChoice := mrCancel;

  colorPanel := NSColorPanel.sharedColorPanel();
  if (colorPanel.respondsToSelector(ObjCSelector('setRestorable:'))) then
    colorPanel.setRestorable(false);
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

  colorPanel.setDelegate(colorDelegate.autorelease);
  colorPanel.setAccessoryView(accessoryView.autorelease);
  colorPanel.setShowsAlpha(False);
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
  isMenuOn := ToggleAppMenu(false);
  try
    colorPanel.makeKeyAndOrderFront(colorDelegate);
    NSApp.runModalForWindow(colorPanel);
  finally
    ToggleAppMenu(isMenuOn);
  end;
end;

{ TCocoaWSFontDialog }

procedure FontToDict(src: TFont; dst: NSMutableDictionary);
const
  UndStyle : array [boolean] of Integer = (NSUnderlineStyleNone, NSUnderlineStyleSingle);
  StkStyle : array [boolean] of Integer = (NSUnderlineStyleNone, NSUnderlineStyleSingle);
begin
  if (src = nil) or (dst = nil) then Exit;
  dst.setObject_forKey(
    NSNumber.numberWithInt( UndStyle[fsUnderline in src.Style]),
    NSUnderlineStyleAttributeName);
  dst.setObject_forKey(
    NSNumber.numberWithInt( StkStyle[fsStrikeOut in src.Style]),
    NSStrikethroughStyleAttributeName);
  if (src.Color <> clDefault) then
    dst.setObject_forKey(ColorToNSColor(src.Color), NSForegroundColorAttributeName);
end;

function ObjToNum(obj: NSObject; defVal: integer): Integer;
begin
  if (obj = nil) or (not obj.isKindOfClass(NSNumber)) then
    Result := defVal
  else
    Result := Integer(NSNumber(obj).integerValue);
end;

procedure DictToFont(src: NSDictionary; dst: TFont);
var
  obj : NSObject;
  fs  : TFontStyles;
  clr : NSColor;
  cl  : TColor;
begin
  if (src = nil) or (dst = nil) then Exit;

  fs := dst.Style;
  cl := dst.Color;

  if ObjToNum( src.objectForKey(NSUnderlineStyleAttributeName), 0) = NSUnderlineStyleNone then
    Exclude(fs, fsUnderline)
  else
    Include(fs, fsUnderline);

  if ObjToNum( src.objectForKey(NSStrikethroughStyleAttributeName), 0) = NSUnderlineStyleNone then
    Exclude(fs, fsStrikeOut)
  else
    Include(fs, fsStrikeOut);

  obj := src.objectForKey(NSForegroundColorAttributeName);
  if (Assigned(obj) and obj.isKindOfClass(NSColor)) then
  begin
    cl := NSColorToColorRef(NSColor(obj));
  end;

  dst.Style := fs;
  dst.Color := cl;
end;


function DictToCocoaFontStyle(src: NSDictionary): TCocoaFontStyle;
begin
  Result := [];
  if (src = nil) then Exit;

  if ObjToNum( src.objectForKey(NSUnderlineStyleAttributeName), 0) = NSUnderlineStyleNone then
    Exclude(Result, cfs_Underline)
  else
    Include(Result, cfs_Underline);

  if ObjToNum( src.objectForKey(NSStrikethroughStyleAttributeName), 0) = NSUnderlineStyleNone then
    Exclude(Result, cfs_Strikeout)
  else
    Include(Result, cfs_Strikeout);
end;


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
  fn : NSFont;
  isMenuOn: Boolean;
  fm : NSFontManager;
begin
  {$IFDEF VerboseWSClass}
  DebugLn('TCocoaWSFontDialog.ShowModal for ' + ACommonDialog.Name);
  {$ENDIF}

  ACommonDialog.UserChoice := mrCancel;

  fm := NSFontManager.sharedFontManager;
  fontPanel := NSFontPanel.sharedFontPanel();
  if (fontPanel.respondsToSelector(ObjCSelector('setRestorable:'))) then
    fontPanel.setRestorable(false);
  inFont := TCocoaFont(FontDialog.Font.Handle);
  fn := inFont.Font;
  if (FontDialog.Font.PixelsPerInch<>72) and (FontDialog.Font.PixelsPerInch<>0) then
  begin
    if (FontDialog.Font.Size<>0) then // assign font size directly to avoid rounding errors
      fn := NSFont.fontWithDescriptor_size(fn.fontDescriptor, Abs(FontDialog.Font.Size)) // ToDo: emulate negative Size values from WinAPI
    else // fallback for default font size: round the result because currently the LCL doesn't support floating-point sizes, so there is no reason to show them to the user
      fn := NSFont.fontWithDescriptor_size(fn.fontDescriptor, Round(fn.pointSize * 72 / FontDialog.Font.PixelsPerInch));
  end;
  fontPanel.setPanelFont_isMultiple(fn, False);

  FontDelegate := TFontPanelDelegate.alloc.init();
  FontDelegate.FontPanel := FontPanel;
  FontDelegate.FontDialog := FontDialog;


  FontToDict(FontDialog.Font, FontDelegate.FontAttr);

  NSFontManager.sharedFontManager.setSelectedAttributes_isMultiple(FontDelegate.FontAttr, false);

  //fm.setDelegate(FontDelegate);

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

  fontPanel.setDelegate(FontDelegate.autorelease);
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
  isMenuOn := ToggleAppMenu(false);
  try
    FontPanel.makeKeyAndOrderFront(FontDelegate);
    NSApp.runModalForWindow(FontPanel);
    fm.setDelegate(nil);
  finally
    ToggleAppMenu(isMenuOn);
  end;
end;

{ TColorPanelDelegate }

procedure TColorPanelDelegate.windowWillClose(notification: NSNotification);
begin
  if didPickColor then
  begin
    ColorDialog.UserChoice := mrOk;
    doPickColor();
  end;
  NSApp.stopModal();
end;

procedure TColorPanelDelegate.doPickColor;
begin
  ColorDialog.Color := NSColorToColorRef(colorPanel.color);
end;

procedure TColorPanelDelegate.pickColor;
begin
  ColorDialog.UserChoice := mrCancel;
  didPickColor := True;
  doPickColor();
  exit();
end;

procedure TColorPanelDelegate.exit;
begin
  colorPanel.close();
end;

{ TFontPanelDelegate }

function TFontPanelDelegate.init: id;
begin
  Result:=inherited init;
  FontAttr := NSMutableDictionary.alloc.init;
end;

procedure TFontPanelDelegate.dealloc;
begin
  FontAttr.release;
  inherited dealloc;
end;

procedure TFontPanelDelegate.windowWillClose(notification: NSNotification);
begin
  if not DontSelectFontOnClose then
  begin
    FontDialog.UserChoice := mrOk;
    doSelectFont();
  end;
  NSApp.stopModal();
end;

procedure TFontPanelDelegate.doSelectFont;
var
  oldHandle, newHandle: TCocoaFont;
  oldFont, newFont: NSFont;
begin
  oldHandle := TCocoaFont(FontDialog.Font.Handle);
  oldFont := oldHandle.Font;
  //oldFont := NSFont.fontWithName_size(NSFont.systemFontOfSize(0).fontDescriptor.postscriptName, 0);
  newFont := FontPanel.panelConvertFont(oldFont);
  if (FontDialog.Font.PixelsPerInch<>72) and (FontDialog.Font.PixelsPerInch<>0) then
    newFont := NSFont.fontWithDescriptor_size(newFont.fontDescriptor, newFont.pointSize * FontDialog.Font.PixelsPerInch / 72);

  newHandle := TCocoaFont.Create(newFont, DictToCocoaFontStyle( FontAttr ));
  FontDialog.Font.Handle := HFONT(newHandle);
  DictToFont( FontAttr, FontDialog.Font );
end;

function TFontPanelDelegate.validModesForFontPanel(afontPanel: NSFontPanel
  ): NSUInteger;
begin
  Result := NSFontPanelFaceModeMask
    or NSFontPanelSizeModeMask
    or NSFontPanelCollectionModeMask
    or NSFontPanelUnderlineEffectModeMask
    or NSFontPanelStrikethroughEffectModeMask
    or NSFontPanelTextColorEffectModeMask;
end;


procedure TFontPanelDelegate.selectFont;
begin
  FontDialog.UserChoice := mrCancel;
  DontSelectFontOnClose := True;
  doSelectFont();
  exit();
end;

procedure TFontPanelDelegate.exit;
begin
  FontDialog.UserChoice := mrOk;
  DontSelectFontOnClose := True;
  FontPanel.close();
end;

procedure TFontPanelDelegate.changeAttributes(sender: id);
var
  d : NSDictionary;
begin
  d:=NSFontManager.sharedFontManager.convertAttributes(FontAttr);
  if (d <> FontAttr) then
  begin
    FontAttr.release;
    FontAttr := NSMutableDictionary.alloc.initWithDictionary(d);
  end;
  NSFontManager.sharedFontManager.setSelectedAttributes_isMultiple(FontAttr, false);
end;

procedure TFontPanelDelegate.changeFont(sender: id);
var
  fp : NSFontPanel;
  oldFont : NSFont;
  newFont : NSFont;
begin
  fp := NSFontPanel.sharedFontPanel;
  oldFont := TCocoaFont(FontDialog.Font.Handle).Font;
  newFont := fp.panelConvertFont(oldFont);
  fp.setPanelFont_isMultiple(
    newFont,
    False);
end;

{ TCocoaFilterComboBox }

class function TCocoaFilterComboBox.alloc: id;
begin
  Result := inherited alloc;
  TCocoaFilterComboBox(Result).NSFilters := NSMutableArray.alloc.init;
end;

procedure TCocoaFilterComboBox.dealloc;
begin
  FreeAndNil(Filters);
  NSFilters.release;
  inherited dealloc;
end;

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
function TCocoaFilterComboBox.setDialogFilter(ASelectedFilterIndex: Integer): Integer;
var
  lCurFilter: TStringList;
  ns: NSString;
  i, lOSVer: Integer;
  lStr: String;
  uti : CFStringRef;
  ext : string;
  j   : integer;
  UTIFilters: NSMutableArray;
begin
  if (Filters = nil) or (Filters.Count=0) then
  begin
    Result := -1;
    Exit;
  end;
  if CocoaUseUTIFilter then
  begin
    if (ASelectedFilterIndex < 0) or (ASelectedFilterIndex >= Filters.Count) then
      ASelectedFilterIndex := 0;
    Result := ASelectedFilterIndex;
    lCurFilter := TStringList(Filters.Objects[ASelectedFilterIndex]);
    UTIFilters := NSMutableArray.alloc.init;
    for i:=0 to lCurFilter.Count-1 do
    begin
      ext := lCurFilter[i];
      if (ext='') then Continue;
      if (ext='*.*') or (ext = '*') then begin
        //uti:=CFSTR('public.content');
        UTIFilters.removeAllObjects;
        break;
      end else begin
        // using the last part of the extension, as Cocoa doesn't suppot
        // complex extensions, such as .tar.gz
        j:=length(ext);
        while (j>0) and (ext[j]<>'.') do dec(j);
        if (j>0) then inc(j);
        ext := System.Copy(ext, j, length(ext));
        // todo: this API is deprecated in macOS 11. There's no UTType ObjC class
        //       to be used
        uti:=UTTypeCreatePreferredIdentifierForTag(kUTTagClassFilenameExtension,
          CFStringRef(NSString.stringWithUTF8String(PChar(ext))), CFSTR('public.data'));
      end;
      if Assigned(uti) then
        UTIFilters.addObject(id(uti));
    end;

    if (UTIFilters.count = 0) then
    begin
      // select any file
      UTIFilters.addObject(id(CFSTR('public.content')));
      UTIFilters.addObject(id(CFSTR('public.data')));
    end;
    DialogHandle.setAllowedFileTypes(UTIFilters);
    UTIFilters.release;
    exit;
  end;

  NSFilters.removeAllObjects();

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
    Result := ASelectedFilterIndex;
  end else
    Result := -1;

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
  begin
    setDialogFilter(indexOfSelectedItem);
    if Assigned(Owner) then
      Owner.IntfFileTypeChanged(indexOfSelectedItem+1);
  end;
  lastSelectedItemIndex := indexOfSelectedItem;
end;

function TCocoaFilterComboBox.panel_shouldEnableURL(sender: id; url: NSURL): LCLObjCBoolean;
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

    if lPath.length >= lCurExt.length then begin
      lExt := lPath.substringFromIndex( lPath.length - lCurExt.length );
      if lExt.caseInsensitiveCompare(lCurExt) = NSOrderedSame then Exit(True);
    end;
  end;
end;

end.
