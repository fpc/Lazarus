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
  CocoaConfig, CocoaConst, CocoaUtils, CocoaGDIObjects, Cocoa_Extra, CocoaMenus;

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

  TCocoaFilterComboBox = objcclass(NSPopUpButton)
  private
    class procedure DoParseFilters(AFileDialog: TFileDialog; AOutput: TStringList); message 'DoParseFilters:AOutput:';
  public
    Owner: TFileDialog;
    DialogHandle: NSSavePanel;
    Filters: TStringList; // filled by updateFilterList()
    NSFilters: NSMutableArray;
    lastSelectedItemIndex: Integer; // -1 means invalid or none selected
    class function alloc: id; override;
    procedure dealloc; override;
    procedure updateFilterList(); message 'updateFilterList';
    function setDialogFilter(ASelectedFilterIndex: Integer): Integer; message 'setDialogFilter:';
    procedure comboboxAction(sender: id); message 'comboboxAction:';
  end;

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
    cocoaFilePanel: NSSavePanel;
    lclOpenFileDialog: TOpenDialog;
    selUrl: NSURL;
    procedure dealloc; override;
    procedure panel_didChangeToDirectoryURL(sender: id; url: NSURL);
    function panel_userEnteredFilename_confirmed(sender: id; filename: NSString; okFlag: LCLObjCBoolean): NSString;
    procedure panel_willExpand(sender: id; expanding: LCLObjCBoolean);
    procedure panelSelectionDidChange(sender: id);
    procedure treatsAsDirAction(sender: id); message 'treatsAsDirAction:';
  end;
{ TOpenSaveDelegate }

procedure TOpenSaveDelegate.dealloc;
begin
  if Assigned(selUrl) then selURL.release;
  inherited dealloc;
end;

procedure TOpenSaveDelegate.panel_didChangeToDirectoryURL(sender: id; url: NSURL);
begin
  if Assigned(lclOpenFileDialog) then
    lclOpenFileDialog.DoFolderChange;
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
  if not Assigned(lclOpenFileDialog) then Exit;

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
    lclOpenFileDialog.FileName := NSStringToString(sp.URL.path);
    lclOpenFileDialog.DoSelectionChange;
  end;
end;

procedure TOpenSaveDelegate.treatsAsDirAction(sender: id);
begin
  cocoaFilePanel.setTreatsFilePackagesAsDirectories(
    NOT cocoaFilePanel.treatsFilePackagesAsDirectories );
  cocoaFilePanel.validateVisibleColumns;
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
  lclFileDialog: TFileDialog absolute ACommonDialog;
  cocoaFilePanel: NSSavePanel;
  cocoaOpenFilePanel: NSOpenPanel absolute cocoaFilePanel;
  callback: TOpenSaveDelegate;
  filterComboBox: TCocoaFilterComboBox = nil;

  isMenuOn: Boolean;
  oldEditMenu: NSMenuItem = nil;
  editMenuIndex: NSInteger = -1;

  // setup panel and its accessory view
  procedure attachAccessoryView(cocoaFileOwner: NSSavePanel);
  var
    // filter accessory view
    accessoryView: NSView;
    treatsAsDirCheckbox: NSButton;
    filterLabel: NSTextField;

    function needFilePackagesSwitch: Boolean;
    begin
      if ofDontShowFilePackagesSwitch in TOpenDialog(lclFileDialog).OptionsEx then
        Exit( False );
      Result:= CocoaConfigFileDialog.accessoryView.showsFilePackagesSwitch;
    end;

    function needFilter: Boolean;
    begin
      Result:= lclFileDialog.Filter <> EmptyStr;
    end;

    function needAccessoryView: Boolean;
    begin
      Result:= needFilePackagesSwitch or needFilter;
    end;

    procedure createAccessoryView;
    begin
      accessoryView:= NSView.alloc.initWithFrame( NSMakeRect(0, 0, 1, 1) );
      cocoaFileOwner.setAccessoryView( accessoryView );
      accessoryView.release;
    end;

    procedure createTreatsAsDirCheckbox;
    begin
      treatsAsDirCheckbox:= NSButton.alloc.init;
      treatsAsDirCheckbox.setButtonType( NSSwitchButton );
      treatsAsDirCheckbox.setTarget( callback );
      treatsAsDirCheckbox.setAction( ObjCSelector('treatsAsDirAction:') );
      treatsAsDirCheckbox.setTitle( StrToNSString('Show File Package Contents') );
      treatsAsDirCheckbox.setToolTip( StrToNSString('Such as .App Bundles') );
      treatsAsDirCheckbox.sizeToFit;
      if (ofAllowsFilePackagesContents in TOpenDialog(lclFileDialog).OptionsEx)
          or CocoaConfigFileDialog.defaultFilePackagesSwitch then
        treatsAsDirCheckbox.setState( NSOnState );
      accessoryView.addSubview( treatsAsDirCheckbox );
      treatsAsDirCheckbox.release;
    end;

    procedure createFilterLabel;
    begin
      filterLabel:= NSTextField.alloc.initWithFrame( NSZeroRect );
      {$ifdef BOOLFIX}
      filterLabel.setBezeled_(Ord(False));
      filterLabel.setDrawsBackground_(Ord(False));
      filterLabel.setEditable_(Ord(False));
      filterLabel.setSelectable_(Ord(False));
      {$else}
      filterLabel.setBezeled( False );
      filterLabel.setDrawsBackground( False );
      filterLabel.setEditable( False );
      filterLabel.setSelectable( False );
      {$endif}
      filterLabel.setStringValue( StrToNSString(rsMacOSFileFormat) );
      filterLabel.setFont(NSFont.systemFontOfSize(NSFont.systemFontSizeForControlSize(NSRegularControlSize)));
      filterLabel.sizeToFit;
      accessoryView.addSubview(filterLabel);
      filterLabel.release;
    end;

    procedure createFilterCombobox;
    begin
      filterComboBox:= TCocoaFilterComboBox.alloc.initWithFrame(NSNullRect);
      filterComboBox.DialogHandle:= cocoaFileOwner;
      filterComboBox.Owner := lclFileDialog;
      filterComboBox.setTarget( filterComboBox );
      filterComboBox.setAction( objcselector('comboboxAction:') );
      filterComboBox.updateFilterList();
      if lclFileDialog.FilterIndex <= 0 then
        filterComboBox.lastSelectedItemIndex := 0
      else
        filterComboBox.lastSelectedItemIndex := lclFileDialog.FilterIndex-1;
      filterComboBox.lastSelectedItemIndex := filterComboBox.setDialogFilter(filterComboBox.lastSelectedItemIndex);
      if lclFileDialog.FilterIndex>0 then
        filterComboBox.selectItemAtIndex(lclFileDialog.FilterIndex-1);
      filterComboBox.sizeToFit;
      accessoryView.addSubview( filterComboBox );
      filterComboBox.release;
    end;

    procedure updateLayout;
    var
      dialogView: NSView;
      accessoryViewSize: NSSize;
      clientWidth: Double;
      treatsAsDirCheckboxX: Double;
      filterWidthWithLabel: Double;
      filterComboBoxWidth: Double;
      currentY: Double = 0;

      procedure updateFilePackagesSwitchLayout;
      begin
        treatsAsDirCheckboxX:= (clientWidth-treatsAsDirCheckbox.frame.size.width)/2;
        if treatsAsDirCheckboxX < 0 then
          treatsAsDirCheckboxX:= 0;
        treatsAsDirCheckbox.setFrameOrigin( NSMakePoint(treatsAsDirCheckboxX,0) );
        currentY:= treatsAsDirCheckbox.frame.origin.y + treatsAsDirCheckbox.frame.size.height + CocoaConfigFileDialog.accessoryView.vertSpacing;
      end;

      procedure updateFilterLayout;
      begin
        // Trying to put controls into the center of the Acc-view
        //  Label must fit in full. Whatever is left is for filter
        filterComboBoxWidth:= filterComboBox.frame.size.width;
        filterWidthWithLabel:= filterLabel.frame.size.width + filterComboBoxWidth + CocoaConfigFileDialog.accessoryView.horzSpacing;
        if filterWidthWithLabel > clientWidth then begin
          filterWidthWithLabel:= clientWidth;
          filterComboBoxWidth:= filterWidthWithLabel - filterLabel.frame.size.width;
        end;

        filterLabel.setFrameOrigin(  NSMakePoint(
           (clientWidth-filterWidthWithLabel) / 2,
           currentY + (filterComboBox.frame.size.height - filterLabel.frame.size.height) / 2
        ));

        filterComboBox.setFrame( NSMakeRect(
           filterLabel.frame.origin.x + filterLabel.frame.size.width + CocoaConfigFileDialog.accessoryView.horzSpacing,
           currentY,
           filterComboBoxWidth,
           filterComboBox.frame.size.height
        ));

        currentY:= currentY + filterComboBox.frame.size.height + CocoaConfigFileDialog.accessoryView.vertSpacing;
      end;

    begin
      // starting with Big Sur, the dialog retains the last openned size
      // causing the width to be increased on every openning of the dialog
      // we'd simply force the width to start with the minimum width
      accessoryViewSize.width := CocoaConfigFileDialog.accessoryView.minWidth;

      // try to obtain the dialog size
      dialogView:= NSView(cocoaFileOwner.contentView);
      if (dialogView<>nil) and (NSAppkitVersionNumber<NSAppKitVersionNumber11_0) then begin
        if dialogView.frame.size.width > CocoaConfigFileDialog.accessoryView.minWidth then
          accessoryViewSize.width := dialogView.frame.size.width;
      end;

      clientWidth:= accessoryViewSize.Width - CocoaConfigFileDialog.accessoryView.horzSpacing * 2;

      if needFilePackagesSwitch then
        updateFilePackagesSwitchLayout;

      if needFilter then
        updateFilterLayout;

      accessoryViewSize.height:= currentY;
      accessoryView.setFrameSize( accessoryViewSize );
    end;

  begin
    if NOT needAccessoryView then
      Exit;

    createAccessoryView;
    if needFilePackagesSwitch then begin
      createTreatsAsDirCheckbox;
    end;
    if needFilter then begin
      createFilterLabel;
      createFilterCombobox;
    end;
    updateLayout;
  end;

  class procedure setCallback;
  begin
    callback:= TOpenSaveDelegate.new.autorelease;
  end;

  class procedure setFilePanel;
  var
    InitName: String;
    InitDir: String;
    title: NSString;
    url: NSURL;
  begin
    // two sources for init dir
    InitName := ExtractFileName(lclFileDialog.FileName);
    InitDir := lclFileDialog.InitialDir;
    if InitDir = '' then
      InitDir := ExtractFileDir(lclFileDialog.FileName);
    title:= StrToNSString(lclFileDialog.Title);
    url:= NSURL.fileURLWithPath(StrToNSString(InitDir));

    if (lclFileDialog.FCompStyle = csOpenFileDialog) or
       (lclFileDialog.FCompStyle = csPreviewFileDialog) or
      (lclFileDialog is TSelectDirectoryDialog)
      then
    begin
      cocoaOpenFilePanel := NSOpenPanel.openPanel;
      cocoaOpenFilePanel.setAllowsMultipleSelection(
        ofAllowMultiSelect in TOpenDialog(lclFileDialog).Options);
      if (lclFileDialog is TSelectDirectoryDialog) then
      begin
        cocoaOpenFilePanel.setCanChooseDirectories(True);
        cocoaOpenFilePanel.setCanChooseFiles(False);
        cocoaOpenFilePanel.setCanCreateDirectories(True);
        cocoaOpenFilePanel.setAccessoryView(nil);
      end
      else
      begin
        cocoaOpenFilePanel.setCanChooseFiles(True);
        cocoaOpenFilePanel.setCanChooseDirectories(False);
        // accessory view
        attachAccessoryView(cocoaOpenFilePanel);
      end;
    end
    else if lclFileDialog.FCompStyle = csSaveFileDialog then
    begin
      cocoaFilePanel := NSSavePanel.savePanel;
      cocoaFilePanel.setCanCreateDirectories(True);
      cocoaFilePanel.setNameFieldStringValue(StrToNSString(InitName));
      // accessory view
      attachAccessoryView(cocoaFilePanel);
    end;

    cocoaFilePanel.setTitle( title );
    cocoaFilePanel.setDirectoryURL( url );
    cocoaFilePanel.setShowsTagField( False );

    if lclFileDialog is TOpenDialog then
    begin
      if (ofAllowsFilePackagesContents in TOpenDialog(lclFileDialog).OptionsEx)
          or CocoaConfigFileDialog.defaultFilePackagesSwitch then
        cocoaFilePanel.setTreatsFilePackagesAsDirectories(True);
      if ofUseAlternativeTitle in TOpenDialog(lclFileDialog).OptionsEx then
        cocoaFilePanel.setMessage(title);
      if ofForceShowHidden in TOpenDialog(lclFileDialog).Options then
        cocoaFilePanel.setShowsHiddenFiles(True);
      callback.lclOpenFileDialog := TOpenDialog(lclFileDialog);
    end;

    callback.cocoaFilePanel := cocoaFilePanel;
    cocoaFilePanel.setDelegate(callback);
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

  procedure getResultFromFilePanel;
  var
    url: NSURL;
  begin
    lclFileDialog.FileName := NSStringToString(cocoaFilePanel.URL.path);
    lclFileDialog.Files.Clear;

    if cocoaFilePanel.isKindOfClass(NSOpenPanel) then begin
      for url in cocoaOpenFilePanel.URLs do
        lclFileDialog.Files.Add( NSStringToString(url.path) );
    end;

    lclFileDialog.UserChoice := mrOk;
    if filterComboBox <> nil then
      lclFileDialog.FilterIndex := filterComboBox.lastSelectedItemIndex+1;
  end;

begin
  {$IFDEF VerboseWSClass}
  DebugLn('TCocoaWSFileDialog.ShowModal for ' + ACommonDialog.Name);
  {$ENDIF}

  setCallback;
  setFilePanel;

  isMenuOn := ToggleAppMenu(false);
  ReplaceEditMenu();

  lclFileDialog.UserChoice := mrCancel;
  try
    if cocoaFilePanel.runModal = NSOKButton then
      getResultFromFilePanel;
    lclFileDialog.DoClose;
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
  if cdShowAlphaChannel in colorDialog.Options then begin
    colorPanel.setShowsAlpha( True );
    colorPanel.setColor( ColorAlphaToNSColor(ColorDialog.Color, ColorDialog.AlphaChannel) );
  end else begin
    colorPanel.setShowsAlpha( False );
    colorPanel.setColor( ColorToNSColor(ColorDialog.Color) );
  end;

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
var
  lclColor: TColor;
  alpha: Byte;
begin
  if cdShowAlphaChannel in colorDialog.Options then begin
    NSColorToColorAlpha( colorPanel.color, lclColor, alpha );
    ColorDialog.Color:= lclColor;
    ColorDialog.AlphaChannel:= alpha;
  end else begin
    ColorDialog.Color:= NSColorToColorRef(colorPanel.color);
  end;
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
  lFilterParser: TParseStringList;
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
          if (Masks[m]='*.*') or (Masks[m]='*') then
            continue;

          i:= Masks[m].LastIndexOf('.');
          if i>=0 then
            lCurExtension := Masks[m].Substring(i+1)
          else
            lCurExtension := Masks[m];

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
  i: Integer;
  ext : string;
  fileTypes: NSMutableArray;
begin
  if (Filters = nil) or (Filters.Count=0) then
  begin
    Result := -1;
    Exit;
  end;
  if (ASelectedFilterIndex < 0) or (ASelectedFilterIndex >= Filters.Count) then
    ASelectedFilterIndex := 0;
  Result := ASelectedFilterIndex;
  lCurFilter := TStringList(Filters.Objects[ASelectedFilterIndex]);
  fileTypes := NSMutableArray.alloc.init;
  for i:=0 to lCurFilter.Count-1 do
  begin
    ext := lCurFilter[i];
    if (ext='') then Continue;
    fileTypes.addObject(StrToNSString(ext));
  end;

  if (fileTypes.count = 0) then
    DialogHandle.setAllowedFileTypes(nil)
  else
    DialogHandle.setAllowedFileTypes(fileTypes);
  fileTypes.release;
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

end.
