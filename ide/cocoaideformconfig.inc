{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  Forms, Menus,
  LazarusIDEStrConsts, SourceEditor, SynEditTypes,
  CocoaAll, CocoaConfig, CocoaToolBar, Cocoa_Extra;

procedure initIDECocoaConfigForms;

implementation

type
  TCocoaIDEMainFormHandler = class
  private
    searchingString: String;
    procedure doFindNext(Data: PtrInt);
    procedure doFocusSearchField( Sender: TObject );
  end;

var
  IDEMainFormHandler: TCocoaIDEMainFormHandler;

procedure TCocoaIDEMainFormHandler.doFindNext(Data: PtrInt);
begin
  SourceEditorManager.ActiveEditor.DoFindAndReplace(
    self.searchingString,
    '',
    [ssoFindContinue] );
  self.searchingString:= '';
end;

procedure TCocoaIDEMainFormHandler.doFocusSearchField(Sender: TObject);
var
  lclForm: TCustomForm;
  cocoaItem: NSToolbarItem;
  cocoaWindow: NSWindow;
begin
  lclForm:= Screen.ActiveCustomForm;
  cocoaItem:= TCocoaToolBarUtils.findItemByIdentifier( lclForm, 'MainIDE.Search' );
  if NOT Assigned(cocoaItem) then
    Exit;
  cocoaWindow:= NSView(lclForm.Handle).window;
  if NOT Assigned(cocoaWindow) then
    Exit;
  cocoaWindow.makeFirstResponder( TCocoaToolBarItemSearch(cocoaItem).searchField );
end;

procedure searchAction( const Sender: id );
var
  searchField: NSSearchField absolute Sender;
begin
  if NOT IDEMainFormHandler.searchingString.IsEmpty then
    Exit;
  IDEMainFormHandler.searchingString:= searchField.stringValue.UTF8String;
  // on MainThread
  Application.QueueAsyncCall( @IDEMainFormHandler.doFindNext, 0 );
end;

procedure jumpBackAction( const Sender: id );
begin
  SourceEditorManager.JumpBackClicked( nil );
end;

procedure jumpForwardAction( const Sender: id );
begin
  SourceEditorManager.JumpForwardClicked( nil );
end;

function onGetCommandMenu: TMenuItem;
var
  menu: TMenuItem;

  function createSearchMenuItem: TMenuItem;
  begin
    Result:= TMenuItem.Create( menu );
    Result.Caption:= cocoaMFSTBISearch;
    Result.OnClick:= @IDEMainFormHandler.doFocusSearchField;
    Result.ShortCut:= ShortCut( Word('F'), [ssCtrl, ssMeta] );
  end;

begin
  menu:= TMenuItem.Create( nil );
  menu.Add( createSearchMenuItem );
  Result:= menu;
end;


const
  backItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainIDE.Back';
    priority: NSToolbarItemVisibilityPriorityHigh;
    navigational: True;
    iconName: 'arrow.left';
    title: cocoaMFSTBIJumpBack;
    tips: cocoaMFSTBIJumpBack;
    bordered: True;
    onAction: @jumpBackAction;
  );

  forwardItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainIDE.Forward';
    priority: NSToolbarItemVisibilityPriorityHigh;
    navigational: True;
    iconName: 'arrow.right';
    title: cocoaMFSTBIJumpForward;
    tips: cocoaMFSTBIJumpForward;
    bordered: True;
    onAction: @jumpForwardAction;
  );

  searchItemConfig: TCocoaConfigToolBarItemSearch = (
    identifier: 'MainIDE.Search';
    priority: NSToolbarItemVisibilityPriorityLow;
    iconName: '';
    title: cocoaMFSTBISearch;
    tips: cocoaMFSTBISearch;
    bordered: True;
    onAction: @searchAction;

    sendWhole: False;
    sendImmediately: False;
    resignsWithCancel: True;
    preferredWidth: 200;
  );

  commandItemConfig: TCocoaConfigToolBarItemMenu = (
    identifier: 'MainIDE.Command';
    iconName: 'ellipsis.circle';
    title: cocoaMFSTBICommand;
    tips: '';
    bordered: True;
    onAction: nil;

    showsIndicator: False;
    menu: nil;
    onGetMenu: @onGetCommandMenu;
  );

  // 1. for docked IDE, integrate with MainIDEBar
  // 2. for undocked IDE, integrate with MainIDEBar and SourceNotebook
  mainIDEFormConfig: TCocoaConfigForm = (
    name: 'MainIDE';
    className: 'TSourceNotebook';
    isMainForm: True;

    titleBar: (
      transparent: True;
      separatorStyle: NSTitlebarSeparatorStyleAutomatic;
    );

    toolBar: (
      identifier: 'MainIDE.ToolBar';
      style: NSWindowToolbarStyleUnifiedCompact;
      displayMode: NSToolbarDisplayModeIconOnly;

      allowsUserCustomization: False;
      autosavesConfiguration: False;

      items: (
      );
      defaultItemsIdentifiers: (
        'MainIDE.Back',
        'MainIDE.Forward',
        'MainIDE.Search',
        'MainIDE.Command'
      );
      allowedItemsIdentifiers: (
        'MainIDE.Back',
        'MainIDE.Forward',
        'MainIDE.Search',
        'MainIDE.Command'
      );
      itemCreator: nil;      // default item Creator
    );
  );

procedure initIDECocoaConfigForms;
begin
  if NSAppKitVersionNumber < NSAppKitVersionNumber11_0 then
    Exit;

  mainIDEFormConfig.toolBar.items:= [
    TCocoaToolBarUtils.toClass(backItemConfig),
    TCocoaToolBarUtils.toClass(forwardItemConfig),
    TCocoaToolBarUtils.toClass(commandItemConfig),
    TCocoaToolBarUtils.toClass(searchItemConfig)
  ];

  CocoaConfigForms:= [ mainIDEFormConfig ];

  IDEMainFormHandler:= TCocoaIDEMainFormHandler.create;
end;

end.

