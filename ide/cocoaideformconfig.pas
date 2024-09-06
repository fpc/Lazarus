unit CocoaIDEFormConfig;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  Forms,
  SourceEditor, SynEditTypes,
  CocoaAll, CocoaConfig, CocoaToolBar, Cocoa_Extra, CocoaUtils;

implementation

type
  TCocoaIDEMainFormHandler = class
  private
    searchingString: String;
    procedure doFindNext(Data: PtrInt);
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

procedure searchHandle( const Sender: id );
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


const
  backItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainIDE.Back';
    priority: NSToolbarItemVisibilityPriorityHigh;
    navigational: True;
    iconName: 'arrow.left';
    title: 'Back';
    tips: 'Jump Back';
    bordered: True;
    onAction: @jumpBackAction;
  );

  forwardItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainIDE.Forward';
    priority: NSToolbarItemVisibilityPriorityHigh;
    navigational: True;
    iconName: 'arrow.right';
    title: 'Forward';
    tips: 'Jump Forward';
    bordered: True;
    onAction: @jumpForwardAction;
  );

  searchItemConfig: TCocoaConfigToolBarItemSearch = (
    identifier: 'MainIDE.Search';
    priority: NSToolbarItemVisibilityPriorityLow;
    iconName: '';
    title: 'Search';
    tips: 'Search Instantly';
    bordered: True;
    onAction: @searchHandle;

    sendWhole: False;
    sendImmediately: False;
    resignsWithCancel: True;
    preferredWidth: 200;
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
        'MainIDE.Search'
      );
      allowedItemsIdentifiers: (
        'MainIDE.Back',
        'MainIDE.Forward',
        'MainIDE.Search'
      );
      itemCreator: nil;      // default item Creator
    );
  );

procedure initCocoaConfigForms;
begin
  mainIDEFormConfig.toolBar.items:= [
    TCocoaToolBarUtils.toClass(backItemConfig),
    TCocoaToolBarUtils.toClass(forwardItemConfig),
    TCocoaToolBarUtils.toClass(searchItemConfig)
  ];

  CocoaConfigForms:= [ mainIDEFormConfig ];
end;

initialization
  initCocoaConfigForms;
  IDEMainFormHandler:= TCocoaIDEMainFormHandler.create;

end.

