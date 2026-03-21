unit CocoaWSModalService;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$include cocoadefines.inc}

interface

uses
  Classes, SysUtils,
  Forms, Menus, LCLType,
  CocoaAll,
  CocoaWSService, CocoaMenus, CocoaConst, CocoaUtils;

type

  { TCocoaWidgetSetModalService }

  TCocoaWidgetSetModalService = class
  private
    // modal session
    _modals : TList;
    _count: Integer; // the cheapest way to determine if modal window was called
                           // used in mouse handling (in callbackobject)
                           // Might not be needed, if native Modality used
  public
    function startModal(awin: NSWindow; hasMenu: Boolean): Boolean;
    procedure endModal(awin: NSWindow);
    function currentModal: NSWindow;
    function isTopModalWindow(awin: NSWindow): Boolean;
    function isModalSession: Boolean;
    property count: Integer read _count;
  public
    destructor Destroy; override;
  end;

var
  CocoaWidgetSetModalService: TCocoaWidgetSetModalService;

implementation

type

  { TCocoaModalSession }

  TCocoaModalSession = class(TObject)
  private
    _window: NSWindow;
    _session: NSModalSession;
    // recording menu state for the modality stack
    // there's no limitation for a modal window to have its own menu
    // if it override the mainMenu, we still need the information
    // to restore the previous state of the mainmenu
    _prevMenuEnabled: Boolean;
    _cocoaMenu: NSMenu;
    _lclMenu: TMenu;
  public
    constructor Create(
      const awin: NSWindow;
      const asess: NSModalSession;
      const APrevMenuEnabled: Boolean;
      const amainmenu: NSMenu;
      const ALCL: TMenu );
  end;

{ TModalSession }

constructor TCocoaModalSession.Create(
  const awin: NSWindow;
  const asess: NSModalSession;
  const APrevMenuEnabled: Boolean;
  const amainmenu: NSMenu;
  const ALCL: TMenu);
begin
  inherited Create;
  _window:= awin;
  _session:= asess;
  _prevMenuEnabled:= APrevMenuEnabled;
  _cocoaMenu:= amainmenu;
  _lclMenu:= alcl;
end;

{ TCocoaWidgetSetModalService }

function TCocoaWidgetSetModalService.startModal(awin: NSWindow; hasMenu: Boolean): Boolean;
var
  sess : NSModalSession;
begin
  Result := false;
  if not Assigned(awin) then Exit;

  sess := NSApplication(NSApp).beginModalSessionForWindow(awin);
  if not Assigned(sess) then Exit;

  if not Assigned(_modals) then _modals := TList.Create;

  TCocoaMenuUtil.trackCancelAll();

  // If a modal menu has it's menu, then SetMainMenu has already been called
  // (Show is called for modal windows prior to ShowModal. Show triggers Activate and Active is doing MainMenu)
  if not hasMenu then begin
    _modals.Add( TCocoaModalSession.Create(awin, sess, CocoaWidgetSetMenuService.mainMenuEnabled, NSApplication(NSApp).mainMenu, CocoaWidgetSetMenuService.currentLCLMenu));
    CocoaWidgetSetMenuService.mainMenuEnabled := false;
    TCocoaMenuUtil.toggleAppMenu(false); // modal menu doesn't have a window, disabling it
  end else
    // if modal window has its own menu, then the prior window is rescord in "Prev" fields
    _modals.Add( TCocoaModalSession.Create(awin, sess, CocoaWidgetSetMenuService.prevMenuEnabled, CocoaWidgetSetMenuService.prevMenu, CocoaWidgetSetMenuService.prevLCLMenu));

  Result := true;
  inc(_count);
end;

procedure TCocoaWidgetSetModalService.endModal(awin: NSWindow);
var
  ms : TCocoaModalSession;
begin
  if not Assigned(_modals) or (_modals.Count = 0) then Exit;
  ms := TCocoaModalSession(_modals[_modals.Count-1]);
  if (ms._window <> awin) then Exit;
  NSApplication(NSApp).endModalSession(ms._session);

  // restoring the menu status that was before the modality
  CocoaWidgetSetMenuService.DoSetMainMenu(ms._cocoaMenu, ms._lclMenu);
  CocoaWidgetSetMenuService.prevMenuEnabled := CocoaWidgetSetMenuService.mainMenuEnabled;
  CocoaWidgetSetMenuService.mainMenuEnabled := ms._prevMenuEnabled;
  TCocoaMenuUtil.toggleAppMenu(ms._prevMenuEnabled); // modal menu doesn't have a window, disabling it

  ms.Free;
  _modals.Delete(_modals.Count-1);

  TCocoaApplicationUtil.wakeupEventLoop;
end;

function TCocoaWidgetSetModalService.currentModal: NSWindow;
begin
  if isModalSession then begin
    Result := TCocoaModalSession(_modals[_modals.Count-1])._window;
  end else begin
    Result:= nil;
  end;
end;

function TCocoaWidgetSetModalService.isTopModalWindow(awin: NSWindow): Boolean;
begin
  if Assigned(awin) then begin
    Result:= currentModal=awin;
  end else begin
    Result:= false;
  end;
end;

function TCocoaWidgetSetModalService.isModalSession: Boolean;
begin
  Result := Assigned(_modals) and (_modals.Count > 0);
end;

destructor TCocoaWidgetSetModalService.Destroy;
begin
  FreeAndNil( _modals );
end;

initialization
  CocoaWidgetSetModalService:= TCocoaWidgetSetModalService.Create;

finalization
  FreeAndNil( CocoaWidgetSetModalService );

end.

