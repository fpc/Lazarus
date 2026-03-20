unit CocoaWSModalService;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$include cocoadefines.inc}

interface

uses
  Classes, SysUtils,
  Forms, Menus, LCLType,
  MacOSAll, CocoaAll,
  CocoaMenus, CocoaConst, CocoaUtils, Cocoa_Extra;

type

  { TModalSession }

  TModalSession = class(TObject)
    window : NSWindow;
    sess   : NSModalSession;
    // recording menu state for the modality stack
    // there's no limitation for a modal window to have its own menu
    // if it override the mainMenu, we still need the information
    // to restore the previous state of the mainmenu
    prevMenuEnabled: Boolean;
    cocoaMenu : NSMenu;
    lclMenu   : TMenu;
    constructor Create(awin: NSWindow; asess: NSModalSession;
      APrevMenuEnabled: Boolean;
      amainmenu: NSMenu; ALCL: TMenu);
  end;


implementation

{ TModalSession }

constructor TModalSession.Create(awin: NSWindow; asess: NSModalSession;
  APrevMenuEnabled: Boolean; amainmenu: NSMenu; ALCL: TMenu);
begin
  inherited Create;
  window := awin;
  sess := asess;
  prevMenuEnabled := APrevMenuEnabled;
  cocoaMenu := amainmenu;
  lclMenu   := alcl;
end;

end.

