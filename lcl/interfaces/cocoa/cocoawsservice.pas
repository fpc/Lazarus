unit CocoaWSService;

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

  { TCocoaWidgetSetService }

  TCocoaWidgetSetService = class

  // on MacOS, the system notifies the APP to open the files by calling
  // TAppDelegate.application_openFiles(). for example, double-click
  // the associated files in Finder to open the APP.
  // at this time, the MainForm may not have been created, and the notifies
  // information about the files will be lost.
  // including Lazarus IDE itself will also be affected by this issue on startup.
  // so save it in _waitingDropFiles first, DropWaitingFiles() will be called
  // in TCocoaWidgetSet.AppRun().
  private
    _readyDropFiles: Boolean;
    _waitingDropFiles: NSMutableArray;
  public
    procedure setReadyDropFiles; inline;
    procedure dropWaitingFiles;
    procedure tryDropFiles( const filenames: NSArray );

  // collecting objects that needs to be released AFTER an event
  // has been processed
  private
    _waitingReleasedLCLObjects: TList;
  public
    procedure addToBeReleasedLCLObjects(const obj: TObject);
    procedure releaseWaitingLCLObjects(const fromIdx: integer);
    function countWaitingReleasedLCLObjects: Integer;

  // Cocoa Autorelease Main Pool
  private
    _autoreleaseMainPool : NSAutoreleasePool;
  public
    procedure initAutoreleaseMainPool;
    procedure finalAutoreleaseMainPool;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TCocoaWidgetSetMenuService }

  TCocoaWidgetSetMenuService = class
  public
    MainMenuEnabled: Boolean; // the latest main menu status
    PrevMenu : NSMenu;
    PrevLCLMenu : TMenu;
    CurLCLMenu: TMenu;
    PrevMenuEnabled: Boolean; // previous mainmenu status
    MainFormMenu: NSMenu;
  public
    procedure DoSetMainMenu(AMenu: NSMenu; ALCLMenu: TMenu);
    procedure SetMainMenu(const AMenu: HMENU; const ALCLMenu: TMenu);
  public
    destructor Destroy; override;
  end;

var
  CocoaWidgetSetService: TCocoaWidgetSetService;
  CocoaWidgetSetMenuService: TCocoaWidgetSetMenuService;

implementation

{ TCocoaWidgetSetService }

procedure TCocoaWidgetSetService.setReadyDropFiles;
begin
  _readyDropFiles:= True;
end;

procedure TCocoaWidgetSetService.dropWaitingFiles;
var
  lFiles: array of string;
  lNSStr: NSString;
  i: Integer;
begin
  if _waitingDropFiles.count = 0 then
    exit;

  SetLength(lFiles, _waitingDropFiles.count);
  for i := 0 to _waitingDropFiles.count-1 do
  begin
    lNSStr := NSString(_waitingDropFiles.objectAtIndex(i));
    lFiles[i] := NSStringToString(lNSStr);
  end;
  Application.IntfDropFiles(lFiles);
  if Application.MainForm<>nil then
    Application.MainForm.IntfDropFiles(lFiles);

  _waitingDropFiles.removeAllObjects;
end;

procedure TCocoaWidgetSetService.tryDropFiles(const filenames: NSArray);
begin
  _waitingDropFiles.addObjectsFromArray(filenames);
  if _readyDropFiles then
    dropWaitingFiles;
end;

procedure TCocoaWidgetSetService.addToBeReleasedLCLObjects(const obj: TObject);
begin
  // let's try to find an object. Do not add a duplicate
  if (_waitingReleasedLCLObjects.IndexOf(Obj)>=0) then Exit;
  _waitingReleasedLCLObjects.Add(obj);
end;

procedure TCocoaWidgetSetService.releaseWaitingLCLObjects(const fromIdx: integer);
var
  i  : integer;
begin
  for i := fromIdx to _waitingReleasedLCLObjects.Count - 1 do
  begin
    TObject(_waitingReleasedLCLObjects[i]).Free;
    _waitingReleasedLCLObjects[i]:=nil;
  end;
  _waitingReleasedLCLObjects.Pack;
end;

function TCocoaWidgetSetService.countWaitingReleasedLCLObjects: Integer;
begin
  Result := _waitingReleasedLCLObjects.Count;
end;

procedure TCocoaWidgetSetService.initAutoreleaseMainPool;
begin
  // MacOSX 10.6 reports a lot of warnings during initialization process
  // adding the autorelease pool for the whole Cocoa widgetset
  _autoreleaseMainPool:= NSAutoreleasePool.alloc.init;
end;

procedure TCocoaWidgetSetService.finalAutoreleaseMainPool;
begin
  if Assigned(_autoreleaseMainPool) then begin
    _autoreleaseMainPool.release;
    _autoreleaseMainPool := nil;
  end;
end;

constructor TCocoaWidgetSetService.Create;
begin
  _waitingDropFiles:= NSMutableArray.new;
  _waitingReleasedLCLObjects := TList.Create;
end;

destructor TCocoaWidgetSetService.Destroy;
begin
  _waitingDropFiles.release;
  _waitingReleasedLCLObjects.Free;
end;

{ TCocoaWidgetSetMenuService }

procedure TCocoaWidgetSetMenuService.DoSetMainMenu(AMenu: NSMenu; ALCLMenu: TMenu);
var
  i: Integer;
  lCurItem: TMenuItem;
  lMenuObj: NSObject;
  lCocoaMenu: TCocoaMenu absolute AMenu;
  appleMenuFound: Boolean = false;
begin
  if Assigned(PrevMenu) then PrevMenu.release;
  PrevMenu := NSApplication(NSApp).mainMenu;
  PrevMenu.retain;

  PrevLCLMenu := CurLCLMenu;
  CurLCLMenu := ALCLMenu;

  if NOT Assigned(self.MainFormMenu) or (self.MainFormMenu.numberOfItems=0) then begin
    self.MainFormMenu.release;
    self.MainFormMenu:= AMenu;
    self.MainFormMenu.retain;
  end;

  if (ALCLMenu = nil) or not ALCLMenu.HandleAllocated then begin
    lCocoaMenu:= TCocoaMenu( self.MainFormMenu );
    if NOT Assigned(lCocoaMenu) then
      lCocoaMenu:= TCocoaMenu.new.autorelease;
    NSApp.setMainMenu( lCocoaMenu );
    Exit;
  end;

  // Find the Apple menu, if the user provided any by setting the Caption to 
  // Some older docs say we should use setAppleMenu to obtain the Services/Hide/Quit items,
  // but its now private and in 10.10 it doesn't seam to do anything
  // NSApp.setAppleMenu(NSMenu(lMenuObj));
  for i := 0 to ALCLMenu.Items.Count-1 do
  begin
    lCurItem := ALCLMenu.Items.Items[i];
    if not AMenu.isKindOfClass_(TCocoaMenu) then Break;
    if not lCurItem.HandleAllocated then Continue;

    lMenuObj := NSObject(lCurItem.Handle);
    if not lMenuObj.isKindOfClass_(TCocoaMenuItem) then Continue;
    if TCocoaMenuItem(lMenuObj).isValidAppleMenu() then
    begin
      lCocoaMenu.overrideAppleMenu(TCocoaMenuItem(lMenuObj));
      appleMenuFound:= true;
      Break;
    end;
  end;

  if AMenu.isKindOfClass(TCocoaMenu) then begin
    if NOT appleMenuFound then
      lCocoaMenu.createAppleMenu();
    lCocoaMenu.attachAppleMenu();
  end;

  NSApp.setMainMenu( AMenu );
end;

procedure TCocoaWidgetSetMenuService.SetMainMenu(const AMenu: HMENU; const ALCLMenu: TMenu);
begin
  DoSetMainMenu(NSMenu(AMenu), ALCLMenu);

  PrevMenuEnabled := MainMenuEnabled;
  MainMenuEnabled := true;
  TCocoaMenuUtil.toggleAppMenu(true);
  //if not Assigned(ACustomForm.Menu) then ToggleAppMenu(false);

  // for modal windows work around bug, but doesn't work :(
  {$ifdef COCOA_USE_NATIVE_MODAL}
  {if CurModalForm <> nil then
  for i := 0 to lNSMenu.numberOfItems()-1 do
  begin
    lNSMenu.itemAtIndex(i).setTarget(TCocoaWSCustomForm.GetWindowFromHandle(CurModalForm));
  end;}
  {$endif}
end;

destructor TCocoaWidgetSetMenuService.Destroy;
begin
  self.MainFormMenu.release;
end;

initialization
  CocoaWidgetSetService:= TCocoaWidgetSetService.Create;
  CocoaWidgetSetMenuService:= TCocoaWidgetSetMenuService.Create;

finalization
  FreeAndNil( CocoaWidgetSetService );
  FreeAndNil( CocoaWidgetSetMenuService );

end.

