unit JcfIdeRegister;

{ AFS 7 Jan 2K
  JEDI Code Format IDE plugin registration }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is JcfIdeRegister, released May 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2000 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele, Juergen Kehrel

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

uses
  { delphi }
  Windows, SysUtils, Classes, ToolsAPI;


procedure Register;

implementation

uses
  { delphi }
  Menus, ActnList,
  { local }
  JcfIdeMain, Delay;

const
  NAME_MENU_TOOLS = 'ToolsMenu';
  FORMAT_MENU_NAME = 'JEDI Code &Format';
  FORMAT_CURRENT_NAME = '&Current Editor Window';
  FORMAT_PROJECT_NAME = '&All Files in Project';
  FORMAT_OPEN_NAME = 'All &Open Windows';
  FORMAT_REG_SETTINGS_MENU_NAME = '&Registry Settings';
  FORMAT_SETTINGS_MENU_NAME = '&Format Settings';
  FORMAT_ABOUT_MENU_NAME = '&About';

{ find the TMenuItem for the IDE main menu tools }
function GetToolsMenu: TMenuItem;
var
  hRes: HResult;
  lciMenuServices: INTAServices40;
  lcMainMenu: TMenu;
  lcTestMenu: TMenuItem;
  liLoop: integer;
begin
  Result := nil;

  { get the menu services }
  hRes := BorlandIDEServices.QueryInterface(INTAServices40, lciMenuServices);
  if hRes <> S_OK then
    exit;
  if lciMenuServices = nil then
    exit;

  { get the main menu }
  lcMainMenu := lciMenuServices.MainMenu;

  { get the tools menu
    find it by looking for the control name
    since this stays the same even if the display text
    has been localised/translated }
  for liLoop := 0 to lcMainMenu.Items.Count - 1 do
  begin
    lcTestMenu := lcMainMenu.Items[liLoop];
    if CompareText(lcTestMenu.Name, NAME_MENU_TOOLS) = 0 then
    begin
      result := lcTestMenu;
      break;
    end;
  end;
end;

{ the object that does all the work
  - created the first time that a JCF menu item is selected }
var
  lcJCFIDE:    TJcfIdeMain;
  { object to delay registration }
  lcDelayedRegister: TDelay;
  { count the number of times that the plugin menus have been attempted }
  miMenuTries: integer = 0;

const
  MAX_TRIES = 100;

{ called from Finalization }
procedure RemoveMenuItems;
var
  fcMainMenu: TMenuItem;

  procedure RemoveMenuItem(const psName: string);
  var
    lcItem: TMenuItem;
  begin
    Assert(psName <> '');
    Assert(fcMainMenu <> nil);

    lcItem := fcMainMenu.Find(psName);
    if lcItem <> nil then
      fcMainMenu.Remove(lcItem);
  end;

var
  fcToolsMenu: TMenuItem;
begin
  { remove any existant menu items -
    this causes crashes of not done right }
  fcToolsMenu := GetToolsMenu;
  if fcToolsMenu = nil then
    exit;

  fcMainMenu := fcToolsMenu.Find(FORMAT_MENU_NAME);

  if fcMainMenu <> nil then
  begin
    RemoveMenuItem(FORMAT_CURRENT_NAME);
    RemoveMenuItem(FORMAT_PROJECT_NAME);
    RemoveMenuItem(FORMAT_OPEN_NAME);
    RemoveMenuItem(FORMAT_SETTINGS_MENU_NAME);

    fcToolsMenu.Remove(fcMainMenu);
  end;

  FreeAndNil(fcMainMenu);
end;

function IDEActionList: TCustomActionList;
var
  NTAServices: INTAServices;
begin
  NTAServices := BorlandIDEServices as INTAServices;
  Result      := NTAServices.ActionList;
end;

procedure AddMenuItems(var pbDoAgain: boolean);
var
  fcMainMenu: TMenuItem;

  procedure AddMenuItem(const psName: string; const pcHandler: TNotifyEvent;
    const piShortCutKey: TShortCut = 0);
  var
    lcItem:   TMenuItem;
    lcAction: TAction;
  begin
    Assert(psName <> '');
    // must have a callback unless it's a seperator line
    Assert(Assigned(pcHandler) or (psName = '-'));

    lcItem := TMenuItem.Create(fcMainMenu);
    Assert(lcItem <> nil);

    if psName = '-' then
    begin
      lcItem.Caption := psName;
      lcItem.OnClick := pcHandler;
    end
    else
    begin
      lcAction := TAction.Create(fcMainMenu);
      lcAction.Category := StripHotKey(FORMAT_MENU_NAME);
      lcAction.Name := 'jcf' + StringReplace(StripHotKey(psName), ' ', '', [rfReplaceAll]) + 'Action';
      lcAction.Caption := psName;
      lcAction.OnExecute := pcHandler;
      lcAction.ActionList := IDEactionList;
      if piShortCutKey <> 0 then
        lcAction.ShortCut := piShortCutKey;

      lcItem.Action := lcAction;
    end;

    Assert(fcMainMenu <> nil);
    fcMainMenu.Add(lcItem);
  end;

var
  fcToolsMenu: TMenuItem;
  liLoop: integer;
begin
  { give up after trying several times }
  if miMenuTries >= MAX_TRIES then
    exit;

  Inc(miMenuTries);

  { this doesn't work during program startup?!? }
  fcToolsMenu := GetToolsMenu;
  if fcToolsMenu = nil then
    exit;

  { make these menu items in the Register proc, &
    free them in finalization }
  fcMainMenu := TMenuItem.Create(fcToolsMenu);
  fcMainMenu.Caption := FORMAT_MENU_NAME;

  // find first separator  jbk
  for liLoop := 0 to fcToolsMenu.Count - 1 do
    begin
      if fcToolsMenu.Items[liLoop].IsLine then
        begin
          fcToolsMenu.Insert(liLoop, fcMainMenu);
          Break;
        end;
    end;

  // is it in there ?
  fcMainMenu := fcToolsMenu.Find(FORMAT_MENU_NAME);

  if (fcMainMenu <> nil) then
  begin
    // it worked. Now add menu subitems

    AddMenuItem(FORMAT_CURRENT_NAME, lcJCFIDE.DoFormatCurrentIDEWindow,
      ShortCut((Ord('F')), [ssCtrl, ssAlt]));

    AddMenuItem(FORMAT_PROJECT_NAME, lcJCFIDE.DoFormatProject);
    AddMenuItem(FORMAT_OPEN_NAME, lcJCFIDE.DoFormatOpen);

    AddMenuItem('-', nil);
    AddMenuItem(FORMAT_REG_SETTINGS_MENU_NAME, lcJCFIDE.DoRegistrySettings);
    AddMenuItem(FORMAT_SETTINGS_MENU_NAME, lcJCFIDE.DoFormatSettings);

    AddMenuItem('-', nil);
    AddMenuItem(FORMAT_ABOUT_MENU_NAME, lcJCFIDE.DoAbout);

    // debug ShowMessage('menu add succeeded on try #' + IntToStr(miTries));
    pbDoAgain := False;
  end
  else
  begin
    // do over, a bit later
    pbDoAgain := True;
    RemoveMenuItems;
    Sleep(0);
  end;
end;

{------------------------------------------------------------
  keyboard binding
  ctrl-alt-f to format current unit 

  based on
  http://community.borland.com/article/0,1410,26389,00.html
  and http://www.gexperts.org/opentools/ }

type
  TJcfKeyBindings = class(TNotifierObject, IOTAKeyboardBinding)
  private
    procedure OnShortcut(const Context: IOTAKeyContext;
      KeyCode: TShortcut; var BindingResult: TKeyBindingResult);

  public
    { IOTAKeyboardBinding methods }
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
  end;

function TJcfKeyBindings.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

function TJcfKeyBindings.GetDisplayName: string;
begin
  Result := 'JCF';
end;

function TJcfKeyBindings.GetName: string;
begin
  Result := 'Jedi.CodeFormatter';
end;

procedure TJcfKeyBindings.BindKeyboard(const BindingServices: IOTAKeyBindingServices);
var
  liShortcut: TShortcut;
begin
  liShortcut := ShortCut((Ord('F')), [ssCtrl, ssAlt]);
  BindingServices.AddKeyBinding([liShortcut], OnShortcut, nil)
end;

procedure TJcfKeyBindings.OnShortcut(const Context: IOTAKeyContext;
  KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
begin
  lcJCFIDE.DoFormatCurrentIDEWindow(self);
end;


procedure Register;
begin
  { delayed reg. technique from sample code by Mike Remec
  http://www.miharemec.com/doc/ota-nmi.html }
  Assert(lcDelayedRegister <> nil);
  lcDelayedRegister.Proc := AddMenuItems;
  lcDelayedRegister.DoItSoon;

  (BorlandIDEServices as IOTAKeyboardServices).AddKeyboardBinding(TJcfKeyBindings.Create);
end;


initialization
  lcJCFIDE := TJcfIdeMain.Create;
  lcDelayedRegister := TDelay.Create;

finalization
  FreeAndNil(lcDelayedRegister);

  RemoveMenuItems;
  FreeAndNil(lcJCFIDE);
end.
