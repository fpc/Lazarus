{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Abstract:
   Registration interface for debug console window plug-ins.

   A debug console window is the sink that shows a debuggee's captured console
   output and feeds typed input back to its stdin. The IDE ships one, and any
   package may register an alternative -- an ANSI terminal, a relay to an
   external tool, a target-specific console over a serial channel.

   Exactly one registered plug-in is active at a time and the user chooses it,
   rather than the first registration claiming the stream. That is the whole
   reason this is a selection and not a handler chain: with two console
   packages installed, registration order is neither visible to the user nor
   changeable by them.

   Debugger state changes are deliberately NOT part of this interface. They
   are generic -- other extensions want them too -- and there is only one base
   class to inherit, so they belong in a registration of their own.
}
unit IdeDebuggerConsolePlugInIntf;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  fgl, SysUtils, IdeDebuggerPlugInIntf, LazDebuggerIntfBaseTypes;

type

  (* ILazDbgIdeTargetIoHook
     Handed to the plug-in when it is added to the IDE's hook, and the only way
     it talks back. An interface rather than events so it can gain methods
     later without changing what plug-ins must implement.

     NotifyDidAutoShow exists so the IDE, not the plug-in, owns "has it already
     popped up this session". The IDE sets the policy through SetAutoShowState
     and is told when the plug-in acted on it; nothing is queried per chunk of
     output, because a debuggee can produce a great deal of it. *)

  ILazDbgIdeTargetIoHook = interface ['{63D0A72E-1948-4C5B-8E7F-2A9B4D31C6F8}']
    procedure SendInput(const AText: String);
    procedure NotifyDidAutoShow;
  end;

  (* ILazDbgIdeConsoleWindowPlugIn
     The console window itself.

     Two pairs of lifecycle calls, deliberately not collapsed into one:
     - HandleUserSelectedAsActive / HandleUserDeselectedFromActive fire when the
       user changes the selection, with or without a debug session.
     - ProcessAddedToPlugInHook / ProcessRemovedFromPlugInHook fire when the IDE
       actually attaches the stream.
     Merging them is what leaves a deselected window docked and mute. *)

  ILazDbgIdeConsoleWindowPlugIn = interface(ILazDbgIdePlugIn) ['{0E8F5C13-7D26-4A94-B3C8-5F1A9E27D0B6}']
    procedure HandleUserSelectedAsActive;
    procedure HandleUserDeselectedFromActive;
    procedure ProcessAddedToPlugInHook(AHook: ILazDbgIdeTargetIoHook);
    procedure ProcessRemovedFromPlugInHook;
    // the user picked the plug-in's menu entry
    procedure HandleUserShow;
    // called at session init; separate from the lifecycle pairs on purpose, so
    // "empty the display" does not imply a state change
    procedure Clear;
    procedure AddOutput(AChannel: TLzDbgTargetIoChannel; const AText: String);
    procedure BringToFront;
    procedure SetAutoShowState(AShowOnInput: Boolean);
  end;

  { TLazDbgIdeConsoleWindowPlugInRegistryEntry }

  TLazDbgIdeConsoleWindowPlugInRegistryEntry = class(TLazDbgIdePlugInRegistryEntry)
  protected
    class function CreateIdeConsoleWindowPlugIn: ILazDbgIdeConsoleWindowPlugIn; virtual; abstract;
  public
    class function CreateIdePlugIn: ILazDbgIdeConsoleWindowPlugIn; override; final;
  end;
  TLazDbgIdeConsoleWindowPlugInRegistryEntryClass = class of TLazDbgIdeConsoleWindowPlugInRegistryEntry;

  { TLazDbgIdeConsoleWindowPlugInRegistry }

  TLazDbgIdeConsoleWindowPlugInRegistry = class(specialize TGenLazDbgIdeConsoleWindowPlugInRegistry<TLazDbgIdeConsoleWindowPlugInRegistryEntry>)
  protected
    function CanRegister(AnEntry: TRegistrationEntryClass): Boolean; override;
  end;

function ConsoleWindowPlugIns: TLazDbgIdeConsoleWindowPlugInRegistry;

(* "package/class", or "package/class/part3". Each part a Pascal identifier.
   Restrictions can be relaxed later, but not tightened, so this is the widest
   form that still guarantees a usable id. *)
function IsValidLazDbgIdePlugInId(const AId: String): Boolean;

implementation

var
  TheConsoleWindowPlugIns: TLazDbgIdeConsoleWindowPlugInRegistry = nil;

function ConsoleWindowPlugIns: TLazDbgIdeConsoleWindowPlugInRegistry;
begin
  if TheConsoleWindowPlugIns = nil then
    TheConsoleWindowPlugIns := TLazDbgIdeConsoleWindowPlugInRegistry.Create;
  Result := TheConsoleWindowPlugIns;
end;

function IsValidLazDbgIdePlugInIdPart(const APart: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if APart = '' then
    exit;
  if not (APart[1] in ['a'..'z', 'A'..'Z', '_']) then
    exit;
  for i := 2 to Length(APart) do
    if not (APart[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) then
      exit;
  Result := True;
end;

function IsValidLazDbgIdePlugInId(const AId: String): Boolean;
var
  p1, p2: Integer;
begin
  Result := False;
  p1 := Pos('/', AId);
  if p1 < 2 then
    exit;
  if not IsValidLazDbgIdePlugInIdPart(Copy(AId, 1, p1 - 1)) then
    exit;

  p2 := Pos('/', AId, p1 + 1);
  if p2 = 0 then
    exit(IsValidLazDbgIdePlugInIdPart(Copy(AId, p1 + 1, Length(AId))));

  if not IsValidLazDbgIdePlugInIdPart(Copy(AId, p1 + 1, p2 - p1 - 1)) then
    exit;
  // a third part is allowed; a fourth is not
  Result := IsValidLazDbgIdePlugInIdPart(Copy(AId, p2 + 1, Length(AId)))
        and (Pos('/', AId, p2 + 1) = 0);
end;

{ TLazDbgIdeConsoleWindowPlugInRegistryEntry }

class function TLazDbgIdeConsoleWindowPlugInRegistryEntry.CreateIdePlugIn: ILazDbgIdeConsoleWindowPlugIn;
begin
  Result := CreateIdeConsoleWindowPlugIn;
end;

{ TLazDbgIdeConsoleWindowPlugInRegistry }

function TLazDbgIdeConsoleWindowPlugInRegistry.CanRegister(AnEntry: TRegistrationEntryClass
  ): Boolean;
begin
  Result := inherited CanRegister(AnEntry);
  if not Result then
    exit;
  Result := IsValidLazDbgIdePlugInId(AnEntry.GetPlugInId);
  if not Result then
    AddRegistrationError('Invalid PluginId: ' + AnEntry.GetPlugInId);
end;

finalization
  FreeAndNil(TheConsoleWindowPlugIns);
end.
