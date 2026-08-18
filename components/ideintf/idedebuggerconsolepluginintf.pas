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
  fgl, SysUtils, LazDebuggerIntfBaseTypes;

type

  (* ILazDbgIdePlugIn
     Base for anything registered as a user-selectable IDE debugger plug-in.
     Carries only what the options dialog needs: the config object, a copy for
     Cancel, and disposal. CORBA interfaces are not reference counted, so Free
     is explicit. *)

  ILazDbgIdePlugIn = interface ['{7A1C4E9D-2B85-4F31-9C0E-3D6A85B41E27}']
    // for TXmlConfig.WriteObject / must have all config in published fields
    function  GetConfigObject: TObject;
    // the options dialog edits a copy, so Cancel can discard it untouched
    function  CreateCopy: ILazDbgIdePlugIn;
    procedure Free;
    function  GetInterface(const iidstr: shortstring; out obj): boolean; // provided by TObject
  end;

  (* ILazDbgIdePlugInSettingsFrameIntf
     Implemented by the TFrame class returned from GetSettingsFrameClass. *)

  ILazDbgIdePlugInSettingsFrameIntf = interface ['{1F4B93A6-58C7-40D2-B1E5-9A2C6F0D7B34}']
    procedure ReadFrom(APlugIn: ILazDbgIdePlugIn);
    function  WriteTo(APlugIn: ILazDbgIdePlugIn): Boolean;
  end;

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

  (* TLazDbgIdePlugInRegistryEntry
     Class-level identity and metadata, so the options dialog can list and
     describe a plug-in without constructing one. *)

  TLazDbgIdePlugInRegistryEntry = class
  public
    class function CreateIdePlugIn: ILazDbgIdePlugIn; virtual; abstract;
    class function GetSettingsFrameClass: TClass; virtual; // class(TFrame, ILazDbgIdePlugInSettingsFrameIntf)
    class function GetDisplayName: String; virtual; abstract;
    (* Stable id, persisted in XmlConfig and in project files. "package/class",
       optionally with a third part; see IsValidLazDbgIdePlugInId. A bare class
       name would collide between two packages that happened to choose the
       same one, which is what the package part is for. *)
    class function GetPlugInId: String; virtual; abstract;
  end;

  { TLazDbgIdeConsoleWindowPlugInRegistryEntry }

  TLazDbgIdeConsoleWindowPlugInRegistryEntry = class(TLazDbgIdePlugInRegistryEntry)
  public
    class function CreateIdeConsoleWindowPlugIn: ILazDbgIdeConsoleWindowPlugIn; virtual; abstract;
    class function CreateIdePlugIn: ILazDbgIdePlugIn; override;
  end;
  TLazDbgIdeConsoleWindowPlugInRegistryEntryClass = class of TLazDbgIdeConsoleWindowPlugInRegistryEntry;

  { TLazDbgIdeConsoleWindowPlugInRegistry }

  TLazDbgIdeConsoleWindowPlugInRegistry = class(specialize TFPGList<TLazDbgIdeConsoleWindowPlugInRegistryEntryClass>)
  public
    procedure RegisterPlugIn(AEntry: TLazDbgIdeConsoleWindowPlugInRegistryEntryClass);
    procedure UnregisterPlugIn(AEntry: TLazDbgIdeConsoleWindowPlugInRegistryEntryClass);
    // ids are compared case-insensitively: differing only in case must not be
    // able to pass itself off as another plug-in
    function  IndexOfPlugInId(const AId: String): Integer;
    function  FindByPlugInId(const AId: String): TLazDbgIdeConsoleWindowPlugInRegistryEntryClass;
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

{ TLazDbgIdePlugInRegistryEntry }

class function TLazDbgIdePlugInRegistryEntry.GetSettingsFrameClass: TClass;
begin
  Result := nil;
end;

{ TLazDbgIdeConsoleWindowPlugInRegistryEntry }

class function TLazDbgIdeConsoleWindowPlugInRegistryEntry.CreateIdePlugIn: ILazDbgIdePlugIn;
begin
  Result := CreateIdeConsoleWindowPlugIn;
end;

{ TLazDbgIdeConsoleWindowPlugInRegistry }

procedure TLazDbgIdeConsoleWindowPlugInRegistry.RegisterPlugIn(
  AEntry: TLazDbgIdeConsoleWindowPlugInRegistryEntryClass);
begin
  if AEntry = nil then
    exit;
  Assert(IsValidLazDbgIdePlugInId(AEntry.GetPlugInId),
    'TLazDbgIdeConsoleWindowPlugInRegistry.RegisterPlugIn: bad id "' + AEntry.GetPlugInId + '"');
  Assert(IndexOfPlugInId(AEntry.GetPlugInId) < 0,
    'TLazDbgIdeConsoleWindowPlugInRegistry.RegisterPlugIn: duplicate id "' + AEntry.GetPlugInId + '"');
  if IndexOf(AEntry) < 0 then
    Add(AEntry);
end;

procedure TLazDbgIdeConsoleWindowPlugInRegistry.UnregisterPlugIn(
  AEntry: TLazDbgIdeConsoleWindowPlugInRegistryEntryClass);
var
  i: Integer;
begin
  i := IndexOf(AEntry);
  if i >= 0 then
    Delete(i);
end;

function TLazDbgIdeConsoleWindowPlugInRegistry.IndexOfPlugInId(const AId: String): Integer;
begin
  for Result := 0 to Count - 1 do
    if SameText(Items[Result].GetPlugInId, AId) then
      exit;
  Result := -1;
end;

function TLazDbgIdeConsoleWindowPlugInRegistry.FindByPlugInId(const AId: String
  ): TLazDbgIdeConsoleWindowPlugInRegistryEntryClass;
var
  i: Integer;
begin
  i := IndexOfPlugInId(AId);
  if i >= 0 then
    Result := Items[i]
  else
    Result := nil;
end;

finalization
  FreeAndNil(TheConsoleWindowPlugIns);
end.
