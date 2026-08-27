unit IdeDebuggerPlugInIntf;
{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Abstract:
   Interface for the package IdeDebugger

 The package defines common base interfaces for extensions to the IdeDebugger.

 Each extension has its own sub-classes.

 Any Plugin to the IDE-Debugger is represented by

 - a sub class of TLazDbgIdePlugInRegistryEntry.
   This class is not for instantiation. It just registers Name, ID and a
   method to return one or more interfaces to the plugin

 - an interface derived from ILazDbgIdePlugIn
   This provides all the functionality of the Plugin.
   Each extension has its own base-class derived from this, with methods specific
   to the extension.

 - Additional/optional interfaces for specific tasks
   Those can be retrieved via
     ILazDbgIdePlugIn.GetInterface(...)

   - ILazDbgIdePlugInConfiguration
   - ILazDbgIdePlugInXmlConfiguration
     Providing the ability to change configuration for the plugin

}

{$mode objfpc}{$H+}
{$INTERFACES CORBA}
{$ModeSwitch typehelpers}

interface

uses
  Classes, SysUtils, fgl, Laz2_XMLCfg, Forms;

type

  TNullableBool = (nbUnknown, nbTrue, nbFalse);
  TFrameClass = class of TFrame;
  TLazDbgIdePlugInRegistryEntry = class;
  TLazDbgIdePlugInRegistryEntryClass = class of TLazDbgIdePlugInRegistryEntry;

  (* ILazDbgIdePlugIn
     An instance of the plugin.
     Depending on the extension, each registered TLazDbgIdePlugInRegistryEntry may
     have exactly one such instance.
     Or it can have zero, one or more, which may be either pre-set, or created by
     the user.
     Each instance is gotten by calling TLazDbgIdePlugInRegistryEntry.CreateIdePlugin
     and then applying the relevant settings.
  *)

  ILazDbgIdePlugIn = interface ['{CEE39919-9F31-49DC-B1B8-10ED2C31126A}']
    (* GetDisplayName: The name of the instance. If there is only one
       instance per registration entry, then this may be the same as
       TLazDbgIdePlugInRegistryEntry.GetDisplayName
    *)
    function  GetDisplayName: String;
    (* Free
       Release the instance *)
    procedure Free;
    function  GetRegistryEntry: TLazDbgIdePlugInRegistryEntryClass;
    (* GetInterface
       Get optional interfaces *)
    function  GetInterface(const iidstr: shortstring; out obj): boolean; // provided by TObject
  end;

  TLazDbgIdePlugInRegistryEntry = class abstract
  public
    class function CreateIdePlugIn: ILazDbgIdePlugIn; virtual; abstract;
    class function GetPlugInId: String; virtual; abstract;
    class function GetDisplayName: String; virtual; abstract;
  end;

  { TLazDbgIdePlugInRegistry }

  TLazDbgIdePlugInRegistry = class
  protected
    function GetIdePlugin(AnIndex: integer): TLazDbgIdePlugInRegistryEntryClass; virtual; abstract;
    function GetIdePluginById(const AnId: String): TLazDbgIdePlugInRegistryEntryClass; virtual; abstract;
  public
    function IndexOfPlugInId(const AnId: String): Integer; virtual; abstract;
    function Count: integer; virtual; abstract;

    property  IdePlugin[AnIndex: integer]: TLazDbgIdePlugInRegistryEntryClass read GetIdePlugin; default;
    property  IdePluginById[AnId: string]: TLazDbgIdePlugInRegistryEntryClass read GetIdePluginById;
  end;

  (* *****
   *
   * Generics
   *
   ***** *)

  { TGenLazDbgIdePlugIn }

  generic TGenLazDbgIdePlugIn<BASE: class; REG_ENTRY: TLazDbgIdePlugInRegistryEntry> = class(BASE)
  private type
    REG_ENTRY_C = class of REG_ENTRY;
  protected
    function  GetDisplayName: String; virtual;
    function  GetRegistryEntry: TLazDbgIdePlugInRegistryEntryClass; virtual;
  end;

  { TGenLazDbgIdeConsoleWindowPlugInRegistry }

  generic TGenLazDbgIdeConsoleWindowPlugInRegistry<ENTRY_C: TLazDbgIdePlugInRegistryEntry> =
    class(TLazDbgIdePlugInRegistry)
  protected type
    TRegistrationEntryClass = class of ENTRY_C;
  private type
    TEntryList = specialize TFPGList<TRegistrationEntryClass>;
  private
    FEntries: TEntryList;
    FRegistrationErrors: String;
  protected
    procedure AddRegistrationError(AnError: string);
    function CanRegister(AnEntry: TRegistrationEntryClass): Boolean; virtual;
    function ArePluginIdsEqual(const AnId1, AnId2: String): Boolean; virtual;
    function GetSpecializedIdePlugin(AnIndex: integer): TRegistrationEntryClass; virtual;
    function GetSpecializedIdePluginById(AnId: string): TRegistrationEntryClass; virtual;
    function GetIdePlugin(AnIndex: integer): TLazDbgIdePlugInRegistryEntryClass; override; final;
    function GetIdePluginById(const AnId: String): TLazDbgIdePlugInRegistryEntryClass; override; final;
  public
    destructor Destroy; override;
    procedure RegisterPlugIn(AnEntry: TRegistrationEntryClass); virtual;
    procedure UnregisterPlugIn(AnEntry: TRegistrationEntryClass); virtual;

    function IndexOfPlugInId(const AnId: String): Integer; override;
    function Count: integer; override;

    property IdePlugin[AnIndex: integer]: TRegistrationEntryClass read GetSpecializedIdePlugin; default;
    property IdePluginById[AnId: string]: TRegistrationEntryClass read GetSpecializedIdePluginById;
    property RegistrationErrors: String read FRegistrationErrors;
  end;

  (* *****
     *
     * Configuration
     *
     * If a plugin has configuration then it must return an interface of type
     *    ILazDbgIdePlugInConfiguration
     * when ask via ILazDbgIdePlugIn.GetInterface
     *
     * If the configuration should be persistent, then it needs to either (or both)
     *
     * - Return an object with published properties, for use in TRttiXmlConfig
     * - Return an interface of type ILazDbgIdePlugInXmlConfiguration
     *   when asked via ILazDbgIdePlugInConfiguration.GetInterface
     *   (Note this is queried from the config, not from the plugin)
     *
     *
     ***** *)

  ILazDbgIdePlugInConfiguration = interface ['{48487C9E-C904-4E0E-AB27-1B21E3BB015C}']
    (* GetSettingsFrameClass
       This frame will be instantiated by the IDE to show and edit config.
       This frame must implement  ILazDbgIdePlugInSettingsFrameIntf interface, and
       must be able to sync with the config interface
    *)
    function GetSettingsFrameClass: TFrameClass;
    (* GetConfigObject
       for TRttiXmlConfig.ReadObject/WriteObject
       This object is owned by the interface
    *)
    function  GetConfigObject: TObject;
    (* CreateCopy
       E.g. the options dialog edits a copy, so Cancel can discard it untouched
    *)
    function  CreateCopy: ILazDbgIdePlugInConfiguration;
    (* FreeCopy
       Free a copy gotten with CreateCopy
       The original interface gotten via ILazDbgIdePlugIn.GetInterface is owned by
       the ILazDbgIdePlugIn.
    *)
    procedure FreeCopy;
    (* Reset: set all values to default / called before loading*)
    procedure ResetOptions;
    procedure AssignOptions(ASource: ILazDbgIdePlugInConfiguration);
    (* CompareOptions: True if equal.
       AnOther may be nil, then this should compare to default
    *)
    function  CompareOptions(AnOther: ILazDbgIdePlugInConfiguration): TNullableBool;

    function  GetInterface(const iidstr: shortstring; out obj): boolean; // provided by TObject
  end;

  ILazDbgIdePlugInXmlConfiguration = interface ['{470F06DF-88A6-4D69-AF1A-C9C6EBCB8A81}']
    procedure LoadDataFromXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);
  end;

  (* ILazDbgIdePlugInSettingsFrameIntf
     Implemented by the TFrame class returned from GetSettingsFrameClass. *)

  ILazDbgIdePlugInSettingsFrameIntf = interface ['{40FF147C-D4E2-4D55-A959-78F35851BE76}']
    procedure ReadFrom(APlugIn: ILazDbgIdePlugInConfiguration);
    function  WriteTo(APlugIn: ILazDbgIdePlugInConfiguration): Boolean;
  end;

  { TGenLazDbgIdePlugInConfiguration }

  generic TGenLazDbgIdePlugInConfiguration<BASE: class> = class(BASE)
  protected
    function  GetSettingsFrameClass: TFrameClass;
    function  GetConfigObject: TObject;
    function  CreateCopy: ILazDbgIdePlugInConfiguration;
    procedure FreeCopy;
    procedure ResetOptions;
    procedure AssignOptions(ASource: ILazDbgIdePlugInConfiguration);
    function  CompareOptions(AnOther: ILazDbgIdePlugInConfiguration): TNullableBool;
  end;

  (* *****
     *
     * Helper
     *
     ***** *)

  { TLazDbgIdePlugInHelper }

  TLazDbgIdePlugInHelper = type helper for ILazDbgIdePlugIn
    function GetConfiguration: ILazDbgIdePlugInConfiguration;
    function GetXmlConfiguration: ILazDbgIdePlugInXmlConfiguration;
    function GetConfigObject: TObject; experimental;
  end;

  { TLazDbgIdePlugInConfigurationHelper }

  TLazDbgIdePlugInConfigurationHelper = type helper for ILazDbgIdePlugInConfiguration
    function GetXmlConfiguration: ILazDbgIdePlugInXmlConfiguration;
  end;

implementation

{ TGenLazDbgIdePlugIn }

function TGenLazDbgIdePlugIn.GetDisplayName: String;
begin
  Result := REG_ENTRY.GetDisplayName;
end;

function TGenLazDbgIdePlugIn.GetRegistryEntry: TLazDbgIdePlugInRegistryEntryClass;
begin
  Result := REG_ENTRY;
end;

{ TGenLazDbgIdeConsoleWindowPlugInRegistry }

function TGenLazDbgIdeConsoleWindowPlugInRegistry.GetSpecializedIdePlugin(AnIndex: integer
  ): TRegistrationEntryClass;
begin
  Result := nil;
  if FEntries <> nil then
    Result := FEntries[AnIndex];
end;

function TGenLazDbgIdeConsoleWindowPlugInRegistry.GetSpecializedIdePluginById(AnId: string
  ): TRegistrationEntryClass;
var
  i: Integer;
begin
  i := IndexOfPlugInId(AnId);
  if i >= 0 then
    Result := GetSpecializedIdePlugin(i)
  else
    Result := nil;
end;

procedure TGenLazDbgIdeConsoleWindowPlugInRegistry.AddRegistrationError(AnError: string);
begin
  if FRegistrationErrors <> '' then
    FRegistrationErrors := FRegistrationErrors + LineEnding;
  FRegistrationErrors := FRegistrationErrors + AnError;
end;

function TGenLazDbgIdeConsoleWindowPlugInRegistry.CanRegister(AnEntry: TRegistrationEntryClass): Boolean;
begin
  Result := False;
  if AnEntry = nil then begin
    AddRegistrationError('Can''t register nil entry');
    exit;
  end;
  if IndexOfPlugInId(AnEntry.GetPlugInId) >= 0 then begin
    AddRegistrationError('Duplicate Plugin: ' + AnEntry.GetPlugInId);
    exit;
  end;
  Result := True;
end;

function TGenLazDbgIdeConsoleWindowPlugInRegistry.ArePluginIdsEqual(const AnId1, AnId2: String
  ): Boolean;
begin
  Result := SameText(AnId1, AnId2);
end;

function TGenLazDbgIdeConsoleWindowPlugInRegistry.GetIdePlugin(AnIndex: integer
  ): TLazDbgIdePlugInRegistryEntryClass;
begin
  Result := GetSpecializedIdePlugin(AnIndex);
end;

function TGenLazDbgIdeConsoleWindowPlugInRegistry.GetIdePluginById(const AnId: String
  ): TLazDbgIdePlugInRegistryEntryClass;
begin
  Result := GetSpecializedIdePluginById(AnId);
end;

destructor TGenLazDbgIdeConsoleWindowPlugInRegistry.Destroy;
begin
  inherited Destroy;
  FEntries.Free;
end;

procedure TGenLazDbgIdeConsoleWindowPlugInRegistry.RegisterPlugIn(AnEntry: TRegistrationEntryClass);
begin
  if not CanRegister(AnEntry) then
    exit;
  if FEntries = nil then
    FEntries := TEntryList.Create;
  FEntries.Add(AnEntry);
end;

procedure TGenLazDbgIdeConsoleWindowPlugInRegistry.UnregisterPlugIn(AnEntry: TRegistrationEntryClass);
begin
  if FEntries <> nil then
    FEntries.Remove(AnEntry);
end;

function TGenLazDbgIdeConsoleWindowPlugInRegistry.IndexOfPlugInId(const AnId: String): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (not ArePluginIdsEqual(FEntries[Result].GetPlugInId, AnId)) do
    dec(Result);
end;

function TGenLazDbgIdeConsoleWindowPlugInRegistry.Count: integer;
begin
  if FEntries <> nil then
    Result := FEntries.Count
  else
    Result := 0;
end;

{ TGenLazDbgIdePlugInConfiguration }

function TGenLazDbgIdePlugInConfiguration.GetSettingsFrameClass: TFrameClass;
begin
  Result := nil;
end;

function TGenLazDbgIdePlugInConfiguration.GetConfigObject: TObject;
begin
  Result := nil;
end;

function TGenLazDbgIdePlugInConfiguration.CreateCopy: ILazDbgIdePlugInConfiguration;
begin
  Result := nil;
end;

procedure TGenLazDbgIdePlugInConfiguration.FreeCopy;
begin
  //
end;

procedure TGenLazDbgIdePlugInConfiguration.ResetOptions;
begin
  //
end;

procedure TGenLazDbgIdePlugInConfiguration.AssignOptions(ASource: ILazDbgIdePlugInConfiguration);
begin
  //
end;

function TGenLazDbgIdePlugInConfiguration.CompareOptions(AnOther: ILazDbgIdePlugInConfiguration
  ): TNullableBool;
begin
  Result := nbUnknown;
end;

{ TLazDbgIdePlugInHelper }

function TLazDbgIdePlugInHelper.GetConfiguration: ILazDbgIdePlugInConfiguration;
begin
  if not GetInterface(ILazDbgIdePlugInConfiguration, Result) then
    Result := nil;
end;

function TLazDbgIdePlugInHelper.GetXmlConfiguration: ILazDbgIdePlugInXmlConfiguration;
var
  C: ILazDbgIdePlugInConfiguration;
begin
  C := GetConfiguration;
  if (C = nil) or not C.GetInterface(ILazDbgIdePlugInXmlConfiguration, Result) then
    Result := nil;
end;

function TLazDbgIdePlugInHelper.GetConfigObject: TObject;
var
  c: ILazDbgIdePlugInConfiguration;
begin
  Result := nil;
  c := GetConfiguration;
  if c <> nil then
    Result := c.GetConfigObject;
end;

{ TLazDbgIdePlugInConfigurationHelper }

function TLazDbgIdePlugInConfigurationHelper.GetXmlConfiguration: ILazDbgIdePlugInXmlConfiguration;
begin
  if not GetInterface(ILazDbgIdePlugInXmlConfiguration, Result) then
    Result := nil;
end;

end.

