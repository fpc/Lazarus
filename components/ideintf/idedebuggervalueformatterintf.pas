{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Abstract:
   Interface for the package IdeDebugger
}
unit IdeDebuggerValueFormatterIntf experimental;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses fgl, SysUtils, LazDebuggerIntf, IdeDebuggerWatchValueIntf,
  DbgIntfDebuggerBase;

type

  TLazDbgIdeValFormatterFeature = (
    vffFormatValue,    // FormatValue() for IWatchResultDataIntf
    vffFormatOldValue  // FormatValue() for older backends TDBGType
  ) experimental;
  TLazDbgIdeValFormatterFeatures = set of TLazDbgIdeValFormatterFeature;

  ILazDbgIdeValueFormatterIntf = interface
    ['{AE8A0E22-E052-4C77-AD88-8812D27F3180}']

    (* *** Experimental - This interface will still change *** *)

    function FormatValue(AWatchValue: IWatchResultDataIntf;
                         ADisplayFormat: TWatchDisplayFormat;
                         AWatchResultPrinter: IWatchResultPrinter;
                         out APrintedValue: String
                        ): Boolean; experimental;

    function FormatValue(aDBGType: TDBGType;
                         aValue: string;
                         ADisplayFormat: TWatchDisplayFormat;
                         out APrintedValue: String
                        ): boolean; experimental; deprecated 'For values from older backends only - to be removed as backends are upgraded';

    function SupportedFeatures: TLazDbgIdeValFormatterFeatures;

    // Config
    function  GetObject: TObject;  // for TXmlConfig.WriteObject / must have all config in published fields
    function  GetDefaultsObject: TObject;  // for TXmlConfig.WriteObject / all published fields with DEFAULT values
    function CreateCopy: ILazDbgIdeValueFormatterIntf;
    procedure Free;
  end;

  (* ILazDbgIdeValueFormatterSettingsFrameIntf
     interface that must be implemented by the TFrame class returned by GetSettingsFrameClass
  *)

  ILazDbgIdeValueFormatterSettingsFrameIntf = interface
    ['{83CDB4A1-6B32-44F0-B225-7F591AE06497}']
    procedure ReadFrom(AFormatter: ILazDbgIdeValueFormatterIntf);
    function  WriteTo(AFormatter: ILazDbgIdeValueFormatterIntf): Boolean;
  end;

  (* TLazDbgIdeValueFormatterRegistryEntry
     Class of a value formatter.
     The user can create any amount of configurable formatters from this.
  *)

  TLazDbgIdeValueFormatterRegistryEntry = class
  public
    class function CreateValueFormatter: ILazDbgIdeValueFormatterIntf; virtual; abstract;
    class function GetSettingsFrameClass: TClass; virtual; // class(TFrame, ILazDbgIdeValueFormatterSettingsFrameIntf)
    class function GetDisplayName: String; virtual; abstract;
    class function GetClassName: String; virtual; abstract; // Used in XmlConfig
  end;
  TLazDbgIdeValueFormatterRegistryEntryClass = class of TLazDbgIdeValueFormatterRegistryEntry;

  (* TLazDbgIdeValueFormatterRegistry
     List of create-able value formatter classes.
  *)

  TLazDbgIdeValueFormatterRegistry = class(specialize TFPGList<TLazDbgIdeValueFormatterRegistryEntryClass>)
  public
    function FindByFormatterClassName(AName: String): TLazDbgIdeValueFormatterRegistryEntryClass;
  end;

  { TLazDbgIdeValueFormatterGeneric }

  generic TLazDbgIdeValueFormatterGeneric<_BASE: TObject> = class(_BASE, ILazDbgIdeValueFormatterIntf)
  private type
    TLazDbgIdeValueFormatterGenericClass = class of TLazDbgIdeValueFormatterGeneric;
  protected
    function GetNewInstance: TLazDbgIdeValueFormatterGeneric; virtual;
    procedure Init; virtual;
    function GetObject: TObject; virtual;
    function GetDefaultsObject: TObject; virtual;
    function CreateCopy: ILazDbgIdeValueFormatterIntf;
    procedure Assign(AnOther: TObject); virtual;
    procedure DoFree; virtual;
    procedure ILazDbgIdeValueFormatterIntf.Free = DoFree;
  public
    constructor Create; // only call init, if overridden can be skipped if init is called directly
    function FormatValue(AWatchValue: IWatchResultDataIntf;
                         ADisplayFormat: TWatchDisplayFormat;
                         AWatchResultPrinter: IWatchResultPrinter;
                         out APrintedValue: String
                        ): Boolean; virtual; experimental;

    function FormatValue(aDBGType: TDBGType;
                         aValue: string;
                         ADisplayFormat: TWatchDisplayFormat;
                         out APrintedValue: String
                        ): boolean; virtual; experimental; deprecated 'For values from older backends only - to be removed as backends are upgraded';

    function SupportedFeatures: TLazDbgIdeValFormatterFeatures; virtual;
  end;

  { TLazDbgIdeValueFormatterRegistryEntryGeneric }

  generic TLazDbgIdeValueFormatterRegistryEntryGeneric<_Formatter> = class(TLazDbgIdeValueFormatterRegistryEntry)
  public
    class function CreateValueFormatter: ILazDbgIdeValueFormatterIntf; override;
    class function GetClassName: String; override;
    class function GetDisplayName: String; override; // calls GetRegisteredDisplayName on the _Formatter
  end;

  { TLazDbgIdeValueFormatterFrameRegistryEntryGeneric }

  generic TLazDbgIdeValueFormatterFrameRegistryEntryGeneric<_Formatter, _Frame> = class(specialize TLazDbgIdeValueFormatterRegistryEntryGeneric<_Formatter>)
    class function GetSettingsFrameClass: TClass; override;
  end;


function ValueFormatterRegistry: TLazDbgIdeValueFormatterRegistry;

implementation

var
  TheValueFormatterRegistry: TLazDbgIdeValueFormatterRegistry;

function ValueFormatterRegistry: TLazDbgIdeValueFormatterRegistry;
begin
  if TheValueFormatterRegistry = nil then
    TheValueFormatterRegistry := TLazDbgIdeValueFormatterRegistry.Create;
  Result := TheValueFormatterRegistry;
end;

{ TLazDbgIdeValueFormatterRegistryEntry }

class function TLazDbgIdeValueFormatterRegistryEntry.GetSettingsFrameClass: TClass;
begin
  Result := nil;
end;

{ TLazDbgIdeValueFormatterRegistry }

function TLazDbgIdeValueFormatterRegistry.FindByFormatterClassName(AName: String
  ): TLazDbgIdeValueFormatterRegistryEntryClass;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].GetClassName = AName then
      exit(Items[i]);
end;

{ TLazDbgIdeValueFormatterGeneric }

function TLazDbgIdeValueFormatterGeneric.GetNewInstance: TLazDbgIdeValueFormatterGeneric;
begin
  Result := TLazDbgIdeValueFormatterGenericClass(ClassType).Create;
end;

procedure TLazDbgIdeValueFormatterGeneric.Init;
begin
  //
end;

function TLazDbgIdeValueFormatterGeneric.GetObject: TObject;
begin
  Result := Self;
end;

function TLazDbgIdeValueFormatterGeneric.GetDefaultsObject: TObject;
begin
  Result := GetNewInstance;
end;

procedure TLazDbgIdeValueFormatterGeneric.Assign(AnOther: TObject);
begin
  //
end;

function TLazDbgIdeValueFormatterGeneric.CreateCopy: ILazDbgIdeValueFormatterIntf;
var
  r: TLazDbgIdeValueFormatterGeneric;
begin
  r := GetNewInstance;
  r.Assign(Self);
  Result := r;
end;

procedure TLazDbgIdeValueFormatterGeneric.DoFree;
begin
  Destroy;
end;

constructor TLazDbgIdeValueFormatterGeneric.Create;
begin
  Init;
end;

function TLazDbgIdeValueFormatterGeneric.FormatValue(
  AWatchValue: IWatchResultDataIntf; ADisplayFormat: TWatchDisplayFormat;
  AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String): Boolean;
begin
  Result := False;
end;

function TLazDbgIdeValueFormatterGeneric.FormatValue(aDBGType: TDBGType;
  aValue: string; ADisplayFormat: TWatchDisplayFormat; out APrintedValue: String
  ): boolean;
begin
  Result := False;
end;

function TLazDbgIdeValueFormatterGeneric.SupportedFeatures: TLazDbgIdeValFormatterFeatures;
begin
  Result := [];
end;

{ TLazDbgIdeValueFormatterRegistryEntryGeneric }

class function TLazDbgIdeValueFormatterRegistryEntryGeneric.CreateValueFormatter: ILazDbgIdeValueFormatterIntf;
begin
  Result := _Formatter.Create;
end;

class function TLazDbgIdeValueFormatterRegistryEntryGeneric.GetClassName: String;
begin
  Result := _Formatter.ClassName;
end;

class function TLazDbgIdeValueFormatterRegistryEntryGeneric.GetDisplayName: String;
begin
  Result := _Formatter.GetRegisteredDisplayName;
end;

{ TLazDbgIdeValueFormatterFrameRegistryEntryGeneric }

class function TLazDbgIdeValueFormatterFrameRegistryEntryGeneric.GetSettingsFrameClass: TClass;
begin
  Result := _Frame;
end;


finalization
  FreeAndNil(TheValueFormatterRegistry);
end.

