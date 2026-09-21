{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Abstract SVG rendering backend and its class registry.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.backend;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, fpsvg.types;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, fpsvg.types;
{$ENDIF FPC_DOTTEDUNITS}

type
  ESVGBackend = class(ESVGError);

  TSVGBackendCapability = (
    bcClipPath, bcMask, bcGroupOpacity, bcPattern,
    bcDashes, bcNativeText, bcFilter);
  TSVGBackendCapabilities = set of TSVGBackendCapability;

  TSVGRenderBackend = class;
  TSVGRenderBackendClass = class of TSVGRenderBackend;

  { Base class for a render backend.
    Geometry arrives in user space, together with the matrix to draw it under.
    It is never flattened to device coordinates beforehand. }
  TSVGRenderBackend = class(TObject)
  public
    constructor Create; virtual;
    // The name the backend registers under. It is unique in the registry.
    class function BackendName: String; virtual; abstract;
    // The operations the backend implements itself. fpsvg.render emulates
    // the rest.
    class function Capabilities: TSVGBackendCapabilities; virtual;

    // Starts a frame of the given device size, discarding what was there.
    procedure BeginFrame(aWidth, aHeight: Integer); virtual; abstract;
    // Ends the frame started by BeginFrame.
    procedure EndFrame; virtual; abstract;

    // Fills a path under aCTM.
    procedure FillPath(aPath: TSVGPath; const aCTM: TSVGMatrix;
      const aPaint: TSVGPaint; aRule: TSVGFillRule;
      aOpacity: Double); virtual; abstract;
    // Strokes a path under aCTM. The pen is in user space.
    procedure StrokePath(aPath: TSVGPath; const aCTM: TSVGMatrix;
      const aPaint: TSVGPaint; const aPen: TSVGPen;
      aOpacity: Double); virtual; abstract;

    // Intersects the clip with a path under aCTM.
    procedure PushClip(aPath: TSVGPath; const aCTM: TSVGMatrix;
      aRule: TSVGFillRule); virtual; abstract;
    // Intersects the clip with the outline that stroking the path would draw.
    // The pen is in user space, as it is for StrokePath.
    procedure PushStrokeClip(aPath: TSVGPath; const aCTM: TSVGMatrix;
      const aPen: TSVGPen); virtual; abstract;
    // Restores the clip that the matching PushClip or PushStrokeClip
    // saved.
    procedure PopClip; virtual; abstract;

    // Composites in this colour space until it is set again.
    // It applies to blending a drawing with the pixels under it.
    // A gradient has its own space for mixing its stops.
    procedure SetColorInterpolation(
      aSpace: TSVGColorInterpolation); virtual; abstract;
    // Composites in sRGB again. That is the state a frame starts in.
    procedure ClearColorInterpolation; virtual;
    // Starts an offscreen layer. PopLayer composites it.
    procedure PushLayer(const aBounds: TSVGRect; aOpacity: Double;
      aIsolate: Boolean); virtual; abstract;
    // Composites the current layer onto its parent.
    procedure PopLayer; virtual; abstract;
    // Applies the current layer to its parent as a mask, instead of drawing it.
    procedure PopLayerAsMask(aMode: TSVGMaskMode); virtual; abstract;
    { Applies the current layer to its parent through a chain of filter primitives, instead of drawing it.
      The layer holds the drawing of the element, which the chain reads as SourceGraphic.
      A backend without filter support draws the layer as PopLayer does. }
    procedure PopLayerAsFilter(const aChain: TSVGFilterChain); virtual;
    { Keeps the current layer as the result of primitive aIndex of the chain, instead of drawing it.
      An feImage that refers to an element of the document is drawn into a layer and kept this way.
      The renderer produces no pixels itself.
      A backend without filter support draws the layer as PopLayer does. }
    procedure PopLayerAsFilterImage(aIndex: Integer); virtual;
    // Draws positioned glyph ids. The backend chooses how to turn them into outlines.
    procedure DrawGlyphRun(aFont: TSVGFontHandle;
      const aGlyphs: TSVGGlyphArray; const aCTM: TSVGMatrix;
      const aPaint: TSVGPaint; aOpacity: Double); virtual; abstract;
    // Draws an image into aRect under aCTM.
    procedure DrawImage(aImage: ISVGImageSource; const aRect: TSVGRect;
      const aCTM: TSVGMatrix; aOpacity: Double); virtual; abstract;
  end;

  { A registry of backend classes, looked up by name at run time. }
  TSVGBackendRegistry = class(TObject)
  private
    FList: TStringList;
    FDefaultBackend: String;
    function GetBackend(aIndex: Integer): TSVGRenderBackendClass;
    function GetCount: Integer;
    function GetName(aIndex: Integer): String;
    procedure SetDefaultBackend(const aValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    // Adds a backend class. The first one registered becomes the default.
    procedure RegisterBackend(aClass: TSVGRenderBackendClass);
    // Removes a backend class. The default is cleared if it was this one.
    procedure UnRegisterBackend(aClass: TSVGRenderBackendClass);
    // The index of a backend by name, or -1.
    function IndexOf(const aName: String): Integer;
    // The backend class with this name, or nil when it is not registered.
    function FindBackend(const aName: String): TSVGRenderBackendClass;
    // The backend class with this name. Raises when it is not registered.
    function BackendByName(const aName: String): TSVGRenderBackendClass;
    // Creates a backend by name. Raises when it is not registered.
    function CreateBackend(const aName: String): TSVGRenderBackend;
    // Creates the default backend. Raises when there is no default.
    function CreateDefaultBackend: TSVGRenderBackend;
    // Number of registered backends.
    property Count: Integer read GetCount;
    // A registered name by index, in alphabetical order.
    property Names[aIndex: Integer]: String read GetName;
    // A registered class by index, in alphabetical order.
    property Backends[aIndex: Integer]: TSVGRenderBackendClass read GetBackend;
    // The name that CreateDefaultBackend uses.
    property DefaultBackend: String read FDefaultBackend write SetDefaultBackend;
  end;

// The backend registry, created on first use.
function SVGBackends: TSVGBackendRegistry;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ELSE FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ENDIF FPC_DOTTEDUNITS}

type
  TSVGBackendEntry = class(TObject)
  private
    FBackendClass: TSVGRenderBackendClass;
  end;

var
  GBackends: TSVGBackendRegistry = nil;

function SVGBackends: TSVGBackendRegistry;

begin
  if GBackends = nil then
    GBackends := TSVGBackendRegistry.Create;
  Result := GBackends;
end;


{ TSVGRenderBackend }

// A backend without filter support has nowhere to keep the layer, so it
// composites it like PopLayer.
procedure TSVGRenderBackend.PopLayerAsFilterImage(aIndex: Integer);

begin
  if aIndex < 0 then ;
  PopLayer;
end;


// A backend without filter support draws the layer unfiltered, as if the
// chain read no further than its source.
procedure TSVGRenderBackend.PopLayerAsFilter(const aChain: TSVGFilterChain);

begin
  if aChain.Region.IsEmpty then ;
  PopLayer;
end;


procedure TSVGRenderBackend.ClearColorInterpolation;

begin
  SetColorInterpolation(ciSRGB);
end;


constructor TSVGRenderBackend.Create;

begin
  inherited Create;
end;


class function TSVGRenderBackend.Capabilities: TSVGBackendCapabilities;

begin
  Result := [];
end;


{ TSVGBackendRegistry }

constructor TSVGBackendRegistry.Create;

begin
  inherited Create;
  FList := TStringList.Create;
  FList.Sorted := True;
  FList.Duplicates := dupError;
  FList.CaseSensitive := False;
  FList.OwnsObjects := True;
end;


destructor TSVGBackendRegistry.Destroy;

begin
  FreeAndNil(FList);
  inherited Destroy;
end;


function TSVGBackendRegistry.GetCount: Integer;

begin
  Result := FList.Count;
end;


function TSVGBackendRegistry.GetName(aIndex: Integer): String;

begin
  Result := FList[aIndex];
end;


function TSVGBackendRegistry.GetBackend(aIndex: Integer): TSVGRenderBackendClass;

begin
  Result := TSVGBackendEntry(FList.Objects[aIndex]).FBackendClass;
end;


procedure TSVGBackendRegistry.SetDefaultBackend(const aValue: String);

begin
  if (aValue <> '') and (IndexOf(aValue) = -1) then
    raise ESVGBackend.CreateFmt(SErrNoBackendNamed, [aValue]);
  FDefaultBackend := aValue;
end;


procedure TSVGBackendRegistry.RegisterBackend(aClass: TSVGRenderBackendClass);

var
  lEntry: TSVGBackendEntry;
  lName: String;

begin
  lName := aClass.BackendName;
  if lName = '' then
    raise ESVGBackend.CreateFmt(SErrBackendClassHasNoName,
      [aClass.ClassName]);
  if IndexOf(lName) <> -1 then
    raise ESVGBackend.CreateFmt(SErrBackendAlreadyRegistered, [lName]);
  lEntry := TSVGBackendEntry.Create;
  lEntry.FBackendClass := aClass;
  FList.AddObject(lName, lEntry);
  if FDefaultBackend = '' then
    FDefaultBackend := lName;
end;


procedure TSVGBackendRegistry.UnRegisterBackend(aClass: TSVGRenderBackendClass);

var
  lIndex: Integer;

begin
  lIndex := IndexOf(aClass.BackendName);
  if lIndex = -1 then
    Exit;
  if SameText(FDefaultBackend, FList[lIndex]) then
    FDefaultBackend := '';
  FList.Delete(lIndex);
end;


function TSVGBackendRegistry.IndexOf(const aName: String): Integer;

begin
  Result := FList.IndexOf(aName);
end;


function TSVGBackendRegistry.FindBackend(const aName: String): TSVGRenderBackendClass;

var
  lIndex: Integer;

begin
  lIndex := IndexOf(aName);
  if lIndex = -1 then
    Result := nil
  else
    Result := GetBackend(lIndex);
end;


function TSVGBackendRegistry.BackendByName(const aName: String): TSVGRenderBackendClass;

begin
  Result := FindBackend(aName);
  if Result = nil then
    raise ESVGBackend.CreateFmt(SErrNoBackendNamed, [aName]);
end;


function TSVGBackendRegistry.CreateBackend(const aName: String): TSVGRenderBackend;

begin
  Result := BackendByName(aName).Create;
end;


function TSVGBackendRegistry.CreateDefaultBackend: TSVGRenderBackend;

begin
  if FDefaultBackend = '' then
    raise ESVGBackend.Create(SErrNoDefaultBackend);
  Result := CreateBackend(FDefaultBackend);
end;


finalization
  FreeAndNil(GBackends);
end.
