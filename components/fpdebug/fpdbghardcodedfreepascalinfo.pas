{ Hardcoded debug-information for some basic FPC-types and -variables

  Copyright (C) 2020 Joost van der Sluis joost@cnoc.nl

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit FpDbgHardcodedFreepascalInfo;

{ Debug-information for some basic types and variables, hardcoded based on
  knowledge about the Free Pascal Compiler. So it is possible to obtain some
  debug-information when the proper debug-information is not available. }

{$mode objfpc}{$H+}

{$IF FPC_FULLVERSION>30100}
  {$DEFINE HasGenObjDict}
{$ENDIF}

interface

uses
  SysUtils,
  {$IFDEF HasGenObjDict}
  generics.collections,
  {$ELSE}
  AvgLvlTree,
  {$ENDIF}
  DbgIntfBaseTypes,
  fpDbgSymTable,
  FpdMemoryTools,
  FpDbgInfo;

type
  TDbgHardcodedFPCClassMember = class;
  TDbgHardcodedVariableValue = class;
  {$IFDEF HasGenObjDict}
  TDbgHardcodedFPCClassMemberCollection = specialize TObjectDictionary<string, TDbgHardcodedFPCClassMember>;
  {$ELSE}
  TDbgHardcodedFPCClassMemberCollection = TStringToPointerTree;
  {$ENDIF}

  { TDbgTypeSymbol }

  // Base class for stType symbols
  TDbgTypeSymbol = class(TFpSymbol)
  protected
    procedure SymbolTypeNeeded; override;
  public
    function GetValueObject: TFpValue; override;
    // Given the Address of the variable, return the Address with the effective
    // data according to the type-information.
    // Take for example an AnsiString, where the real data is not at the
    // location of the AnsiString itself. (Which is a pointer) Same holds for
    // classes but maybe also others.
    function GetDataAddress(AValueObj: TDbgHardcodedVariableValue; var AnAddress: TFpDbgMemLocation): Boolean; virtual;
  end;

  { TDbgValueSymbol }

  // Base class for stValue symbols
  TDbgValueSymbol = class(TFpSymbol)
  protected
    procedure SymbolTypeNeeded; override;
  public
    // In principle a TFpSymbol (stValue) does not contain the address of the
    // variable. But in some cases it does. Like when the location of the variable
    // is defined within the debug-info.
    // This function must be used by a TFpValue to obtain the corresponding
    // address.
    function DoReadDataAddress(const AValueObj: TDbgHardcodedVariableValue; out AnAddress: TFpDbgMemLocation): Boolean; virtual;
  end;

  { TDbgHardcodedFPCClassTypeSymbol }

  // Base class for the type-symbol debuginformation for classes
  TDbgHardcodedFPCClassTypeSymbol = class(TDbgTypeSymbol)
  private
    FFields: TDbgHardcodedFPCClassMemberCollection;
  protected
    // Override this function to set the members of the class.
    procedure FillFields; virtual;
    function GetFields: TDbgHardcodedFPCClassMemberCollection;
    function GetNestedSymbolCount: Integer; override;
    function GetNestedSymbolByName(AIndex: string): TFpSymbol; override;
    function GetNestedSymbol(AIndex: Int64): TFpSymbol; override;
  public
    function GetDataAddress(AValueObj: TDbgHardcodedVariableValue; var AnAddress: TFpDbgMemLocation): Boolean; override;
  public
    destructor Destroy; override;
  end;

  { TDbgHardcodedFPCExceptionTypeSymbol }

  // Type-symbol information for the Exception-class
  TDbgHardcodedFPCExceptionTypeSymbol = class(TDbgHardcodedFPCClassTypeSymbol)
  protected
    procedure FillFields; override;
  end;

  { TDbgHardcodedFPCShortstringTypeSymbol }

  // Type-symbol information for shortstrings
  TDbgHardcodedFPCShortstringTypeSymbol = class(TDbgTypeSymbol)
  protected
    function DoReadSize(const AValueObj: TFpValue; out ASize: TFpDbgValueSize): Boolean; override;
    procedure KindNeeded; override;
  public
    function GetDataAddress(AValueObj: TDbgHardcodedVariableValue; var AnAddress: TFpDbgMemLocation): Boolean; override;
  end;

  { TDbgHardcodedFPCAnsistringTypeSymbol }

  // Type-symbol information for ansistrings
  TDbgHardcodedFPCAnsistringTypeSymbol = class(TDbgTypeSymbol)
  protected
    function DoReadSize(const AValueObj: TFpValue; out ASize: TFpDbgValueSize): Boolean; override;
    procedure KindNeeded; override;
  public
    function GetDataAddress(AValueObj: TDbgHardcodedVariableValue; var AnAddress: TFpDbgMemLocation): Boolean; override;
  end;

  { TDbgHardcodedVariableAtMemLocation }

  // Value-symbol information for a variable at a given location
  TDbgHardcodedVariableAtMemLocation = class(TDbgValueSymbol)
  protected
    function GetValueObject: TFpValue; override;
  public
    constructor Create(const AName: String; AKind: TDbgSymbolKind; ATypeSymbol: TDbgTypeSymbol; AMemLocation: TFpDbgMemLocation);
  end;

  { TDbgHardcodedFPCClassMember }

  // Value-symbol information for member of a class
  TDbgHardcodedFPCClassMember = class(TDbgValueSymbol)
  private
    FFieldIndex: Int64;
  protected
    function GetValueObject: TFpValue; override;
  public
    constructor Create(const AName: String; AKind: TDbgSymbolKind; ATypeSymbol: TDbgTypeSymbol; AFieldIndex: Integer);
    function DoReadDataAddress(const AValueObj: TDbgHardcodedVariableValue; out AnAddress: TFpDbgMemLocation): Boolean; override;
    property FieldIndex: Int64 read FFieldIndex;
  end;

  { TDbgHardcodedVariableValue }

  // Value of a variable
  TDbgHardcodedVariableValue = class(TFpValue)
  private
    FTypeSymbol: TFpSymbol;
    FDataSymbol: TFpSymbol;
    FContext: TFpDbgInfoContext;

    // Cached:
    FDataAddress: TFpDbgMemLocation;
    FStructureValue: TDbgHardcodedVariableValue;
    procedure SetStructureValue(AValue: TDbgHardcodedVariableValue);
    procedure SetContext(AValue: TFpDbgInfoContext);
  protected
    function GetAsString: AnsiString; override;
    function GetAddress: TFpDbgMemLocation; override;
    function GetDataAddress: TFpDbgMemLocation; override;
  public
    constructor Create(ATypeSymbol: TFpSymbol);
    destructor Destroy; override;
    procedure SetDataSymbol(AValueSymbol: TFpSymbol);

    property Context: TFpDbgInfoContext read FContext write SetContext;
  end;

  { TDbgHardcodedFPCClassValue }

  // Value of class-instance
  TDbgHardcodedFPCClassValue = class(TDbgHardcodedVariableValue)
  protected
    function GetMemberByName(AIndex: string): TFpValue; override;
  public
    function GetClassName: string;
  end;

  { TFpDbgHardcodedContext }

  // Just a hack to simulate a real context, when FindContext does not return
  // a context.
  TFpDbgHardcodedContext = class(TFpDbgInfoContext)
  private
    FMemManager: TFpDbgMemManager;
    FAddressSize: Integer;
    FThreadId: Integer;
  protected
    function GetMemManager: TFpDbgMemManager; override;
    function GetSizeOfAddress: Integer; override;
    function GetThreadId: Integer; override;
    function GetStackFrame: Integer; override;
    function GetAddress: TDbgPtr; override;
  public
    constructor Create(AMemManager: TFpDbgMemManager; AnAdressSize: Integer; AThreadId: Integer);
  end;

implementation

{ TDbgValueSymbol }

function TDbgValueSymbol.DoReadDataAddress(const AValueObj: TDbgHardcodedVariableValue; out AnAddress: TFpDbgMemLocation): Boolean;
begin
  Result := True;
  AnAddress := Address;
end;

procedure TDbgValueSymbol.SymbolTypeNeeded;
begin
  SetSymbolType(stValue);
end;

{ TDbgHardcodedFPCAnsistringTypeSymbol }

procedure TDbgHardcodedFPCAnsistringTypeSymbol.KindNeeded;
begin
  SetKind(skString);
end;

function TDbgHardcodedFPCAnsistringTypeSymbol.DoReadSize(const AValueObj: TFpValue; out ASize: TFpDbgValueSize): Boolean;
var
  SizeAddr: TFpDbgMemLocation;
  ValueObj: TDbgHardcodedVariableValue;
  Size: Int64;
begin
  ValueObj := AValueObj as TDbgHardcodedVariableValue;
  SizeAddr := AValueObj.DataAddress + SizeVal(-ValueObj.Context.SizeOfAddress);
  result := ValueObj.Context.MemManager.ReadSignedInt(SizeAddr, SizeVal(ValueObj.Context.SizeOfAddress), Size, ValueObj.Context);
  ASize := SizeVal(Size);
end;

function TDbgHardcodedFPCAnsistringTypeSymbol.GetDataAddress(AValueObj: TDbgHardcodedVariableValue; var AnAddress: TFpDbgMemLocation): Boolean;
var
  Context: TFpDbgInfoContext;
begin
  // Dereference the pointer that points to the real string-data
  Context := AValueObj.Context;
  AnAddress := Context.MemManager.ReadAddress(AnAddress, SizeVal(Context.SizeOfAddress), Context);
  Result := AnAddress.MType <> mlfInvalid;
end;

{ TDbgHardcodedFPCClassMember }

function TDbgHardcodedFPCClassMember.GetValueObject: TFpValue;
begin
  Result := TypeInfo.Value;
  (Result as TDbgHardcodedVariableValue).SetDataSymbol(Self);
end;

constructor TDbgHardcodedFPCClassMember.Create(const AName: string; AKind: TDbgSymbolKind; ATypeSymbol: TDbgTypeSymbol; AFieldIndex: Integer);
begin
  inherited Create(AName, AKind, InvalidLoc);
  SetTypeInfo(ATypeSymbol);
  FFieldIndex := AFieldIndex;
end;

function TDbgHardcodedFPCClassMember.DoReadDataAddress(const AValueObj: TDbgHardcodedVariableValue; out AnAddress: TFpDbgMemLocation): Boolean;
var
  Context: TFpDbgInfoContext;
begin
  Context := (AValueObj as TDbgHardcodedVariableValue).Context;
  AnAddress := AValueObj.FStructureValue.DataAddress + (SizeVal(Context.SizeOfAddress) * FFieldIndex);
  Result := True;
end;

{ TDbgHardcodedFPCExceptionTypeSymbol }

procedure TDbgHardcodedFPCExceptionTypeSymbol.FillFields;
var
  FieldDef: TDbgHardcodedFPCClassMember;
  FieldTypeDef: TDbgTypeSymbol;
begin
  FieldTypeDef := TDbgTypeSymbol.Create('longint');
  try
    FieldDef := TDbgHardcodedFPCClassMember.Create('HelpContext', skInteger, FieldTypeDef, 0);
    {$IFDEF HasGenObjDict}
    FFields.Add(FieldDef.Name, FieldDef);
    {$ELSE}
    FFields[FieldDef.Name]:=FieldDef;
    {$ENDIF}
  finally
    FieldTypeDef.ReleaseReference;
  end;

  FieldTypeDef := TDbgHardcodedFPCAnsistringTypeSymbol.Create('string');
  try
    FieldDef := TDbgHardcodedFPCClassMember.Create('Message', skAnsiString, FieldTypeDef, 1);
    {$IFDEF HasGenObjDict}
    FFields.Add(FieldDef.Name, FieldDef);
    {$ELSE}
    FFields[FieldDef.Name]:=FieldDef;
    {$ENDIF}
  finally
    FieldTypeDef.ReleaseReference;
  end;
end;

{ TFpDbgHardcodedContext }

constructor TFpDbgHardcodedContext.Create(AMemManager: TFpDbgMemManager; AnAdressSize: Integer; AThreadId: Integer);
begin
  inherited Create;
  AddReference;
  FMemManager := AMemManager;
  FAddressSize := AnAdressSize;
  FThreadId := AThreadId;
end;

function TFpDbgHardcodedContext.GetMemManager: TFpDbgMemManager;
begin
  Result := FMemManager;
end;

function TFpDbgHardcodedContext.GetSizeOfAddress: Integer;
begin
  Result := FAddressSize;
end;

function TFpDbgHardcodedContext.GetThreadId: Integer;
begin
  Result := FThreadId;
end;

function TFpDbgHardcodedContext.GetStackFrame: Integer;
begin
  Result := 0;
end;

function TFpDbgHardcodedContext.GetAddress: TDbgPtr;
begin
  raise Exception.Create('It is not possible to get the address of this context');
end;

{ TDbgHardcodedFPCShortstringTypeSymbol }

procedure TDbgHardcodedFPCShortstringTypeSymbol.KindNeeded;
begin
  SetKind(skString);
end;

function TDbgHardcodedFPCShortstringTypeSymbol.DoReadSize(const AValueObj: TFpValue; out ASize: TFpDbgValueSize): Boolean;
var
  Size: Byte;
  ValueObj: TDbgHardcodedVariableValue;
  SizeAddr: TFpDbgMemLocation;
begin
  ValueObj := AValueObj as TDbgHardcodedVariableValue;
  SizeAddr := AValueObj.DataAddress + SizeVal(-1);
  Result := ValueObj.Context.MemManager.ReadMemory(SizeAddr, SizeVal(1), @Size, ValueObj.Context);
  ASize := SizeVal(Size);
end;

function TDbgHardcodedFPCShortstringTypeSymbol.GetDataAddress(AValueObj: TDbgHardcodedVariableValue; var AnAddress: TFpDbgMemLocation): Boolean;
begin
  AnAddress := AnAddress + SizeVal(1);
  Result := True;
end;

{ TDbgTypeSymbol }

procedure TDbgTypeSymbol.SymbolTypeNeeded;
begin
  SetSymbolType(stType);
end;

function TDbgTypeSymbol.GetDataAddress(AValueObj: TDbgHardcodedVariableValue; var AnAddress: TFpDbgMemLocation): Boolean;
begin
  // Do nothing
end;

function TDbgTypeSymbol.GetValueObject: TFpValue;
begin
  Result := TDbgHardcodedFPCClassValue.Create(Self);
end;

{ TDbgHardcodedFPCClassValue }

function TDbgHardcodedFPCClassValue.GetClassName: string;
var
  ShortstringTypeSymbol: TDbgHardcodedFPCShortstringTypeSymbol;
  VMTAddr, ClassnameAddr, ObjAddr: TFpDbgMemLocation;
  ShortstringVariable: TDbgHardcodedVariableAtMemLocation;
  ShortStringValue: TDbgHardcodedVariableValue;
begin
  Result := '';

  ObjAddr := GetDataAddress;

  // Dereference the pointer to the class to get the address of the VMT
  VMTAddr := Context.MemManager.ReadAddress(ObjAddr, SizeVal(Context.SizeOfAddress), Context);
  if IsValidLoc(VMTAddr) and IsTargetNil(VMTAddr) then
    exit;

  // Calculate the location of the pointer to the classname as shortstring
  ClassnameAddr := VMTAddr + SizeVal(3*Context.SizeOfAddress);

  // Dereference, so we have the address of the shortstring with the classname
  ClassNameAddr := FContext.MemManager.ReadAddress(ClassnameAddr, SizeVal(FContext.SizeOfAddress));
  if IsTargetNil(ClassNameAddr) then
    exit;

  ShortstringTypeSymbol := TDbgHardcodedFPCShortstringTypeSymbol.Create('shortstring', skString, InvalidLoc);
  try
    ShortstringVariable := TDbgHardcodedVariableAtMemLocation.Create('classname', skString, ShortstringTypeSymbol, ClassNameAddr);
    try
      ShortStringValue := ShortstringVariable.Value as TDbgHardcodedVariableValue;
      try
        ShortStringValue.Context := Context;

        Result := ShortStringValue.AsString;
      finally
        ShortStringValue.ReleaseReference;
      end;
    finally
      ShortstringVariable.ReleaseReference;
    end;
  finally
    ShortstringTypeSymbol.ReleaseReference;
  end;
end;

function TDbgHardcodedFPCClassValue.GetMemberByName(AIndex: string): TFpValue;
begin
  Result := FTypeSymbol.NestedSymbolByName[AIndex].Value;
  if Result is TDbgHardcodedVariableValue then
    begin
    TDbgHardcodedVariableValue(Result).Context := Context;
    TDbgHardcodedVariableValue(Result).SetStructureValue(Self);
    end;
end;

{ TDbgHardcodedVariableValue }

procedure TDbgHardcodedVariableValue.SetDataSymbol(AValueSymbol: TFpSymbol);
begin
  if FDataSymbol = AValueSymbol then
    exit;

  FDataSymbol.ReleaseReference;
  FDataSymbol := AValueSymbol;
  if FDataSymbol <> nil then
    FDataSymbol.AddReference;
end;

constructor TDbgHardcodedVariableValue.Create(ATypeSymbol: TFpSymbol);
begin
  FTypeSymbol := ATypeSymbol;
  inherited Create;
end;

function TDbgHardcodedVariableValue.GetAsString: AnsiString;
var
  Size: TFpDbgValueSize;
  SizeInBytes: Int64;
begin
  if FTypeSymbol.Kind=skString then
    begin
    if not FTypeSymbol.ReadSize(Self, Size) then
      begin
      Result := '';
      Exit;
      end;
    SizeInBytes := SizeToFullBytes(Size);
    SetLength(Result, SizeInBytes);
    Context.MemManager.ReadMemory(DataAddress, Size, @Result[1], Context);
    end
  else
    Result := '';
end;

function TDbgHardcodedVariableValue.GetDataAddress: TFpDbgMemLocation;
begin
  if not IsInitializedLoc(FDataAddress) then
    begin
    if FDataSymbol is TDbgValueSymbol then
      begin
      if not TDbgValueSymbol(FDataSymbol).DoReadDataAddress(Self, FDataAddress) then
        FDataAddress := InvalidLoc
      else
        begin
        if FTypeSymbol is TDbgTypeSymbol then
          begin
          TDbgTypeSymbol(FTypeSymbol).GetDataAddress(Self, FDataAddress);
          end;
        end;
      end
    else
      Result := FDataSymbol.Address;
    end;
  Result := FDataAddress;
end;

function TDbgHardcodedVariableValue.GetAddress: TFpDbgMemLocation;
begin
  Result := FDataSymbol.Address;
end;

procedure TDbgHardcodedVariableValue.SetStructureValue(AValue: TDbgHardcodedVariableValue);
begin
  FStructureValue := AValue;
end;

procedure TDbgHardcodedVariableValue.SetContext(AValue: TFpDbgInfoContext);
begin
  if FContext = AValue then
    exit;

  FContext.ReleaseReference;
  FContext := AValue;
  if FContext <> nil then
    FContext.AddReference;
end;

destructor TDbgHardcodedVariableValue.Destroy;
begin
  SetDataSymbol(nil);
  SetContext(nil);
  inherited Destroy;
end;

{ TDbgHardcodedVariableAtMemLocation }

function TDbgHardcodedVariableAtMemLocation.GetValueObject: TFpValue;
begin
  Result := TypeInfo.Value;
  (Result as TDbgHardcodedVariableValue).SetDataSymbol(Self);
end;

constructor TDbgHardcodedVariableAtMemLocation.Create(const AName: string; AKind: TDbgSymbolKind; ATypeSymbol: TDbgTypeSymbol; AMemLocation: TFpDbgMemLocation);
begin
  inherited create(AName, AKind, AMemLocation);
  // This is strange, as it is already set in the inherited call. But the cache-
  // flags are not properly set.
  SetAddress(AMemLocation);
  SetTypeInfo(ATypeSymbol);
end;

{ TDbgHardcodedFPCClassTypeSymbol }

function TDbgHardcodedFPCClassTypeSymbol.GetFields: TDbgHardcodedFPCClassMemberCollection;
begin
  if not Assigned(FFields) then
    begin
    {$IFDEF HasGenObjDict}
    FFields := TDbgHardcodedFPCClassMemberCollection.Create;
    {$ELSE}
    FFields := TStringToPointerTree.Create(true);
    {$ENDIF}
    FillFields;
    end;
  Result := FFields;
end;

function TDbgHardcodedFPCClassTypeSymbol.GetNestedSymbol(AIndex: Int64): TFpSymbol;
{$IFDEF HasGenObjDict}
var
  Member: TDbgHardcodedFPCClassMember;
begin
  Result := nil;
  for Member in GetFields.Values do
    if Member.FieldIndex= AIndex then
      begin
      Result := Member;
      Break;
      end;
end;
{$ELSE}
var
  Node: PStringToPointerTreeItem;
begin
  Result := nil;
  for Node in FFields do
    if TDbgHardcodedFPCClassMember(Node^.Value).FieldIndex=AIndex then
      exit(TDbgHardcodedFPCClassMember(Node^.Value));
end;
{$ENDIF}

function TDbgHardcodedFPCClassTypeSymbol.GetNestedSymbolByName(AIndex: string): TFpSymbol;
begin
  {$IFDEF HasGenObjDict}
  Result := GetFields.Items[AIndex]
  {$ELSE}
  Result := TFpSymbol(FFields[AIndex]);
  {$ENDIF}
end;

function TDbgHardcodedFPCClassTypeSymbol.GetNestedSymbolCount: Integer;
begin
  Result := GetFields.Count
end;

destructor TDbgHardcodedFPCClassTypeSymbol.Destroy;
{$IFDEF HasGenObjDict}
var
  Field: TDbgHardcodedFPCClassMember;
{$ELSE}
var
  Node: PStringToPointerTreeItem;
{$ENDIF}
begin
  {$IFDEF HasGenObjDict}
  for Field in FFields.Values do
    Field.ReleaseReference;
  {$ELSE}
  for Node in FFields do
    TDbgHardcodedFPCClassMember(Node^.Value).ReleaseReference;
  {$ENDIF}
  FFields.Free;
  inherited Destroy;
end;

procedure TDbgHardcodedFPCClassTypeSymbol.FillFields;
begin
  // Override
end;

function TDbgHardcodedFPCClassTypeSymbol.GetDataAddress(AValueObj: TDbgHardcodedVariableValue; var AnAddress: TFpDbgMemLocation): Boolean;
var
  Context: TFpDbgInfoContext;
begin
  // Dereference the pointer that points to the real class-data
  Context := AValueObj.Context;
  AnAddress := Context.MemManager.ReadAddress(AnAddress, SizeVal(Context.SizeOfAddress), Context);
  Result := AnAddress.MType <> mlfInvalid;
end;

end.

