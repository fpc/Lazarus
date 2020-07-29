{ $Id$}
{
 *****************************************************************************
 *                               lclclasses.pp                               *
 *                               -------------                               *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Defines the base class for all LCL TComponents including controls.
}
unit LCLClasses;

{$mode objfpc}{$H+}

{ Add -dVerboseWSBrunoK switch to compile with $DEFINE VerboseWSBrunoK }

interface

uses
  Classes, WSLCLClasses, WSReferences, LCLType, LCLProc;

type

  // SysUtils.LongRec has unsigned Word for Lo and Hi,
  //  we need a similar record with signed SmallInt
  LazLongRec = packed record
{$ifdef FPC_LITTLE_ENDIAN}
    Lo,Hi : SmallInt;
{$else FPC_LITTLE_ENDIAN}
    Hi,Lo : SmallInt;
{$endif FPC_LITTLE_ENDIAN}
  end;

  { TLCLComponent }

  TLCLComponent = class(TComponent)
  private
    FWidgetSetClass: TWSLCLComponentClass;
    FLCLRefCount: integer;
  protected
    class procedure WSRegisterClass; virtual;
    class function GetWSComponentClass(ASelf: TLCLComponent): TWSLCLComponentClass; virtual;
  public
    {$IFDEF DebugLCLComponents}
    constructor Create(TheOwner: TComponent); override;
    {$ENDIF}
    destructor Destroy; override;
    class function NewInstance: TObject; override;
    procedure RemoveAllHandlersOfObject(AnObject: TObject); virtual;
    procedure IncLCLRefCount;
    procedure DecLCLRefCount;
    property LCLRefCount: integer read FLCLRefCount;
    property WidgetSetClass: TWSLCLComponentClass read FWidgetSetClass;
  end;

  { TLCLReferenceComponent }

  // A base class for all components having a handle
  TLCLReferenceComponent = class(TLCLComponent)
  private
    FReferencePtr: PWSReference;
    FCreating: Boolean; // Set if we are creating the handle
    function  GetHandle: THandle;
    function  GetReferenceAllocated: Boolean;
  protected
    procedure CreateParams(var AParams: TCreateParams); virtual;
    procedure DestroyReference;
    function  GetReferenceHandle: THandle; virtual; abstract;
    procedure ReferenceCreated; virtual;    // gets called after the Handle is created
    procedure ReferenceDestroying; virtual; // gets called before the Handle is destroyed
    procedure ReferenceNeeded;
    function  WSCreateReference(AParams: TCreateParams): PWSReference; virtual;
    procedure WSDestroyReference; virtual;
  protected
  public
    destructor Destroy; override;
    property HandleAllocated: Boolean read GetReferenceAllocated;
    property ReferenceAllocated: Boolean read GetReferenceAllocated;
  end;

implementation

uses
  InterfaceBase;

const
  cWSRegisterOffset : integer = 0; // Offset of WSRegisterClass in class virtual methods
  cLCLComponentWSReg : CodePointer = nil; // Adress of TLCLComponent.WSRegisterClass

type
  TLCLComponentClass = class of TLCLComponent;

function WSRegisterLCLComponent: boolean;
begin
  RegisterWSComponent(TLCLComponent, TWSLCLComponent);
  Result := True;
end;

class procedure TLCLComponent.WSRegisterClass;
const
  Registered : boolean = False;
begin
  if Registered then
    Exit;
  WSRegisterLCLComponent;
  Registered := True;
end;

procedure UpdateOffset;
var
  lWSRegisterProc : CodePointer;
  lPPtrArray : PPointerArray;
  I : integer;
begin
  cLCLComponentWSReg := @TLCLComponent.WSRegisterClass;
  lPPtrArray := Pointer(TLCLComponent);
  I := 0;
  while lPPtrArray^[i]<>cLCLComponentWSReg do
    inc(i);
  cWSRegisterOffset := I * SizeOf(Pointer);
end;

{ This method allows descendents to override the FWidgetSetClass, handles
  registration of the component in WSLVLClasses list of components. It is only
  called if there wasn't a direct or parent hit at the beginining of NewInstance. }
class function TLCLComponent.GetWSComponentClass(ASelf: TLCLComponent): TWSLCLComponentClass;
var
  lPSelfWSReg,
  lPSelfParentWSReg : CodePointer;
  lClassParent : TLCLComponentClass;
begin
  if cWSRegisterOffset = 0 then begin
    UpdateOffset;
    TLCLComponent.WSRegisterClass;  { Always create the top node ! }
  end;

  WSRegisterClass;
  Result := FindWSRegistered(Self);
  if Result <> nil then
    Exit;

  lClassParent := TLCLComponentClass(ClassParent);
  lPSelfWSReg := PCodePointer(Pointer(Self) + cWSRegisterOffset)^;
  lPSelfParentWSReg := PCodePointer(Pointer(lClassParent) + cWSRegisterOffset)^;

  { Self.ComponentClass didn't register itself but the parent should now be registered }
  repeat
    if lPSelfWSReg = lPSelfParentWSReg then begin
      Result := FindWSRegistered(TComponentClass(lClassParent));
      if Assigned(Result) then
        Break
      else
        { Force creation of intermediate nodes for parent }
        RegisterNewWSComp(TComponentClass(lClassParent));
    end
    else begin
      { Force creation of intermediate nodes for Self and a leaf node for Self }
      RegisterNewWSComp(Self);
      Result := FindWSRegistered(Self);
      Break;
    end;
  until False;
end;

{$IFDEF DebugLCLComponents}
constructor TLCLComponent.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  //DebugLn('TLCLComponent.Create ',DbgSName(Self));
  DebugLCLComponents.MarkCreated(Self,DbgSName(Self));
end;
{$ENDIF}

destructor TLCLComponent.Destroy;
begin
  {$IFNDEF DisableChecks}
  if FLCLRefCount>0 then begin
    DebugLn(['WARNING: ' + ClassName + '.Destroy with LCLRefCount>0. Hint: Maybe the component is processing an event?']);
    {$IFDEF DebugTLCLComponentDestroy}
    DumpStack;
    {$ENDIF}
  end;
  {$ENDIF}
  {$IFDEF DebugLCLComponents}
  //DebugLn('TLCLComponent.Destroy ',DbgSName(Self));
  DebugLCLComponents.MarkDestroyed(Self);
  {$ENDIF}
  inherited Destroy;
end;

class function TLCLComponent.NewInstance: TObject;
var
  lWidgetSetClass: TWSLCLComponentClass;
  lClassParent : TLCLComponentClass;
begin
  Result := inherited NewInstance;

  { Test if directly inherits WSRegisterClass from its parent }
  lClassParent := TLCLComponentClass(ClassParent);
  if (PCodePointer(Pointer(Self)  + cWSRegisterOffset)^
     = PCodePointer(Pointer(lClassParent) + cWSRegisterOffset)^)
  then begin
    { Retrieve WidgetSetClass from Parent }
    lWidgetSetClass := FindWSRegistered(lClassParent);
    if Assigned(lWidgetSetClass) then begin
      TLCLComponent(Result).FWidgetSetClass := lWidgetSetClass;
      Exit;
    end;
  end
  else begin
    { Look if already registered. If true set FWidgetSetClass and exit }
    lWidgetSetClass := FindWSRegistered(Self);
    if Assigned(lWidgetSetClass) then begin
      TLCLComponent(Result).FWidgetSetClass := lWidgetSetClass;
      {$IFDEF VerboseWSBrunoK} inc(cWSLCLDirectHit); {$ENDIF}
      Exit;
    end;
  end;

  { WSRegisterClass and manage WSLVLClasses list }
  TLCLComponent(Result).FWidgetSetClass := GetWSComponentClass(TLCLComponent(Result));
  {$IFDEF VerboseWSBrunoK} inc(cWSLCLRegister); {$ENDIF}
end;

procedure TLCLComponent.RemoveAllHandlersOfObject(AnObject: TObject);
begin
end;

procedure TLCLComponent.IncLCLRefCount;
begin
  inc(FLCLRefCount);
end;

procedure TLCLComponent.DecLCLRefCount;
begin
  dec(FLCLRefCount);
end;

{ TLCLReferenceComponent }

procedure TLCLReferenceComponent.CreateParams(var AParams: TCreateParams);
begin
end;

destructor TLCLReferenceComponent.Destroy;
begin
  DestroyReference;
  inherited Destroy;
end;

procedure TLCLReferenceComponent.DestroyReference;
begin
  if ReferenceAllocated then
  begin
    ReferenceDestroying;
    WSDestroyReference;
    FReferencePtr^._Clear;
    FReferencePtr := nil;
  end;
end;

function TLCLReferenceComponent.GetHandle: THandle;
begin
  ReferenceNeeded;
  Result := GetReferenceHandle;
end;

function TLCLReferenceComponent.GetReferenceAllocated: Boolean;
begin
  Result := (FReferencePtr <> nil) and FReferencePtr^.Allocated;
end;

procedure TLCLReferenceComponent.ReferenceCreated;
begin
end;

procedure TLCLReferenceComponent.ReferenceDestroying;
begin
end;

procedure TLCLReferenceComponent.ReferenceNeeded;
var
  Params: TCreateParams;
begin
  if ReferenceAllocated then Exit;

  if FCreating
  then begin
    // raise some error ?
    {$IFNDEF DisableChecks}
    DebugLn('TLCLReferenceComponent: Circular reference creation');
    {$ENDIF}
    Exit;
  end;

  CreateParams(Params);
  FCreating := True;
  try
    FReferencePtr := WSCreateReference(Params);
    if not ReferenceAllocated
    then begin
      // raise some error ?
      {$IFNDEF DisableChecks}
      DebugLn('TLCLHandleComponent: Reference creation failed');
      {$ENDIF}
      Exit;
    end;
  finally
    FCreating := False;
  end;
  ReferenceCreated;
end;

function TLCLReferenceComponent.WSCreateReference(AParams: TCreateParams): PWSReference;
begin
  // this function should be overriden in derrived class
  Result := nil;
end;

procedure TLCLReferenceComponent.WSDestroyReference;
begin
  TWSLCLReferenceComponentClass(WidgetSetClass).DestroyReference(Self);
end;

end.

