{ $Id$}
{
 *****************************************************************************
 *                              wslclclasses.pp                              *
 *                              ---------------                              *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSLCLClasses;

{$mode objfpc}{$H+}
{$I lcl_defines.inc}

{off$DEFINE VerboseWSRegistration}
{off$DEFINE VerboseWSRegistration_methods}
{off$DEFINE VerboseWSRegistration_treedump}
{$IFDEF VerboseWSRegistration_methods}
{$DEFINE VerboseWSRegistration}
{$ENDIF}

interface
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as possible circles, the uses
//    clause should contain only those LCL units
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the
//    initialization section which actually
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
  Classes, SysUtils, LCLProc;

type
  { TWSPrivate }

  {
    Internal WidgetSet specific object tree
  }
  TWSPrivate = class(TObject)
  end;
  TWSPrivateClass = class of TWSPrivate;

  { For non-TComponent WS objects }

  TWSObjectClass = class of TWSObject;

  { TWSObject }

  TWSObject = class(TObject)
  public
    class function GetImplementation: TWSObjectClass; virtual;
    class procedure SetImplementation(AImpl: TWSObjectClass); virtual;
  end;

  { TWSLCLComponent }

{$M+}
  TWSLCLComponent = class(TWSObject)
  public
    class function WSPrivate: TWSPrivateClass; //inline;
  end;
{$M-}
  TWSLCLComponentClass = class of TWSLCLComponent;

  { TWSLCLHandleComponent }

  TWSLCLReferenceComponent = class(TWSLCLComponent)
  published
    class procedure DestroyReference(AComponent: TComponent); virtual;
  end;
  TWSLCLReferenceComponentClass = class of TWSLCLReferenceComponent;


function FindWSComponentClass(const AComponent: TComponentClass): TWSLCLComponentClass;
procedure RegisterWSComponent(const AComponent: TComponentClass;
                              const AWSComponent: TWSLCLComponentClass;
                              const AWSPrivate: TWSPrivateClass = nil);
// Only for non-TComponent based objects
function GetWSLazAccessibleObject: TWSObjectClass;
procedure RegisterWSLazAccessibleObject(const AWSObject: TWSObjectClass);
function GetWSLazDeviceAPIs: TWSObjectClass;
procedure RegisterWSLazDeviceAPIs(const AWSObject: TWSObjectClass);

implementation

uses
  LCLClasses;

procedure DoInitialization; forward;

////////////////////////////////////////////////////
// Registration code
////////////////////////////////////////////////////
type
  PClassNode = ^TClassNode;
  TClassNode = record
    LCLClass: TComponentClass;
    WSClass: TWSLCLComponentClass;
    VClass: TWSLCLComponentClass;
  end;

const
  // vmtAutoTable is something Delphi 2 and not used, we 'borrow' the vmt entry
  vmtWSPrivate = vmtAutoTable;

var
  MComponentIndex: TStringList;
  MWSRegisterIndex: TStringList;
  WSLazAccessibleObjectClass: TWSObjectClass;
  WSLazDeviceAPIsClass: TWSObjectClass;

function FindWSComponentClass(
  const AComponent: TComponentClass): TWSLCLComponentClass;
var
  idx: Integer;
  cls: TClass;
  Node: PClassNode;
begin
  if MWSRegisterIndex = nil then
    DoInitialization;

  Result := nil;
  cls := AComponent;
  while cls <> nil do
  begin
    idx := MWSRegisterIndex.IndexOf(cls.ClassName);
    if idx <> -1 then
    begin
      Node := PClassNode(MWSRegisterIndex.Objects[idx]);
      Result := TWSLCLComponentClass(Node^.VClass);
      Exit;
    end;
    cls := cls.ClassParent;
  end;
end;

// ANewRegistration - If true, VClass is not created during runtime,
// but instead normal, Object Pascal class creation is used
procedure RegisterWSComponent(const AComponent: TComponentClass;
  const AWSComponent: TWSLCLComponentClass;
  const AWSPrivate: TWSPrivateClass = nil);

  function GetNode(const AClass: TClass): PClassNode;
  var
    idx: Integer;
    Name: String;
  begin
    if (AClass = nil)
    or not (AClass.InheritsFrom(TLCLComponent))
    then begin
      Result := nil;
      Exit;
    end;

    Name := AClass.ClassName;
    idx := MComponentIndex.IndexOf(Name);
    if idx = -1
    then begin
      New(Result);
      Result^.LCLClass := TComponentClass(AClass);
      Result^.WSClass := nil;
      Result^.VClass := nil;
      MComponentIndex.AddObject(Name, TObject(Result));
    end
    else begin
      Result := PClassNode(MComponentIndex.Objects[idx]);
    end;
  end;

var
  Node: PClassNode;
  OldPrivate: TClass;
begin
  AWSComponent.SetImplementation(AWSComponent);

  if MWSRegisterIndex = nil then
    DoInitialization;
  Node := GetNode(AComponent);
  if Node = nil then Exit;

  if Node^.WSClass = nil
  then MWSRegisterIndex.AddObject(AComponent.ClassName, TObject(Node));
  Node^.WSClass := AWSComponent;
  Node^.VClass := AWSComponent;

  // childclasses "inherit" the private from their parent
  // the child privates should only be updated when their private is still
  // the same as their parents
  if Node^.VClass = nil
  then OldPrivate := nil
  else OldPrivate := PClass(Pointer(Node^.VClass) + vmtWSPrivate)^;
end;

function GetWSLazAccessibleObject: TWSObjectClass;
begin
  Result := WSLazAccessibleObjectClass;
end;

procedure RegisterWSLazAccessibleObject(const AWSObject: TWSObjectClass);
begin
  WSLazAccessibleObjectClass := AWSObject;
end;

function GetWSLazDeviceAPIs: TWSObjectClass;
begin
  Result := WSLazDeviceAPIsClass;
end;

procedure RegisterWSLazDeviceAPIs(const AWSObject: TWSObjectClass);
begin
  WSLazDeviceAPIsClass := AWSObject;
end;

{ TWSObject }

class function TWSObject.GetImplementation: TWSObjectClass;
begin
  Result := nil;
end;

class procedure TWSObject.SetImplementation(AImpl: TWSObjectClass);
begin

end;

{ TWSLCLComponent }

class function TWSLCLComponent.WSPrivate: TWSPrivateClass; //inline;
begin
  Result := TWSPrivateClass(PClass(Pointer(Self) + vmtWSPrivate)^);
end;

{ TWSLCLHandleComponent }

class procedure TWSLCLReferenceComponent.DestroyReference(AComponent: TComponent);
begin
end;

procedure DoInitialization;
begin
  MComponentIndex := TStringList.Create;
  MComponentIndex.Sorted := True;
  MComponentIndex.Duplicates := dupError;

  MWSRegisterIndex := TStringList.Create;
  MWSRegisterIndex.Sorted := True;
  MWSRegisterIndex.Duplicates := dupError;
end;

{$ifdef VerboseWSRegistration_treedump}
procedure DumpVTree;
  procedure DumpNode(ANode: PClassNode; AIndent: String = '');
  begin
    if ANode = nil then Exit;

    DbgOut(AIndent);

    DbgOut('LCLClass=');
    if ANode^.LCLClass = nil
    then DbgOut('nil')
    else DbgOut(ANode^.LCLClass.Classname);

    DbgOut(' WSClass=');
    if ANode^.WSClass = nil
    then DbgOut('nil')
    else DbgOut(ANode^.WSClass.Classname);

    DbgOut(' VClass=');
    if ANode^.VClass = nil
    then DbgOut('nil')
    else begin
      DbgOut(TClass(ANode^.VClass).Classname);
      DbgOut(' VClass.Parent=');
      if TClass(ANode^.VClass).ClassParent = nil
      then DbgOut('nil')
      else DbgOut(TClass(ANode^.VClass).ClassParent.ClassName);
      
      DbgOut(' Private=');
      if PClass(ANode^.VClass + vmtWSPrivate)^ = nil
      then DbgOut('nil')
      else DbgOut(PClass(ANode^.VClass + vmtWSPrivate)^.ClassName);
    end;

    DbgOut(' VClassName=''', ANode^.VClassName, '''');
    DebugLn;

    DumpNode(ANode^.Child, AIndent + ' ');

    DumpNode(ANode^.Sibling, AIndent);
  end;

var
  n: Integer;
  Node: PClassNode;
begin
  for n := 0 to MComponentIndex.Count - 1 do
  begin
    Node := PClassNode(MComponentIndex.Objects[n]);
    if Node^.Parent = nil
    then DumpNode(Node);
  end;
end;
{$endif}

procedure DoFinalization;
var
  n: Integer;
  Node: PClassNode;
begin
  {$ifdef VerboseWSRegistration_treedump}
  DumpVTree;
  {$endif}

  for n := 0 to MComponentIndex.Count - 1 do
  begin
    Node := PClassNode(MComponentIndex.Objects[n]);
    Dispose(Node);
  end;
  FreeAndNil(MComponentIndex);
  FreeAndNil(MWSRegisterIndex);
end;


finalization
  DoFinalization;

end.
