{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
 
  Author: Mattias Gaertner

  Abstract:
    This unit defines a list of forms descendents. The forms are normal TForm
    descendents with one exception: Every form has its own class. These classes
    are changeable at runtime, so that IDEs can add, remove or rename methods
    and such stuff. Also these forms can be loaded from streams and missing
    components and methods are added just-in-time to the class definition.
    Hence the name for the class: TJITForms.
    Subcomponents are looked up in the list of registered components
    (TJITForms.RegCompList).

  ToDo:
    -Add recursion needed for frames.
}
unit JITForms;

{$mode objfpc}{$H+}

{$I ide.inc}

{ $DEFINE VerboseJITForms}

interface

uses
  {$IFDEF IDE_MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, AvgLvlTree, TypInfo, LCLProc, LResources, Forms, Controls,
  LCLIntf, Dialogs, JITForm, ComponentReg, IDEProcs;

type
  //----------------------------------------------------------------------------
  TJITFormError = (
      jfeNone,
      jfeUnknown,
      jfeUnknownProperty,
      jfeUnknownComponentClass,
      jfeReaderError
    );
  TJITFormErrors = set of TJITFormError;
  
  TJITReaderErrorEvent = procedure(Sender: TObject; ErrorType: TJITFormError;
    var Action: TModalResult) of object;
  TJITPropertyNotFoundEvent = procedure(Sender: TObject; Reader: TReader;
    Instance: TPersistent; var PropName: string; IsPath: boolean;
    var Handled, Skip: Boolean) of object;


  { TJITComponentList }
  
  TJITCompListFlag = (
    jclAutoRenameComponents
    );
  TJITCompListFlags = set of TJITCompListFlag;

  TJITComponentList = class(TPersistentWithTemplates)
  private
    FCurUnknownClass: string;
    FCurUnknownProperty: string;
    FErrors: TLRPositionLinks;
    FOnPropertyNotFound: TJITPropertyNotFoundEvent;
  protected
    FCurReadErrorMsg: string;
    FCurReadJITComponent:TComponent;
    FCurReadClass:TClass;
    FCurReadChild: TComponent;
    FCurReadChildClass: TComponentClass;
    FOnReaderError: TJITReaderErrorEvent;
    FJITComponents: TList;
    FFlags: TJITCompListFlags;
    // jit procedures
    function CreateNewJITClass(ParentClass: TClass;
                          const NewClassName, NewUnitName: ShortString): TClass;
    procedure FreeJITClass(var AClass: TClass);
    procedure DoAddNewMethod(JITClass: TClass; const AName: ShortString;
      ACode: Pointer); // Note: AddNewMethod does not check if method already exists
    procedure DoRemoveMethod(JITClass: TClass; AName: ShortString;
      var OldCode: Pointer); // Note: RemoveMethod does not free code memory
    procedure DoRenameMethod(JITClass: TClass; OldName, NewName: ShortString);
    procedure DoRenameClass(JITClass: TClass; const NewName: ShortString);
    procedure DoRenameUnitNameOfClass(JITClass: TClass;
                                      const NewUnitName: ShortString);
    // TReader events
    procedure ReaderFindMethod(Reader: TReader; const FindMethodName: Ansistring;
      var Address: Pointer; var Error: Boolean);
    procedure ReaderSetMethodProperty(Reader: TReader; Instance: TPersistent;
      PropInfo: PPropInfo; const TheMethodName: string; var Handled: boolean);
    procedure ReaderPropertyNotFound(Reader: TReader; Instance: TPersistent;
      var PropName: string; IsPath: Boolean; var Handled, Skip: Boolean);
    procedure ReaderSetName(Reader: TReader; Component: TComponent;
      var NewName: Ansistring);
    procedure ReaderReferenceName(Reader: TReader; var RefName: Ansistring);
    procedure ReaderAncestorNotFound(Reader: TReader;
      const ComponentName: Ansistring; ComponentClass: TPersistentClass;
      var Component: TComponent);
    procedure ReaderError(Reader: TReader; const ErrorMsg: Ansistring;
      var Handled: Boolean);
    procedure ReaderFindComponentClass(Reader: TReader;
      const FindClassName: Ansistring; var ComponentClass: TComponentClass);
    procedure ReaderCreateComponent(Reader: TReader;
      ComponentClass: TComponentClass; var Component: TComponent);
    procedure ReaderReadComponent(Component: TComponent);
    // some useful functions
    function GetItem(Index:integer):TComponent;
    function OnFindGlobalComponent(const AName:AnsiString):TComponent;
    procedure InitReading(BinStream: TStream; var Reader: TReader;
                          DestroyDriver: Boolean); virtual;
    function DoCreateJITComponent(const NewComponentName, NewClassName,
                         NewUnitName: shortstring; ParentClass: TClass;
                         Visible: boolean):integer;
    procedure DoFinishReading; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property Items[Index: integer]: TComponent read GetItem; default;
    function Count: integer;
    function AddNewJITComponent(const NewUnitName: shortstring;
                                ParentClass: TClass): integer;
    function AddJITComponentFromStream(BinStream: TStream;
                                ParentClass: TClass; ParentBinStream: TStream;
                                const NewUnitName: ShortString;
                                Interactive, Visible: Boolean):integer;
    procedure DestroyJITComponent(JITComponent: TComponent);
    procedure DestroyJITComponent(Index: integer);
    function IndexOf(JITComponent: TComponent): integer;
    function Contains(JITComponent: TComponent): boolean;
    function FindComponentByClassName(const AClassName: shortstring): integer;
    function FindComponentByClass(AClass: TComponentClass): integer;
    function FindComponentByName(const AName: shortstring): integer;
    procedure GetUnusedNames(var ComponentName, ComponentClassName: shortstring);
    function CreateNewMethod(JITComponent: TComponent;
                             const AName: ShortString): TMethod;
    procedure RemoveMethod(JITComponent: TComponent; const AName: ShortString);
    procedure RenameMethod(JITComponent: TComponent;
                           const OldName, NewName: ShortString);
    procedure RenameComponentClass(JITComponent: TComponent;
                                   const NewName: ShortString);
    procedure RenameComponentUnitname(JITComponent: TComponent;
                                      const NewUnitName: ShortString);
    // child components
    function AddJITChildComponentFromStream(JITOwnerComponent: TComponent;
      BinStream: TStream; ComponentClass: TComponentClass;
      ParentControl: TWinControl): TComponent;
  public
    property OnReaderError: TJITReaderErrorEvent
                                       read FOnReaderError write FOnReaderError;
    property OnPropertyNotFound: TJITPropertyNotFoundEvent
                             read FOnPropertyNotFound write FOnPropertyNotFound;
    property CurReadJITComponent: TComponent read FCurReadJITComponent;
    property CurReadClass: TClass read FCurReadClass;
    property CurReadChild: TComponent read FCurReadChild;
    property CurReadChildClass: TComponentClass read FCurReadChildClass;
    property CurReadErrorMsg: string read FCurReadErrorMsg;
    property CurUnknownProperty: string read FCurUnknownProperty;
    property CurUnknownClass: string read FCurUnknownClass;
    property Errors: TLRPositionLinks read FErrors;
  end;


  { TJITForms }
  
  TJITForms = class(TJITComponentList)
  private
    function GetItem(Index: integer): TForm;
  public
    function IsJITForm(AComponent: TComponent): boolean;
    property Items[Index:integer]: TForm read GetItem; default;
  end;
  
  
  { TJITNonFormComponents }
  
  TJITNonFormComponents = class(TJITComponentList)
  public
    function IsJITNonForm(AComponent: TComponent): boolean;
  end;


  TJITMethods = class;

  { TJITMethod }

  TJITMethod = class
  private
    FMethod: TMethod;
    FOwner: TJITMethods;
    FTheClass: TClass;
    FTheMethodName: shortstring;
  public
    constructor Create(AnOwner: TJITMethods; aClass: TClass;
                       const aMethodName: shortstring);
    destructor Destroy; override;
    property Method: TMethod read FMethod;
    property TheClass: TClass read FTheClass;
    property TheMethodName: shortstring read FTheMethodName;
    property Owner: TJITMethods read FOwner;
  end;
  
  
  { TJITMethods }

  TJITMethods = class
  private
    fClearing: boolean;
    fMethods: TAvgLvlTree;// sorted with CompareJITMethod
    procedure InternalAdd(const AMethod: TJITMethod);
    procedure InternalRemove(const AMethod: TJITMethod);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(aClass: TClass; const aMethodName: shortstring): TJITMethod;
    function Find(aClass: TClass; const aMethodName: shortstring): TJITMethod;
    function Delete(aMethod: TJITMethod): boolean;
    function Delete(aClass: TClass; const aMethodName: shortstring): boolean;
    procedure DeleteAllOfClass(aClass: TClass);
    function Rename(aClass: TClass;
                    const OldMethodName, NewMethodName: shortstring): boolean;
  end;
  
function IsJITMethod(const aMethod: TMethod): boolean;
function CompareJITMethod(Data1, Data2: Pointer): integer;

var
  JITMethods: TJITMethods = nil;


function ClassAsString(AClass: TClass): string;
function ClassMethodTableAsString(AClass: TClass): string;
function ClassTypeInfoAsString(AClass: TClass): string;
function ClassFieldTableAsString(AClass: TClass): string;

function CalculateTypeDataSize(PropInfoCount: integer): integer;
function CalculateTypeInfoSize(const AClassName: shortstring;
                               PropInfoCount: integer): integer;
function GetTypeDataPropCountAddr(TypeData: PTypeData): PWord;

const
  DefaultJITUnitName = 'VirtualUnitForJITClasses';


implementation

{$IFOPT R+}{$DEFINE RangeCheckOn}{$ENDIF}

//------------------------------------------------------------------------------
// adding, removing and renaming of classes and methods at runtime

const
  vmtInstanceSizeNeg = vmtInstanceSize+sizeof(ptrint);

type
  // these definitions are copied from objpas.inc

  TMethodNameRec = packed record
    Name : PShortString;
    Addr : Pointer;
  end;

  TMethodNameTable = packed record
    Count : DWord;
    // for runtime range checking it is important to give a range
    Entries : packed array[0..1000000] of TMethodNameRec;
  end;
  PMethodNameTable =  ^TMethodNameTable;

  PFieldClassTable = ^TFieldClassTable;
  TFieldClassTable = 
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  record  
    Count: Word;
    Entries: array[Word] of TPersistentClass;
  end;

  PFieldInfo = ^TFieldInfo;
  TFieldInfo = 
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  record  
    FieldOffset: LongWord;
    ClassTypeIndex: Word;
    Name: ShortString;
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = 
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  record  
    FieldCount: Word;
    ClassTable: PFieldClassTable;
    { should be array[Word] of TFieldinfo;  but Elements have variant size! force at least proper alignment }
    Fields: array[0..0] of TFieldInfo;
  end;

function GetVMTSize(AClass: TClass): integer;
var
  p: PPointer;
begin
  Result:=10000;
  if AClass=nil then exit;
  p:=PPointer(pointer(AClass)+vmtMethodStart);
  Result:=vmtMethodStart;
  while (p^<>nil) and (Result<10000) do begin
    inc(p);
    inc(Result,SizeOf(Pointer));
  end;
end;

function FindVMTMethodOffset(AClass: TClass; MethodPointer: Pointer): integer;
var
  i: Integer;
  p: Pointer;
begin
  i:=vmtMethodStart div SizeOf(Pointer);
  while i<=10000 do begin
    p:=PPointer(pointer(AClass))[i];
    if p=nil then break;
    if p=MethodPointer then begin
      Result:=i*SizeOf(Pointer);
      exit;
    end;
    inc(i);
  end;
  Result:=0;
end;

function GetVMTVirtualMethodOffset(
  ParentClassWithVirtualMethod: TClass; MethodOfParentClass: Pointer;
  ClassWithOverrideMethod: TClass; OverrideMethodOfClass: Pointer
  ): integer;
var
  ParentMethodOffset: LongInt;
  OverrideMethodOffset: LongInt;
begin
  ParentMethodOffset:=FindVMTMethodOffset(
                              ParentClassWithVirtualMethod,MethodOfParentClass);
  if ParentMethodOffset<=0 then
    raise Exception.Create('GetVMTVirtualMethodOffset Parent Virtual Method not found');
  OverrideMethodOffset:=FindVMTMethodOffset(
                                 ClassWithOverrideMethod,OverrideMethodOfClass);
  if OverrideMethodOffset<=0 then
    raise Exception.Create('GetVMTVirtualMethodOffset Override Method not found');
  if ParentMethodOffset<>OverrideMethodOffset then
    raise Exception.Create('GetVMTVirtualMethodOffset Virtual Method Offset <> Override Method Offset');
  Result:=OverrideMethodOffset;
end;

{ TComponentWithOverrideValidateRename }
type
  TComponentWithOverrideValidateRename = class(TComponent)
  public
    procedure ValidateRename(AComponent: TComponent;
      const CurName, NewName: string); override;
  end;

var
  TComponentValidateRenameOffset: LongInt = 0;

procedure TComponentWithOverrideValidateRename.ValidateRename(
  AComponent: TComponent; const CurName, NewName: string);
var
  Designer: TIDesigner;
begin
  //debugln('TComponentWithOverrideValidateRename.ValidateRename ',DbgSName(Self));
  inherited ValidateRename(AComponent, CurName, NewName);
  Designer:=FindRootDesigner(Self);
  if Designer <> nil then
    Designer.ValidateRename(AComponent, CurName, NewName);
end;


function GetTComponentValidateRenameVMTOffset: integer;
begin
  Result:=GetVMTVirtualMethodOffset(TComponent,
    @TComponent.ValidateRename,
    TComponentWithOverrideValidateRename,
    @TComponentWithOverrideValidateRename.ValidateRename);
end;

var
  MyFindGlobalComponentProc: function(const AName: AnsiString): TComponent of object;

function MyFindGlobalComponent(const AName: AnsiString): TComponent;
begin
  Result:=MyFindGlobalComponentProc(AName);
end;

function IsJITMethod(const aMethod: TMethod): boolean;
begin
  Result:=(aMethod.Data<>nil) and (aMethod.Code=nil)
      and (TObject(aMethod.Data).ClassType=TJITMethod);
end;

function ClassAsString(AClass: TClass): string;
var
  ParentClass: TClass;
begin
  Result:='Class='+DbgS(AClass);
  if AClass=nil then exit;
  Result:=Result+' Name="'+AClass.ClassName+'"';
  ParentClass:=AClass.ClassParent;
  if ParentClass<>nil then
    Result:=Result+' Parent='+DbgS(ParentClass)+'-"'+ParentClass.ClassName+'"';
  Result:=Result+LineEnding;
  Result:=Result+' vmtInstanceSize='+IntToStr(PLongInt(pointer(AClass)+vmtInstanceSize)^);
  Result:=Result+' vmtInstanceSizeNeg='+IntToStr(PLongInt(pointer(AClass)+vmtInstanceSizeNeg)^);
  Result:=Result+' vmtParent='+DbgS(pcardinal(pointer(AClass)+vmtParent)^);
  Result:=Result+' vmtClassName="'+PShortString((Pointer(AClass)+vmtClassName)^)^+'"';
  Result:=Result+' vmtDynamicTable='+DbgS(pcardinal(pointer(AClass)+vmtDynamicTable)^);
  Result:=Result+' vmtMethodTable='+DbgS(pcardinal(pointer(AClass)+vmtMethodTable)^);
  Result:=Result+' vmtFieldTable='+DbgS(pcardinal(pointer(AClass)+vmtFieldTable)^);
  Result:=Result+' vmtTypeInfo='+DbgS(pcardinal(pointer(AClass)+vmtTypeInfo)^);
  Result:=Result+' vmtInitTable='+DbgS(pcardinal(pointer(AClass)+vmtInitTable)^);
  Result:=Result+' vmtAutoTable='+DbgS(pcardinal(pointer(AClass)+vmtAutoTable)^);
  Result:=Result+' vmtIntfTable='+DbgS(pcardinal(pointer(AClass)+vmtIntfTable)^);
  Result:=Result+' vmtMsgStrPtr='+DbgS(pcardinal(pointer(AClass)+vmtMsgStrPtr)^);
  Result:=Result+LineEnding;
  Result:=Result+' MethodTable=['+ClassMethodTableAsString(AClass)+']';
  Result:=Result+LineEnding;
  Result:=Result+' TypeInfo=['+ClassTypeInfoAsString(AClass)+']';
  Result:=Result+LineEnding;
  Result:=Result+' FieldTable=['+ClassFieldTableAsString(AClass)+']';
end;

function ClassMethodTableAsString(AClass: TClass): string;
var
  MethodTable: PMethodNameTable;
  i: Integer;
begin
  Result:='';
  if AClass=nil then exit;
  MethodTable:=PMethodNameTable((Pointer(AClass)+vmtMethodTable)^);
  if MethodTable=nil then exit;
  for i:=0 to MethodTable^.Count-1 do begin
    if i>0 then Result:=Result+',';
    Result:=Result+IntToStr(i)+':"'+(MethodTable^.Entries[i].Name^)+'"'
      +':'+DbgS(MethodTable^.Entries[i].Addr);
  end;
end;

function ClassTypeInfoAsString(AClass: TClass): string;
var
  TypeInfo: PTypeInfo;
  TypeData: PTypeData;
  PropInfo: PPropInfo;
  CurCount: integer;
  i: Integer;
begin
  Result:='';
  if AClass=nil then exit;
  TypeInfo:=AClass.ClassInfo;
  if TypeInfo=nil then exit;
  Result:=Result+'ClassInfo^.Name="'+TypeInfo^.Name+'"';
  // read all property infos of current class
  TypeData:=GetTypeData(TypeInfo);
  if TypeData=nil then exit;
  Result:=Result+' ClassType='+DbgS(TypeData^.ClassType);
  if TypeData^.ClassType<>AClass then
    Result:=Result+LineEnding
      +' WARNING: ClassType<>AClass('+DbgS(AClass)+')'+LineEnding;
  Result:=Result+' ParentInfo='+DbgS(TypeData^.ParentInfo);
  if (AClass.ClassParent<>nil)
  and (TypeData^.ParentInfo<>AClass.ClassParent.ClassInfo) then
    Result:=Result+LineEnding
      +' WARNING: TypeData^.ParentInfo<>AClass.ClassParent.ClassInfo('
      +DbgS(TypeData^.ParentInfo)+'<>'
      +DbgS(AClass.ClassParent.ClassInfo)+'<>'+')'+LineEnding;
  Result:=Result+' PropCount='+IntToStr(TypeData^.PropCount);
  Result:=Result+' UnitName="'+TypeData^.UnitName+'"';

  // skip unitname
  PropInfo:=PPropInfo(PByte(@TypeData^.UnitName)+Length(TypeData^.UnitName)+1);
  // read property count
  CurCount:=PWord(PropInfo)^;
  Result:=Result+' CurPropCnt='+IntToStr(CurCount);
  inc(PropInfo,SizeOf(Word));

  // read properties
  Result:=Result+' Properties={';
  for i:=0 to CurCount-1 do begin
    if i>0 then Result:=Result+',';
    // point PropInfo to next propinfo record.
    // Located at Name[Length(Name)+1] !
    Result:=Result+IntToStr(i)+':PropName="'+PropInfo^.Name+'"'
                  +':Type="'+PropInfo^.PropType^.Name+'"';
    PropInfo:=PPropInfo(pointer(@PropInfo^.Name)+PByte(@PropInfo^.Name)^+1);
  end;
  Result:=Result+'}';
end;

function ClassFieldTableAsString(AClass: TClass): string;
var
  FieldTable: PFieldTable;
  FieldInfo: PFieldInfo;
  i: Integer;
  ClassTable: PFieldClassTable;
begin
  Result:='';
  if AClass=nil then exit;
  FieldTable:=PFieldTable((Pointer(AClass)+vmtFieldTable)^);
  if FieldTable=nil then exit;
  Result:=Result+'FieldCount='+IntToStr(FieldTable^.FieldCount);
  ClassTable:=FieldTable^.ClassTable;
  Result:=Result+' ClassTable='+DbgS(ClassTable);
  if ClassTable<>nil then begin
    Result:=Result+'={';
    for i:=0 to ClassTable^.Count-1 do begin
      if i>0 then Result:=Result+',';
      Result:=Result+IntToStr(i)+':Name="'+ClassTable^.Entries[i].ClassName+'"';
    end;
  end;
  Result:=Result+'}';
  FieldInfo := @FieldTable^.Fields[0];
  Result := Result + ' Fields={';
  for i := 0 to FieldTable^.FieldCount-1 do begin
    if i > 0 then Result:=Result+',';
    Result := Result + IntToStr(i) 
      + ':Name="' + FieldInfo^.Name + '"'
      + ':Offset=' +IntToStr(FieldInfo^.FieldOffset);
    FieldInfo := PFieldInfo(PByte(@FieldInfo^.Name) + 1 + Length(FieldInfo^.Name));
    {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
    FieldInfo := Align(FieldInfo, SizeOf(Pointer));
    {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  end;
  Result := Result+'}';
end;

function CalculateTypeDataSize(PropInfoCount: integer): integer;
begin
  Result := SizeOf(TTypeData) + 2; // TTypeData + one word for new prop count
  // Actually the size depends on the UnitName. But SizeOf(TTypeData) already
  // uses the maximum size of the shortstring.
  {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  if Result and (SizeOf(Pointer) - 1) <> 0 then
    Inc(Result, SizeOf(Pointer)); // a few bytes too much, but at least enough
  {$endif}
  inc(Result,PropInfoCount*SizeOf(TPropInfo));
end;

function GetTypeDataPropCountAddr(TypeData: PTypeData): PWord;
begin
  Result:=PWord(PByte(@TypeData^.UnitName)+Length(TypeData^.UnitName)+1);
  {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := Align(Result, SizeOf(Pointer));
  {$endif}
end;

function CompareJITMethod(Data1, Data2: Pointer): integer;
var
  JITMethod1: TJITMethod;
  JITMethod2: TJITMethod;
begin
  JITMethod1:=TJITMethod(Data1);
  JITMethod2:=TJITMethod(Data2);
  Result:=ComparePointers(JITMethod1.TheClass,JITMethod2.TheClass);
  if Result<>0 then exit;
  Result:=CompareText(JITMethod1.TheMethodName,JITMethod2.TheMethodName);
end;

function CalculateTypeInfoSize(const AClassName: shortstring;
  PropInfoCount: integer): integer;
begin
  Result := SizeOf(TTypeKind) + 1 + length(AClassName)
           + CalculateTypeDataSize(PropInfoCount);
  {$warnings off}
  if SizeOf(TTypeKind)<>1 then
    raise Exception.Create('CalculateTypeInfoSize SizeOf(TTypeInfo^.Kind)<>1');
  {$warnings on}
end;

//------------------------------------------------------------------------------


{ TJITComponentList }

constructor TJITComponentList.Create;
begin
  inherited Create;
  FJITComponents:=TList.Create;
  FErrors:=TLRPositionLinks.Create;
end;

destructor TJITComponentList.Destroy;
begin
  while FJITComponents.Count>0 do DestroyJITComponent(FJITComponents.Count-1);
  FJITComponents.Free;
  FErrors.Free;
  inherited Destroy;
end;

function TJITComponentList.GetItem(Index:integer):TComponent;
begin
  Result:=TComponent(FJITComponents[Index]);
end;

function TJITComponentList.Count:integer;
begin
  Result:=FJITComponents.Count;
end;

function TJITComponentList.IndexOf(JITComponent:TComponent):integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Items[Result]<>JITComponent) do dec(Result);
end;

function TJITComponentList.Contains(JITComponent: TComponent): boolean;
begin
  Result:=IndexOf(JITComponent)>=0;
end;

procedure TJITComponentList.DestroyJITComponent(JITComponent:TComponent);
var a:integer;
begin
  if JITComponent=nil then
    RaiseException('TJITComponentList.DestroyJITForm JITComponent=nil');
  a:=IndexOf(JITComponent);
  if a<0 then
    RaiseException('TJITComponentList.DestroyJITForm JITComponent.ClassName='+
      JITComponent.ClassName);
  if a>=0 then DestroyJITComponent(a);
end;

procedure TJITComponentList.DestroyJITComponent(Index:integer);
var
  OldClass: TClass;
begin
  OldClass:=Items[Index].ClassType;
  try
    Items[Index].Free;
  except
    on E: Exception do begin
      DebugLn('[TJITComponentList.DestroyJITComponent] ERROR destroying component ',E.Message);
    end;
  end;
  FreeJITClass(OldClass);
  FJITComponents.Delete(Index);
end;

function TJITComponentList.FindComponentByClassName(
  const AClassName:shortstring):integer;
begin
  Result:=FJITComponents.Count-1;
  while (Result>=0)
  and (CompareText(Items[Result].ClassName,AClassName)<>0) do
    dec(Result);
end;

function TJITComponentList.FindComponentByClass(AClass: TComponentClass
  ): integer;
begin
  Result:=FJITComponents.Count-1;
  while (Result>=0) and (Items[Result].ClassType<>AClass) do
    dec(Result);
end;

function TJITComponentList.FindComponentByName(const AName:shortstring):integer;
begin
  Result:=FJITComponents.Count-1;
  while (Result>=0)
  and (CompareText(Items[Result].Name,AName)<>0) do
    dec(Result);
end;

procedure TJITComponentList.GetUnusedNames(
  var ComponentName,ComponentClassName:shortstring);
var a:integer;
  ComponentPrefix: String;
begin
  a:=1;
  ComponentPrefix:=ComponentClassName;
  if ComponentPrefix='' then
    ComponentPrefix:='Component';
  if ComponentPrefix[1] in ['t','T'] then
    ComponentPrefix:=copy(ComponentPrefix,2,length(ComponentPrefix));
  repeat
    ComponentName:=ComponentPrefix+IntToStr(a);
    ComponentClassName:='T'+ComponentName;
    inc(a);
  until (FindComponentByName(ComponentName)<0)
        and (FindComponentByClassName(ComponentClassName)<0);
end;

function TJITComponentList.AddNewJITComponent(const NewUnitName: shortstring;
  ParentClass: TClass): integer;
var
  NewComponentName, NewClassName: shortstring;
begin
  {$IFDEF VerboseJITForms}
  debugln('[TJITComponentList] AddNewJITComponent');
  {$ENDIF}
  NewClassName:=ParentClass.ClassName;
  GetUnusedNames(NewComponentName,NewClassName);
  {$IFDEF VerboseJITForms}
  debugln('TJITComponentList.AddNewJITComponent NewComponentName=',NewComponentName,' NewClassName=',NewClassName,
    ' NewUnitName=',NewUnitName,' ParentClass=',ParentClass.ClassName);
  {$ENDIF}
  Result:=DoCreateJITComponent(NewComponentName,NewClassName,NewUnitName,
                               ParentClass,true);
end;

function TJITComponentList.AddJITComponentFromStream(BinStream: TStream;
  ParentClass: TClass; ParentBinStream: TStream;
  const NewUnitName: ShortString;
  Interactive, Visible: Boolean): integer;
//  returns new index
// -1 = invalid stream

  procedure ReadStream(AStream: TStream);
  var
    Reader:TReader;
    DestroyDriver: Boolean;
  begin
    {$IFDEF VerboseJITForms}
    debugln('[TJITComponentList.AddJITComponentFromStream] InitReading ...');
    {$ENDIF}

    DestroyDriver:=false;
    InitReading(AStream,Reader,DestroyDriver);
    {$IFDEF VerboseJITForms}
    debugln('[TJITComponentList.AddJITComponentFromStream] Read ...');
    {$ENDIF}
    try
      Reader.ReadRootComponent(FCurReadJITComponent);
      {$IFDEF VerboseJITForms}
      debugln('[TJITComponentList.AddJITComponentFromStream] Finish Reading ...');
      {$ENDIF}
      DoFinishReading;
    finally
      UnregisterFindGlobalComponentProc(@MyFindGlobalComponent);
      Application.FindGlobalComponentEnabled:=true;
      if DestroyDriver then Reader.Driver.Free;
      Reader.Free;
    end;
  end;

var
  NewClassName: shortstring;
  NewName: string;
  IsInherited: Boolean;
begin
  Result:=-1;
  NewClassName:=GetClassNameFromLRSStream(BinStream, IsInherited);
  if IsInherited then ;
  if NewClassName='' then begin
    MessageDlg('No classname in stream found.',mtError,[mbOK],0);
    exit;
  end;

  {$IFDEF VerboseJITForms}
  debugln('[TJITComponentList.AddJITComponentFromStream] Create ...');
  {$ENDIF}
  try
    Result:=DoCreateJITComponent('',NewClassName,NewUnitName,ParentClass,Visible);
    if Result<0 then exit;
    if ParentBinStream<>nil then
      ReadStream(ParentBinStream);
    ReadStream(BinStream);

    if FCurReadJITComponent.Name='' then begin
      NewName:=FCurReadJITComponent.ClassName;
      if NewName[1] in ['T','t'] then
        System.Delete(NewName,1,1);
      FCurReadJITComponent.Name:=NewName;
    end;
  except
    on E: Exception do begin
      DebugLn('[TJITComponentList.AddJITChildComponentFromStream] ERROR reading form stream'
         +' of Class ''',NewClassName,''' Error: ',E.Message);
      if Result>=0 then begin
        // try freeing the unfinished thing
        FCurReadJITComponent:=nil;
        DestroyJITComponent(Result);
        Result:=-1;
      end;
    end;
  end;
  FCurReadJITComponent:=nil;
end;

function TJITComponentList.OnFindGlobalComponent(
  const AName: AnsiString): TComponent;
begin
  Result:=Application.FindComponent(AName);
end;

procedure TJITComponentList.InitReading(BinStream: TStream;
  var Reader: TReader; DestroyDriver: Boolean);
begin
  FFlags:=FFlags-[jclAutoRenameComponents];
  
  DestroyDriver:=false;
  Reader:=CreateLRSReader(BinStream,DestroyDriver);
  MyFindGlobalComponentProc:=@OnFindGlobalComponent;
  RegisterFindGlobalComponentProc(@MyFindGlobalComponent);
  Application.FindGlobalComponentEnabled:=false;

  {$IFDEF VerboseJITForms}
  debugln('[TJITComponentList.InitReading] A');
  {$ENDIF}
  // connect TReader events
  Reader.OnError:=@ReaderError;
  Reader.OnPropertyNotFound:=@ReaderPropertyNotFound;
  Reader.OnFindMethod:=@ReaderFindMethod;
  Reader.OnSetMethodProperty:=@ReaderSetMethodProperty;
  Reader.OnSetName:=@ReaderSetName;
  Reader.OnReferenceName:=@ReaderReferenceName;
  Reader.OnAncestorNotFound:=@ReaderAncestorNotFound;
  Reader.OnCreateComponent:=@ReaderCreateComponent;
  Reader.OnFindComponentClass:=@ReaderFindComponentClass;

  {$IFDEF VerboseJITForms}
  debugln('[TJITComponentList.InitReading] B');
  {$ENDIF}

  FCurReadChildClass:=nil;
  FCurReadChild:=nil;
  FCurReadErrorMsg:='';
end;

function TJITComponentList.DoCreateJITComponent(
  const NewComponentName, NewClassName, NewUnitName: shortstring;
  ParentClass: TClass; Visible: boolean):integer;
var
  Instance:TComponent;
  ok: boolean;
begin
  Result:=-1;
  Instance:=nil;
  FCurReadClass:=nil;
  FCurReadJITComponent:=nil;
  
  try
    ok:=false;
    // create new class and an instance
    //debugln('[TJITForms.DoCreateJITComponent] Creating new JIT class '''+NewClassName+''' ...');
    Pointer(FCurReadClass):=CreateNewJITClass(ParentClass,NewClassName,
                                              NewUnitName);
    //debugln('[TJITForms.DoCreateJITComponent] Creating an instance of JIT class "'+NewClassName+'" = class('+ParentClass.ClassName+') ...');
    Instance:=TComponent(FCurReadClass.NewInstance);
    //debugln('[TJITForms.DoCreateJITComponent] Initializing new instance ... ',DbgS(Instance));
    TComponent(FCurReadJITComponent):=Instance;
    try
      // set into design mode
      SetComponentDesignMode(Instance,true);
      if (not Visible) and (Instance is TControl) then
        TControl(Instance).ControlStyle:=
                            TControl(Instance).ControlStyle+[csNoDesignVisible];
      // finish 'create' component
      Instance.Create(nil);
      if NewComponentName<>'' then
        Instance.Name:=NewComponentName;
      DoRenameClass(FCurReadClass,NewClassName);
      ok:=true;
    //debugln('[TJITForms.DoCreateJITComponent] Initialization was successful! FormName="',NewFormName,'"');
    finally
      if not ok then begin
        TComponent(FCurReadJITComponent):=nil;
        DebugLn('[TJITForms.DoCreateJITComponent] Error while creating instance: NewComponentName="',NewComponentName,'" NewClassName="',NewClassName,'" NewUnitName="',NewUnitName,'"');
      end;
    end;
  except
    on E: Exception do begin
      DebugLn('[TJITForms.DoCreateJITComponent] Error ',E.Message);
      try
        if FCurReadClass<>nil then
          FreeJITClass(FCurReadClass);
        Instance.Free;
      except
        on E: Exception do begin
          DebugLn('[TJITForms.DoCreateJITComponent] Error while destroying instance: NewComponentName="',NewComponentName,'" NewClassName="',NewClassName,'" NewUnitName="',NewUnitName,'" ',E.Message);
        end;
      end;
    end;
  end;
  if FCurReadJITComponent<>nil then
    Result:=FJITComponents.Add(FCurReadJITComponent);
end;

procedure TJITComponentList.DoFinishReading;
begin

end;

procedure TJITComponentList.RemoveMethod(JITComponent:TComponent;
  const AName:ShortString);
var OldCode:Pointer;
begin
  {$IFDEF VerboseJITForms}
  debugln('TJITComponentList.RemoveMethod ',JITComponent.Name,':',JITComponent.Name,' Method=',AName);
  {$ENDIF}
  if JITComponent=nil then
    raise Exception.Create('TJITComponentList.RemoveMethod JITComponent=nil');
  if IndexOf(JITComponent)<0 then
    raise Exception.Create('TJITComponentList.RemoveMethod JITComponent.ClassName='+
      JITComponent.ClassName);
  if (AName='') or (not IsValidIdent(AName)) then
    raise Exception.Create('TJITComponentList.RemoveMethod invalid name: "'+AName+'"');

  // delete TJITMethod
  if JITMethods.Delete(JITComponent.ClassType,AName) then begin
    // this was a TJITmethod
    exit;
  end;
  
  // delete real method
  OldCode:=nil;
  DoRemoveMethod(JITComponent.ClassType,AName,OldCode);
  FreeMem(OldCode);
end;

procedure TJITComponentList.RenameMethod(JITComponent:TComponent;
  const OldName,NewName:ShortString);
begin
  {$IFDEF VerboseJITForms}
  debugln('TJITComponentList.RenameMethod ',JITComponent.Name,':',JITComponent.Name,' Old=',OldName,' NewName=',NewName);
  {$ENDIF}
  if JITComponent=nil then
    raise Exception.Create('TJITComponentList.RenameMethod JITComponent=nil');
  if IndexOf(JITComponent)<0 then
    raise Exception.Create('TJITComponentList.RenameMethod JITComponent.ClassName='+
      JITComponent.ClassName);
  if (NewName='') or (not IsValidIdent(NewName)) then
    raise Exception.Create('TJITComponentList.RenameMethod invalid name: "'+NewName+'"');
    
  // rename TJITMethod
  if JITMethods.Rename(JITComponent.ClassType,OldName,NewName) then begin
    // this was a TJITMethod
    exit;
  end;
    
  // rename real method
  DoRenameMethod(JITComponent.ClassType,OldName,NewName);
end;

procedure TJITComponentList.RenameComponentClass(JITComponent:TComponent;
  const NewName:ShortString);
begin
  {$IFDEF VerboseJITForms}
  debugln('TJITComponentList.RenameComponentClass ',JITComponent.Name,':',JITComponent.Name,' New=',NewName);
  {$ENDIF}
  if JITComponent=nil then
    raise Exception.Create('TJITComponentList.RenameComponentClass JITComponent=nil');
  if IndexOf(JITComponent)<0 then
    raise Exception.Create('TJITComponentList.RenameComponentClass JITComponent.ClassName='+
      JITComponent.ClassName);
  if (NewName='') or (not IsValidIdent(NewName)) then
    raise Exception.Create('TJITComponentList.RenameComponentClass invalid name: "'+NewName+'"');
  DoRenameClass(JITComponent.ClassType,NewName);
end;

procedure TJITComponentList.RenameComponentUnitname(JITComponent: TComponent;
  const NewUnitName: ShortString);
begin
  {$IFDEF VerboseJITForms}
  debugln('TJITComponentList.RenameComponentUnitname ',JITComponent.Name,':',JITComponent.Name,' New=',NewUnitName);
  {$ENDIF}
  if JITComponent=nil then
    raise Exception.Create('TJITComponentList.RenameComponentUnitname JITComponent=nil');
  if IndexOf(JITComponent)<0 then
    raise Exception.Create('TJITComponentList.RenameComponentUnitname JITComponent.ClassName='+
      JITComponent.ClassName);
  if (NewUnitName='') or (not IsValidIdent(NewUnitName)) then
    raise Exception.Create('TJITComponentList.RenameComponentUnitname invalid name: "'+NewUnitName+'"');
  DoRenameUnitNameOfClass(JITComponent.ClassType,NewUnitName);
end;

function TJITComponentList.AddJITChildComponentFromStream(
  JITOwnerComponent: TComponent; BinStream: TStream;
  ComponentClass: TComponentClass; ParentControl: TWinControl): TComponent;
var
  Reader: TReader;
  NewComponent: TComponent;
  DestroyDriver: Boolean;
begin
  Result:=nil;
  NewComponent:=nil;
  if IndexOf(JITOwnerComponent)<0 then
    RaiseException('TJITComponentList.AddJITChildComponentFromStream');
  {$IFDEF VerboseJITForms}
  debugln('[TJITComponentList.AddJITChildComponentFromStream] A');
  {$ENDIF}
  try
    DestroyDriver:=false;
    InitReading(BinStream,Reader,DestroyDriver);
    {$IFDEF VerboseJITForms}
    debugln('[TJITComponentList.AddJITChildComponentFromStream] B');
    {$ENDIF}
    try
      FCurReadJITComponent:=JITOwnerComponent;
      FCurReadClass:=JITOwnerComponent.ClassType;

      FFlags:=FFlags+[jclAutoRenameComponents];
      {$IFDEF VerboseJITForms}
      debugln('[TJITComponentList.AddJITChildComponentFromStream] C1 ',ComponentClass.ClassName);
      {$ENDIF}
      Reader.Root := FCurReadJITComponent;
      Reader.Owner := FCurReadJITComponent;
      Reader.Parent := ParentControl;
      Reader.BeginReferences;
      try
        Reader.Driver.BeginRootComponent;
        NewComponent:=Reader.ReadComponent(nil);
        Reader.FixupReferences;
      finally
        Reader.EndReferences;
      end;
      DebugLn('[TJITComponentList.AddJITChildComponentFromStream] C6 ');

      {$IFDEF VerboseJITForms}
      debugln('[TJITComponentList.AddJITChildComponentFromStream] D');
      {$ENDIF}
      DoFinishReading;
    finally
      UnregisterFindGlobalComponentProc(@MyFindGlobalComponent);
      Application.FindGlobalComponentEnabled:=true;
      if DestroyDriver then Reader.Driver.Free;
      Reader.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('[TJITComponentList.AddJITChildComponentFromStream] ERROR reading form stream'
         +' of Class ''',ComponentClass.ClassName,''' Error: ',E.Message);
    end;
  end;
  Result:=NewComponent;
end;

function TJITComponentList.CreateNewMethod(JITComponent: TComponent;
  const AName: ShortString): TMethod;
var
  OldCode: Pointer;
  {$IFDEF EnableFakeMethods}
  JITMethod: TJITMethod;
  {$ELSE}
  CodeTemplate, NewCode: Pointer;
  CodeSize: integer;
  {$ENDIF}
begin
  {$IFDEF VerboseJITForms}
  debugln('TJITComponentList.CreateNewMethod ',JITComponent.Name,':',JITComponent.Name,' Method=',AName);
  {$ENDIF}
  if JITComponent=nil then
    raise Exception.Create('TJITComponentList.CreateNewMethod JITComponent=nil');
  if IndexOf(JITComponent)<0 then
    raise Exception.Create('TJITComponentList.CreateNewMethod JITComponent.ClassName='+
      JITComponent.ClassName);
  if (AName='') or (not IsValidIdent(AName)) then
    raise Exception.Create('TJITComponentList.CreateNewMethod invalid name: "'+AName+'"');
  OldCode:=JITComponent.MethodAddress(AName);
  if OldCode<>nil then begin
    // there is already a real method with this name
    Result.Data:=JITComponent;
    Result.Code:=OldCode;
    exit;
  end;
  {$IFDEF EnableFakeMethods}
  // create a TJITMethod
  JITMethod:=JITMethods.Add(JITComponent.ClassType,AName);
  Result:=JITMethod.Method;
  {$ELSE}
  CodeTemplate:=MethodAddress('DoNothing');
  CodeSize:=100; // !!! what is the real codesize of DoNothing? !!!
  GetMem(NewCode,CodeSize);
  Move(CodeTemplate^,NewCode^,CodeSize);
  DoAddNewMethod(JITComponent.ClassType,AName,NewCode);
  Result.Data:=JITComponent;
  Result.Code:=NewCode;
  {$ENDIF}
end;

function TJITComponentList.CreateNewJITClass(ParentClass: TClass;
  const NewClassName, NewUnitName: ShortString): TClass;
// Create a new class (vmt, virtual method table, field table and typeinfo)
// that descends from ParentClass.
// The new class will have no new variables, no new methods and no new fields.
var
  NewVMT: Pointer;
  ClassNamePShortString: Pointer;
  NewFieldTable: PFieldTable;
  NewClassTable: PFieldClassTable;
  NewTypeInfo: PTypeInfo;
  NewTypeData: PTypeData;
  TypeInfoSize: Integer;
  AddedPropCount: PWord;
  vmtSize: Integer;
  vmtTailSize: Integer;
begin
  if ParentClass=nil then
    raise Exception.Create('CreateNewClass ParentClass=nil');
  if NewClassName='' then
    raise Exception.Create('CreateNewClass NewClassName empty');
  if not IsValidIdent(NewClassName) then
    raise Exception.Create('CreateNewClass NewClassName is not a valid identifier');
  if NewUnitName='' then
    raise Exception.Create('CreateNewClass NewUnitName empty');
  if not IsValidIdent(NewUnitName) then
    raise Exception.Create('CreateNewClass NewUnitName is not a valid identifier');
  Result:=nil;

  // create vmt
  vmtSize:=GetVMTSize(ParentClass);
  vmtTailSize:=vmtSize-vmtMethodStart;
  GetMem(NewVMT,vmtSize);
  FillChar(NewVMT^,vmtSize,0);

  // set vmtInstanceSize
  PPtrInt(NewVMT+vmtInstanceSize)^:=ParentClass.InstanceSize;
  PPtrInt(NewVMT+vmtInstanceSizeNeg)^:=-ParentClass.InstanceSize;

  // set vmtParent
  TClass(Pointer(NewVMT+vmtParent)^):=ParentClass;

  // set vmtClassName: create pointer to classname (PShortString)
  GetMem(ClassNamePShortString,SizeOf(ShortString));
  System.Move(NewClassName[0],ClassNamePShortString^,SizeOf(ShortString));
  Pointer(Pointer(NewVMT+vmtClassName)^):=ClassNamePShortString;// don't use
                 // PShortString, so that the compiler does not get silly ideas

  // set vmtFieldTable
  GetMem(NewFieldTable,SizeOf(TFieldTable));
  FillChar(NewFieldTable^,SizeOf(TFieldTable),0);
  PFieldTable(Pointer(NewVMT+vmtFieldTable)^):=NewFieldTable;

  // ClassTable
  GetMem(NewClassTable,SizeOf(Word));
  FillChar(NewClassTable^,SizeOf(Word),0);
  NewFieldTable^.ClassTable:=NewClassTable;

  // set vmtTypeInfo
  TypeInfoSize := CalculateTypeInfoSize(NewClassName,0);
  GetMem(NewTypeInfo,TypeInfoSize);
  FillChar(NewTypeInfo^,TypeInfoSize,0);
  Pointer(Pointer(NewVMT+vmtTypeInfo)^):=NewTypeInfo;

  // set TypeInfo Kind and Name
  NewTypeInfo^.Kind:=tkClass;
  System.Move(NewClassName[0],NewTypeInfo^.Name[0],length(NewClassName)+1);
  NewTypeData:=GetTypeData(NewTypeInfo);
  if NewTypeData<>Pointer(Pointer(@NewTypeInfo^.Name[0])+1+length(NewClassName))
  then
    raise Exception.Create('CreateNewClass new aligned TypeData');

  // set TypeData (PropCount is the total number of properties, including ancestors)
  NewTypeData^.ClassType:=TClass(NewVMT);
  NewTypeData^.ParentInfo:=ParentClass.ClassInfo;
  NewTypeData^.PropCount:=GetTypeData(NewTypeData^.ParentInfo)^.PropCount;
  NewTypeData^.UnitName:=NewUnitName;
  AddedPropCount:=GetTypeDataPropCountAddr(NewTypeData);
  AddedPropCount^:=0;

  // copy the standard methods
  System.Move(Pointer(Pointer(ParentClass)+vmtMethodStart)^,
              Pointer(NewVMT+vmtMethodStart)^,
              vmtTailSize);

  // override 'ValidateRename' for TComponent descendants
  if ParentClass.InheritsFrom(TComponent) then begin
    Pointer(Pointer(NewVMT+TComponentValidateRenameOffset)^):=
                           @TComponentWithOverrideValidateRename.ValidateRename;
  end;

  Result:=TClass(NewVMT);
end;

procedure TJITComponentList.FreeJITClass(var AClass: TClass);

  procedure FreeMethodTableEntries(MethodTable: PMethodNameTable);
  var
    CurCount, i: integer;
    CurMethod: TMethodNameRec;
  begin
    if MethodTable=nil then exit;
    CurCount:=MethodTable^.Count;
    i:=CurCount;
    while i>0 do begin
      CurMethod:=MethodTable^.Entries[i-1];
      if CurMethod.Name<>nil then
        FreeMem(CurMethod.Name);
      if CurMethod.Addr<>nil then
        FreeMem(CurMethod.Addr);
      dec(i);
    end;
  end;

var
  OldVMT: Pointer;
  ClassNamePShortString: Pointer;
  OldFieldTable: PFieldTable;
  OldTypeInfo: PTypeInfo;
  OldMethodTable: PMethodNameTable;
begin
  // free TJITMethods
  JITMethods.DeleteAllOfClass(AClass);

  OldVMT:=Pointer(AClass);
  // free methodtable
  OldMethodTable:=PMethodNameTable((OldVMT+vmtMethodTable)^);
  if Assigned(OldMethodTable) then begin
    FreeMethodTableEntries(OldMethodTable);
    FreeMem(OldMethodTable);
  end;
  // free classname
  ClassNamePShortString:=Pointer(Pointer(OldVMT+vmtClassName)^);
  FreeMem(ClassNamePShortString);
  // free field table
  OldFieldTable:=PFieldTable(Pointer(OldVMT+vmtFieldTable)^);
  ReallocMem(OldFieldTable^.ClassTable,0);
  FreeMem(OldFieldTable);
  // free typeinfo
  OldTypeInfo:=PTypeInfo(Pointer(OldVMT+vmtTypeInfo)^);
  FreeMem(OldTypeInfo);
  // free vmt
  FreeMem(OldVMT);
  AClass:=nil;
end;

procedure TJITComponentList.DoAddNewMethod(JITClass:TClass;
  const AName:ShortString;  ACode:Pointer);
var OldMethodTable, NewMethodTable: PMethodNameTable;
  NewMethodTableSize:integer;
begin
  //debugln('[TJITComponentList.AddNewMethod] '''+JITClass.ClassName+'.'+AName+'''');
  OldMethodTable:=PMethodNameTable((Pointer(JITClass)+vmtMethodTable)^);
  if Assigned(OldMethodTable) then begin
    NewMethodTableSize:=SizeOf(DWord)+
                     (OldMethodTable^.Count + 1)*SizeOf(TMethodNameRec);
  end else begin
    NewMethodTableSize:=SizeOf(DWord)+SizeOf(TMethodNameRec);
  end;
  GetMem(NewMethodTable,NewMethodTableSize);
  if Assigned(OldMethodTable) then begin
    Move(OldMethodTable^,NewMethodTable^,
      NewMethodTableSize-SizeOf(TMethodNameRec));
    NewMethodTable^.Count:=NewMethodTable^.Count+1;
  end else begin
    NewMethodTable^.Count:=1;
  end;
  {$R-}
  //for a:=0 to NewMethodTable^.Count-2 do
  //  debugln(a,'=',NewMethodTable^.Entries[a].Name^,' $'
  //    ,DbgS(PtrInt(NewMethodTable^.Entries[a].Name),8));
  with NewMethodTable^.Entries[NewMethodTable^.Count-1] do begin
    GetMem(Name,256);
    Name^:=AName;
    Addr:=ACode;
  end;
  //for a:=0 to NewMethodTable^.Count-1 do
  //  debugln(a,'=',NewMethodTable^.Entries[a].Name^,' $'
  //    ,DbgS(PtrInt(NewMethodTable^.Entries[a].Name),8));
  {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
  PMethodNameTable((Pointer(JITClass)+vmtMethodTable)^):=NewMethodTable;
  if Assigned(OldMethodTable) then
    FreeMem(OldMethodTable);
end;

procedure TJITComponentList.DoRemoveMethod(JITClass:TClass;
  AName:ShortString; var OldCode:Pointer);
// Note: does not free OldCode
var OldMethodTable, NewMethodTable: PMethodNameTable;
  NewMethodTableSize:integer;
  a:cardinal;
begin
  {$IFDEF VerboseJITForms}
  debugln('[TJITComponentList.DoRemoveMethod] '''+JITClass.ClassName+'.'+AName+'''');
  {$ENDIF}
  AName:=uppercase(AName);
  OldMethodTable:=PMethodNameTable((Pointer(JITClass)+vmtMethodTable)^);
  OldCode:=nil;
  if Assigned(OldMethodTable) then begin
    a:=0;
    while a<OldMethodTable^.Count do begin
      {$R-}
      if uppercase(OldMethodTable^.Entries[a].Name^)=AName then begin
        OldCode:=OldMethodTable^.Entries[a].Addr;
        FreeMem(OldMethodTable^.Entries[a].Name);
        if OldMethodTable^.Count>0 then begin
          NewMethodTableSize:=SizeOf(DWord)+
                              OldMethodTable^.Count*SizeOf(TMethodNameRec);
          GetMem(NewMethodTable,NewMethodTableSize);
          NewMethodTable^.Count:=OldMethodTable^.Count-1;
          Move(OldMethodTable^,NewMethodTable^,
               SizeOf(DWord)+a*SizeOf(TMethodNameRec));
          Move(OldMethodTable^.Entries[a],NewMethodTable^.Entries[a+1],
               SizeOf(DWord)+a*SizeOf(TMethodNameRec));
        end else begin
          NewMethodTable:=nil;
        end;
        PMethodNameTable((Pointer(JITClass)+vmtMethodTable)^):=NewMethodTable;
        FreeMem(OldMethodTable);
        break;
      end;
      {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
      inc(a);
    end;
  end;
end;

procedure TJITComponentList.DoRenameMethod(JITClass:TClass;
  OldName,NewName:ShortString);
var MethodTable: PMethodNameTable;
  a:integer;
begin
  {$IFDEF VerboseJITForms}
  debugln('[TJITComponentList.DoRenameMethod] ClassName='''+JITClass.ClassName+''''
    +' OldName='''+OldName+''' NewName='''+OldName+'''');
  {$ENDIF}
  OldName:=uppercase(OldName);
  MethodTable:=PMethodNameTable((Pointer(JITClass)+vmtMethodTable)^);
  if Assigned(MethodTable) then begin
    for a:=0 to MethodTable^.Count-1 do begin
      if uppercase(MethodTable^.Entries[a].Name^)=OldName then
        MethodTable^.Entries[a].Name^:=NewName;
    end;
  end;
end;

procedure TJITComponentList.DoRenameClass(JITClass:TClass;
  const NewName:ShortString);
begin
  {$IFDEF VerboseJITForms}
  debugln('[TJITComponentList.DoRenameClass] OldName='''+JITClass.ClassName
    +''' NewName='''+NewName+''' ');
  {$ENDIF}
  PShortString((Pointer(JITClass)+vmtClassName)^)^:=NewName;
end;

procedure TJITComponentList.DoRenameUnitNameOfClass(JITClass: TClass;
  const NewUnitName: ShortString);
var
  TypeInfo: PTypeInfo;
  TypeData: PTypeData;
  OldPropCount: Word;
begin
  TypeInfo:=PTypeInfo(JITClass.ClassInfo);
  if TypeInfo=nil then
    RaiseException('TJITComponentList.DoRenameUnitNameOfClass');
  TypeData:=GetTypeData(TypeInfo);
  //DebugLn(['TJITComponentList.DoRenameUnitNameOfClass Old=',TypeData^.UnitName,' New=',NewUnitName]);
  OldPropCount:=GetTypeDataPropCountAddr(TypeData)^;
  if OldPropCount<>0 then
    RaiseGDBException('TJITComponentList.DoRenameUnitNameOfClass TODO: move properties and realloc mem');
  TypeData^.UnitName:=NewUnitName;
  GetTypeDataPropCountAddr(TypeData)^:=OldPropCount;
end;

//------------------------------------------------------------------------------

{
  TReader events.
  Normally at runtime a LFM is streamed back into the corresponding TForm
  descendent, all methods and components are published members and TReader can
  set these values.
  But at design time we do not have the corresponding TForm descendent. And
  there is no compiled code, thus it must be produced
  (just-in-time).
}
procedure TJITComponentList.ReaderFindMethod(Reader: TReader;
  const FindMethodName: Ansistring;  var Address: Pointer; var Error: Boolean);
{$IFNDEF EnableFakeMethods}
var NewMethod: TMethod;
{$ENDIF}
begin
  {$IFDEF IDE_DEBUG}
  debugln('[TJITComponentList.ReaderFindMethod] A "'+FindMethodName+'" Address=',DbgS(Address));
  {$ENDIF}
  {$IFDEF EnableFakeMethods}
  RaiseGDBException('TJITComponentList.ReaderFindMethod this event should never be called -> this is a bug in TReader, or misuse of TReader.OnFindMethod');
  {$ELSE}
  if Address=nil then begin
    // there is no method in the ancestor class with this name
    // => add a JIT method with this name to the JITForm
    NewMethod:=CreateNewMethod(FCurReadJITComponent,FindMethodName);
    Address:=NewMethod.Code;
    Error:=false;
  end;
  {$ENDIF}
end;

procedure TJITComponentList.ReaderPropertyNotFound(Reader: TReader;
  Instance: TPersistent; var PropName: string; IsPath: Boolean;
  var Handled, Skip: Boolean);
begin
  DebugLn('TJITComponentList.ReaderPropertyNotFound ',Instance.ClassName,'.',PropName);
  if Assigned(OnPropertyNotFound) then
    OnPropertyNotFound(Self,Reader,Instance,PropName,IsPath,Handled,Skip);
end;

procedure TJITComponentList.ReaderSetMethodProperty(Reader: TReader;
  Instance: TPersistent; PropInfo: PPropInfo; const TheMethodName: string;
  var Handled: boolean);
{$IFDEF EnableFakeMethods}
var
  Method: TMethod;
  JITMethod: TJITMethod;
{$ENDIF}
begin
  {$IFDEF EnableFakeMethods}
  debugln('TJITComponentList.ReaderSetMethodProperty ',DbgSName(Instance),' ',PropInfo^.Name,':=',TheMethodName);
  Method.Code:=FCurReadJITComponent.MethodAddress(TheMethodName);
  if Method.Code<>nil then begin
    // there is a real method with this name
    Method.Data := FCurReadJITComponent;
  end else begin
    // create a fake TJITMethod
    JITMethod:=JITMethods.Add(FCurReadJITComponent.ClassType,TheMethodName);
    Method:=JITMethod.Method;
  end;
  SetMethodProp(Instance, PropInfo, Method);
  
  Handled:=true;
  {$ENDIF}
end;

procedure TJITComponentList.ReaderSetName(Reader: TReader;
  Component: TComponent; var NewName: Ansistring);
begin
//  debugln('[TJITComponentList.ReaderSetName] OldName="'+Component.Name+'" NewName="'+NewName+'"');
  if jclAutoRenameComponents in FFlags then begin
    while FCurReadJITComponent.FindComponent(NewName)<>nil do
      NewName:=CreateNextIdentifier(NewName);
  end;
end;

procedure TJITComponentList.ReaderReferenceName(Reader: TReader;
  var RefName: Ansistring);
begin
//  debugln('[TJITComponentList.ReaderReferenceName] Name='''+RefName+'''');
end;

procedure TJITComponentList.ReaderAncestorNotFound(Reader: TReader;
  const ComponentName: Ansistring;  ComponentClass: TPersistentClass;
  var Component: TComponent);
begin
  // ToDo: this is for custom form templates
  debugln('[TJITComponentList.ReaderAncestorNotFound] ComponentName='''+ComponentName
    +''' Component='''+dbgsName(Component)+'''');
end;

procedure TJITComponentList.ReaderError(Reader: TReader;
  const ErrorMsg: Ansistring; var Handled: Boolean);
// ToDo: use SUnknownProperty when it is published by the fpc team
const
  SUnknownProperty = 'Unknown property';
var
  ErrorType: TJITFormError;
  Action: TModalResult;
  ErrorBinPos: Int64;
begin
  ErrorType:=jfeReaderError;
  Action:=mrCancel;
  FCurReadErrorMsg:=ErrorMsg;
  FCurUnknownProperty:=''; // ToDo find name property
  // find out, what error occured
  if RightStr(ErrorMsg,length(SUnknownProperty))=SUnknownProperty then begin
    ErrorType:=jfeUnknownProperty;
    Action:=mrIgnore;
  end;
  if Reader.Driver is TLRSObjectReader then begin
    // save error position
    ErrorBinPos:=TLRSObjectReader(Reader.Driver).Stream.Position;
    FErrors.Add(-1,ErrorBinPos,nil);
  end;
  if Assigned(OnReaderError) then
    OnReaderError(Self,ErrorType,Action);
  Handled:=Action in [mrIgnore];
  FCurUnknownProperty:='';
  
  DebugLn('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>');
  DebugLn(['[TJITComponentList.ReaderError] "'+ErrorMsg+'" ignoring=',Handled]);
  DebugLn('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<');
end;

procedure TJITComponentList.ReaderFindComponentClass(Reader: TReader;
  const FindClassName: Ansistring; var ComponentClass: TComponentClass);
var
  RegComp: TRegisteredComponent;
begin
  fCurReadChild:=nil;
  fCurReadChildClass:=ComponentClass;
  FCurUnknownClass:=FindClassName;
  if ComponentClass=nil then begin
    RegComp:=IDEComponentPalette.FindComponent(FindClassName);
    if RegComp<>nil then begin
      //debugln('[TJITComponentList.ReaderFindComponentClass] '''+FindClassName
      //   +''' is registered');
      ComponentClass:=RegComp.ComponentClass;
    end else begin
      DebugLn('[TJITComponentList.ReaderFindComponentClass] '''+FindClassName
         +''' is unregistered');
      // The reader will create a ReaderError automatically
    end;
  end;
  FCurUnknownClass:='';
end;

procedure TJITComponentList.ReaderCreateComponent(Reader: TReader;
  ComponentClass: TComponentClass; var Component: TComponent);
begin
  fCurReadChild:=Component;
  fCurReadChildClass:=ComponentClass;
//  debugln('[TJITComponentList.ReaderCreateComponent] Class='''+ComponentClass.ClassName+'''');
end;

procedure TJITComponentList.ReaderReadComponent(Component: TComponent);
begin
  DebugLn('TJITComponentList.ReaderReadComponent A ',Component.Name,':',Component.ClassName);
end;

//==============================================================================


{ TJITForms }

function TJITForms.IsJITForm(AComponent: TComponent): boolean;
begin
  Result:=(AComponent<>nil) and (AComponent is TForm)
      and (TForm(AComponent).Parent=nil) and (IndexOf(AComponent)>=0);
end;

function TJITForms.GetItem(Index: integer): TForm;
begin
  Result:=TForm(inherited Items[Index]);
end;

{ TJITNonFormComponents }

function TJITNonFormComponents.IsJITNonForm(AComponent: TComponent): boolean;
begin
  Result:=(AComponent<>nil) and (not (AComponent is TForm))
          and (IndexOf(AComponent)>=0);
end;

{ TJITMethod }

constructor TJITMethod.Create(AnOwner: TJITMethods;
  aClass: TClass; const aMethodName: shortstring);
begin
  FMethod.Data:=Self;
  FMethod.Code:=nil;
  fTheClass:=AClass;
  fTheMethodName:=aMethodName;
  FOwner:=AnOwner;
  Owner.InternalAdd(Self);
end;

destructor TJITMethod.Destroy;
begin
  if Owner<>nil then
    Owner.InternalRemove(Self);
  inherited Destroy;
end;

{ TJITMethods }

procedure TJITMethods.InternalAdd(const AMethod: TJITMethod);
begin
  fMethods.Add(AMethod);
  AMethod.fOwner:=Self;
end;

procedure TJITMethods.InternalRemove(const AMethod: TJITMethod);
begin
  AMethod.fOwner:=nil;
  if not fClearing then
    fMethods.Remove(AMethod);
end;

constructor TJITMethods.Create;
begin
  fMethods:=TAvgLvlTree.Create(@CompareJITMethod);
end;

destructor TJITMethods.Destroy;
begin
  Clear;
  FreeAndNil(fMethods);
  inherited Destroy;
end;

procedure TJITMethods.Clear;
begin
  fClearing:=true;
  fMethods.FreeAndClear;
  fClearing:=false;
end;

function TJITMethods.Add(aClass: TClass;
  const aMethodName: shortstring): TJITMethod;
begin
  Result:=Find(aClass,aMethodName);
  if Result=nil then begin
    //DebugLn(['TJITMethods.Add Create Class=',dbgsname(aClass),' aMethodName=',aMethodName]);
    Result:=TJITMethod.Create(Self,aClass,aMethodName);
  end;
end;

function TJITMethods.Find(aClass: TClass;
  const aMethodName: shortstring): TJITMethod;
var
  CurMethod: TJITMethod;
  Node: TAvgLvlTreeNode;
  Comp: LongInt;
begin
  //DebugLn(['TJITMethods.Find  Class=',dbgsname(aClass),' aMethodName=',aMethodName]);
  Node:=fMethods.Root;
  while (Node<>nil) do begin
    CurMethod:=TJITMethod(Node.Data);
    Comp:=ComparePointers(aClass,CurMethod.TheClass);
    if Comp=0 then
      Comp:=CompareText(aMethodName,CurMethod.TheMethodName);
    if Comp=0 then
      exit(CurMethod);
    if Comp<0 then begin
      Node:=Node.Left
    end else begin
      Node:=Node.Right
    end;
  end;
  Result:=nil;
end;

function TJITMethods.Delete(aMethod: TJITMethod): boolean;
begin
  //DebugLn(['TJITMethods.Delete  Class=',dbgsname(AMethod.TheClass),' aMethodName=',aMethod.TheMethodName]);
  if (aMethod=nil) then
    Result:=false
  else if aMethod.Owner<>Self then
    RaiseGDBException('TJITMethods.DeleteJITMethod')
  else begin
    Result:=true;
    InternalRemove(aMethod);
    aMethod.Free;
  end;
end;

function TJITMethods.Delete(aClass: TClass;
  const aMethodName: shortstring): boolean;
var
  CurMethod: TJITMethod;
begin
  CurMethod:=Find(aClass,aMethodName);
  if CurMethod=nil then begin
    Result:=false;
  end else begin
    Result:=true;
    InternalRemove(CurMethod);
    CurMethod.Free;
  end;
end;

procedure TJITMethods.DeleteAllOfClass(aClass: TClass);
var
  CurMethod: TJITMethod;
  Node: TAvgLvlTreeNode;
  Comp: LongInt;
  NextNode: TAvgLvlTreeNode;
begin
  Node:=fMethods.Root;
  while (Node<>nil) do begin
    CurMethod:=TJITMethod(Node.Data);
    Comp:=ComparePointers(aClass,CurMethod.TheClass);
    if Comp<0 then begin
      Node:=Node.Left
    end else if Comp>0 then begin
      Node:=Node.Right
    end else begin
      // one node found
      
      // search lowest
      repeat
        NextNode:=fMethods.FindPrecessor(Node);
        if (NextNode=nil)
           or (ComparePointers(aClass,TJITMethod(NextNode.Data).TheClass)<>0)
        then
          break;
        Node:=NextNode;
      until false;
      
      // delete all nodes of this class
      repeat
        NextNode:=fMethods.FindSuccessor(Node);
        CurMethod:=TJITMethod(Node.Data);
        CurMethod.FOwner:=nil;
        fMethods.Delete(Node);
        CurMethod.Free;
        Node:=NextNode;
      until (Node=nil)
           or (ComparePointers(aClass,TJITMethod(Node.Data).TheClass)<>0);

      exit;
    end;
  end;
end;

function TJITMethods.Rename(aClass: TClass; const OldMethodName,
  NewMethodName: shortstring): boolean;
var
  CurMethod: TJITMethod;
begin
  CurMethod:=Find(aClass,OldMethodName);
  if CurMethod=nil then begin
    Result:=false;
  end else begin
    Result:=true;
    //DebugLn(['TJITMethods.Rename Class=',DbgSName(aClass),' Old=',CurMethod.TheMethodName,' New=',NewMethodName]);
    fMethods.Remove(CurMethod);
    CurMethod.fTheMethodName:=NewMethodName;
    fMethods.Add(CurMethod);
  end;
end;

Initialization
  TComponentValidateRenameOffset:=GetTComponentValidateRenameVMTOffset;
  JITMethods:=TJITMethods.Create;
  
finalization
  FreeAndNil(JITMethods);

end.

