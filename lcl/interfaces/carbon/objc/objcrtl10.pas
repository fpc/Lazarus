{
 objcrtl10.pas

 Copyright (C) 2009 Dmitry Boyarintsev

 This unit is implementation for dynamic Objective-C Run-time Library based on run-time version 1.0
 headers included with XCode 3.1.2
 The original copyright note of is kept on each include file
}
{.$DEFINE DEBUG}

unit objcrtl10;

{$mode objfpc}{$H+}

interface

uses
  ctypes, objcrtl, dynlibs;

{
  Mac OS X Version 10.5 Delta
  ---------------------------

  The low-level Objective-C runtime API is significantly updated
 in Mac OS X version 10.5. Many functions and all existing data structures
 are replaced with new functions. This document describes the differences
 between the 10.5 version and previous versions.

  http://developer.apple.com/documentation/Cocoa/Reference/ObjCRuntimeRef/Articles/ocr10_5delta.html#//apple_ref/doc/uid/TP40002981-TPXREF101
}

function InitializeObjCRtl10(const ObjCLibName: AnsiString): Boolean;

implementation

{$HINTS OFF} {.Parameter not used.}

function ObjCAllocMem(size: Integer): Pointer;
begin
  //todo: store the mem pointers
  //      and release them at finalization section
  //      this must be thread safe, so allocating additional NsObject
  //      that can be used with objc_sync is recommended
  Result := AllocMem(size);
end;

procedure ObjCFreeMem(p: Pointer);
begin
  //todo:
  Freemem(p);
end;


function allocstr(const src: String): Pchar;
begin
  Result := ObjCAllocMem(length(src)+1);
  if src <> '' then System.Move(src[1], Result^, length(src));
end;


const
  CLS_CLASS	 	      = $1;
  CLS_META		      = $2;
  // CLS_INITIALIZED	  = $4;
  // CLS_POSING		    = $8;
  // CLS_MAPPED		    = $10;
  // CLS_FLUSH_CACHE	  = $20;
  // CLS_GROW_CACHE 	  = $40;
  // CLS_NEED_BIND		  = $80;
  // CLS_METHOD_ARRAY  = $100;
  // the JavaBridge constructs classes with these markers
  // CLS_JAVA_HYBRID	  = $200;
  // CLS_JAVA_CLASS	  = $400;
  // thread-safe +initialize
  // CLS_INITIALIZING	= $800;
  // bundle unloading
  // CLS_FROM_BUNDLE	  = $1000;
  // C++ ivar support
  // CLS_HAS_CXX_STRUCTORS	= $2000;
  // Lazy method list arrays
  // CLS_NO_METHOD_ARRAY	  = $4000;//L
  // +load implementation
  // CLS_HAS_LOAD_METHOD	  = $8000;


// all obj-c types are postfixed with 1, to avoid type name confilcts

type
  // P_Class = ^_Class;

  Pobjc_class1 = ^objc_class1;

  _Class1 = Pobjc_class1; // can be casted to _Class directly

  Pobjc_object1 = ^objc_object1;

  objc_object1 = record
  	isa: _Class1;
  end;

  Pid1 = ^id;
  // id1 = Pobjc_object1;

  Pobjc_selector1 = Pointer;

  // PSEL1 = ^SEL1;

  SEL1 = Pobjc_selector1;

  {$WARNINGS OFF}

  IMP1 = function (param1: id; param2: SEL; param3: array of const): id; cdecl;

  Pobjc_ivar_list1 = ^objc_ivar_list1;
  {$WARNINGS ON}

  Pobjc_method_list1 = ^objc_method_list1;
  PPobjc_method_list1 = ^Pobjc_method_list1;

  Pobjc_cache1 = ^objc_cache1;

  Pobjc_protocol_list1 = ^objc_protocol_list1;

  objc_class1 = packed record
	  isa           : Pobjc_class1;
	  super_class   : Pobjc_class1;
	  name          : PChar;
	  version       : culong;
	  info          : culong;
	  instance_size : culong;
	  ivars         : Pobjc_ivar_list1;
	  methodLists   : PPobjc_method_list1;
	  cache         : Pobjc_cache1;
 	  protocols     : Pobjc_protocol_list1;
  end;


  {* Category Template}
  //Pobjc_category1 = ^objc_category1;

  //Category1 = Pobjc_category1;

  //objc_category1 = packed record
  // 	category_name     : PChar;
	//  class_name        : PChar;
  //  instance_methods  : Pobjc_method_list1;
	//  class_methods     : Pobjc_method_list1;
  //  protocols         : Pobjc_protocol_list1;
  //end;

  {* Instance Variable Template}
  Pobjc_ivar1 = ^objc_ivar1;

  Ivar1 = Pobjc_ivar1;

  objc_ivar1 = packed record
  	ivar_name   : PChar;
	  ivar_type   : PChar;
  	ivar_offset : cint;
    {$ifdef __alpha__}
  	space: cint;
    {$endif}
  end;

  objc_ivar_list1 = packed record
  	ivar_count: cint;
    {$ifdef __alpha__}
    space: cint;
    {$endif}
  	ivar_list: array[0..0] of objc_ivar1;		{ variable length structure }
  end;

  {* Method Template }
  Pobjc_method1 = ^objc_method1;
  Method1 = Pobjc_method1;

  objc_method1 = packed record
    method_name   : SEL1;
    method_types  : PChar;
    method_imp    : IMP1;
  end;

  objc_method_list1 = packed record
    obsolete      : Pobjc_method_list1;
    method_count  : cint;
    {$ifdef __alpha__}
    space: cint;
    {$endif}
    method_list1  : array[0..0] of objc_method1;	{ variable length structure }
  end;

  { Protocol support }

  Protocol1 = objc_object1;

  objc_protocol_list1 = record
    next    : Pobjc_protocol_list1;
    count   : cint;
    list    : array[0..0] of Protocol1;
  end;

  { Constants here moved down }

  { Structure for method cache - allocated/sized at runtime }

//  Cache1 = Pobjc_cache1;

  objc_cache1 = record
    mask      : cuint;            { total = mask + 1 }
    occupied  : cuint;
    buckets   : array[0..0] of Method1;
  end;

// objective-c 1.0 runtime functions. They are obsolete, for 2.0
// and no longer available as interface functions
// these functions are used by wrapper-functions !

var
  objc_addClass    : procedure (myClass: _Class); cdecl = nil;
  //class_addMethods : procedure (myClass: _Class1; methodList : Pobjc_method_list1); cdecl;

type

  { TClassMethod1Reg }

  TClassMethod1Reg = class(TObject)
  private
    methods : array of objc_method1;
    count   : Integer;
  public
    procedure AddMethod(name:SEL; imp:IMP; types:pchar);
    function AllocMethodList: Pobjc_method_list1;
  end;
  //PClassMethod1Reg = ^TClassMethod1Reg;

  TIVar1Reg = record
    size      : Integer;
    types     : String;
    name      : String;
    alignment : Uint8_t;
  end;

  { TClassIVar1Reg }

  TClassIVar1Reg = class(TObject)
  private
    ivarscount  : Integer;
    ivars       : array of TIVar1Reg;
  public
    procedure AddIVar(name:pchar; size:size_t; alignment:uint8_t; types:pchar);
    function AllocIVarsList(ivarOffset: Integer; out ivarssize: Integer): Pobjc_ivar_list1;
  end;

{ TClassMethod1Reg }

procedure TClassMethod1Reg.AddMethod(name: SEL; imp: IMP; types: pchar);
begin
  if length(methods) = count then begin
    if count = 0 then SetLength(methods, 4)
    else begin
      SetLength(methods, count * 2);
    end;
  end;
  methods[count].method_imp := IMP1(imp);
  methods[count].method_types := allocstr(types);
  methods[count].method_name := name;
  inc(count);
end;

function TClassMethod1Reg.AllocMethodList: Pobjc_method_list1;
var
  i : Integer;
begin
  if count = 0 then Result := nil
  else begin
    Result := ObjCAllocMem( sizeof(objc_method_list1) + (count-1)*sizeof(objc_method1) );
    Pobjc_method_list1(Result)^.method_count := count;
    for i := 0 to count - 1 do begin
      Pobjc_method_list1(Result)^.method_list1[i].method_name := methods[i].method_name;
      Pobjc_method_list1(Result)^.method_list1[i].method_types := methods[i].method_types;
      Pobjc_method_list1(Result)^.method_list1[i].method_imp := methods[i].method_imp;
    end;
  end;
end;

procedure TClassIVar1Reg.AddIVar(name: pchar; size: size_t; alignment: uint8_t;
  types: pchar);
begin
  if ivarscount = length(ivars) then begin
    if ivarscount = 0 then SetLength(ivars, 4)
    else setLength(ivars, ivarscount * 2);
  end;
  ivars[ivarscount].name := name;
  ivars[ivarscount].size := size;
  ivars[ivarscount].types := types;
  ivars[ivarscount].alignment := alignment;
  inc(ivarscount);
end;

function TClassIVar1Reg.AllocIVarsList(ivarOffset: Integer; out ivarssize: Integer): Pobjc_ivar_list1;
var
  i   : Integer;
begin
  if ivarscount = 0 then begin
    Result := nil;
    ivarssize := 0;
  end else begin
    ivarssize := 0;
    Result := ObjCAllocMem( sizeof(objc_ivar_list1) + (ivarscount-1)*sizeof(objc_ivar1) );
    Result^.ivar_count := ivarscount;
    for i := 0 to ivarscount - 1 do begin
      Result^.ivar_list[i].ivar_name := allocstr(ivars[i].name);
      Result^.ivar_list[i].ivar_offset := ivarOffset + ivarssize;
      Result^.ivar_list[i].ivar_type := allocstr(ivars[i].types);
      inc(ivarssize, ivars[i].size);
    end;
  end;
end;

function object_getClass10(obj:id): _Class; cdecl;
begin
  if obj = nil then Result := nil
  else begin
    Result := _Class(Pobjc_object1(obj)^.isa);
  end;
end;

function object_setClass10(obj:id; cls: _Class): _Class; cdecl;
begin
  // can this be done in that way?
  Result := _Class(Pobjc_object1(obj)^.isa);
  Pobjc_object1(obj)^.isa := _Class1(cls);
end;

function object_getIvar10(obj:id; _ivar:Ivar):id; cdecl;
begin
  Result := nil;
  if not Assigned(obj) or not Assigned(_ivar) then Exit;
  Result := Pid1(PtrUInt(obj) + ivar_getOffset(_ivar))^;
end;

procedure object_setIvar10(obj:id; _ivar:Ivar; value:id); cdecl;
begin
  if not Assigned(obj) or not Assigned(_ivar) then Exit;
  Pid1(PtrUInt(obj) + ivar_getOffset(_ivar))^ := value;
end;

function class_getName10(cls:_Class):PChar; cdecl;
begin
  Result := _Class1(cls)^.name;
end;

function class_getSuperclass10(cls:_Class):_Class; cdecl;
begin
  Result := _Class1(cls)^.super_class;
end;

function class_isMetaClass10(cls:_Class):BOOL; cdecl;
begin
  Result := Assigned(cls) and (_Class1(cls)^.Info = CLS_META);
end;

function class_copyMethodList10(cls:_Class; outCount: pdword):PMethod; cdecl;
begin
  //todo:
  Result := nil; //todo: ??
end;

function class_getMethodImplementation10(cls:_Class; name:SEL):IMP; cdecl;
begin
  //todo:
  Result := nil;
end;

function class_respondsToSelector10(cls:_Class; sel:SEL):BOOL; cdecl;
begin
  //todo:
  Result := false;
end;

function class_conformsToProtocol10(cls:_Class; var protocol: Protocol):BOOL; cdecl;
begin
  //todo:
  Result := false;
end;

function class_copyProtocolList10(cls:_Class; var outCount: dword):PArrayPProtocol; cdecl;
begin
  //todo:
  Result := nil;
end;

function class_copyIvarList10(cls:_Class; outCount:pdword):PIvar; cdecl;
begin
  //todo:
  Result := nil;
end;

function class_getMethodImplementation_stret10(cls:_Class; name:SEL):IMP; cdecl;
begin
  //todo:
  Result := nil;
end;

function objc_allocateClassPair10(superclass:_Class; name:pchar; extraBytes:size_t):_Class; cdecl;
var
  super  : _Class1;
  root_class   : _Class1;

  new_class   : _Class1;
  meta_class  : _Class1;
  namelen     : Integer;
begin
  Result := nil;
  if (superclass = nil) or (_Class1(objc_lookUpClass(name)) <> nil) then begin
    {$ifdef DEBUG}
    if not Assigned(superclass) then
      writeln('no super class!')
    else begin
      if Assigned(objc_lookUpClass(name)) then
        writeln('the class ', name, ' already exists')
      else
        writeln('this situation is impossible!');
    end;
    {$endif}
    Exit;
  end;
  super := _Class1(superclass);

  // Find the root class
  root_class := super;
  while root_class^.super_class <> nil do
    root_class := root_class^.super_class;

  // Allocate space for the class and its metaclass
  new_class := ObjCAllocMem(2 * SizeOf(objc_class1));
  meta_class := @new_class[1];

  // setup class
  new_class^.isa      := meta_class;
  new_class^.info     := CLS_CLASS;
  meta_class^.info    := CLS_META;

  // Create a copy of the class name.
  // For efficiency, we have the metaclass and the class itself
  // to share this copy of the name, but this is not a requirement
  // imposed by the runtime.
  namelen := strlen(name);
  new_class^.name := ObjCAllocMem(namelen + 1);
  Move(name^, new_class^.name^, namelen);
  meta_class^.name := new_class^.name;

  // Allocate empty method lists.
  // We can add methods later.
  new_class^.methodLists := Pointer(TClassMethod1Reg.Create); // PPObjc_method_list1(AllocMethodReg); //AllocMem ( SizeOf(TClassMethod1Reg)) ;// SizeOf(Pobjc_method_list1));
  meta_class^.methodLists := Pointer(TClassMethod1Reg.Create); //PPObjc_method_list1(AllocMethodReg);

  // Connect the class definition to the class hierarchy:
  // Connect the class to the superclass.
  // Connect the metaclass to the metaclass of the superclass.
  // Connect the metaclass of the metaclass to the metaclass of  the root class.
  new_class^.super_class  := super;
  meta_class^.super_class := super^.isa;
  meta_class^.isa         := Pointer(root_class^.isa);

  // Set the sizes of the class and the metaclass.
  new_class^.instance_size := super^.instance_size;
  meta_class^.instance_size := meta_class^.super_class^.instance_size;
  new_class^.ivars := Pointer(TClassIVar1Reg.Create);

  Result := new_class;
end;

procedure objc_registerClassPair10(aClass:_Class); cdecl;
var
  meta_class : _Class1;
  new_class  : _Class1;

  MethodReg : TClassMethod1Reg;

  iVarReg   : TClassIVar1Reg;
  ivarslist : Pobjc_ivar_list1;
  sz        : Integer;

  procedure RegisterMethodList(reg: TClassMethod1Reg; cls1: _Class1);
  var
    mtdlist   : Pobjc_method_list1;
  begin
    if not Assigned(reg) then Exit;
    cls1^.methodLists := ObjCAllocMem(sizeof(Pobjc_method_list1));
    mtdList := reg.AllocMethodList;
    if not Assigned(mtdlist)
      then cls1^.methodLists^ := Pointer(-1)
      else cls1^.methodLists^ := mtdlist;
  end;

begin
  new_class := _Class1(aClass);
  meta_class := _Class1(new_Class)^.isa;
  // Finally, register the class with the runtime.

  MethodReg := TClassMethod1Reg(new_class^.methodLists);
  RegisterMethodList(MethodReg, new_class);
  MethodReg.Free;

  MethodReg := TClassMethod1Reg(meta_class^.methodLists);
  RegisterMethodList(MethodReg, meta_class);
  MethodReg.Free;

  iVarReg := TClassIVar1Reg(new_class^.ivars);
  ivarslist := iVarReg.AllocIVarsList(new_class^.instance_size, sz);
  new_class^.ivars := ivarslist;
  inc(new_class^.instance_size, sz);
  iVarReg.Free;


  if new_class <> nil then objc_addClass(new_class);
end;

function objc_duplicateClass10(original:_Class; name:pchar; extraBytes:size_t):_Class; cdecl;
begin
  //todo:
  Result := nil;
end;

procedure objc_disposeClassPair10(cls:_Class); cdecl;
begin
  //todo:
end;

function class_addMethod10(cls:_Class; name:SEL; _imp:IMP; types:pchar):BOOL; cdecl;
begin
  if not Assigned(cls) or not Assigned(name) or not Assigned(_imp) or not Assigned(types) then begin
    {$IFDEF DEBUG}
    write('* Bad params?: cls  = ', Integer(cls));
    write(' name = ', PChar(name));
    write(' imp  = ', Integer(_imp));
    writeln(' type = ', types);
    {$ENDIF}
    Result := false;
    Exit;
  end;

  {$IFDEF DEBUG}
  write('* method list = ', Integer(_Class1(cls)^.methodLists));
  if Assigned (TClassMethod1Reg(_Class1(cls)^.methodLists)) then
    writeln(', ', TClassMethod1Reg(_Class1(cls)^.methodLists).ClassName)
  else
    writeln;
  try
  {$ENDIF}
  TClassMethod1Reg(_Class1(cls)^.methodLists).AddMethod(name, _imp, types);
  {$IFDEF DEBUG}
    writeln('"',PChar(name), '" added successfully');
  except
    writeln('* exception while adding method');
  end;
  {$ENDIF}
  Result := true;
end;

function class_addIvar10(cls:_Class; name:pchar; size:size_t; alignment:uint8_t; types:pchar):BOOL; cdecl;
var
  cls1 : _Class1;
begin
  if (alignment <> 1) or (class_isMetaClass10(cls)) then begin
    Result := false;
    Exit;
  end;
  cls1 := _Class1(cls);
  TClassIVar1Reg(cls1^.ivars).AddIVar(name, size, alignment, types);
  Result := true;
end;

function class_addProtocol10(cls:_Class; protocol:pProtocol):BOOL; cdecl;
begin
  //todo:
  Result := false;
end;


function method_getName10(m:Method):SEL; cdecl;
begin
  Result := Method1(m)^.method_name;
end;

function method_getImplementation10(m:Method):IMP; cdecl;
begin
  Result := IMP(Method1(m)^.method_imp);
end;

function method_getTypeEncoding10(m:Method):Pchar; cdecl;
begin
  Result := IMP(Method1(m)^.method_types);
end;

function method_copyReturnType10(m:Method):Pchar; cdecl;
begin
  //todo:
  Result := nil;
end;

function method_copyArgumentType10(m:Method; index:dword):Pchar; cdecl;
begin
  //todo:
  Result := nil;
end;

function method_setImplementation10(m:Method; _imp:IMP):IMP; cdecl;
begin
  //todo:! ???? check!
  Result := IMP(Method1(m)^.method_imp);
  Method1(m)^.method_imp := IMP1(_imp);
end;

function ivar_getName10(v:Ivar):Pchar; cdecl;
begin
  Result := IVar1(v)^.ivar_name;
end;

function ivar_getTypeEncoding10(v:Ivar):Pchar; cdecl;
begin
  Result := IVar1(v)^.ivar_type;
end;

function ivar_getOffset10(v:Ivar):ptrdiff_t; cdecl;
begin
  Result := ptrdiff_t(IVar1(v)^.ivar_offset);
end;

function sel_isEqual10(lhs:SEL; rhs:SEL):BOOL; cdecl;
begin
  Result := lhs = rhs; //???
end;

function objc_getProtocol10(name:pchar): PProtocol; cdecl;
begin
  //todo:
  Result := nil;
end;

function objc_copyProtocolList10(outCount:pdword):PArrayPProtocol; cdecl;
begin
  //todo:
  Result := nil;
end;

function InitializeObjCRtl10(const ObjCLibName: AnsiString): Boolean;
var
  hnd : TLibHandle;
begin
  hnd := LoadLibrary(ObjCLibName);
  Result := hnd <> 0;
  if not Result then Exit;

  //Exceptions - are unchanged:
  LoadDefaultObjCExepction(hnd);
  //Synchronization - unchanged:
  LoadDefaultObjCSync(hnd);

  // Instances

  // The following functions are unchanged:
  Pointer(object_dispose) := GetProcedureAddress(hnd, 'object_dispose');
  Pointer(object_getClassName) := GetProcedureAddress(hnd, 'object_getClassName');
  Pointer(object_getIndexedIvars) := GetProcedureAddress(hnd, 'object_getIndexedIvars');
  Pointer(object_setInstanceVariable) := GetProcedureAddress(hnd, 'object_setInstanceVariable');
  Pointer(object_getInstanceVariable) := GetProcedureAddress(hnd, 'object_getInstanceVariable');

  //The following function is modified:
  // needs wrapper?
  // object_copy (The nBytes parameter is changed from unsigned to size_t.)
  Pointer(object_copy)    := GetProcedureAddress(hnd, 'object_copy');

  //The following functions are added:
  object_getClass := @object_getClass10;
  object_setClass := @object_setClass10;
  object_getIvar := @object_getIvar10;
  object_setIvar := @object_setIVar10;

  // The following functions are deprecated:
  //object_copyFromZone: deprecated in favor of object_copy
  //object_realloc
  //object_reallocFromZone: no substitute
  //_alloc: no substitute
  //_copy: no substitute
  //_realloc: no substitute
  //_dealloc: no substitute
  //_zoneAlloc: no substitute
  //_zoneRealloc: no substitute
  //_zoneCopy: no substitute
  //_error: no substitute

  //Class Inspection

  //The following functions are unchanged:
  Pointer(objc_getClassList) := GetProcedureAddress(hnd, 'objc_getClassList');
  Pointer(objc_lookUpClass) := GetProcedureAddress(hnd, 'objc_lookUpClass');
  Pointer(objc_getClass)    := GetProcedureAddress(hnd, 'objc_getClass');
  Pointer(objc_getMetaClass) := GetProcedureAddress(hnd, 'objc_getMetaClass');
  Pointer(class_getVersion) := GetProcedureAddress(hnd, 'class_getVersion');
  Pointer(class_getInstanceVariable) := GetProcedureAddress(hnd, 'class_getInstanceVariable');
  Pointer(class_getInstanceMethod) := GetProcedureAddress(hnd, 'class_getInstanceMethod');
  Pointer(class_getClassMethod) := GetProcedureAddress(hnd, 'class_getClassMethod');

  // The following function is modified:
  // needs wrapper?
  // class_createInstance: idxIvars parameter Changed from unsigned to size_t
  Pointer(class_createInstance) := GetProcedureAddress(hnd, 'class_createInstance');

  // The following functions are added:
  class_getName:=@class_getName10;
  class_getSuperclass:=@class_getSuperclass10;
  class_isMetaClass:=@class_isMetaClass10;
  class_copyMethodList:=@class_copyMethodList10;
  class_getMethodImplementation:=@class_getMethodImplementation10;
  class_getMethodImplementation_stret:=@class_getMethodImplementation_stret10;
  class_respondsToSelector:=@class_respondsToSelector10;
  class_conformsToProtocol:=@class_conformsToProtocol10;
  class_copyProtocolList:=@class_copyProtocolList10;
  class_copyIvarList:=@class_copyIvarList10;

  //The following functions are deprecated:
  //objc_getClasses: deprecated in favor of objc_getClassList
  //class_createInstanceFromZone: deprecated in favor of class_createInstance
  //class_nextMethodList: deprecated in favor of new class_copyMethodList
  //class_lookupMethod: deprecated in favor of class_getMethodImplementation
  //class_respondsToMethod: deprecated in favor of class_respondsToSelector

  //The following function is used only by ZeroLink:
  //objc_getRequiredClass

  // Class Manipulation

  //The following function is unchanged:
  Pointer(class_setVersion) := GetProcedureAddress(hnd, 'class_setVersion');

  //The following functions are added:
  objc_allocateClassPair := @objc_allocateClassPair10;
  objc_registerClassPair := @objc_registerClassPair10;
  objc_duplicateClass := @objc_duplicateClass10;
  class_addMethod := @class_addMethod10;
  class_addIvar := @class_addIvar10;
  class_addProtocol := @class_addProtocol10;

  //The following functions are deprecated:
  //objc_addClass: deprecated in favor of objc_allocateClassPair and objc_registerClassPair
  //class_addMethods: deprecated in favor of new class_addMethod
  //class_removeMethods: deprecated with no substitute
  //class_poseAs: deprecated in favor of categories and method_setImplementation


  //Methods

  //The following function is unchanged:
  Pointer(method_getNumberOfArguments) := GetProcedureAddress(hnd, 'method_getNumberOfArguments');

  //The following functions are added:
  method_getName            := @method_getName10;
  method_getImplementation  := @method_getImplementation10;
  method_getTypeEncoding    := @method_getTypeEncoding10;
  method_copyReturnType     := @method_copyReturnType10;
  method_copyArgumentType   := @method_copyArgumentType10;
  method_setImplementation  := @method_setImplementation10;

  //The following functions are deprecated:
  //method_getArgumentInfo
  //method_getSizeOfArguments


  //Instance Variables

  //The following functions are added:
  ivar_getName := @ivar_getName10;
  ivar_getTypeEncoding := @ivar_getTypeEncoding10;
  ivar_getOffset := @ivar_getOffset10;

  //Selectors
  //The following functions are unchanged:
  Pointer(sel_getName) := GetProcedureAddress(hnd, 'sel_getName');
  Pointer(sel_registerName) := GetProcedureAddress(hnd, 'sel_registerName');
  Pointer(sel_getUid) := GetProcedureAddress(hnd, 'sel_getUid');

  //The following function is added:
  sel_isEqual := @sel_isEqual10;

  //The following function is deprecated:
  //sel_isMapped: deprecated with no substitute


  //Runtime
  //The following functions are deprecated favor of dyld:
  //objc_loadModules
  //objc_loadModule
  //objc_unloadModules

  //The following functions are deprecated:
  //objc_setClassHandler: deprecated with no substitute
  //objc_setMultithreaded: deprecated with no substitute

  //The following previously undocumented functions are deprecated with no substitute:
  //objc_getOrigClass, _objc_create_zone, _objc_error, _objc_flush_caches,
  //_objc_resolve_categories_for_class, _objc_setClassLoader,_ objc_setNilReceiver,
  //_objc_getNilReceiver,_ objcInit

  //The following undocumented functions are unchanged:
  //_objc_getFreedObjectClass, instrumentObjcMessageSends, _objc_debug_class_hash
  //_class_printDuplicateCacheEntries, _class_printMethodCaches, _class_printMethodCacheStatistics


  //Messaging
  LoadDefaultObjCMessaging(hnd);


  //The following functions are removed:objc_msgSendv	Given an argument list, send a message with a simple return value.
  //objc_msgSendv_stret	Given an argument list, send a message with a data-structure return value.
  //objc_msgSendv_fpret	Given an argument list, send a message with a floating point return value.

  //Protocols
  //The following functions are added:
  objc_getProtocol := @objc_getProtocol10;
  objc_copyProtocolList := @objc_copyProtocolList10;

  // Initializating additional objective-c runtime 1.0 functions

  Pointer(objc_addClass) := GetProcedureAddress(hnd, 'objc_addClass');
  //Pointer(class_addMethods) := GetProcedureAddress(hnd, 'objc_addMethods');

end;

end.
