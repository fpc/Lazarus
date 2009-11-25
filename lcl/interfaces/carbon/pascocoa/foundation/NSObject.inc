{%MainUnit Foundation.pas}
{	NSObject.h
	Copyright (c) 1994-2005, Apple, Inc. All rights reserved.
}

{$ifdef HEADER}
{$ifndef NSOBJECT_PAS_H}
{$define NSOBJECT_PAS_H}

//#import <Foundation/NSObjCRuntime.h>
//#import <Foundation/NSZone.h>

{ Class and method name strings }
const
  {***********	Base class		***********}

  Str_NSObject = 'NSObject';

  Str_alloc = 'alloc';
  Str_init = 'init';

  Str_version = 'version';
  Str_setVersion = 'setVersion:';

  {***************	Basic protocols		***************}

  Str_retain = 'retain';
  Str_release = 'release';
  Str_autorelease = 'autorelease';

{***********	Object Allocation / Deallocation		*******}

{FOUNDATION_EXPORT id <NSObject> NSAllocateObject(Class aClass, unsigned extraBytes, NSZone *zone);

FOUNDATION_EXPORT void NSDeallocateObject(id <NSObject>object);

FOUNDATION_EXPORT id <NSObject> NSCopyObject(id <NSObject>object, unsigned extraBytes, NSZone *zone);

FOUNDATION_EXPORT BOOL NSShouldRetainWithZone(id <NSObject> anObject, NSZone *requestedZone);

FOUNDATION_EXPORT void NSIncrementExtraRefCount(id object);

FOUNDATION_EXPORT BOOL NSDecrementExtraRefCountWasZero(id object);

FOUNDATION_EXPORT unsigned NSExtraRefCount(id object);}

{$endif}
{$endif}
{$ifdef CLASSES}
{$ifndef NSOBJECT_PAS_C}
{$define NSOBJECT_PAS_C}

{***********	Base class		***********}

  NSObject = class
  public
    { class id }
    ClassId: lobjc.id;
    { object references }
    allocbuf, Handle: lobjc.id;
    { Constructor / Destructor }
    constructor Create; virtual;
    constructor CreateWithHandle(aHandle: lobjc.id);
    destructor Destroy; override;
    { Extra binding functions }
    class function getClass: lobjc.id; virtual;
    procedure AddMethods; virtual;
    { Class creation methods }
    procedure AddMethod(aName, aParameters: string; aPointer: Pointer);
    function CreateClassDefinition(const name, superclassName: string): Boolean;
  public
{+ (void)load;

+ (void)initialize;
- (id)init;

+ (id)new;
+ (id)allocWithZone:(NSZone *)zone;
+ (id)alloc;
- (void)dealloc;

#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4
- (void)finalize;
#endif

- (id)copy;
- (id)mutableCopy;

+ (id)copyWithZone:(NSZone *)zone;
+ (id)mutableCopyWithZone:(NSZone *)zone;

+ (Class)superclass;
+ (Class)class;
+ (void)poseAsClass:(Class)aClass;
+ (BOOL)instancesRespondToSelector:(SEL)aSelector;
+ (BOOL)conformsToProtocol:(Protocol *)protocol;
- (IMP)methodForSelector:(SEL)aSelector;
+ (IMP)instanceMethodForSelector:(SEL)aSelector;}
    function version: PtrInt;
    procedure setVersion(aVersion: PtrInt);
{- (void)doesNotRecognizeSelector:(SEL)aSelector;
- (void)forwardInvocation:(NSInvocation *)anInvocation;
- (NSMethodSignature *)methodSignatureForSelector:(SEL)aSelector;

+ (NSMethodSignature *)instanceMethodSignatureForSelector:(SEL)aSelector;

#if MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED
+ (BOOL)isSubclassOfClass:(Class)aClass;
#endif

+ (NSString *)description;

- (Class)classForCoder;
- (id)replacementObjectForCoder:(NSCoder *)aCoder;
- (id)awakeAfterUsingCoder:(NSCoder *)aDecoder;}

//@class NSInvocation, NSMethodSignature, NSCoder, NSString, NSEnumerator;
//@class Protocol;

{***************	Basic protocols		***************}

{@protocol NSObject

- (BOOL)isEqual:(id)object;
- (unsigned)hash;

- (Class)superclass;
- (Class)class;
- (id)self;
- (NSZone *)zone;

- (id)performSelector:(SEL)aSelector;
- (id)performSelector:(SEL)aSelector withObject:(id)object;
- (id)performSelector:(SEL)aSelector withObject:(id)object1 withObject:(id)object2;

- (BOOL)isProxy;

- (BOOL)isKindOfClass:(Class)aClass;
- (BOOL)isMemberOfClass:(Class)aClass;
- (BOOL)conformsToProtocol:(Protocol *)aProtocol;

- (BOOL)respondsToSelector:(SEL)aSelector;}

    function retain: lobjc.id;
    procedure release;
    function autorelease: lobjc.id;
{- (unsigned)retainCount;

- (NSString *)description;

@end

@protocol NSCopying

- (id)copyWithZone:(NSZone *)zone;

@end

@protocol NSMutableCopying

- (id)mutableCopyWithZone:(NSZone *)zone;

@end

@protocol NSCoding

- (void)encodeWithCoder:(NSCoder *)aCoder;
- (id)initWithCoder:(NSCoder *)aDecoder;

@end     }

  end;

{$endif}
{$endif}
{$ifdef IMPLEMENTATION}

{***********	Base class		***********}

constructor NSObject.Create;
begin
  ClassId := getClass();
  allocbuf := objc_msgSend(ClassId, sel_registerName(PChar(Str_alloc)), []);
  Handle := objc_msgSend(allocbuf, sel_registerName(PChar(Str_init)), []);
  
  { Adds custom methods, if any }
  AddMethods();
end;

constructor NSObject.CreateWithHandle(aHandle: lobjc.id);
begin
  ClassId := getClass();
  Handle := aHandle;
end;

destructor NSObject.Destroy;
begin
  if Handle <> nil then release;
end;

class function NSObject.getClass: lobjc.id;
var
  Str: string;
begin
  Str := ClassName();
  Result := objc_getClass(PChar(Str));
end;

{ Defines the appropriate place to register methods in the objective-c runtime }
procedure NSObject.AddMethods;
begin

end;

procedure NSObject.AddMethod(aName, aParameters: string; aPointer: Pointer);
var
  method_list: Pobjc_method_list;
begin
  method_list := GetMem(SizeOf(objc_method_list)); { We can't free this until the last instance is freed }

  method_list^.method_count := 1;
  method_list^.method_list[0].method_name := sel_registerName(PChar(aName));
  method_list^.method_list[0].method_types := PChar(aParameters);
  method_list^.method_list[0].method_imp := IMP(aPointer);
  class_addMethods(ClassId, method_list);
end;

function NSObject.CreateClassDefinition(const name, superclassName: string): Boolean;
var
  meta_class, super_class, new_class, root_class: Pobjc_class;
begin
  // Ensure that the superclass exists and that someone
  // hasn't already implemented a class with the same name
  //
  super_class := Pobjc_class(objc_lookUpClass (PChar(superclassName)));

  if (super_class = nil) then Exit(False);

  if (objc_lookUpClass (PChar(name)) <> nil) then Exit(False);

  // Find the root class
  //
  root_class := super_class;
  while ( root_class^.super_class <> nil ) do
  begin
    root_class := root_class^.super_class;
  end;

  // Allocate space for the class and its metaclass
  //
  new_class := CFAllocatorAllocate(kCFAllocatorMalloc, 2 * SizeOf(objc_class), 0);
  FillChar(new_class^, 2 * SizeOf(objc_class), $0);
  meta_class := @new_class[1];

  // setup class
  new_class^.isa      := meta_class;
  new_class^.info     := CLS_CLASS;
  meta_class^.info    := CLS_META;

  // Create a copy of the class name.
  // For efficiency, we have the metaclass and the class itself
  // to share this copy of the name, but this is not a requirement
  // imposed by the runtime.
  //
  new_class^.name := CFAllocatorAllocate(kCFAllocatorMalloc, Length(name) + 1, 0);
  SysUtils.strcopy(new_class^.name, PChar(name));
  meta_class^.name := new_class^.name;

  // Allocate empty method lists.
  // We can add methods later.
  //
  new_class^.methodLists := CFAllocatorAllocate(kCFAllocatorMalloc, sizeof(Pobjc_method_list), 0);
  new_class^.methodLists^ := Pointer(-1);
  meta_class^.methodLists := CFAllocatorAllocate(kCFAllocatorMalloc, sizeof(Pobjc_method_list), 0);
  meta_class^.methodLists^ := Pointer(-1);

  // Connect the class definition to the class hierarchy:
  // Connect the class to the superclass.
  // Connect the metaclass to the metaclass of the superclass.
  // Connect the metaclass of the metaclass to the metaclass of  the root class.
  //
  new_class^.super_class  := super_class;
  meta_class^.super_class := super_class^.isa;
  meta_class^.isa         := Pointer(root_class^.isa);

  // Set the sizes of the class and the metaclass.
  //
  new_class^.instance_size := super_class^.instance_size;
  meta_class^.instance_size := meta_class^.super_class^.instance_size;

  // Finally, register the class with the runtime.
  //
  objc_addClass( new_class );

  Result := True;
end;

function NSObject.version: PtrInt;
begin
  Result := PtrInt(objc_msgSend(Handle, sel_registerName(PChar(Str_version)), []));
end;

procedure NSObject.setVersion(aVersion: PtrInt);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anInt: PtrInt); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(Str_setVersion)), aVersion);
end;

{***************	Basic protocols		***************}

function NSObject.retain: lobjc.id;
begin
  Result := objc_msgSend(Handle, sel_registerName(PChar(Str_retain)), []);
end;

procedure NSObject.release;
begin
  objc_msgSend(Handle, sel_registerName(PChar(Str_release)), []);
  Handle:=nil;
end;

function NSObject.autorelease: lobjc.id;
begin
  Result := objc_msgSend(Handle, sel_registerName(PChar(Str_autorelease)), []);
end;

{$endif}

