{
 objcrtl20.pas

 Copyright (C) 2009 Dmitry Boyarintsev

 This unit is implementation for dynamic Objective-C Run-time Library based on run-time version 2.0
 headers included with XCode 3.1.2
 The original copyright note of is kept on each include file
}

unit objcrtl20;

{$mode objfpc}{$H+}

interface

uses
  objcrtl, dynlibs;

function InitializeObjcRtl20(const ObjCLibName: AnsiString): Boolean;

implementation

function InitializeObjcRtl20(const ObjCLibName: AnsiString): Boolean;
var
  hnd : TLibHandle;
begin
  hnd := LoadLibrary(ObjCLibName);
  Result := hnd <> 0;
  if not Result then Exit;

  LoadDefaultObjCExepction(hnd);
  LoadDefaultObjCSync(hnd);

  Pointer(sel_getName) := GetProcedureAddress(hnd, 'sel_getName');
  Pointer(sel_registerName) := GetProcedureAddress(hnd, 'sel_registerName');
  Pointer(object_getClassName) := GetProcedureAddress(hnd, 'object_getClassName');
  Pointer(object_getIndexedIvars) := GetProcedureAddress(hnd, 'object_getIndexedIvars');

  Pointer(sel_isMapped) := GetProcedureAddress(hnd, 'sel_isMapped');
  Pointer(sel_getUid)   := GetProcedureAddress(hnd, 'sel_getUid');

  Pointer(object_copy)    := GetProcedureAddress(hnd, 'object_copy');
  Pointer(object_dispose) := GetProcedureAddress(hnd, 'object_dispose');

  Pointer(object_getClass) := GetProcedureAddress(hnd, 'object_getClass');
  Pointer(object_setClass) := GetProcedureAddress(hnd, 'object_setClass');

  Pointer(object_getIvar) := GetProcedureAddress(hnd, 'object_getIvar');
  Pointer(object_setIvar) := GetProcedureAddress(hnd, 'object_setIvar');

  Pointer(object_setInstanceVariable) := GetProcedureAddress(hnd, 'object_setInstanceVariable');
  Pointer(object_getInstanceVariable) := GetProcedureAddress(hnd, 'object_getInstanceVariable');

  Pointer(objc_getClass)    := GetProcedureAddress(hnd, 'objc_getClass');
  Pointer(objc_getMetaClass) := GetProcedureAddress(hnd, 'objc_getMetaClass');
  Pointer(objc_lookUpClass) := GetProcedureAddress(hnd, 'objc_lookUpClass');
  Pointer(objc_getRequiredClass) := GetProcedureAddress(hnd, 'objc_getRequiredClass');
  Pointer(objc_getFutureClass)   := GetProcedureAddress(hnd, 'objc_getFutureClass');
  Pointer(objc_setFutureClass)   := GetProcedureAddress(hnd, 'objc_setFutureClass');
  Pointer(objc_getClassList)     := GetProcedureAddress(hnd, 'objc_getClassList');

  Pointer(objc_getProtocol)      := GetProcedureAddress(hnd, 'objc_getProtocol');
  Pointer(objc_copyProtocolList) := GetProcedureAddress(hnd, 'objc_copyProtocolList');

  Pointer(class_getName)      := GetProcedureAddress(hnd, 'class_getName');
  Pointer(class_isMetaClass)  := GetProcedureAddress(hnd, 'class_isMetaClass');
  Pointer(class_getSuperclass) := GetProcedureAddress(hnd, 'class_getSuperclass');
  Pointer(class_setSuperclass) := GetProcedureAddress(hnd, 'class_setSuperclass');

  Pointer(class_getVersion) := GetProcedureAddress(hnd, 'class_getVersion');
  Pointer(class_setVersion) := GetProcedureAddress(hnd, 'class_setVersion');

  Pointer(class_getInstanceSize) := GetProcedureAddress(hnd, 'class_getInstanceSize');

  Pointer(class_getInstanceVariable) := GetProcedureAddress(hnd, 'class_getInstanceVariable');
  Pointer(class_getClassVariable)    := GetProcedureAddress(hnd, 'class_getClassVariable');
  Pointer(class_copyIvarList)       := GetProcedureAddress(hnd, 'class_copyIvarList');

  Pointer(class_getInstanceMethod) := GetProcedureAddress(hnd, 'class_getInstanceMethod');
  Pointer(class_getClassMethod)    := GetProcedureAddress(hnd, 'class_getClassMethod');
  Pointer(class_getMethodImplementation)       := GetProcedureAddress(hnd, 'class_getMethodImplementation');
  Pointer(class_getMethodImplementation_stret) := GetProcedureAddress(hnd, 'class_getMethodImplementation_stret');
  Pointer(class_respondsToSelector) := GetProcedureAddress(hnd, 'class_respondsToSelector');
  Pointer(class_copyMethodList)     := GetProcedureAddress(hnd, 'class_copyMethodList');

  Pointer(class_conformsToProtocol) := GetProcedureAddress(hnd, 'class_conformsToProtocol');
  Pointer(class_copyProtocolList)   := GetProcedureAddress(hnd, 'class_copyProtocolList');

  Pointer(class_getProperty)      := GetProcedureAddress(hnd, 'class_getProperty');
  Pointer(class_copyPropertyList) := GetProcedureAddress(hnd, 'class_copyPropertyList');

  Pointer(class_getIvarLayout)     := GetProcedureAddress(hnd, 'class_getIvarLayout');
  Pointer(class_getWeakIvarLayout) := GetProcedureAddress(hnd, 'class_getWeakIvarLayout');

  Pointer(class_createInstance) := GetProcedureAddress(hnd, 'class_createInstance');

  Pointer(objc_allocateClassPair) := GetProcedureAddress(hnd, 'objc_allocateClassPair');
  Pointer(objc_registerClassPair) := GetProcedureAddress(hnd, 'objc_registerClassPair');
  Pointer(objc_duplicateClass)    := GetProcedureAddress(hnd, 'objc_duplicateClass');
  Pointer(objc_disposeClassPair)  := GetProcedureAddress(hnd, 'objc_disposeClassPair');

  Pointer(class_addMethod)     := GetProcedureAddress(hnd, 'class_addMethod');
  Pointer(class_replaceMethod) := GetProcedureAddress(hnd, 'class_replaceMethod');
  Pointer(class_addIvar)       := GetProcedureAddress(hnd, 'class_addIvar');
  Pointer(class_addProtocol)   := GetProcedureAddress(hnd, 'class_addProtocol');
  Pointer(class_setIvarLayout)     := GetProcedureAddress(hnd, 'class_setIvarLayout');
  Pointer(class_setWeakIvarLayout) := GetProcedureAddress(hnd, 'class_setWeakIvarLayout');

  Pointer(method_getName) := GetProcedureAddress(hnd, 'method_getName');
  Pointer(method_getImplementation) := GetProcedureAddress(hnd, 'method_getImplementation');
  Pointer(method_getTypeEncoding) := GetProcedureAddress(hnd, 'method_getTypeEncoding');

  Pointer(method_getNumberOfArguments) := GetProcedureAddress(hnd, 'method_getNumberOfArguments');
  Pointer(method_copyReturnType)   := GetProcedureAddress(hnd, 'method_copyReturnType');
  Pointer(method_copyArgumentType) := GetProcedureAddress(hnd, 'method_copyArgumentType');
  Pointer(method_getReturnType)    := GetProcedureAddress(hnd, 'method_getReturnType');
  Pointer(method_getArgumentType)  := GetProcedureAddress(hnd, 'method_getArgumentType');
  Pointer(method_getDescription)   := GetProcedureAddress(hnd, 'method_getDescription');

  Pointer(method_setImplementation) := GetProcedureAddress(hnd, 'method_setImplementation');
  Pointer(method_exchangeImplementations)   := GetProcedureAddress(hnd, 'method_exchangeImplementations');

  Pointer(ivar_getName)                     := GetProcedureAddress(hnd, 'ivar_getName');
  Pointer(ivar_getTypeEncoding) := GetProcedureAddress(hnd, 'ivar_getTypeEncoding');
  Pointer(ivar_getOffset)      := GetProcedureAddress(hnd, 'ivar_getOffset');

  Pointer(property_getName)     := GetProcedureAddress(hnd, 'property_getName');
  Pointer(property_getAttributes) := GetProcedureAddress(hnd, 'property_getAttributes');

  Pointer(protocol_conformsToProtocol) := GetProcedureAddress(hnd, 'protocol_conformsToProtocol');
  Pointer(protocol_isEqual)  := GetProcedureAddress(hnd, 'protocol_isEqual');
  Pointer(protocol_getMethodDescription)       := GetProcedureAddress(hnd, 'protocol_getMethodDescription');
  Pointer(protocol_copyMethodDescriptionList)  := GetProcedureAddress(hnd, 'protocol_copyMethodDescriptionList');
  Pointer(protocol_getProperty)      := GetProcedureAddress(hnd, 'protocol_getProperty');
  Pointer(protocol_copyPropertyList) := GetProcedureAddress(hnd, 'protocol_copyPropertyList');
  Pointer(protocol_copyProtocolList) := GetProcedureAddress(hnd, 'protocol_copyProtocolList');

  Pointer(objc_copyImageNames)  := GetProcedureAddress(hnd, 'objc_copyImageNames');
  Pointer(class_getImageName)   := GetProcedureAddress(hnd, 'class_getImageName');
  Pointer(objc_copyClassNamesForImage) := GetProcedureAddress(hnd, 'objc_copyClassNamesForImage');

  Pointer(sel_isEqual) := GetProcedureAddress(hnd, 'sel_isEqual');
  Pointer(objc_enumerationMutation) := GetProcedureAddress(hnd, 'objc_enumerationMutation');
  Pointer(objc_setEnumerationMutationHandler) := GetProcedureAddress(hnd, 'objc_setEnumerationMutationHandler');
  Pointer(objc_setForwardHandler) := GetProcedureAddress(hnd, 'objc_setForwardHandler');

  //Messaging
  LoadDefaultObjCMessaging(hnd);

  Pointer(method_invoke) := GetProcedureAddress(hnd, 'method_invoke');
  Pointer(method_invoke_stret) := GetProcedureAddress(hnd, 'method_invoke_stret');
  Pointer(objc_collect) := GetProcedureAddress(hnd, 'objc_collect');
  Pointer(objc_collectingEnabled) := GetProcedureAddress(hnd, 'objc_collectingEnabled');

end;


end.

