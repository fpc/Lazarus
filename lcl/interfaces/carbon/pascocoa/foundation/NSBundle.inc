{%mainunit foundation.pas}
(*	NSBundle.h
	Copyright (c) 1994-2005, Apple, Inc. All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSBUNDLE_PAS_H}
{$define NSBUNDLE_PAS_H}

{$include NSObject.inc}
{const
  NSLocalizedString(key, = comment) \;
  NSLocalizedStringFromTable(key, = tbl, comment) \;
  NSLocalizedStringFromTableInBundle(key, = tbl, bundle, comment) \;
  NSLocalizedStringWithDefaultValue(key, = tbl, bundle, val, comment) \;}

//[[NSBundle mainBundle] localizedStringForKey:(key) value:@"" table:nil]
//[[NSBundle mainBundle] localizedStringForKey:(key) value:@"" table:(tbl)]
//[bundle localizedStringForKey:(key) value:@"" table:(tbl)]
//[bundle localizedStringForKey:(key) value:(val) table:(tbl)]
//FOUNDATION_EXPORT NSString * const NSBundleDidLoadNotification;
//FOUNDATION_EXPORT NSString * const NSLoadedClasses;	// notification key
{$endif}
{$endif}

{$ifdef FORWARD}
  NSBundle = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSBUNDLE_PAS_C}
{$define NSBUNDLE_PAS_C}

{$include NSObject.inc}
    //  Because NSBundle caches allocated instances, subclasses should be prepared
    //    to receive an already initialized object back from [super initWithPath:]

  { NSBundle }

  NSBundle = class(NSObject)
  public
    class function getClass: lobjc.id; override;
    constructor mainBundle;
    constructor bundleWithPath(_path: CFStringRef);
    constructor initWithPath(_path: CFStringRef);
    constructor bundleForClass(_aClass: _Class);
    constructor bundleWithIdentifier(_identifier: CFStringRef);
    class function allBundles: CFArrayRef;
    class function allFrameworks: CFArrayRef;
    function load: LongBool;
    {.$ifdef  MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED}
    function isLoaded: LongBool;
    {.$endif}
    function bundlePath: CFStringRef;
    function resourcePath: CFStringRef;
    function executablePath: CFStringRef;
    function pathForAuxiliaryExecutable(_executableName: CFStringRef): CFStringRef;
    function privateFrameworksPath: CFStringRef;
    function sharedFrameworksPath: CFStringRef;
    function sharedSupportPath: CFStringRef;
    function builtInPlugInsPath: CFStringRef;
    function bundleIdentifier: CFStringRef;
    function classNamed(_className: CFStringRef): _Class;
    function principalClass: _Class;
    //  In the following methods, bundlePath is an absolute path to a bundle, and may not
    // be nil; subpath is a relative path to a subdirectory inside the relevant global or
    // localized resource directory, and should be nil if the resource file in question
    // is not in a subdirectory.
    class function pathForResource_ofType_inDirectory_(_name: CFStringRef; _ext: CFStringRef; _bundlePath: CFStringRef): CFStringRef;
    function pathForResource_ofType(_name: CFStringRef; _ext: CFStringRef): CFStringRef;
    function pathForResource_ofType_inDirectory(_name: CFStringRef; _ext: CFStringRef; _subpath: CFStringRef): CFStringRef;
    function pathForResource_ofType_inDirectory_forLocalization(_name: CFStringRef; _ext: CFStringRef; _subpath: CFStringRef; _localizationName: CFStringRef): CFStringRef;
    class function pathsForResourcesOfType_inDirectory_(_ext: CFStringRef; _bundlePath: CFStringRef): CFArrayRef;
    function pathsForResourcesOfType_inDirectory(_ext: CFStringRef; _subpath: CFStringRef): CFArrayRef;
    function pathsForResourcesOfType_inDirectory_forLocalization(_ext: CFStringRef; _subpath: CFStringRef; _localizationName: CFStringRef): CFArrayRef;
    function localizedStringForKey_value_table(_key: CFStringRef; _value: CFStringRef; _tableName: CFStringRef): CFStringRef;
    function infoDictionary: CFDictionaryRef;
    {.$ifdef  MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED}
    function localizedInfoDictionary: CFDictionaryRef;
    function objectForInfoDictionaryKey(_key: CFStringRef): lobjc.id;
    {.$endif}
    function localizations: CFArrayRef;
    function preferredLocalizations: CFArrayRef;
    {.$ifdef  MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED}
    function developmentLocalization: CFStringRef;
    {.$endif}
    class function preferredLocalizationsFromArray(_localizationsArray: CFArrayRef): CFArrayRef;
    {.$ifdef  MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED}
    class function preferredLocalizationsFromArray_forPreferences(_localizationsArray: CFArrayRef; _preferencesArray: CFArrayRef): CFArrayRef;
    {.$endif}
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSBundle_NSBundle = 'NSBundle';
  StrNSBundle_mainBundle = 'mainBundle';
  StrNSBundle_bundleWithPath = 'bundleWithPath:';
  StrNSBundle_initWithPath = 'initWithPath:';
  StrNSBundle_bundleForClass = 'bundleForClass:';
  StrNSBundle_bundleWithIdentifier = 'bundleWithIdentifier:';
  StrNSBundle_allBundles = 'allBundles';
  StrNSBundle_allFrameworks = 'allFrameworks';
  StrNSBundle_load = 'load';
  StrNSBundle_isLoaded = 'isLoaded';
  StrNSBundle_bundlePath = 'bundlePath';
  StrNSBundle_resourcePath = 'resourcePath';
  StrNSBundle_executablePath = 'executablePath';
  StrNSBundle_pathForAuxiliaryExecutable = 'pathForAuxiliaryExecutable:';
  StrNSBundle_privateFrameworksPath = 'privateFrameworksPath';
  StrNSBundle_sharedFrameworksPath = 'sharedFrameworksPath';
  StrNSBundle_sharedSupportPath = 'sharedSupportPath';
  StrNSBundle_builtInPlugInsPath = 'builtInPlugInsPath';
  StrNSBundle_bundleIdentifier = 'bundleIdentifier';
  StrNSBundle_classNamed = 'classNamed:';
  StrNSBundle_principalClass = 'principalClass';
  StrNSBundle_pathForResource_ofType_inDirectory = 'pathForResource:ofType:inDirectory:';
  StrNSBundle_pathForResource_ofType = 'pathForResource:ofType:';
  StrNSBundle_pathForResource_ofType_inDirectory_forLocalization = 'pathForResource:ofType:inDirectory:forLocalization:';
  StrNSBundle_pathsForResourcesOfType_inDirectory = 'pathsForResourcesOfType:inDirectory:';
  StrNSBundle_pathsForResourcesOfType_inDirectory_forLocalization = 'pathsForResourcesOfType:inDirectory:forLocalization:';
  StrNSBundle_localizedStringForKey_value_table = 'localizedStringForKey:value:table:';
  StrNSBundle_infoDictionary = 'infoDictionary';
  StrNSBundle_localizedInfoDictionary = 'localizedInfoDictionary';
  StrNSBundle_objectForInfoDictionaryKey = 'objectForInfoDictionaryKey:';
  StrNSBundle_localizations = 'localizations';
  StrNSBundle_preferredLocalizations = 'preferredLocalizations';
  StrNSBundle_developmentLocalization = 'developmentLocalization';
  StrNSBundle_preferredLocalizationsFromArray = 'preferredLocalizationsFromArray:';
  StrNSBundle_preferredLocalizationsFromArray_forPreferences = 'preferredLocalizationsFromArray:forPreferences:';

{ NSBundle }

class function NSBundle.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSBundle_NSBundle);
end;

constructor NSBundle.mainBundle;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSBundle_mainBundle)));
end;

constructor NSBundle.bundleWithPath(_path: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSBundle_bundleWithPath)), _path);
end;

constructor NSBundle.initWithPath(_path: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSBundle_initWithPath)), _path);
end;

constructor NSBundle.bundleForClass(_aClass: _Class);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aClass: _Class): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSBundle_bundleForClass)), _aClass);
end;

constructor NSBundle.bundleWithIdentifier(_identifier: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_identifier: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSBundle_bundleWithIdentifier)), _identifier);
end;

class function NSBundle.allBundles: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(getClass, sel_registerName(PChar(StrNSBundle_allBundles)), []));
end;

class function NSBundle.allFrameworks: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(getClass, sel_registerName(PChar(StrNSBundle_allFrameworks)), []));
end;

function NSBundle.load: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSBundle_load)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED}
function NSBundle.isLoaded: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSBundle_isLoaded)), []));
end;

{.$endif}
function NSBundle.bundlePath: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSBundle_bundlePath)), []));
end;

function NSBundle.resourcePath: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSBundle_resourcePath)), []));
end;

function NSBundle.executablePath: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSBundle_executablePath)), []));
end;

function NSBundle.pathForAuxiliaryExecutable(_executableName: CFStringRef): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_executableName: CFStringRef): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSBundle_pathForAuxiliaryExecutable)), _executableName));
end;

function NSBundle.privateFrameworksPath: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSBundle_privateFrameworksPath)), []));
end;

function NSBundle.sharedFrameworksPath: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSBundle_sharedFrameworksPath)), []));
end;

function NSBundle.sharedSupportPath: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSBundle_sharedSupportPath)), []));
end;

function NSBundle.builtInPlugInsPath: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSBundle_builtInPlugInsPath)), []));
end;

function NSBundle.bundleIdentifier: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSBundle_bundleIdentifier)), []));
end;

function NSBundle.classNamed(_className: CFStringRef): _Class;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_className: CFStringRef): _Class; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := _Class(vmethod(Handle, sel_registerName(PChar(StrNSBundle_classNamed)), _className));
end;

function NSBundle.principalClass: _Class;
begin
  Result := _Class(objc_msgSend(Handle, sel_registerName(PChar(StrNSBundle_principalClass)), []));
end;

class function NSBundle.pathForResource_ofType_inDirectory_(_name: CFStringRef; _ext: CFStringRef; _bundlePath: CFStringRef): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_name: CFStringRef; _ext: CFStringRef; _bundlePath: CFStringRef): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(getClass, sel_registerName(PChar(StrNSBundle_pathForResource_ofType_inDirectory)), _name, _ext, _bundlePath));
end;

function NSBundle.pathForResource_ofType(_name: CFStringRef; _ext: CFStringRef): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_name: CFStringRef; _ext: CFStringRef): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSBundle_pathForResource_ofType)), _name, _ext));
end;

function NSBundle.pathForResource_ofType_inDirectory(_name: CFStringRef; _ext: CFStringRef; _subpath: CFStringRef): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_name: CFStringRef; _ext: CFStringRef; _subpath: CFStringRef): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSBundle_pathForResource_ofType_inDirectory)), _name, _ext, _subpath));
end;

function NSBundle.pathForResource_ofType_inDirectory_forLocalization(_name: CFStringRef; _ext: CFStringRef; _subpath: CFStringRef; _localizationName: CFStringRef): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_name: CFStringRef; _ext: CFStringRef; _subpath: CFStringRef; _localizationName: CFStringRef): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSBundle_pathForResource_ofType_inDirectory_forLocalization)), _name, _ext, _subpath, _localizationName));
end;

class function NSBundle.pathsForResourcesOfType_inDirectory_(_ext: CFStringRef; _bundlePath: CFStringRef): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_ext: CFStringRef; _bundlePath: CFStringRef): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(getClass, sel_registerName(PChar(StrNSBundle_pathsForResourcesOfType_inDirectory)), _ext, _bundlePath));
end;

function NSBundle.pathsForResourcesOfType_inDirectory(_ext: CFStringRef; _subpath: CFStringRef): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_ext: CFStringRef; _subpath: CFStringRef): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(Handle, sel_registerName(PChar(StrNSBundle_pathsForResourcesOfType_inDirectory)), _ext, _subpath));
end;

function NSBundle.pathsForResourcesOfType_inDirectory_forLocalization(_ext: CFStringRef; _subpath: CFStringRef; _localizationName: CFStringRef): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_ext: CFStringRef; _subpath: CFStringRef; _localizationName: CFStringRef): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(Handle, sel_registerName(PChar(StrNSBundle_pathsForResourcesOfType_inDirectory_forLocalization)), _ext, _subpath, _localizationName));
end;

function NSBundle.localizedStringForKey_value_table(_key: CFStringRef; _value: CFStringRef; _tableName: CFStringRef): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_key: CFStringRef; _value: CFStringRef; _tableName: CFStringRef): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSBundle_localizedStringForKey_value_table)), _key, _value, _tableName));
end;

function NSBundle.infoDictionary: CFDictionaryRef;
begin
  Result := CFDictionaryRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSBundle_infoDictionary)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED}
function NSBundle.localizedInfoDictionary: CFDictionaryRef;
begin
  Result := CFDictionaryRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSBundle_localizedInfoDictionary)), []));
end;

function NSBundle.objectForInfoDictionaryKey(_key: CFStringRef): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_key: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSBundle_objectForInfoDictionaryKey)), _key));
end;

{.$endif}
function NSBundle.localizations: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSBundle_localizations)), []));
end;

function NSBundle.preferredLocalizations: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSBundle_preferredLocalizations)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED}
function NSBundle.developmentLocalization: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSBundle_developmentLocalization)), []));
end;

{.$endif}
class function NSBundle.preferredLocalizationsFromArray(_localizationsArray: CFArrayRef): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_localizationsArray: CFArrayRef): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(getClass, sel_registerName(PChar(StrNSBundle_preferredLocalizationsFromArray)), _localizationsArray));
end;

{.$ifdef  MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED}
class function NSBundle.preferredLocalizationsFromArray_forPreferences(_localizationsArray: CFArrayRef; _preferencesArray: CFArrayRef): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_localizationsArray: CFArrayRef; _preferencesArray: CFArrayRef): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(getClass, sel_registerName(PChar(StrNSBundle_preferredLocalizationsFromArray_forPreferences)), _localizationsArray, _preferencesArray));
end;

{.$endif}
{$endif}
