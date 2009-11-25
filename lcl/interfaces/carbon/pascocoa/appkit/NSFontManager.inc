{%mainunit appkit.pas}
(*
	NSFontManager.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSFONTMANAGER_PAS_H}
{$define NSFONTMANAGER_PAS_H}

//#import <Foundation/NSObject.h>
type
  NSFontTraitMask = LongWord;

  //  * Font Traits
  //  *
  //  * This list should be kept small since the more traits that are assigned
  //  * to a given font, the harder it will be to map it to some other family.
  //  * Some traits are mutually exclusive such as NSExpanded and NSCondensed.
  // 
const
  NSItalicFontMask = $00000001;
  NSBoldFontMask = $00000002;
  NSUnboldFontMask = $00000004;
  NSNonStandardCharacterSetFontMask = $00000008;
  NSNarrowFontMask = $00000010;
  NSExpandedFontMask = $00000020;
  NSCondensedFontMask = $00000040;
  NSSmallCapsFontMask = $00000080;
  NSPosterFontMask = $00000100;
  NSCompressedFontMask = $00000200;
  NSFixedPitchFontMask = $00000400;
  NSUnitalicFontMask = $01000000;

const
  NSFontCollectionApplicationOnlyMask = 1 shl 0;

  //  And these "actions" are really tag values in Font Menu cells which send any of the
  // action messages listed above.  Normally, they're pre-defined in the font panel.
type
  NSFontAction = (
    NSNoFontChangeAction = 0, 
    NSViaPanelFontAction = 1, 
    NSAddTraitFontAction = 2, 
    NSSizeUpFontAction = 3, 
    NSSizeDownFontAction = 4, 
    NSHeavierFontAction = 5, 
    NSLighterFontAction = 6, 
    NSRemoveTraitFontAction = 7
  );


  //  This method is perhaps inappropriately named; it is sent to the delegate of the
  // shared NSFontPanel instance, not to the delegate of the NSFontManager.
  //  This is the message that's propagated up the responder chain.

{$endif}
{$endif}

{$ifdef FORWARD}
  NSFontManager = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSFONTMANAGER_PAS_C}
{$define NSFONTMANAGER_PAS_C}


  { NSFontManager }

  NSFontManager = class(NSObject)
  public
    class function getClass: lobjc.id; override;
    // All instance variables are private
    class procedure setFontPanelFactory_(_factoryId: _Class);
    class procedure setFontManagerFactory_(_factoryId: _Class);
    constructor sharedFontManager;
    function isMultiple: LongBool;
    function selectedFont: lobjc.id;
    procedure setSelectedFont_isMultiple_(_fontObj: lobjc.id; _flag: LongBool);
    procedure setFontMenu_(_newMenu: lobjc.id);
    function fontMenu_(_create: LongBool): lobjc.id;
    function fontPanel_(_create: LongBool): lobjc.id;
    function fontWithFamily_traits_weight_size_(_family: CFStringRef; _traits: NSFontTraitMask; _weight: Integer; _size: Single): lobjc.id;
    function traitsOfFont_(_fontObj: lobjc.id): NSFontTraitMask;
    function weightOfFont_(_fontObj: lobjc.id): Integer;
    //  Three methods for supporting app font management.  The fonts and names used/returned
    // by these functions are the same as those used by the NSFontPanel.  The third method
    // below takes as input a name as returned by "availableFontFamilies" and returns an
    // NSArray of NSArrays.  The elements of the "inner" arrays are: (0) the font's name,
    // (1) non-family part of the name as used by NSFontPanel, (2) an NSNumber indicating
    // the weight, and (3) an NSNumber indicating the traits.
    function availableFonts: CFArrayRef;
    function availableFontFamilies: CFArrayRef;
    function availableMembersOfFontFamily_(_fam: CFStringRef): CFArrayRef;
    function convertFont_(_fontObj: lobjc.id): lobjc.id;
    function convertFont_toSize_(_fontObj: lobjc.id; _size: Single): lobjc.id;
    function convertFont_toFace_(_fontObj: lobjc.id; _typeface: CFStringRef): lobjc.id;
    function convertFont_toFamily_(_fontObj: lobjc.id; _family: CFStringRef): lobjc.id;
    function convertFont_toHaveTrait_(_fontObj: lobjc.id; _trait: NSFontTraitMask): lobjc.id;
    function convertFont_toNotHaveTrait_(_fontObj: lobjc.id; _trait: NSFontTraitMask): lobjc.id;
    function convertWeight_ofFont_(_upFlag: LongBool; _fontObj: lobjc.id): lobjc.id;
    function isEnabled: LongBool;
    procedure setEnabled_(_flag: LongBool);
    function action: SEL;
    procedure setAction_(_aSelector: SEL);
    function sendAction: LongBool;
    procedure setDelegate_(_anObject: lobjc.id);
    function delegate: lobjc.id;
    function localizedNameForFamily_face_(_family: CFStringRef; _faceKey: CFStringRef): CFStringRef;
    procedure setSelectedAttributes_isMultiple_(_attributes: CFDictionaryRef; _flag: LongBool);
    function convertAttributes_(_attributes: CFDictionaryRef): CFDictionaryRef;
    function availableFontNamesMatchingFontDescriptor_(_descriptor: lobjc.id): CFArrayRef;
    function collectionNames: CFArrayRef;
    function fontDescriptorsInCollection_(_collectionNames: CFStringRef): CFArrayRef;
    function addCollection_options_(_collectionName: CFStringRef; _collectionOptions: Integer): LongBool;
    function removeCollection_(_collectionName: CFStringRef): LongBool;
    procedure addFontDescriptors_toCollection_(_descriptors: CFArrayRef; _collectionName: CFStringRef);
    procedure removeFontDescriptor_fromCollection_(_descriptor: lobjc.id; _collection: CFStringRef);
    function fontNamed_hasTraits_(_fName: CFStringRef; _someTraits: NSFontTraitMask): LongBool;
    //  The following is like "availableFonts", but checks traits.  Returns an auto-released
    // instance.  To get fonts with "non-italic" or "non-bold" combine the flags NSUnitalicFontMask
    // and/or NSUnboldFontMask as the argument "someTraits".
    function availableFontNamesWithTraits_(_someTraits: NSFontTraitMask): CFArrayRef;
    //  These methods are action messages sent from Font Menu items:
    procedure addFontTrait_(_sender: lobjc.id);
    procedure removeFontTrait_(_sender: lobjc.id);
    procedure modifyFontViaPanel_(_sender: lobjc.id);
    procedure modifyFont_(_sender: lobjc.id);
    procedure orderFrontFontPanel_(_sender: lobjc.id);
    procedure orderFrontStylesPanel_(_sender: lobjc.id);
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSFontManager_NSFontManager = 'NSFontManager';
  StrNSFontManager_setFontPanelFactory_ = 'setFontPanelFactory:';
  StrNSFontManager_setFontManagerFactory_ = 'setFontManagerFactory:';
  StrNSFontManager_sharedFontManager = 'sharedFontManager';
  StrNSFontManager_isMultiple = 'isMultiple';
  StrNSFontManager_selectedFont = 'selectedFont';
  StrNSFontManager_setSelectedFont_isMultiple_ = 'setSelectedFont:isMultiple:';
  StrNSFontManager_setFontMenu_ = 'setFontMenu:';
  StrNSFontManager_fontMenu_ = 'fontMenu:';
  StrNSFontManager_fontPanel_ = 'fontPanel:';
  StrNSFontManager_fontWithFamily_traits_weight_size_ = 'fontWithFamily:traits:weight:size:';
  StrNSFontManager_traitsOfFont_ = 'traitsOfFont:';
  StrNSFontManager_weightOfFont_ = 'weightOfFont:';
  StrNSFontManager_availableFonts = 'availableFonts';
  StrNSFontManager_availableFontFamilies = 'availableFontFamilies';
  StrNSFontManager_availableMembersOfFontFamily_ = 'availableMembersOfFontFamily:';
  StrNSFontManager_convertFont_ = 'convertFont:';
  StrNSFontManager_convertFont_toSize_ = 'convertFont:toSize:';
  StrNSFontManager_convertFont_toFace_ = 'convertFont:toFace:';
  StrNSFontManager_convertFont_toFamily_ = 'convertFont:toFamily:';
  StrNSFontManager_convertFont_toHaveTrait_ = 'convertFont:toHaveTrait:';
  StrNSFontManager_convertFont_toNotHaveTrait_ = 'convertFont:toNotHaveTrait:';
  StrNSFontManager_convertWeight_ofFont_ = 'convertWeight:ofFont:';
  StrNSFontManager_isEnabled = 'isEnabled';
  StrNSFontManager_setEnabled_ = 'setEnabled:';
  StrNSFontManager_action = 'action';
  StrNSFontManager_setAction_ = 'setAction:';
  StrNSFontManager_sendAction = 'sendAction';
  StrNSFontManager_setDelegate_ = 'setDelegate:';
  StrNSFontManager_delegate = 'delegate';
  StrNSFontManager_localizedNameForFamily_face_ = 'localizedNameForFamily:face:';
  StrNSFontManager_setSelectedAttributes_isMultiple_ = 'setSelectedAttributes:isMultiple:';
  StrNSFontManager_convertAttributes_ = 'convertAttributes:';
  StrNSFontManager_availableFontNamesMatchingFontDescriptor_ = 'availableFontNamesMatchingFontDescriptor:';
  StrNSFontManager_collectionNames = 'collectionNames';
  StrNSFontManager_fontDescriptorsInCollection_ = 'fontDescriptorsInCollection:';
  StrNSFontManager_addCollection_options_ = 'addCollection:options:';
  StrNSFontManager_removeCollection_ = 'removeCollection:';
  StrNSFontManager_addFontDescriptors_toCollection_ = 'addFontDescriptors:toCollection:';
  StrNSFontManager_removeFontDescriptor_fromCollection_ = 'removeFontDescriptor:fromCollection:';

  StrNSFontManager_fontNamed_hasTraits_ = 'fontNamed:hasTraits:';
  StrNSFontManager_availableFontNamesWithTraits_ = 'availableFontNamesWithTraits:';
  StrNSFontManager_addFontTrait_ = 'addFontTrait:';
  StrNSFontManager_removeFontTrait_ = 'removeFontTrait:';
  StrNSFontManager_modifyFontViaPanel_ = 'modifyFontViaPanel:';
  StrNSFontManager_modifyFont_ = 'modifyFont:';
  StrNSFontManager_orderFrontFontPanel_ = 'orderFrontFontPanel:';
  StrNSFontManager_orderFrontStylesPanel_ = 'orderFrontStylesPanel:';

{ NSFontManager }

class function NSFontManager.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSFontManager_NSFontManager);
end;

class procedure NSFontManager.setFontPanelFactory_(_factoryId: _Class);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_factoryId: _Class);cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSFontManager_setFontPanelFactory_)), _factoryId);
end;

class procedure NSFontManager.setFontManagerFactory_(_factoryId: _Class);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_factoryId: _Class);cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSFontManager_setFontManagerFactory_)), _factoryId);
end;

constructor NSFontManager.sharedFontManager;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSFontManager_sharedFontManager)));
end;

function NSFontManager.isMultiple: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSFontManager_isMultiple)), []));
end;

function NSFontManager.selectedFont: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSFontManager_selectedFont)), []));
end;

procedure NSFontManager.setSelectedFont_isMultiple_(_fontObj: lobjc.id; _flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_fontObj: lobjc.id; _flag: LongBool);cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSFontManager_setSelectedFont_isMultiple_)), _fontObj, _flag);
end;

procedure NSFontManager.setFontMenu_(_newMenu: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_newMenu: lobjc.id);cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSFontManager_setFontMenu_)), _newMenu);
end;

function NSFontManager.fontMenu_(_create: LongBool): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_create: LongBool): lobjc.id;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_fontMenu_)), _create));
end;

function NSFontManager.fontPanel_(_create: LongBool): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_create: LongBool): lobjc.id;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_fontPanel_)), _create));
end;

function NSFontManager.fontWithFamily_traits_weight_size_(_family: CFStringRef; _traits: NSFontTraitMask; _weight: Integer; _size: Single): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_family: CFStringRef; _traits: NSFontTraitMask; _weight: Integer; _size: Single): lobjc.id;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_fontWithFamily_traits_weight_size_)), _family, _traits, _weight, _size));
end;

function NSFontManager.traitsOfFont_(_fontObj: lobjc.id): NSFontTraitMask;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontObj: lobjc.id): NSFontTraitMask;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := NSFontTraitMask(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_traitsOfFont_)), _fontObj));
end;

function NSFontManager.weightOfFont_(_fontObj: lobjc.id): Integer;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontObj: lobjc.id): Integer;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Integer(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_weightOfFont_)), _fontObj));
end;

function NSFontManager.availableFonts: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSFontManager_availableFonts)), []));
end;

function NSFontManager.availableFontFamilies: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSFontManager_availableFontFamilies)), []));
end;

function NSFontManager.availableMembersOfFontFamily_(_fam: CFStringRef): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fam: CFStringRef): CFArrayRef;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_availableMembersOfFontFamily_)), _fam));
end;

function NSFontManager.convertFont_(_fontObj: lobjc.id): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontObj: lobjc.id): lobjc.id;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_convertFont_)), _fontObj));
end;

function NSFontManager.convertFont_toSize_(_fontObj: lobjc.id; _size: Single): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontObj: lobjc.id; _size: Single): lobjc.id;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_convertFont_toSize_)), _fontObj, _size));
end;

function NSFontManager.convertFont_toFace_(_fontObj: lobjc.id; _typeface: CFStringRef): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontObj: lobjc.id; _typeface: CFStringRef): lobjc.id;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_convertFont_toFace_)), _fontObj, _typeface));
end;

function NSFontManager.convertFont_toFamily_(_fontObj: lobjc.id; _family: CFStringRef): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontObj: lobjc.id; _family: CFStringRef): lobjc.id;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_convertFont_toFamily_)), _fontObj, _family));
end;

function NSFontManager.convertFont_toHaveTrait_(_fontObj: lobjc.id; _trait: NSFontTraitMask): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontObj: lobjc.id; _trait: NSFontTraitMask): lobjc.id;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_convertFont_toHaveTrait_)), _fontObj, _trait));
end;

function NSFontManager.convertFont_toNotHaveTrait_(_fontObj: lobjc.id; _trait: NSFontTraitMask): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontObj: lobjc.id; _trait: NSFontTraitMask): lobjc.id;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_convertFont_toNotHaveTrait_)), _fontObj, _trait));
end;

function NSFontManager.convertWeight_ofFont_(_upFlag: LongBool; _fontObj: lobjc.id): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_upFlag: LongBool; _fontObj: lobjc.id): lobjc.id;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_convertWeight_ofFont_)), _upFlag, _fontObj));
end;

function NSFontManager.isEnabled: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSFontManager_isEnabled)), []));
end;

procedure NSFontManager.setEnabled_(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool);cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSFontManager_setEnabled_)), _flag);
end;

function NSFontManager.action: SEL;
begin
  Result := SEL(objc_msgSend(Handle, sel_registerName(PChar(StrNSFontManager_action)), []));
end;

procedure NSFontManager.setAction_(_aSelector: SEL);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aSelector: SEL);cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSFontManager_setAction_)), _aSelector);
end;

function NSFontManager.sendAction: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSFontManager_sendAction)), []));
end;

procedure NSFontManager.setDelegate_(_anObject: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anObject: lobjc.id);cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSFontManager_setDelegate_)), _anObject);
end;

function NSFontManager.delegate: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSFontManager_delegate)), []));
end;

function NSFontManager.localizedNameForFamily_face_(_family: CFStringRef; _faceKey: CFStringRef): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_family: CFStringRef; _faceKey: CFStringRef): CFStringRef;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_localizedNameForFamily_face_)), _family, _faceKey));
end;

procedure NSFontManager.setSelectedAttributes_isMultiple_(_attributes: CFDictionaryRef; _flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_attributes: CFDictionaryRef; _flag: LongBool);cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSFontManager_setSelectedAttributes_isMultiple_)), _attributes, _flag);
end;

function NSFontManager.convertAttributes_(_attributes: CFDictionaryRef): CFDictionaryRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_attributes: CFDictionaryRef): CFDictionaryRef;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFDictionaryRef(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_convertAttributes_)), _attributes));
end;

function NSFontManager.availableFontNamesMatchingFontDescriptor_(_descriptor: lobjc.id): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_descriptor: lobjc.id): CFArrayRef;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_availableFontNamesMatchingFontDescriptor_)), _descriptor));
end;

function NSFontManager.collectionNames: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSFontManager_collectionNames)), []));
end;

function NSFontManager.fontDescriptorsInCollection_(_collectionNames: CFStringRef): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_collectionNames: CFStringRef): CFArrayRef;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_fontDescriptorsInCollection_)), _collectionNames));
end;

function NSFontManager.addCollection_options_(_collectionName: CFStringRef; _collectionOptions: Integer): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_collectionName: CFStringRef; _collectionOptions: Integer): LongBool;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_addCollection_options_)), _collectionName, _collectionOptions));
end;

function NSFontManager.removeCollection_(_collectionName: CFStringRef): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_collectionName: CFStringRef): LongBool;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_removeCollection_)), _collectionName));
end;

procedure NSFontManager.addFontDescriptors_toCollection_(_descriptors: CFArrayRef; _collectionName: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_descriptors: CFArrayRef; _collectionName: CFStringRef);cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSFontManager_addFontDescriptors_toCollection_)), _descriptors, _collectionName);
end;

procedure NSFontManager.removeFontDescriptor_fromCollection_(_descriptor: lobjc.id; _collection: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_descriptor: lobjc.id; _collection: CFStringRef);cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSFontManager_removeFontDescriptor_fromCollection_)), _descriptor, _collection);
end;

function NSFontManager.fontNamed_hasTraits_(_fName: CFStringRef; _someTraits: NSFontTraitMask): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fName: CFStringRef; _someTraits: NSFontTraitMask): LongBool;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_fontNamed_hasTraits_)), _fName, _someTraits));
end;

function NSFontManager.availableFontNamesWithTraits_(_someTraits: NSFontTraitMask): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_someTraits: NSFontTraitMask): CFArrayRef;cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(Handle, sel_registerName(PChar(StrNSFontManager_availableFontNamesWithTraits_)), _someTraits));
end;

procedure NSFontManager.addFontTrait_(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id);cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSFontManager_addFontTrait_)), _sender);
end;

procedure NSFontManager.removeFontTrait_(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id);cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSFontManager_removeFontTrait_)), _sender);
end;

procedure NSFontManager.modifyFontViaPanel_(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id);cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSFontManager_modifyFontViaPanel_)), _sender);
end;

procedure NSFontManager.modifyFont_(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id);cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSFontManager_modifyFont_)), _sender);
end;

procedure NSFontManager.orderFrontFontPanel_(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id);cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSFontManager_orderFrontFontPanel_)), _sender);
end;

procedure NSFontManager.orderFrontStylesPanel_(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id);cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSFontManager_orderFrontStylesPanel_)), _sender);
end;

{$endif}
