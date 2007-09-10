{
     File:       CommonPanels/FontPanel.h
 
     Contains:   Carbon Font Panel package Interfaces.
 
     Version:    CommonPanels-73~983
 
     Copyright:  © 2002-2005 by Apple Computer, Inc., all rights reserved
 
}

{	 Pascal Translation:  Gale R Paeper, <gpaeper@empirenet.com>, 2006 }

{
    Modified for use with Free Pascal
    Version 200
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$CALLING MWPASCAL}

unit FontPanel;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0200}

{$ifc not defined USE_CFSTR_CONSTANT_MACROS}
    {$setc USE_CFSTR_CONSTANT_MACROS := TRUE}
{$endc}

{$ifc defined CPUPOWERPC and defined CPUI386}
	{$error Conflicting initial definitions for CPUPOWERPC and CPUI386}
{$endc}
{$ifc defined FPC_BIG_ENDIAN and defined FPC_LITTLE_ENDIAN}
	{$error Conflicting initial definitions for FPC_BIG_ENDIAN and FPC_LITTLE_ENDIAN}
{$endc}

{$ifc not defined __ppc__ and defined CPUPOWERPC}
	{$setc __ppc__ := 1}
{$elsec}
	{$setc __ppc__ := 0}
{$endc}
{$ifc not defined __i386__ and defined CPUI386}
	{$setc __i386__ := 1}
{$elsec}
	{$setc __i386__ := 0}
{$endc}

{$ifc defined __ppc__ and __ppc__ and defined __i386__ and __i386__}
	{$error Conflicting definitions for __ppc__ and __i386__}
{$endc}

{$ifc defined __ppc__ and __ppc__}
	{$setc TARGET_CPU_PPC := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
{$elsec}
	{$error Neither __ppc__ nor __i386__ is defined.}
{$endc}
{$setc TARGET_CPU_PPC_64 := FALSE}

{$ifc defined FPC_BIG_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := TRUE}
	{$setc TARGET_RT_LITTLE_ENDIAN := FALSE}
{$elifc defined FPC_LITTLE_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := FALSE}
	{$setc TARGET_RT_LITTLE_ENDIAN := TRUE}
{$elsec}
	{$error Neither FPC_BIG_ENDIAN nor FPC_LITTLE_ENDIAN are defined.}
{$endc}
{$setc ACCESSOR_CALLS_ARE_FUNCTIONS := TRUE}
{$setc CALL_NOT_IN_CARBON := FALSE}
{$setc OLDROUTINENAMES := FALSE}
{$setc OPAQUE_TOOLBOX_STRUCTS := TRUE}
{$setc OPAQUE_UPP_TYPES := TRUE}
{$setc OTCARBONAPPLICATION := TRUE}
{$setc OTKERNEL := FALSE}
{$setc PM_USE_SESSION_APIS := TRUE}
{$setc TARGET_API_MAC_CARBON := TRUE}
{$setc TARGET_API_MAC_OS8 := FALSE}
{$setc TARGET_API_MAC_OSX := TRUE}
{$setc TARGET_CARBON := TRUE}
{$setc TARGET_CPU_68K := FALSE}
{$setc TARGET_CPU_MIPS := FALSE}
{$setc TARGET_CPU_SPARC := FALSE}
{$setc TARGET_OS_MAC := TRUE}
{$setc TARGET_OS_UNIX := FALSE}
{$setc TARGET_OS_WIN32 := FALSE}
{$setc TARGET_RT_MAC_68881 := FALSE}
{$setc TARGET_RT_MAC_CFM := FALSE}
{$setc TARGET_RT_MAC_MACHO := TRUE}
{$setc TYPED_FUNCTION_POINTERS := TRUE}
{$setc TYPE_BOOL := FALSE}
{$setc TYPE_EXTENDED := FALSE}
{$setc TYPE_LONGLONG := TRUE}
uses MacTypes, AEDataModel, AERegistry, ATSTypes, CarbonEventsCore, CFArray, CFBase, CFDictionary, Quickdraw;

{$ALIGN MAC68K}

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Font Panel-Related Events
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}

{
 *  Discussion:
 *    Event classes
 }
const
{
   * Events related to font selection or handling.
   }
	kEventClassFont = $666F6E74 (* 'font' *);


{
 *  Summary:
 *    Common command IDs
 }
const
{
   * The state of the Font Panel should be toggled, displaying it or
   * hiding it as necessary. If the user closes the Font Panel directly
   * from the window, the application will receive a
   * kEventFontPanelClosed event.
   }
	kHICommandShowHideFontPanel = $73686670 (* 'shfp' *);

{ Font Events }

{
 *  Summary:
 *    Font events (kEventClassFont)
 *  
 *  Discussion:
 *    When the user closes the Font Panel, a kEventWindowClosed event
 *    will be detected by the Carbon event handler installed by the
 *    system. The system then notifies the application that the Font
 *    Panel has closed by posting a Carbon Event Manager event. This
 *    allows the application to update any menu items or other controls
 *    whose state may have to change because the Font Panel has closed.
 *    kEventWindowClosed has no parameters. When the user selects an
 *    item in the Font Panel, the system will send a
 *    kEventFontSelection event to the event target specified when the
 *    application called SetFontPanelInfo(). kEventFontSelection will
 *    contain parameters reflecting the current Font Panel selection in
 *    all supported formats. Font events are available after Mac OS X
 *    10.2 in the Carbon framework.
 }
const
{
   * The Font Panel has been closed. The application should update its
   * corresponding UI element (e.g., a menu item) accordingly.
   }
	kEventFontPanelClosed = 1;

  {
   * The user has specified font settings in the Font Panel. The
   * application can obtain these settings from the event, in which
   * they are stored as parameters. Not all parameters are guaranteed
   * to be present; the application should check for all those which it
   * recognizes and apply the ones found as appropriate to the target
   * text.
   }
	kEventFontSelection = 2;

{
    Parameters for font events:

    kEventFontPanelClosed
        None.
        
    kEventFontSelection
        -->     kEventParamATSUFontID               typeATSUFontID
        -->     kEventParamATSUFontSize             typeATSUSize
        -->     kEventParamFMFontFamily             typeFMFontFamily
        -->     kEventParamFMFontSize               typeFMFontSize
        -->     kEventParamFontColor                typeFontColor
        -->     kEventParamDictionary               typeCFDictionary 
}
const
	typeATSUFontID = typeUInt32; { ATSUI font ID.}
	typeATSUSize = typeFixed; { ATSUI font size.}
	typeFMFontFamily = typeSInt16; { Font family reference.}
	typeFMFontStyle = typeSInt16; { Quickdraw font style}
	typeFMFontSize = typeSInt16; { Integer font size.}
	typeFontColor = typeRGBColor; { Font color spec (optional).}
	kEventParamATSUFontID = $61756964 (* 'auid' *); { typeATSUFontID}
	kEventParamATSUFontSize = $6175737A (* 'ausz' *); { typeATSUSize}
	kEventParamFMFontFamily = $666D666D (* 'fmfm' *); { typeFMFontFamily}
	kEventParamFMFontStyle = $666D7374 (* 'fmst' *); { typeFMFontStyle}
	kEventParamFMFontSize = $666D737A (* 'fmsz' *); { typeFMFontSize}
	kEventParamFontColor = $66636C72 (* 'fclr' *); { typeFontColor}
	kEventParamDictionary = $64696374 (* 'dict' *); {    typeCFDictionaryRef}

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Key constants to be used to access data inside the dictionary that may
    be contained in the kEventFontSelection dictionary.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
{
 *  kFontPanelATSUFontIDKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kFontPanelATSUFontIDKey: CFStringRef; external name '_kFontPanelATSUFontIDKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{Value is a CFNumber containing the ATSU Font ID}
{
 *  kFontPanelVariationAxesKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kFontPanelVariationAxesKey: CFStringRef; external name '_kFontPanelVariationAxesKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{ Value is a CFDataRef containing one or more ATSUI Variation Axes}
{
 *  kFontPanelVariationValuesKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kFontPanelVariationValuesKey: CFStringRef; external name '_kFontPanelVariationValuesKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{Value is a CFDataRef containing one or more ATSU Variation values}
{
 *  kFontPanelFeatureTypesKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kFontPanelFeatureTypesKey: CFStringRef; external name '_kFontPanelFeatureTypesKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{  Value is a CFDataRef containing one or more ATSUI feature types}
{
 *  kFontPanelFeatureSelectorsKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kFontPanelFeatureSelectorsKey: CFStringRef; external name '_kFontPanelFeatureSelectorsKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{  Value is a CFDataRef containing one or more ATSUI feature selectors}
{
 *  kFontPanelAttributesKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kFontPanelAttributesKey: CFStringRef; external name '_kFontPanelAttributesKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{
  const <CFString> string kFontPanelAttributesKey           =   "FontAttributes";
    Value is a CFDictionaryRef containing three keyed values.  Each value is
    a CFDataRef.  One CFDataRef contains one or more ATSUAttributeTags.
    One CFDataRef contains one or more value sizes for each tag.  And the last
    CFDataRef contains the actual values.  It is important to understand that
    these are the actual values and not value ptrs.  To pass these values to
    ATSUI they must be converted into ptrs.  The following code fragment demonstrates
    one technique
    CFDataRef       values;
    CFDataRef       tags;
    CFDataRef       sizes;
    if (    CFDictionaryGetValueIfPresent( attributesDict, kFontPanelAttributeValuesKey, &values ) &&
            CFDictionaryGetValueIfPresent( attributesDict, kFontPanelAttributeTagsKey, &tags )
            CFDictionaryGetValueIfPresent( attributesDict, kFontPanelAttributeSizesKey, &sizes ))
    (
        ItemCount               count = CFDataGetLength( tags )/sizeof(ATSUAttributeTag);
        CFIndex                 index;
        ATSUAttributeValuePtr   valuePtrs = malloc( count * sizeof(ATSUAttributeValuePtr) );
        UInt32*                 sizePtr = (UInt32*)CFDataGetBytePtr(sizes);
        UInt32*                 bytePtr = (UInt32*)CFDataGetBytePtr(values);
        for ( index = 0; index < count; index++ )
        (
            valuePtrs[index] = bytePtr;
            bytePtr = (UInt32*)( (UInt8*)bytePtr + sizePtr[index]);
        )
        verify_noerr( ATSUSetAttributes( someATSUStyle, count, (ATSUAttributeTag*)CFDataGetBytePtr(tags),sizePtr, valuePtrs ) );
        free( valuePtrs );
}
{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Keys to access the CFDataRefs inside the attributes dictionary (see above)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
{
 *  kFontPanelAttributeTagsKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kFontPanelAttributeTagsKey: CFStringRef; external name '_kFontPanelAttributeTagsKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{Value is a CFDataRef containing one or more style attribute tags}
{
 *  kFontPanelAttributeSizesKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kFontPanelAttributeSizesKey: CFStringRef; external name '_kFontPanelAttributeSizesKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{Value is a CFDataRef containing one or more style attribute sizes}
{
 *  kFontPanelAttributeValuesKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kFontPanelAttributeValuesKey: CFStringRef; external name '_kFontPanelAttributeValuesKey'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{Value is a CFDataRef containing one or more style values}
{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Other Font Panel Constants
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
{
Error codes (Font Panel codes in range [-8880,-8899]).
}
const
	fontPanelShowErr = -8880; { Can't display the Font Panel.}
	fontPanelSelectionStyleErr = -8881; { Bad font selection style info.}
	fontPanelFontSelectionQDStyleVersionErr = -8882; { Unsupported record version.}

{
Type of font information passed in SetFontPanelInfo(). If the client is
sending ATSUI style data, it specifies kFontSelectionATSUIType; if it is
sending Quickdraw style data, it specifies kFontSelectionQDType.
}
const
	kFontSelectionATSUIType = $6173746C (* 'astl' *); { Use ATSUIStyle collection.}
	kFontSelectionQDType = $7173746C (* 'qstl' *); { Use FontSelectionQDStyle record.}

{
Supported versions of the FontSelectionQDStyle record. Clients should always set
the <version> field to one of these values.
}
const
	kFontSelectionQDStyleVersionZero = 0;


{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Font Panel Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
{
Record specifying the font information to be specified in the Font
Panel. This record is used if the client is sending Quickdraw style data
(i.e., it specified kFontSelectionQDType in SetFontPanelInfo()).
}
type
	FontSelectionQDStyle = record
		version: UInt32;                { Version number of struct.}
		instance: FMFontFamilyInstance;             { Font instance data.}
		size: FMFontSize;                   { Size of font in points.}
		hasColor: Boolean;               { true if color info supplied.}
		reserved: SInt8;               { Filler byte.}
		color: RGBColor;                  { Color specification for font.}
	end;
	FontSelectionQDStylePtr = ^FontSelectionQDStyle;
{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Font Panel Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
{
 *  FPIsFontPanelVisible()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FPIsFontPanelVisible: Boolean; external name '_FPIsFontPanelVisible';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  FPShowHideFontPanel()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FPShowHideFontPanel: OSStatus; external name '_FPShowHideFontPanel';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  SetFontInfoForSelection()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function SetFontInfoForSelection( iStyleType: OSType; iNumStyles: UInt32; iStyles: {variable-size-array} UnivPtr; iFPEventTarget: EventTargetRef ): OSStatus; external name '_SetFontInfoForSelection';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Font Collection Functions

    In 10.3 the ability to create and modify font collections is available.  Font
    collections are files containing font descriptions.  Font descriptions are
    encapsulated in the opaque object FCFontDescriptorRef. A FCFontDescriptroRef
    is a CFType.  To release one call CFRelease.
    
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Font Collection Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
type
	FCFontDescriptorRef = ^SInt32; { an opaque 32-bit type }
{
 *  FCCopyCollectionNames()
 *  
 *  Discussion:
 *    FCCopyCollectionNames returns a copy of the CFArrayRef containing
 *    the displayable names of every font collection available to the
 *    current user.
 *  
 *  Result:
 *    A CFArrayRef containing CFStringRefs where each CFStringRef
 *    contains a font collection's displayable name. Callers are
 *    responsible for releasing the returned CFArrayRef.  If the
 *    operation is not successful NULL is returned.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function FCCopyCollectionNames: CFArrayRef; external name '_FCCopyCollectionNames';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  FCCopyFontDescriptorsInCollection()
 *  
 *  Discussion:
 *    FCCopyFontDescriptorsInCollection copies the fontDescriptors in a
 *    named collection into an array.
 *  
 *  Parameters:
 *    
 *    iCollection:
 *      The name of a collection that descriptors should be copied from.
 *  
 *  Result:
 *    A CFArrayRef containing copies of the FCFontDescriptorRefs
 *    contained in the name collection.  Callers are responsible for
 *    releasing the returned CFArrayRef.  The FCFontDescriptorRefs are
 *    retained when added to the array and released when the array is
 *    destroyed.  You can access a font descriptor in the array in the
 *    following manner: fd =
 *    (FCFontDescriptorRef)CFArrayGetValueAtIndex(array, i);
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function FCCopyFontDescriptorsInCollection( iCollection: CFStringRef ): CFArrayRef; external name '_FCCopyFontDescriptorsInCollection';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  FCAddCollection()
 *  
 *  Discussion:
 *    Add a collection to the font descriptor collections available to
 *    the current user. If the collection is successfully added noErr
 *    is returned.  If the collection is not added an error code is
 *    returned.
 *  
 *  Parameters:
 *    
 *    iCollection:
 *      the name of the collection to add.
 *    
 *    iCollectionOptions:
 *      currently there are no options.  Set to kNilOptions.  This
 *      parameter is available for future expansion.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function FCAddCollection( iCollection: CFStringRef; iCollectionOptions: OptionBits ): OSStatus; external name '_FCAddCollection';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  FCRemoveCollection()
 *  
 *  Discussion:
 *    Remove a named collection from the font descriptor collections
 *    available to the current user. Returns noErr if the collection
 *    was successfully removed.  An appropriate error code is returned
 *    if the operation was not successful.
 *  
 *  Parameters:
 *    
 *    iCollection:
 *      the name of the collection to remove.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function FCRemoveCollection( iCollection: CFStringRef ): OSStatus; external name '_FCRemoveCollection';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  FCAddFontDescriptorToCollection()
 *  
 *  Discussion:
 *    Add a font descriptor to the named collection.  noErr is returned
 *    if the font descriptor is added. An error code describing the
 *    failure is returned if the descriptor is not added.
 *  
 *  Parameters:
 *    
 *    iDescriptor:
 *      the font descriptor that should be added.  The
 *      FCFontDescriptorRef is retained when it is added to the
 *      collection.  After calling this function the caller may release
 *      their copy.
 *    
 *    iCollection:
 *      the name of the collection to which the font descriptor should
 *      be added.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function FCAddFontDescriptorToCollection( iDescriptor: FCFontDescriptorRef; iCollection: CFStringRef ): OSStatus; external name '_FCAddFontDescriptorToCollection';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  FCRemoveFontDescriptorFromCollection()
 *  
 *  Discussion:
 *    Remove a font descriptor from the named collection.  An error is
 *    returned if the font descriptor can not be removed.  noErr is
 *    returned if the descriptor is removed.
 *  
 *  Parameters:
 *    
 *    iDescriptor:
 *      the descriptor that should be removed.
 *    
 *    iCollection:
 *      the name of the collection that the descriptor should be
 *      removed from.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function FCRemoveFontDescriptorFromCollection( iDescriptor: FCFontDescriptorRef; iCollection: CFStringRef ): OSStatus; external name '_FCRemoveFontDescriptorFromCollection';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Font Descriptor Attribute Keys
    
    Font Descriptors contain font attributes that are set and accessed via a set of 
    keys.  The keys are all constant CFStringRefs.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}

{
 *  kFCFontFamilyAttribute
 *  
 *  Discussion:
 *    The key for a CFStringRef that contains a font family name (e.g.
 *    Baskerville).
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kFCFontFamilyAttribute: CFStringRef; external name '_kFCFontFamilyAttribute'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{
 *  kFCFontNameAttribute
 *  
 *  Discussion:
 *    The key for a CFStringRef containing a font name (e.g.
 *    Baskerville-Italic).
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kFCFontNameAttribute: CFStringRef; external name '_kFCFontNameAttribute'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{
 *  kFCFontFaceAttribute
 *  
 *  Discussion:
 *    The key for a CFStringRef containing a face name (e.g. Italic).
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kFCFontFaceAttribute: CFStringRef; external name '_kFCFontFaceAttribute'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{
 *  kFCFontSizeAttribute
 *  
 *  Discussion:
 *    The key for a CFNumber containg the font size (e.g. 12).
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kFCFontSizeAttribute: CFStringRef; external name '_kFCFontSizeAttribute'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{
 *  kFCFontVisibleNameAttribute
 *  
 *  Discussion:
 *    The Key for a CFStringRef containing the name that should be used
 *    in a UI to describe the font.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kFCFontVisibleNameAttribute: CFStringRef; external name '_kFCFontVisibleNameAttribute'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{
 *  kFCFontCGColorAttribute
 *  
 *  Discussion:
 *    The Key for a CGColorRef containing the fonts color.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kFCFontCGColorAttribute: CFStringRef; external name '_kFCFontCGColorAttribute'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{
 *  FCFontDescriptorCreateWithFontAttributes()
 *  
 *  Discussion:
 *    Create a font descriptor using the attributes contained in the
 *    dictionary.
 *  
 *  Parameters:
 *    
 *    iAttributes:
 *      a dictionary containing one or more of the attributes described
 *      above.
 *  
 *  Result:
 *    A valid FCFontDescriptorRef.  NULL if one cannot be created.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function FCFontDescriptorCreateWithFontAttributes( iAttributes: CFDictionaryRef ): FCFontDescriptorRef; external name '_FCFontDescriptorCreateWithFontAttributes';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  FCFontDescriptorCreateWithName()
 *  
 *  Discussion:
 *    Create a font descriptor using a fontname and font size.
 *  
 *  Parameters:
 *    
 *    iFontName:
 *      The name of the font (e.g. Baskerville-Italic).
 *    
 *    iSize:
 *      the size of the font. (e.g. 12.0).
 *  
 *  Result:
 *    A valid FCFontDescriptorRef.  NULL if one cannot be created.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function FCFontDescriptorCreateWithName( iFontName: CFStringRef; iSize: Float32 ): FCFontDescriptorRef; external name '_FCFontDescriptorCreateWithName';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)




end.
