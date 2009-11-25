{%mainunit foundation.pas}
(*	NSString.h
	Copyright (c) 1994-2005, Apple, Inc. All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSSTRING_PAS_H}
{$define NSSTRING_PAS_H}

{$include NSObject.inc}
{$include NSRange.inc}
  {.$ifndef  !defined(_OBJC_UNICHAR_H_)}
  {.$endif}
const
  NSMaximumStringLength = (High(cint)-1);
  {$define _OBJC_UNICHAR_H_}
  NS_UNICHAR_IS_EIGHT_BIT = 0;
  {.$ifndef  !defined(_OBJC_UNICHAR_H_)}
  {.$endif}

type
  unichar = Word;

//FOUNDATION_EXPORT NSString * const NSParseErrorException; // raised by -propertyList
  //  These options apply to the various search/find and comparison methods (except where
  // noted).
const
  NSCaseInsensitiveSearch = 1;
  NSLiteralSearch = 2;
  NSBackwardsSearch = 4;
  NSAnchoredSearch = 8;
  NSNumericSearch = 64;

type
  NSStringEncoding = LongWord;

const
  NSASCIIStringEncoding = 1;
  NSNEXTSTEPStringEncoding = 2;
  NSJapaneseEUCStringEncoding = 3;
  NSUTF8StringEncoding = 4;
  NSISOLatin1StringEncoding = 5;
  NSSymbolStringEncoding = 6;
  NSNonLossyASCIIStringEncoding = 7;
  NSShiftJISStringEncoding = 8;
  NSISOLatin2StringEncoding = 9;
  NSUnicodeStringEncoding = 10;
  NSWindowsCP1251StringEncoding = 11;
  NSWindowsCP1252StringEncoding = 12;
  NSWindowsCP1253StringEncoding = 13;
  NSWindowsCP1254StringEncoding = 14;
  NSWindowsCP1250StringEncoding = 15;
  NSISO2022JPStringEncoding = 21;
  NSMacOSRomanStringEncoding = 30;
  NSProprietaryStringEncoding = 65536;

//FOUNDATION_EXPORT NSString * const NSCharacterConversionException;
//extern void *_NSConstantStringClassReference;
{$endif}
{$endif}

{$ifdef FORWARD}
  NSString = class;
  NSMutableString = class;
  NSSimpleCString = class;
  NSConstantString = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSTRING_PAS_C}
{$define NSSTRING_PAS_C}

{$include NSObject.inc}
{$include NSRange.inc}

  { NSString }

  NSString = class(NSObject)
  public
    class function getClass: lobjc.id; override;
    //  NSString primitive (funnel) methods. See below for the other methods.
    function length: LongWord;
    function characterAtIndex(__index: LongWord): unichar;
    procedure getCharacters(_buffer: unichar);
    procedure getCharacters_range(_buffer: unichar; _aRange: NSRange);
    function substringFromIndex(_from: LongWord): CFStringRef;
    function substringToIndex(__to: LongWord): CFStringRef;
    function substringWithRange(_range: NSRange): CFStringRef;
    function compare(__string: CFStringRef): NSComparisonResult;
    function compare_options(__string: CFStringRef; _mask: LongWord): NSComparisonResult;
    function compare_options_range(__string: CFStringRef; _mask: LongWord; _compareRange: NSRange): NSComparisonResult;
    function compare_options_range_locale(__string: CFStringRef; _mask: LongWord; _compareRange: NSRange; _dict: CFDictionaryRef): NSComparisonResult;
    function caseInsensitiveCompare(__string: CFStringRef): NSComparisonResult;
    function localizedCompare(__string: CFStringRef): NSComparisonResult;
    function localizedCaseInsensitiveCompare(__string: CFStringRef): NSComparisonResult;
    function isEqualToString(_aString: CFStringRef): LongBool;
    function hasPrefix(_aString: CFStringRef): LongBool;
    function hasSuffix(_aString: CFStringRef): LongBool;
    //  These methods return length==0 if the target string is not found. So, to check for
    // containment: ([str rangeOfString:@"target"].length > 0).  Note that the length of
    // the range returned by these methods might be different than the length of the target
    // string, due composed characters and such.
    function rangeOfString(_aString: CFStringRef): NSRange;
    function rangeOfString_options(_aString: CFStringRef; _mask: LongWord): NSRange;
    function rangeOfString_options_range(_aString: CFStringRef; _mask: LongWord; _searchRange: NSRange): NSRange;
    //  These return the range of the first character from the set in the string, not the
    // range of a sequence of characters.
    function rangeOfCharacterFromSet(_aSet: CFCharacterSetRef): NSRange;
    function rangeOfCharacterFromSet_options(_aSet: CFCharacterSetRef; _mask: LongWord): NSRange;
    function rangeOfCharacterFromSet_options_range(_aSet: CFCharacterSetRef; _mask: LongWord; _searchRange: NSRange): NSRange;
    function rangeOfComposedCharacterSequenceAtIndex(__index: LongWord): NSRange;
    function stringByAppendingString(_aString: CFStringRef): CFStringRef;
    function stringByAppendingFormat(_format: CFStringRef): CFStringRef;
    function doubleValue: double;
    function floatValue: Single;
    function intValue: Integer;
    function componentsSeparatedByString(_separator: CFStringRef): CFArrayRef;
    function commonPrefixWithString_options(_aString: CFStringRef; _mask: LongWord): CFStringRef;
    function uppercaseString: CFStringRef;
    function lowercaseString: CFStringRef;
    function capitalizedString: CFStringRef;
    {.$ifdef  MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED}
    function stringByTrimmingCharactersInSet(__set: CFCharacterSetRef): CFStringRef;
    function stringByPaddingToLength_withString_startingAtIndex(_newLength: LongWord; _padString: CFStringRef; _padIndex: LongWord): CFStringRef;
    {.$endif}
    procedure getLineStart_end_contentsEnd_forRange(_startPtr: PLongWord; _lineEndPtr: PLongWord; _contentsEndPtr: PLongWord; _range: NSRange);
    function lineRangeForRange(_range: NSRange): NSRange;
    {.$ifdef  MAC_OS_X_VERSION_10_3 <= MAC_OS_X_VERSION_MAX_ALLOWED}
    procedure getParagraphStart_end_contentsEnd_forRange(_startPtr: PLongWord; _parEndPtr: PLongWord; _contentsEndPtr: PLongWord; _range: NSRange);
    function paragraphRangeForRange(_range: NSRange): NSRange;
    {.$endif}
    function description: CFStringRef;
    function hash: LongWord;
    // ** Encoding methods **
    function fastestEncoding: NSStringEncoding;
    //  Result in O(1) time; a rough estimate
    function smallestEncoding: NSStringEncoding;
    //  Result in O(n) time; the encoding in which the string is most compact
    function dataUsingEncoding_allowLossyConversion(_encoding: NSStringEncoding; _lossy: LongBool): CFDataRef;
    function dataUsingEncoding(_encoding: NSStringEncoding): CFDataRef;
    function canBeConvertedToEncoding(_encoding: NSStringEncoding): LongBool;
    {.$ifdef  MAC_OS_X_VERSION_10_4 <= MAC_OS_X_VERSION_MAX_ALLOWED}
    //  Methods to convert NSString to a NULL-terminated cString using the specified encoding.
    // Note, these are the "new" cString methods, and are not deprecated like the older
    // cString methods which do not take encoding arguments.
    function cStringUsingEncoding(_encoding: NSStringEncoding): PChar;
    //  "Autoreleased"; NULL return if encoding conversion not possible; for performance
    // reasons, lifetime of this should not be considered longer than the lifetime of the
    // receiving string (if the receiver string is freed, this might go invalid then, before
    // the end of the autorelease scope)
    function getCString_maxLength_encoding(_buffer: PChar; _maxBufferCount: LongWord; _encoding: NSStringEncoding): LongBool;
    //  NO return if conversion not possible due to encoding errors or too small of a buffer.
    // The buffer should include room for maxBufferCount bytes plus the NULL termination
    // character, which this method adds. (So pass in one less than the size of the buffer.)
    //  These return the maximum and exact number of bytes needed to store the receiver
    // in the specified encoding in non-external representation. The first one is O(1),
    // while the second one is O(n). These do not include space for a terminating null.
    function maximumLengthOfBytesUsingEncoding(_enc: NSStringEncoding): LongWord;
    //  Result in O(1) time; the estimate may be way over what's needed
    function lengthOfBytesUsingEncoding(_enc: NSStringEncoding): LongWord;
    {.$endif}
    {.$ifdef  MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED}
    function decomposedStringWithCanonicalMapping: CFStringRef;
    function precomposedStringWithCanonicalMapping: CFStringRef;
    function decomposedStringWithCompatibilityMapping: CFStringRef;
    function precomposedStringWithCompatibilityMapping: CFStringRef;
    {.$endif}
    function UTF8String: PChar;
    //  Convenience to return null-terminated UTF8 representation
    //  User-dependent encoding who value is derived from user's default language and potentially
    // other factors. The use of this encoding might sometimes be needed when interpreting
    // user documents with unknown encodings, in the absence of other hints.  This encoding
    // should be used rarely, if at all. Note that some potential values here might result
    // in unexpected encoding conversions of even fairly straightforward NSString content
    // --- for instance, punctuation characters with a bidirectional encoding.
    class function defaultCStringEncoding: NSStringEncoding;
    //  Should be rarely used
    class function availableStringEncodings: NSStringEncoding;
    constructor localizedNameOfStringEncoding(_encoding: NSStringEncoding);
    // ** Creation methods **
    //  In general creation methods in NSString do not apply to subclassers, as subclassers
    // are assumed to provide their own init methods which create the string in the way
    // the subclass wishes.  Designated initializers of NSString are thus init and initWithCoder:.
    constructor init;
    constructor initWithCharactersNoCopy_length_freeWhenDone(_characters: unichar; _length: LongWord; _freeBuffer: LongBool);
    //  "NoCopy" is a hint
    constructor initWithCharacters_length(_characters: unichar; _length: LongWord);
    constructor initWithUTF8String(_nullTerminatedCString: PChar);
    constructor initWithString(_aString: CFStringRef);
    constructor initWithFormat(_format: CFStringRef);
//    constructor initWithFormat_arguments(_format: CFStringRef; const _argList: array of const);
    constructor initWithFormat_locale(_format: CFStringRef; _dict: CFDictionaryRef);
//    constructor initWithFormat_locale_arguments(_format: CFStringRef; _dict: CFDictionaryRef; const _argList: array of const);
    constructor initWithData_encoding(_data: CFDataRef; _encoding: NSStringEncoding);
    constructor initWithBytes_length_encoding(_bytes: Pointer; _len: LongWord; _encoding: NSStringEncoding);
    {.$ifdef  MAC_OS_X_VERSION_10_3 <= MAC_OS_X_VERSION_MAX_ALLOWED}
    constructor initWithBytesNoCopy_length_encoding_freeWhenDone(_bytes: Pointer; _len: LongWord; _encoding: NSStringEncoding; _freeBuffer: LongBool);
    {.$endif}
    constructor string_;
    constructor stringWithString(__string: CFStringRef);
    constructor stringWithCharacters_length(_characters: unichar; _length: LongWord);
    constructor stringWithUTF8String(_nullTerminatedCString: PChar);
    constructor stringWithFormat(_format: CFStringRef);
    constructor localizedStringWithFormat(_format: CFStringRef);
    {.$ifdef  MAC_OS_X_VERSION_10_4 <= MAC_OS_X_VERSION_MAX_ALLOWED}
    constructor initWithCString_encoding(_nullTerminatedCString: PChar; _encoding: NSStringEncoding);
    constructor stringWithCString_encoding(_cString: PChar; _enc: NSStringEncoding);
    //  These use the specified encoding.  If nil is returned, the optional error return
    // indicates problem that was encountered (for instance, file system or encoding errors).
    constructor initWithContentsOfURL_encoding_error(_url: CFURLRef; _enc: NSStringEncoding; _error: lobjc.id {NSError});
    constructor initWithContentsOfFile_encoding_error(_path: CFStringRef; _enc: NSStringEncoding; _error: lobjc.id {NSError});
    constructor stringWithContentsOfURL_encoding_error(_url: CFURLRef; _enc: NSStringEncoding; _error: lobjc.id {NSError});
    constructor stringWithContentsOfFile_encoding_error(_path: CFStringRef; _enc: NSStringEncoding; _error: lobjc.id {NSError});
    //  These try to determine the encoding, and return the encoding which was used.  Note
    // that these methods might get "smarter" in subsequent releases of the system, and
    // use additional techniques for recognizing encodings. If nil is returned, the optional
    // error return indicates problem that was encountered (for instance, file system or
    // encoding errors).
    constructor initWithContentsOfURL_usedEncoding_error(_url: CFURLRef; _enc: NSStringEncoding; _error: lobjc.id {NSError});
    constructor initWithContentsOfFile_usedEncoding_error(_path: CFStringRef; _enc: NSStringEncoding; _error: lobjc.id {NSError});
    constructor stringWithContentsOfURL_usedEncoding_error(_url: CFURLRef; _enc: NSStringEncoding; _error: lobjc.id {NSError});
    constructor stringWithContentsOfFile_usedEncoding_error(_path: CFStringRef; _enc: NSStringEncoding; _error: lobjc.id {NSError});
    //  Write to specified url or path using the specified encoding.  The optional error
    // return is to indicate file system or encoding errors.
    function writeToURL_atomically_encoding_error(_url: CFURLRef; _useAuxiliaryFile: LongBool; _enc: NSStringEncoding; _error: lobjc.id {NSError}): LongBool;
    function writeToFile_atomically_encoding_error(_path: CFStringRef; _useAuxiliaryFile: LongBool; _enc: NSStringEncoding; _error: lobjc.id {NSError}): LongBool;
    {.$endif}
    function propertyList: lobjc.id;
    function propertyListFromStringsFileFormat: CFDictionaryRef;
    //  The methods in this category are deprecated and will be removed from this header
    // file in the near future. These methods use [NSString defaultCStringEncoding] as the
    // encoding to convert to, which means the results depend on the user's language and
    // potentially other settings. This might be appropriate in some cases, but often these
    // methods are misused, resulting in issues when running in languages other then English.
    // UTF8String in general is a much better choice when converting arbitrary NSStrings
    // into 8-bit representations. Additional potential replacement methods are being introduced
    // in NSString as appropriate.
    function cString: PChar;
    function lossyCString: PChar;
    function cStringLength: LongWord;
    procedure getCString(_bytes: PChar);
    procedure getCString_maxLength(_bytes: PChar; _maxLength: LongWord);
    procedure getCString_maxLength_range_remainingRange(_bytes: PChar; _maxLength: LongWord; _aRange: NSRange; _leftoverRange: NSRangePointer);
    function writeToFile_atomically(_path: CFStringRef; _useAuxiliaryFile: LongBool): LongBool;
    function writeToURL_atomically(_url: CFURLRef; _atomically: LongBool): LongBool;
    constructor initWithContentsOfFile(_path: CFStringRef);
    constructor initWithContentsOfURL(_url: CFURLRef);
    constructor stringWithContentsOfFile(_path: CFStringRef);
    constructor stringWithContentsOfURL(_url: CFURLRef);
    constructor initWithCStringNoCopy_length_freeWhenDone(_bytes: PChar; _length: LongWord; _freeBuffer: LongBool);
    constructor initWithCString_length(_bytes: PChar; _length: LongWord);
    constructor initWithCString(_bytes: PChar);
    constructor stringWithCString_length(_bytes: PChar; _length: LongWord);
    constructor stringWithCString(_bytes: PChar);
  end;


  { NSMutableString }

  NSMutableString = class(NSString)
  public
    class function getClass: lobjc.id; override;
    //  NSMutableString primitive (funnel) method. See below for the other mutation methods.
    procedure replaceCharactersInRange_withString(_range: NSRange; _aString: CFStringRef);
    procedure insertString_atIndex(_aString: CFStringRef; _loc: LongWord);
    procedure deleteCharactersInRange(_range: NSRange);
    procedure appendString(_aString: CFStringRef);
    procedure appendFormat(_format: CFStringRef);
    procedure setString(_aString: CFStringRef);
    //  In addition to these two, NSMutableString responds properly to all NSString creation
    // methods.
    constructor initWithCapacity(_capacity: LongWord);
    constructor stringWithCapacity(_capacity: LongWord);
    {.$ifdef  MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED}
    //  This method replaces all occurrences of the target string with the replacement string,
    // in the specified range of the receiver string, and returns the number of replacements.
    // NSBackwardsSearch means the search is done from the end of the range (the results
    // could be different); NSAnchoredSearch means only anchored (but potentially multiple)
    // instances will be replaced. NSLiteralSearch and NSCaseInsensitiveSearch also apply.
    // NSNumericSearch is ignored. Use NSMakeRange(0, [receiver length]) to process whole
    // string.
    function replaceOccurrencesOfString_withString_options_range(_target: CFStringRef; _replacement: CFStringRef; _opts: LongWord; _searchRange: NSRange): LongWord;
    {.$endif}
  end;

    {.$ifndef  !defined(_OBJC_UNICHAR_H_)}
    {.$endif}

  { NSSimpleCString }

  NSSimpleCString = class(NSString)
  public
    class function getClass: lobjc.id; override;
  end;


  { NSConstantString }

  NSConstantString = class(NSSimpleCString)
  public
    class function getClass: lobjc.id; override;
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSString_NSString = 'NSString';
  StrNSString_length = 'length';
  StrNSString_characterAtIndex = 'characterAtIndex:';

  StrNSString_getCharacters = 'getCharacters:';
  StrNSString_getCharacters_range = 'getCharacters:range:';
  StrNSString_substringFromIndex = 'substringFromIndex:';
  StrNSString_substringToIndex = 'substringToIndex:';
  StrNSString_substringWithRange = 'substringWithRange:';
  StrNSString_compare = 'compare:';
  StrNSString_compare_options = 'compare:options:';
  StrNSString_compare_options_range = 'compare:options:range:';
  StrNSString_compare_options_range_locale = 'compare:options:range:locale:';
  StrNSString_caseInsensitiveCompare = 'caseInsensitiveCompare:';
  StrNSString_localizedCompare = 'localizedCompare:';
  StrNSString_localizedCaseInsensitiveCompare = 'localizedCaseInsensitiveCompare:';
  StrNSString_isEqualToString = 'isEqualToString:';
  StrNSString_hasPrefix = 'hasPrefix:';
  StrNSString_hasSuffix = 'hasSuffix:';
  StrNSString_rangeOfString = 'rangeOfString:';
  StrNSString_rangeOfString_options = 'rangeOfString:options:';
  StrNSString_rangeOfString_options_range = 'rangeOfString:options:range:';
  StrNSString_rangeOfCharacterFromSet = 'rangeOfCharacterFromSet:';
  StrNSString_rangeOfCharacterFromSet_options = 'rangeOfCharacterFromSet:options:';
  StrNSString_rangeOfCharacterFromSet_options_range = 'rangeOfCharacterFromSet:options:range:';
  StrNSString_rangeOfComposedCharacterSequenceAtIndex = 'rangeOfComposedCharacterSequenceAtIndex:';
  StrNSString_stringByAppendingString = 'stringByAppendingString:';
  StrNSString_stringByAppendingFormat = 'stringByAppendingFormat:';
  StrNSString_doubleValue = 'doubleValue';
  StrNSString_floatValue = 'floatValue';
  StrNSString_intValue = 'intValue';
  StrNSString_componentsSeparatedByString = 'componentsSeparatedByString:';
  StrNSString_commonPrefixWithString_options = 'commonPrefixWithString:options:';
  StrNSString_uppercaseString = 'uppercaseString';
  StrNSString_lowercaseString = 'lowercaseString';
  StrNSString_capitalizedString = 'capitalizedString';
  StrNSString_stringByTrimmingCharactersInSet = 'stringByTrimmingCharactersInSet:';
  StrNSString_stringByPaddingToLength_withString_startingAtIndex = 'stringByPaddingToLength:withString:startingAtIndex:';
  StrNSString_getLineStart_end_contentsEnd_forRange = 'getLineStart:end:contentsEnd:forRange:';
  StrNSString_lineRangeForRange = 'lineRangeForRange:';
  StrNSString_getParagraphStart_end_contentsEnd_forRange = 'getParagraphStart:end:contentsEnd:forRange:';
  StrNSString_paragraphRangeForRange = 'paragraphRangeForRange:';
  StrNSString_description = 'description';
  StrNSString_hash = 'hash';
  StrNSString_fastestEncoding = 'fastestEncoding';
  StrNSString_smallestEncoding = 'smallestEncoding';
  StrNSString_dataUsingEncoding_allowLossyConversion = 'dataUsingEncoding:allowLossyConversion:';
  StrNSString_dataUsingEncoding = 'dataUsingEncoding:';
  StrNSString_canBeConvertedToEncoding = 'canBeConvertedToEncoding:';
  StrNSString_cStringUsingEncoding = 'cStringUsingEncoding:';
  StrNSString_getCString_maxLength_encoding = 'getCString:maxLength:encoding:';
  StrNSString_maximumLengthOfBytesUsingEncoding = 'maximumLengthOfBytesUsingEncoding:';
  StrNSString_lengthOfBytesUsingEncoding = 'lengthOfBytesUsingEncoding:';
  StrNSString_decomposedStringWithCanonicalMapping = 'decomposedStringWithCanonicalMapping';
  StrNSString_precomposedStringWithCanonicalMapping = 'precomposedStringWithCanonicalMapping';
  StrNSString_decomposedStringWithCompatibilityMapping = 'decomposedStringWithCompatibilityMapping';
  StrNSString_precomposedStringWithCompatibilityMapping = 'precomposedStringWithCompatibilityMapping';
  StrNSString_UTF8String = 'UTF8String';
  StrNSString_defaultCStringEncoding = 'defaultCStringEncoding';
  StrNSString_availableStringEncodings = 'availableStringEncodings';
  StrNSString_localizedNameOfStringEncoding = 'localizedNameOfStringEncoding:';
  StrNSString_init = 'init';
  StrNSString_initWithCharactersNoCopy_length_freeWhenDone = 'initWithCharactersNoCopy:length:freeWhenDone:';
  StrNSString_initWithCharacters_length = 'initWithCharacters:length:';
  StrNSString_initWithUTF8String = 'initWithUTF8String:';
  StrNSString_initWithString = 'initWithString:';
  StrNSString_initWithFormat = 'initWithFormat:';
  StrNSString_initWithFormat_arguments = 'initWithFormat:arguments:';
  StrNSString_initWithFormat_locale = 'initWithFormat:locale:';
  StrNSString_initWithFormat_locale_arguments = 'initWithFormat:locale:arguments:';
  StrNSString_initWithData_encoding = 'initWithData:encoding:';
  StrNSString_initWithBytes_length_encoding = 'initWithBytes:length:encoding:';
  StrNSString_initWithBytesNoCopy_length_encoding_freeWhenDone = 'initWithBytesNoCopy:length:encoding:freeWhenDone:';
  StrNSString_string = 'string';
  StrNSString_stringWithString = 'stringWithString:';
  StrNSString_stringWithCharacters_length = 'stringWithCharacters:length:';
  StrNSString_stringWithUTF8String = 'stringWithUTF8String:';
  StrNSString_stringWithFormat = 'stringWithFormat:';
  StrNSString_localizedStringWithFormat = 'localizedStringWithFormat:';
  StrNSString_initWithCString_encoding = 'initWithCString:encoding:';
  StrNSString_stringWithCString_encoding = 'stringWithCString:encoding:';
  StrNSString_initWithContentsOfURL_encoding_error = 'initWithContentsOfURL:encoding:error:';
  StrNSString_initWithContentsOfFile_encoding_error = 'initWithContentsOfFile:encoding:error:';
  StrNSString_stringWithContentsOfURL_encoding_error = 'stringWithContentsOfURL:encoding:error:';
  StrNSString_stringWithContentsOfFile_encoding_error = 'stringWithContentsOfFile:encoding:error:';
  StrNSString_initWithContentsOfURL_usedEncoding_error = 'initWithContentsOfURL:usedEncoding:error:';
  StrNSString_initWithContentsOfFile_usedEncoding_error = 'initWithContentsOfFile:usedEncoding:error:';
  StrNSString_stringWithContentsOfURL_usedEncoding_error = 'stringWithContentsOfURL:usedEncoding:error:';
  StrNSString_stringWithContentsOfFile_usedEncoding_error = 'stringWithContentsOfFile:usedEncoding:error:';
  StrNSString_writeToURL_atomically_encoding_error = 'writeToURL:atomically:encoding:error:';
  StrNSString_writeToFile_atomically_encoding_error = 'writeToFile:atomically:encoding:error:';

  StrNSMutableString_NSMutableString = 'NSMutableString';
  StrNSMutableString_replaceCharactersInRange_withString = 'replaceCharactersInRange:withString:';

  StrNSMutableString_insertString_atIndex = 'insertString:atIndex:';
  StrNSMutableString_deleteCharactersInRange = 'deleteCharactersInRange:';
  StrNSMutableString_appendString = 'appendString:';
  StrNSMutableString_appendFormat = 'appendFormat:';
  StrNSMutableString_setString = 'setString:';
  StrNSMutableString_initWithCapacity = 'initWithCapacity:';
  StrNSMutableString_stringWithCapacity = 'stringWithCapacity:';
  StrNSMutableString_replaceOccurrencesOfString_withString_options_range = 'replaceOccurrencesOfString:withString:options:range:';

  StrNSString_propertyList = 'propertyList';
  StrNSString_propertyListFromStringsFileFormat = 'propertyListFromStringsFileFormat';

  StrNSString_cString = 'cString';
  StrNSString_lossyCString = 'lossyCString';
  StrNSString_cStringLength = 'cStringLength';
  StrNSString_getCString = 'getCString:';
  StrNSString_getCString_maxLength = 'getCString:maxLength:';
  StrNSString_getCString_maxLength_range_remainingRange = 'getCString:maxLength:range:remainingRange:';
  StrNSString_writeToFile_atomically = 'writeToFile:atomically:';
  StrNSString_writeToURL_atomically = 'writeToURL:atomically:';
  StrNSString_initWithContentsOfFile = 'initWithContentsOfFile:';
  StrNSString_initWithContentsOfURL = 'initWithContentsOfURL:';
  StrNSString_stringWithContentsOfFile = 'stringWithContentsOfFile:';
  StrNSString_stringWithContentsOfURL = 'stringWithContentsOfURL:';
  StrNSString_initWithCStringNoCopy_length_freeWhenDone = 'initWithCStringNoCopy:length:freeWhenDone:';
  StrNSString_initWithCString_length = 'initWithCString:length:';
  StrNSString_initWithCString = 'initWithCString:';
  StrNSString_stringWithCString_length = 'stringWithCString:length:';
  StrNSString_stringWithCString = 'stringWithCString:';

  StrNSSimpleCString_NSSimpleCString = 'NSSimpleCString';

  StrNSConstantString_NSConstantString = 'NSConstantString';

{ NSString }

class function NSString.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSString_NSString);
end;

function NSString.length: LongWord;
begin
  Result := LongWord(objc_msgSend(Handle, sel_registerName(PChar(StrNSString_length)), []));
end;

function NSString.characterAtIndex(__index: LongWord): unichar;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__index: LongWord): unichar; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := unichar(vmethod(Handle, sel_registerName(PChar(StrNSString_characterAtIndex)), __index));
end;

procedure NSString.getCharacters(_buffer: unichar);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_buffer: unichar); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSString_getCharacters)), _buffer);
end;

procedure NSString.getCharacters_range(_buffer: unichar; _aRange: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_buffer: unichar; _aRange: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSString_getCharacters_range)), _buffer, _aRange);
end;

function NSString.substringFromIndex(_from: LongWord): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_from: LongWord): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSString_substringFromIndex)), _from));
end;

function NSString.substringToIndex(__to: LongWord): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__to: LongWord): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSString_substringToIndex)), __to));
end;

function NSString.substringWithRange(_range: NSRange): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_range: NSRange): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSString_substringWithRange)), _range));
end;

function NSString.compare(__string: CFStringRef): NSComparisonResult;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__string: CFStringRef): NSComparisonResult; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := NSComparisonResult(vmethod(Handle, sel_registerName(PChar(StrNSString_compare)), __string));
end;

function NSString.compare_options(__string: CFStringRef; _mask: LongWord): NSComparisonResult;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__string: CFStringRef; _mask: LongWord): NSComparisonResult; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := NSComparisonResult(vmethod(Handle, sel_registerName(PChar(StrNSString_compare_options)), __string, _mask));
end;

function NSString.compare_options_range(__string: CFStringRef; _mask: LongWord; _compareRange: NSRange): NSComparisonResult;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__string: CFStringRef; _mask: LongWord; _compareRange: NSRange): NSComparisonResult; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := NSComparisonResult(vmethod(Handle, sel_registerName(PChar(StrNSString_compare_options_range)), __string, _mask, _compareRange));
end;

function NSString.compare_options_range_locale(__string: CFStringRef; _mask: LongWord; _compareRange: NSRange; _dict: CFDictionaryRef): NSComparisonResult;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__string: CFStringRef; _mask: LongWord; _compareRange: NSRange; _dict: CFDictionaryRef): NSComparisonResult; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := NSComparisonResult(vmethod(Handle, sel_registerName(PChar(StrNSString_compare_options_range_locale)), __string, _mask, _compareRange, _dict));
end;

function NSString.caseInsensitiveCompare(__string: CFStringRef): NSComparisonResult;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__string: CFStringRef): NSComparisonResult; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := NSComparisonResult(vmethod(Handle, sel_registerName(PChar(StrNSString_caseInsensitiveCompare)), __string));
end;

function NSString.localizedCompare(__string: CFStringRef): NSComparisonResult;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__string: CFStringRef): NSComparisonResult; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := NSComparisonResult(vmethod(Handle, sel_registerName(PChar(StrNSString_localizedCompare)), __string));
end;

function NSString.localizedCaseInsensitiveCompare(__string: CFStringRef): NSComparisonResult;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__string: CFStringRef): NSComparisonResult; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := NSComparisonResult(vmethod(Handle, sel_registerName(PChar(StrNSString_localizedCaseInsensitiveCompare)), __string));
end;

function NSString.isEqualToString(_aString: CFStringRef): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aString: CFStringRef): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSString_isEqualToString)), _aString));
end;

function NSString.hasPrefix(_aString: CFStringRef): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aString: CFStringRef): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSString_hasPrefix)), _aString));
end;

function NSString.hasSuffix(_aString: CFStringRef): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aString: CFStringRef): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSString_hasSuffix)), _aString));
end;

function NSString.rangeOfString(_aString: CFStringRef): NSRange;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_aString: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSString_rangeOfString)), _aString);
end;

function NSString.rangeOfString_options(_aString: CFStringRef; _mask: LongWord): NSRange;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_aString: CFStringRef; _mask: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSString_rangeOfString_options)), _aString, _mask);
end;

function NSString.rangeOfString_options_range(_aString: CFStringRef; _mask: LongWord; _searchRange: NSRange): NSRange;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_aString: CFStringRef; _mask: LongWord; _searchRange: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSString_rangeOfString_options_range)), _aString, _mask, _searchRange);
end;

function NSString.rangeOfCharacterFromSet(_aSet: CFCharacterSetRef): NSRange;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_aSet: CFCharacterSetRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSString_rangeOfCharacterFromSet)), _aSet);
end;

function NSString.rangeOfCharacterFromSet_options(_aSet: CFCharacterSetRef; _mask: LongWord): NSRange;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_aSet: CFCharacterSetRef; _mask: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSString_rangeOfCharacterFromSet_options)), _aSet, _mask);
end;

function NSString.rangeOfCharacterFromSet_options_range(_aSet: CFCharacterSetRef; _mask: LongWord; _searchRange: NSRange): NSRange;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_aSet: CFCharacterSetRef; _mask: LongWord; _searchRange: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSString_rangeOfCharacterFromSet_options_range)), _aSet, _mask, _searchRange);
end;

function NSString.rangeOfComposedCharacterSequenceAtIndex(__index: LongWord): NSRange;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;__index: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSString_rangeOfComposedCharacterSequenceAtIndex)), __index);
end;

function NSString.stringByAppendingString(_aString: CFStringRef): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aString: CFStringRef): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSString_stringByAppendingString)), _aString));
end;

function NSString.stringByAppendingFormat(_format: CFStringRef): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_format: CFStringRef): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSString_stringByAppendingFormat)), _format));
end;

function NSString.doubleValue: double;
begin
  Result := objc_msgSend_fpret(Handle, sel_registerName(PChar(StrNSString_doubleValue)), []);
end;

function NSString.floatValue: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSString_floatValue)), []));
end;

function NSString.intValue: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSString_intValue)), []));
end;

function NSString.componentsSeparatedByString(_separator: CFStringRef): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_separator: CFStringRef): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(Handle, sel_registerName(PChar(StrNSString_componentsSeparatedByString)), _separator));
end;

function NSString.commonPrefixWithString_options(_aString: CFStringRef; _mask: LongWord): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aString: CFStringRef; _mask: LongWord): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSString_commonPrefixWithString_options)), _aString, _mask));
end;

function NSString.uppercaseString: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSString_uppercaseString)), []));
end;

function NSString.lowercaseString: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSString_lowercaseString)), []));
end;

function NSString.capitalizedString: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSString_capitalizedString)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED}
function NSString.stringByTrimmingCharactersInSet(__set: CFCharacterSetRef): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__set: CFCharacterSetRef): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSString_stringByTrimmingCharactersInSet)), __set));
end;

function NSString.stringByPaddingToLength_withString_startingAtIndex(_newLength: LongWord; _padString: CFStringRef; _padIndex: LongWord): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_newLength: LongWord; _padString: CFStringRef; _padIndex: LongWord): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSString_stringByPaddingToLength_withString_startingAtIndex)), _newLength, _padString, _padIndex));
end;

{.$endif}
procedure NSString.getLineStart_end_contentsEnd_forRange(_startPtr: PLongWord; _lineEndPtr: PLongWord; _contentsEndPtr: PLongWord; _range: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_startPtr: PLongWord; _lineEndPtr: PLongWord; _contentsEndPtr: PLongWord; _range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSString_getLineStart_end_contentsEnd_forRange)), _startPtr, _lineEndPtr, _contentsEndPtr, _range);
end;

function NSString.lineRangeForRange(_range: NSRange): NSRange;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSString_lineRangeForRange)), _range);
end;

{.$ifdef  MAC_OS_X_VERSION_10_3 <= MAC_OS_X_VERSION_MAX_ALLOWED}
procedure NSString.getParagraphStart_end_contentsEnd_forRange(_startPtr: PLongWord; _parEndPtr: PLongWord; _contentsEndPtr: PLongWord; _range: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_startPtr: PLongWord; _parEndPtr: PLongWord; _contentsEndPtr: PLongWord; _range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSString_getParagraphStart_end_contentsEnd_forRange)), _startPtr, _parEndPtr, _contentsEndPtr, _range);
end;

function NSString.paragraphRangeForRange(_range: NSRange): NSRange;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSString_paragraphRangeForRange)), _range);
end;

{.$endif}
function NSString.description: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSString_description)), []));
end;

function NSString.hash: LongWord;
begin
  Result := LongWord(objc_msgSend(Handle, sel_registerName(PChar(StrNSString_hash)), []));
end;

function NSString.fastestEncoding: NSStringEncoding;
begin
  Result := NSStringEncoding(objc_msgSend(Handle, sel_registerName(PChar(StrNSString_fastestEncoding)), []));
end;

function NSString.smallestEncoding: NSStringEncoding;
begin
  Result := NSStringEncoding(objc_msgSend(Handle, sel_registerName(PChar(StrNSString_smallestEncoding)), []));
end;

function NSString.dataUsingEncoding_allowLossyConversion(_encoding: NSStringEncoding; _lossy: LongBool): CFDataRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_encoding: NSStringEncoding; _lossy: LongBool): CFDataRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFDataRef(vmethod(Handle, sel_registerName(PChar(StrNSString_dataUsingEncoding_allowLossyConversion)), _encoding, _lossy));
end;

function NSString.dataUsingEncoding(_encoding: NSStringEncoding): CFDataRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_encoding: NSStringEncoding): CFDataRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFDataRef(vmethod(Handle, sel_registerName(PChar(StrNSString_dataUsingEncoding)), _encoding));
end;

function NSString.canBeConvertedToEncoding(_encoding: NSStringEncoding): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_encoding: NSStringEncoding): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSString_canBeConvertedToEncoding)), _encoding));
end;

{.$ifdef  MAC_OS_X_VERSION_10_4 <= MAC_OS_X_VERSION_MAX_ALLOWED}
function NSString.cStringUsingEncoding(_encoding: NSStringEncoding): PChar;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_encoding: NSStringEncoding): PChar; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := PChar(vmethod(Handle, sel_registerName(PChar(StrNSString_cStringUsingEncoding)), _encoding));
end;

function NSString.getCString_maxLength_encoding(_buffer: PChar; _maxBufferCount: LongWord; _encoding: NSStringEncoding): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_buffer: PChar; _maxBufferCount: LongWord; _encoding: NSStringEncoding): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSString_getCString_maxLength_encoding)), _buffer, _maxBufferCount, _encoding));
end;

function NSString.maximumLengthOfBytesUsingEncoding(_enc: NSStringEncoding): LongWord;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_enc: NSStringEncoding): LongWord; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongWord(vmethod(Handle, sel_registerName(PChar(StrNSString_maximumLengthOfBytesUsingEncoding)), _enc));
end;

function NSString.lengthOfBytesUsingEncoding(_enc: NSStringEncoding): LongWord;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_enc: NSStringEncoding): LongWord; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongWord(vmethod(Handle, sel_registerName(PChar(StrNSString_lengthOfBytesUsingEncoding)), _enc));
end;

{.$endif}
{.$ifdef  MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED}
function NSString.decomposedStringWithCanonicalMapping: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSString_decomposedStringWithCanonicalMapping)), []));
end;

function NSString.precomposedStringWithCanonicalMapping: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSString_precomposedStringWithCanonicalMapping)), []));
end;

function NSString.decomposedStringWithCompatibilityMapping: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSString_decomposedStringWithCompatibilityMapping)), []));
end;

function NSString.precomposedStringWithCompatibilityMapping: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSString_precomposedStringWithCompatibilityMapping)), []));
end;

{.$endif}
function NSString.UTF8String: PChar;
begin
  Result := PChar(objc_msgSend(Handle, sel_registerName(PChar(StrNSString_UTF8String)), []));
end;

class function NSString.defaultCStringEncoding: NSStringEncoding;
begin
  Result := NSStringEncoding(objc_msgSend(getClass, sel_registerName(PChar(StrNSString_defaultCStringEncoding)), []));
end;

class function NSString.availableStringEncodings: NSStringEncoding;
begin
  Result := NSStringEncoding(objc_msgSend(getClass, sel_registerName(PChar(StrNSString_availableStringEncodings)), []));
end;

constructor NSString.localizedNameOfStringEncoding(_encoding: NSStringEncoding);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_encoding: NSStringEncoding): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSString_localizedNameOfStringEncoding)), _encoding);
end;

constructor NSString.init;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_init)));
end;

constructor NSString.initWithCharactersNoCopy_length_freeWhenDone(_characters: unichar; _length: LongWord; _freeBuffer: LongBool);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_characters: unichar; _length: LongWord; _freeBuffer: LongBool): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithCharactersNoCopy_length_freeWhenDone)), _characters, _length, _freeBuffer);
end;

constructor NSString.initWithCharacters_length(_characters: unichar; _length: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_characters: unichar; _length: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithCharacters_length)), _characters, _length);
end;

constructor NSString.initWithUTF8String(_nullTerminatedCString: PChar);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_nullTerminatedCString: PChar): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithUTF8String)), _nullTerminatedCString);
end;

constructor NSString.initWithString(_aString: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aString: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithString)), _aString);
end;

constructor NSString.initWithFormat(_format: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_format: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithFormat)), _format);
end;

{constructor NSString.initWithFormat_arguments(_format: CFStringRef; const _argList: array of const);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_format: CFStringRef; const _argList: array of const): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithFormat_arguments)), _format, _argList);
end;}

constructor NSString.initWithFormat_locale(_format: CFStringRef; _dict: CFDictionaryRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_format: CFStringRef; _dict: CFDictionaryRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithFormat_locale)), _format, _dict);
end;

{constructor NSString.initWithFormat_locale_arguments(_format: CFStringRef; _dict: CFDictionaryRef; const _argList: array of const);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_format: CFStringRef; _dict: CFDictionaryRef; const _argList: array of const): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithFormat_locale_arguments)), _format, _dict, _argList);
end;}

constructor NSString.initWithData_encoding(_data: CFDataRef; _encoding: NSStringEncoding);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_data: CFDataRef; _encoding: NSStringEncoding): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithData_encoding)), _data, _encoding);
end;

constructor NSString.initWithBytes_length_encoding(_bytes: Pointer; _len: LongWord; _encoding: NSStringEncoding);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_bytes: Pointer; _len: LongWord; _encoding: NSStringEncoding): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithBytes_length_encoding)), _bytes, _len, _encoding);
end;

{.$ifdef  MAC_OS_X_VERSION_10_3 <= MAC_OS_X_VERSION_MAX_ALLOWED}
constructor NSString.initWithBytesNoCopy_length_encoding_freeWhenDone(_bytes: Pointer; _len: LongWord; _encoding: NSStringEncoding; _freeBuffer: LongBool);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_bytes: Pointer; _len: LongWord; _encoding: NSStringEncoding; _freeBuffer: LongBool): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithBytesNoCopy_length_encoding_freeWhenDone)), _bytes, _len, _encoding, _freeBuffer);
end;

{.$endif}
constructor NSString.string_;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSString_string)));
end;

constructor NSString.stringWithString(__string: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__string: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSString_stringWithString)), __string);
end;

constructor NSString.stringWithCharacters_length(_characters: unichar; _length: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_characters: unichar; _length: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSString_stringWithCharacters_length)), _characters, _length);
end;

constructor NSString.stringWithUTF8String(_nullTerminatedCString: PChar);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_nullTerminatedCString: PChar): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSString_stringWithUTF8String)), _nullTerminatedCString);
end;

constructor NSString.stringWithFormat(_format: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_format: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSString_stringWithFormat)), _format);
end;

constructor NSString.localizedStringWithFormat(_format: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_format: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSString_localizedStringWithFormat)), _format);
end;

{.$ifdef  MAC_OS_X_VERSION_10_4 <= MAC_OS_X_VERSION_MAX_ALLOWED}
constructor NSString.initWithCString_encoding(_nullTerminatedCString: PChar; _encoding: NSStringEncoding);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_nullTerminatedCString: PChar; _encoding: NSStringEncoding): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithCString_encoding)), _nullTerminatedCString, _encoding);
end;

constructor NSString.stringWithCString_encoding(_cString: PChar; _enc: NSStringEncoding);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_cString: PChar; _enc: NSStringEncoding): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSString_stringWithCString_encoding)), _cString, _enc);
end;

constructor NSString.initWithContentsOfURL_encoding_error(_url: CFURLRef; _enc: NSStringEncoding; _error: lobjc.id {NSError});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef; _enc: NSStringEncoding; _error: lobjc.id {NSError}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithContentsOfURL_encoding_error)), _url, _enc, _error);
end;

constructor NSString.initWithContentsOfFile_encoding_error(_path: CFStringRef; _enc: NSStringEncoding; _error: lobjc.id {NSError});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef; _enc: NSStringEncoding; _error: lobjc.id {NSError}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithContentsOfFile_encoding_error)), _path, _enc, _error);
end;

constructor NSString.stringWithContentsOfURL_encoding_error(_url: CFURLRef; _enc: NSStringEncoding; _error: lobjc.id {NSError});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef; _enc: NSStringEncoding; _error: lobjc.id {NSError}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSString_stringWithContentsOfURL_encoding_error)), _url, _enc, _error);
end;

constructor NSString.stringWithContentsOfFile_encoding_error(_path: CFStringRef; _enc: NSStringEncoding; _error: lobjc.id {NSError});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef; _enc: NSStringEncoding; _error: lobjc.id {NSError}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSString_stringWithContentsOfFile_encoding_error)), _path, _enc, _error);
end;

constructor NSString.initWithContentsOfURL_usedEncoding_error(_url: CFURLRef; _enc: NSStringEncoding; _error: lobjc.id {NSError});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef; _enc: NSStringEncoding; _error: lobjc.id {NSError}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithContentsOfURL_usedEncoding_error)), _url, _enc, _error);
end;

constructor NSString.initWithContentsOfFile_usedEncoding_error(_path: CFStringRef; _enc: NSStringEncoding; _error: lobjc.id {NSError});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef; _enc: NSStringEncoding; _error: lobjc.id {NSError}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithContentsOfFile_usedEncoding_error)), _path, _enc, _error);
end;

constructor NSString.stringWithContentsOfURL_usedEncoding_error(_url: CFURLRef; _enc: NSStringEncoding; _error: lobjc.id {NSError});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef; _enc: NSStringEncoding; _error: lobjc.id {NSError}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSString_stringWithContentsOfURL_usedEncoding_error)), _url, _enc, _error);
end;

constructor NSString.stringWithContentsOfFile_usedEncoding_error(_path: CFStringRef; _enc: NSStringEncoding; _error: lobjc.id {NSError});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef; _enc: NSStringEncoding; _error: lobjc.id {NSError}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSString_stringWithContentsOfFile_usedEncoding_error)), _path, _enc, _error);
end;

function NSString.writeToURL_atomically_encoding_error(_url: CFURLRef; _useAuxiliaryFile: LongBool; _enc: NSStringEncoding; _error: lobjc.id {NSError}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef; _useAuxiliaryFile: LongBool; _enc: NSStringEncoding; _error: lobjc.id {NSError}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSString_writeToURL_atomically_encoding_error)), _url, _useAuxiliaryFile, _enc, _error));
end;

function NSString.writeToFile_atomically_encoding_error(_path: CFStringRef; _useAuxiliaryFile: LongBool; _enc: NSStringEncoding; _error: lobjc.id {NSError}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef; _useAuxiliaryFile: LongBool; _enc: NSStringEncoding; _error: lobjc.id {NSError}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSString_writeToFile_atomically_encoding_error)), _path, _useAuxiliaryFile, _enc, _error));
end;

{.$endif}
function NSString.propertyList: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSString_propertyList)), []));
end;

function NSString.propertyListFromStringsFileFormat: CFDictionaryRef;
begin
  Result := CFDictionaryRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSString_propertyListFromStringsFileFormat)), []));
end;

function NSString.cString: PChar;
begin
  Result := PChar(objc_msgSend(Handle, sel_registerName(PChar(StrNSString_cString)), []));
end;

function NSString.lossyCString: PChar;
begin
  Result := PChar(objc_msgSend(Handle, sel_registerName(PChar(StrNSString_lossyCString)), []));
end;

function NSString.cStringLength: LongWord;
begin
  Result := LongWord(objc_msgSend(Handle, sel_registerName(PChar(StrNSString_cStringLength)), []));
end;

procedure NSString.getCString(_bytes: PChar);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_bytes: PChar); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSString_getCString)), _bytes);
end;

procedure NSString.getCString_maxLength(_bytes: PChar; _maxLength: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_bytes: PChar; _maxLength: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSString_getCString_maxLength)), _bytes, _maxLength);
end;

procedure NSString.getCString_maxLength_range_remainingRange(_bytes: PChar; _maxLength: LongWord; _aRange: NSRange; _leftoverRange: NSRangePointer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_bytes: PChar; _maxLength: LongWord; _aRange: NSRange; _leftoverRange: NSRangePointer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSString_getCString_maxLength_range_remainingRange)), _bytes, _maxLength, _aRange, _leftoverRange);
end;

function NSString.writeToFile_atomically(_path: CFStringRef; _useAuxiliaryFile: LongBool): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef; _useAuxiliaryFile: LongBool): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSString_writeToFile_atomically)), _path, _useAuxiliaryFile));
end;

function NSString.writeToURL_atomically(_url: CFURLRef; _atomically: LongBool): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef; _atomically: LongBool): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSString_writeToURL_atomically)), _url, _atomically));
end;

constructor NSString.initWithContentsOfFile(_path: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithContentsOfFile)), _path);
end;

constructor NSString.initWithContentsOfURL(_url: CFURLRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithContentsOfURL)), _url);
end;

constructor NSString.stringWithContentsOfFile(_path: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSString_stringWithContentsOfFile)), _path);
end;

constructor NSString.stringWithContentsOfURL(_url: CFURLRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSString_stringWithContentsOfURL)), _url);
end;

constructor NSString.initWithCStringNoCopy_length_freeWhenDone(_bytes: PChar; _length: LongWord; _freeBuffer: LongBool);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_bytes: PChar; _length: LongWord; _freeBuffer: LongBool): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithCStringNoCopy_length_freeWhenDone)), _bytes, _length, _freeBuffer);
end;

constructor NSString.initWithCString_length(_bytes: PChar; _length: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_bytes: PChar; _length: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithCString_length)), _bytes, _length);
end;

constructor NSString.initWithCString(_bytes: PChar);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_bytes: PChar): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSString_initWithCString)), _bytes);
end;

constructor NSString.stringWithCString_length(_bytes: PChar; _length: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_bytes: PChar; _length: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSString_stringWithCString_length)), _bytes, _length);
end;

constructor NSString.stringWithCString(_bytes: PChar);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_bytes: PChar): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSString_stringWithCString)), _bytes);
end;

{ NSMutableString }

class function NSMutableString.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSMutableString_NSMutableString);
end;

procedure NSMutableString.replaceCharactersInRange_withString(_range: NSRange; _aString: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_range: NSRange; _aString: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableString_replaceCharactersInRange_withString)), _range, _aString);
end;

procedure NSMutableString.insertString_atIndex(_aString: CFStringRef; _loc: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aString: CFStringRef; _loc: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableString_insertString_atIndex)), _aString, _loc);
end;

procedure NSMutableString.deleteCharactersInRange(_range: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableString_deleteCharactersInRange)), _range);
end;

procedure NSMutableString.appendString(_aString: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aString: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableString_appendString)), _aString);
end;

procedure NSMutableString.appendFormat(_format: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_format: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableString_appendFormat)), _format);
end;

procedure NSMutableString.setString(_aString: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aString: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableString_setString)), _aString);
end;

constructor NSMutableString.initWithCapacity(_capacity: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_capacity: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSMutableString_initWithCapacity)), _capacity);
end;

constructor NSMutableString.stringWithCapacity(_capacity: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_capacity: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSMutableString_stringWithCapacity)), _capacity);
end;

{.$ifdef  MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED}
function NSMutableString.replaceOccurrencesOfString_withString_options_range(_target: CFStringRef; _replacement: CFStringRef; _opts: LongWord; _searchRange: NSRange): LongWord;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_target: CFStringRef; _replacement: CFStringRef; _opts: LongWord; _searchRange: NSRange): LongWord; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongWord(vmethod(Handle, sel_registerName(PChar(StrNSMutableString_replaceOccurrencesOfString_withString_options_range)), _target, _replacement, _opts, _searchRange));
end;

{.$endif}
{ NSSimpleCString }

class function NSSimpleCString.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSSimpleCString_NSSimpleCString);
end;

{ NSConstantString }

class function NSConstantString.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSConstantString_NSConstantString);
end;

{$endif}
