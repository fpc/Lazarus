{%mainunit appkit.pas}
(*
	NSNibLoading.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSNIBLOADING_PAS_H}
{$define NSNIBLOADING_PAS_H}

{@class NSString, NSDictionary;

@interface NSBundle(NSNibLoading)

+ (BOOL)loadNibFile:(NSString *)fileName externalNameTable:(NSDictionary *)context withZone:(NSZone *)zone;
    /* This is the primitive that loads the contents of a .nib file.  Context holds key value pairs that can name objects that are referenced by the objects within the nib file (e.g., "NSOwner").  Objects from the nib are allocated in zone. */

+ (BOOL)loadNibNamed:(NSString *)nibName owner:(id)owner;
    /* A cover method where an external name table is created with owner as the only element with key "NSOwner".  Objects from the nib are allocated in owner's zone. If there is a bundle for owner's class, this method looks in that bundle for a nib with the given file name.  Otherwise, it looks in the mainBundle. */

- (BOOL)loadNibFile:(NSString *)fileName externalNameTable:(NSDictionary *)context withZone:(NSZone *)zone;
    /* This is the instance version of the above class method by the same name. */

@end

@interface NSObject (NSNibAwaking)
- (void)awakeFromNib;
@end}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSBundleNib = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSNIBLOADING_PAS_C}
{$define NSNIBLOADING_PAS_C}

  { NSBundleNib }

  NSBundleNib = class(NSObject)
  public
    //class function loadNibFile_externalNameTable_withZone(fileName: CFStringRef; context: NSDictionary; zone: NSZone);
    { This is the primitive that loads the contents of a .nib file.  Context holds key value pairs that can name objects that are referenced by the objects within the nib file (e.g., "NSOwner").  Objects from the nib are allocated in zone. }

    class function loadNibNamed_owner(nibName: CFStringRef; owner: lobjc.id): lobjc.BOOL;
    { A cover method where an external name table is created with owner as the only element with key "NSOwner".  Objects from the nib are allocated in owner's zone. If there is a bundle for owner's class, this method looks in that bundle for a nib with the given file name.  Otherwise, it looks in the mainBundle. }

    //- (BOOL)loadNibFile:(NSString *)fileName externalNameTable:(NSDictionary *)context withZone:(NSZone *)zone;
    { This is the instance version of the above class method by the same name. }
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}

const
  StrNSBundle_NSBundle = 'NSBundle';
  StrNSBundle_loadNibNamed_owner = 'loadNibNamed:owner:';

class function NSBundleNib.loadNibNamed_owner(nibName: CFStringRef; owner: lobjc.id): lobjc.BOOL;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL; nibName: CFStringRef; owner: lobjc.id): lobjc.BOOL; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := vmethod(objc_getClass(PChar(StrNSBundle_NSBundle)), sel_registerName(PChar(StrNSBundle_loadNibNamed_owner)), nibName, owner);
end;

{$endif}

