{%mainunit foundation.pas}
(*	NSDate.h
	Copyright (c) 1994-2005, Apple, Inc. All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSDATE_PAS_H}
{$define NSDATE_PAS_H}

{$include NSObject.inc}
const
  NSTimeIntervalSince1970 = 978307200.0;
type
  NSTimeInterval = double;

{$endif}
{$endif}

{$ifdef FORWARD}
{$ifndef NSDATE_PAS__FORWARD}
{$define NSDATE_PAS__FORWARD}

  NSDate = class;
{$endif}
{$endif}

{$ifdef CLASSES}
{$ifndef NSDATE_PAS_C}
{$define NSDATE_PAS_C}

{$include NSObject.inc}

  { NSDate }

  NSDate = class(NSObject)
  public
    class function getClass: lobjc.id; override;
    function timeIntervalSinceReferenceDate: NSTimeInterval;
    function timeIntervalSinceDate(_anotherDate: CFDateRef): NSTimeInterval;
    function timeIntervalSinceNow: NSTimeInterval;
    function timeIntervalSince1970: NSTimeInterval;
    function addTimeInterval(_seconds: NSTimeInterval): lobjc.id;
    function earlierDate(_anotherDate: CFDateRef): CFDateRef;
    function laterDate(_anotherDate: CFDateRef): CFDateRef;
    function compare(_other: CFDateRef): NSComparisonResult;
    function description: CFStringRef;
    function isEqualToDate(_otherDate: CFDateRef): LongBool;
    class function timeIntervalSinceReferenceDate_: NSTimeInterval;
    constructor date;
    constructor dateWithTimeIntervalSinceNow(_secs: NSTimeInterval);
    constructor dateWithTimeIntervalSinceReferenceDate(_secs: NSTimeInterval);
    constructor dateWithTimeIntervalSince1970(_secs: NSTimeInterval);
    constructor distantFuture;
    constructor distantPast;
    constructor init;
    constructor initWithTimeIntervalSinceReferenceDate(_secsToBeAdded: NSTimeInterval);
    constructor initWithTimeInterval_sinceDate(_secsToBeAdded: NSTimeInterval; _anotherDate: CFDateRef);
    constructor initWithTimeIntervalSinceNow(_secsToBeAddedToNow: NSTimeInterval);
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSDate_NSDate = 'NSDate';
  StrNSDate_timeIntervalSinceReferenceDate = 'timeIntervalSinceReferenceDate';

  StrNSDate_timeIntervalSinceDate = 'timeIntervalSinceDate:';
  StrNSDate_timeIntervalSinceNow = 'timeIntervalSinceNow';
  StrNSDate_timeIntervalSince1970 = 'timeIntervalSince1970';
  StrNSDate_addTimeInterval = 'addTimeInterval:';
  StrNSDate_earlierDate = 'earlierDate:';
  StrNSDate_laterDate = 'laterDate:';
  StrNSDate_compare = 'compare:';
  StrNSDate_description = 'description';
  StrNSDate_isEqualToDate = 'isEqualToDate:';

  StrNSDate_date = 'date';
  StrNSDate_dateWithTimeIntervalSinceNow = 'dateWithTimeIntervalSinceNow:';
  StrNSDate_dateWithTimeIntervalSinceReferenceDate = 'dateWithTimeIntervalSinceReferenceDate:';
  StrNSDate_dateWithTimeIntervalSince1970 = 'dateWithTimeIntervalSince1970:';
  StrNSDate_distantFuture = 'distantFuture';
  StrNSDate_distantPast = 'distantPast';
  StrNSDate_init = 'init';
  StrNSDate_initWithTimeIntervalSinceReferenceDate = 'initWithTimeIntervalSinceReferenceDate:';
  StrNSDate_initWithTimeInterval_sinceDate = 'initWithTimeInterval:sinceDate:';
  StrNSDate_initWithTimeIntervalSinceNow = 'initWithTimeIntervalSinceNow:';

{ NSDate }

class function NSDate.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSDate_NSDate);
end;

function NSDate.timeIntervalSinceReferenceDate: NSTimeInterval;
begin
  Result := objc_msgSend_fpret(Handle, sel_registerName(PChar(StrNSDate_timeIntervalSinceReferenceDate)), []);
end;

function NSDate.timeIntervalSinceDate(_anotherDate: CFDateRef): NSTimeInterval;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_anotherDate: CFDateRef): NSTimeInterval; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_stret);
  Result := vmethod(Handle, sel_registerName(PChar(StrNSDate_timeIntervalSinceDate)), _anotherDate);
end;

function NSDate.timeIntervalSinceNow: NSTimeInterval;
begin
  Result := objc_msgSend_fpret(Handle, sel_registerName(PChar(StrNSDate_timeIntervalSinceNow)), []);
end;

function NSDate.timeIntervalSince1970: NSTimeInterval;
begin
  Result := objc_msgSend_fpret(Handle, sel_registerName(PChar(StrNSDate_timeIntervalSince1970)), []);
end;

function NSDate.addTimeInterval(_seconds: NSTimeInterval): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_seconds: NSTimeInterval): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSDate_addTimeInterval)), _seconds));
end;

function NSDate.earlierDate(_anotherDate: CFDateRef): CFDateRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_anotherDate: CFDateRef): CFDateRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFDateRef(vmethod(Handle, sel_registerName(PChar(StrNSDate_earlierDate)), _anotherDate));
end;

function NSDate.laterDate(_anotherDate: CFDateRef): CFDateRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_anotherDate: CFDateRef): CFDateRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFDateRef(vmethod(Handle, sel_registerName(PChar(StrNSDate_laterDate)), _anotherDate));
end;

function NSDate.compare(_other: CFDateRef): NSComparisonResult;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_other: CFDateRef): NSComparisonResult; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := NSComparisonResult(vmethod(Handle, sel_registerName(PChar(StrNSDate_compare)), _other));
end;

function NSDate.description: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSDate_description)), []));
end;

function NSDate.isEqualToDate(_otherDate: CFDateRef): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_otherDate: CFDateRef): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSDate_isEqualToDate)), _otherDate));
end;

class function NSDate.timeIntervalSinceReferenceDate_: NSTimeInterval;
begin
  Result := objc_msgSend_fpret(getClass, sel_registerName(PChar(StrNSDate_timeIntervalSinceReferenceDate)), []);
end;

constructor NSDate.date;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSDate_date)));
end;

constructor NSDate.dateWithTimeIntervalSinceNow(_secs: NSTimeInterval);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_secs: NSTimeInterval): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSDate_dateWithTimeIntervalSinceNow)), _secs);
end;

constructor NSDate.dateWithTimeIntervalSinceReferenceDate(_secs: NSTimeInterval);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_secs: NSTimeInterval): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSDate_dateWithTimeIntervalSinceReferenceDate)), _secs);
end;

constructor NSDate.dateWithTimeIntervalSince1970(_secs: NSTimeInterval);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_secs: NSTimeInterval): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSDate_dateWithTimeIntervalSince1970)), _secs);
end;

constructor NSDate.distantFuture;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSDate_distantFuture)));
end;

constructor NSDate.distantPast;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSDate_distantPast)));
end;

constructor NSDate.init;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSDate_init)));
end;

constructor NSDate.initWithTimeIntervalSinceReferenceDate(_secsToBeAdded: NSTimeInterval);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_secsToBeAdded: NSTimeInterval): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSDate_initWithTimeIntervalSinceReferenceDate)), _secsToBeAdded);
end;

constructor NSDate.initWithTimeInterval_sinceDate(_secsToBeAdded: NSTimeInterval; _anotherDate: CFDateRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_secsToBeAdded: NSTimeInterval; _anotherDate: CFDateRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSDate_initWithTimeInterval_sinceDate)), _secsToBeAdded, _anotherDate);
end;

constructor NSDate.initWithTimeIntervalSinceNow(_secsToBeAddedToNow: NSTimeInterval);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_secsToBeAddedToNow: NSTimeInterval): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSDate_initWithTimeIntervalSinceNow)), _secsToBeAddedToNow);
end;

{$endif}
