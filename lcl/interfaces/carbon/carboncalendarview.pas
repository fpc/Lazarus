{ $Id: $
                  --------------------------------------------
                  carbonprivate.pp  -  Carbon internal classes
                  --------------------------------------------

 This unit contains the Carbon Calendar view implementation. Pure carbon, no LCL controls

 The code is based on CalendarView code sample.
 http://developer.apple.com/legacy/mac/library/samplecode/CalendarView/index.html

 The calendar view, has been "modernized":
  * using HIShape in GetBoundsEvent
  * removed loop in FindPart
  * changed drawing ratios
  * added day selection (selDay)
 TODO: remove QuickDraw deperacted functions

 The best size is: width = 180. height = 140
 
 Ported by: Dmitry 'skalogryz' Boyarintsev 

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CarbonCalendarView;

{$mode objfpc}{$H+}

interface

uses
  MacOSAll;

// -----------------------------------------------------------------------------
// CalendarView API
// -----------------------------------------------------------------------------
//
function CalendarViewCreate(inWindow : WindowRef;
	const inBounds : Rect; var outControl: ControlRef): OSStatus;

function CalendarGetDate(Calendar: ControlRef; var Date: CFGregorianDate): Boolean;
function CalendarSetDate(Calendar: ControlRef; const Date: CFGregorianDate): Boolean;

// -----------------------------------------------------------------------------
// CalendarView Set/GetData tags
// -----------------------------------------------------------------------------
//
const
	kControlCalendarTitleRatioTag	  = {$IFDEF FPC_LITTLE_ENDIAN}$74615274{$else}$74526174{$endif}; //tRat float
	kControlCalendarDayNameRatioTag	= {$IFDEF FPC_LITTLE_ENDIAN}$7461526E{$else}$6E526174{$endif}; //nRat float
	kControlCalendarDayRatioTag		  = {$IFDEF FPC_LITTLE_ENDIAN}$74615264{$else}$64526174{$endif}; //dRat float
	kControlCalendarDateTag			    = {$IFDEF FPC_LITTLE_ENDIAN}$65746144{$else}$44617465{$endif}; //Date CFGregorianDate -- on Set, day, hour, etc are ignored
	kControlCalendarDrawProcTag		  = {$IFDEF FPC_LITTLE_ENDIAN}$77617244{$else}$44726177{$endif}; //Draw CalendarDrawUPP
	kControlCalendarLabelProcTag	  = {$IFDEF FPC_LITTLE_ENDIAN}$6C62614C{$else}$4C61626C{$endif}; //Labl CalendarDrawUPP

// -----------------------------------------------------------------------------
// CalendarView Draw/LabelProc callback data and prototypes
// -----------------------------------------------------------------------------
//
type
  CalendarDrawData = packed record
	  context     : CGContextRef;
  	daysInMonth : UInt8;
	  hilitePart  : ControlPartCode;
  	date        : CFGregorianDate;
  end;

type
  CalendarDrawProc = procedure (inPart: ControlPartCode; const inPartRect: HIRect; const inData :CalendarDrawData);
  CalendarDrawProcPtr = ^CalendarDrawProc;

implementation

var
  kCalendarViewClassID    : CFStringRef = nil;
  _kHIViewClassID         : CFStringRef = nil;
  CalendarViewHandlerUPP  : EventHandlerUPP;
  months                  : array[0..11] of CFStringRef;
  dow                     : array[0..6] of CFStringRef;

const
	kCalendarMonthPart          = 100;
	kCalendarPreviousYearPart   = 101;
	kCalendarPreviousMonthPart  = 102;
	kCalendarNextMonthPart      = 103;
	kCalendarNextYearPart       = 104;
	kCalendarSundayNamePart     = 200;
	kCalendarMondayNamePart     = 201;
	kCalendarTuesdayNamePart    = 202;
	kCalendarWednesdayNamePart  = 203;
	kCalendarThursdayNamePart   = 204;
	kCalendarFridayNamePart     = 205;
	kCalendarSaturdayNamePart   = 206;

// -----------------------------------------------------------------------------
//	types
// -----------------------------------------------------------------------------
//
type
  CalendarViewDataPtr = ^CalendarViewData;
  CalendarViewData = record
  	view : HIViewRef;
  	// Geometry
	  titleRowRatio   : single;
  	dayNameRowRatio : single;
	  dayRowRatio     : single;

  	// Date stuff
	  date        : CFGregorianDate;
  	timeZone    : CFTimeZoneRef;
	  firstDay    : UInt8;
  	daysInMonth : UInt8;
    selDay      : UInt8;  // selected day

  	// Proc stuff
	  drawProc    : CalendarDrawProc;
  	labelProc   : CalendarDrawProc;

  end;

// -----------------------------------------------------------------------------
//	utilities
// -----------------------------------------------------------------------------
//

// -----------------------------------------------------------------------------
//	SetUpDateData
// -----------------------------------------------------------------------------
//
procedure SetUpDateData(var inData: CalendarViewData);
var
	tempDate : CFGregorianDate;
	tempTime : CFAbsoluteTime;
  nextMonthMinusADay : CFGregorianUnits;
begin
  with nextMonthMinusADay do begin
		years:=0;
		months:=1;
		days:=-1;
		hours:=0;
		minutes:=0;
		seconds:=0;
  end;
	// What is the first day of this month?
	tempTime := CFGregorianDateGetAbsoluteTime( inData.date, inData.timeZone );
	inData.firstDay := CFAbsoluteTimeGetDayOfWeek( tempTime, inData.timeZone ) mod 7;

	// How many days in this month?
	tempTime := CFAbsoluteTimeAddGregorianUnits( tempTime, inData.timeZone, nextMonthMinusADay );
	tempDate := CFAbsoluteTimeGetGregorianDate( tempTime, inData.timeZone );
	inData.daysInMonth := tempDate.day;
end;

// -----------------------------------------------------------------------------
//	DefaultDrawPart
// -----------------------------------------------------------------------------
//
procedure DefaultDrawPart(inPart: ControlPartCode; const inPartRect: HIRect; const inData: CalendarDrawData );
var
  color: RGBColor;
begin
	case inPart of
		kControlStructureMetaPart:
    begin
  		CGContextSetRGBFillColor( inData.context, 0.95, 0.95, 0.95, 1 );
	  	CGContextFillRect( inData.context, inPartRect );
			//#elif 1
			//	ShadeRect( .8, .95, inPartRect, inData->context );
			//#else
			//{
			//	CGRGB	start = { 1, 0, 0 };
			//	CGRGB	end = { 0, 0, 1 };
		  //	ShadeRectColor( &start, &end, inPartRect, inData->context );
			//}
			//#endif
    end
  else
  	if (inData.hilitePart > 0) and (inData.hilitePart = inPart) then begin
      GetThemeBrushAsColor( kThemeBrushPrimaryHighlightColor, 32, true, color );
      CGContextSetRGBFillColor( inData.context, color.red / 65536, color.green / 65536, color.blue / 65536, 1 );
      CGContextFillRect(inData.context, inPartRect );
    end;
  end;
end;

// -----------------------------------------------------------------------------
//	DefaultDrawPartLabel
// -----------------------------------------------------------------------------
//
procedure DefaultDrawPartLabel(inPart: ControlPartCode; const inPartRect: HIRect;	const inData: CalendarDrawData);
var
	qdBounds  : Rect;
	s         : CFStringRef;
  dateString: CFMutableStringRef;
begin
	// Set up a quickdraw rectangle for DrawThemeTextBox
	qdBounds.top := Round(inPartRect.origin.y);
	qdBounds.left := Round(inPartRect.origin.x);
	qdBounds.bottom := qdBounds.top + Round(inPartRect.size.height);
	qdBounds.right := qdBounds.left + Round(inPartRect.size.width);

	CGContextSetRGBFillColor( inData.context, 0, 0, 0, 1 );

 case (inPart) of
		kCalendarMonthPart:
    begin
			dateString := CFStringCreateMutableCopy( nil, 16, months[ inData.date.month-1 ] );
			CFStringAppend( dateString, CFSTR(' '));
			CFStringAppendFormat( dateString, nil, CFSTR( '%d' ), inData.date.year );

			DrawThemeTextBox( dateString, kThemeSystemFont,
					kThemeStateActive, false, qdBounds, teJustCenter, inData.context );

			CFRelease( dateString );
		end;

		kCalendarPreviousYearPart:
			DrawThemeTextBox( CFSTR('<'), kThemeEmphasizedSystemFont,
				kThemeStateActive, false, qdBounds, teJustCenter, inData.context );

		kCalendarPreviousMonthPart:
			DrawThemeTextBox( CFSTR( '<' ), kThemeSmallSystemFont,
				kThemeStateActive, false, qdBounds, teJustCenter, inData.context );

		kCalendarNextMonthPart:
			DrawThemeTextBox( CFSTR( '>' ), kThemeSmallSystemFont,
				kThemeStateActive, false, qdBounds, teJustCenter, inData.context );

		kCalendarNextYearPart:
			DrawThemeTextBox( CFSTR( '>' ), kThemeEmphasizedSystemFont,
   			kThemeStateActive, false, qdBounds, teJustCenter, inData.context );

		kCalendarSundayNamePart,
		kCalendarMondayNamePart,
		kCalendarTuesdayNamePart,
		kCalendarWednesdayNamePart,
		kCalendarThursdayNamePart,
		kCalendarFridayNamePart,
		kCalendarSaturdayNamePart:
  		DrawThemeTextBox( dow[ inPart mod kCalendarSundayNamePart ],
        //kThemeSystemFont,
        kThemeEmphasizedSystemFont,
				kThemeStatePressed, false, qdBounds, teJustCenter, inData.context );

	else
		if (inPart > 0) and (inPart <= inData.daysInMonth ) then begin
   		s := CFStringCreateWithFormat( nil, nil, CFSTR( '%d' ), inPart );
      qdBounds.right := qdBounds.right - 4;
			DrawThemeTextBox(s, kThemeSystemFont, kThemeStatePressed, false, qdBounds, teJustRight, inData.context);
			CFRelease( s );
		end;
  end;
end;



// -----------------------------------------------------------------------------
//	CalendarViewConstruct
// -----------------------------------------------------------------------------
//

function CalendarViewConstruct(inEvent : EventRef): OSStatus;
var
	err  : OSStatus;
	data : CalendarViewDataPtr;
begin
	// don't CallNextEventHandler!
  try
   	data :=GetMem( sizeof( CalendarViewData ) );
    if data=nil then begin
      err := memFullErr;
      Exit;
    end;

    FillChar(data^, sizeof(CalendarViewData), 0);
    try
    	// Set up the row height ratios
    	data^.titleRowRatio := 1.0;		// half a row
    	data^.dayNameRowRatio := 1.0;	// half a row
    	data^.dayRowRatio := 1.0;		// full row

  	  // Set up the current timezone
    	data^.timeZone := CFTimeZoneCopySystem();
      if data^.timeZone = nil then begin
        err := memFullErr;
        Exit;
      end;

	    // Set up the current month
    	data^.date := CFAbsoluteTimeGetGregorianDate( CFAbsoluteTimeGetCurrent(), data^.timeZone );
    	data^.date.day := 1;
    	data^.date.hour := 0;
    	data^.date.minute := 0;
    	data^.date.second := 0;
    	SetUpDateData( data^ );

    	// Set up the default drawing callbacks
    	data^.drawProc := @DefaultDrawPart;
    	data^.labelProc := @DefaultDrawPartLabel;

	    // Keep a copy of the created HIViewRef
    	err := GetEventParameter( inEvent, kEventParamHIObjectInstance, typeHIObjectRef,
      			nil, sizeof( HIObjectRef ), nil, @data^.view );
      if err <> noErr then Exit;

      // Set the userData that will be used with all subsequent eventHandler calls
      err := SetEventParameter( inEvent, kEventParamHIObjectInstance, typeVoidPtr,sizeof( CalendarViewDataPtr ), @data );

    finally
      if err <> noErr then FreeMem(data);
    end;
  finally
    Result := err;
  end;
end;


// -----------------------------------------------------------------------------
//	CalendarViewInitialize
// -----------------------------------------------------------------------------
//
function CalendarViewInitialize(inCallRef: EventHandlerCallRef; inEvent : EventRef;
  const inData : CalendarViewData): OSStatus;
var
	bounds    : Rect;
  features  : UInt32;
const
  BounName : PChar = 'Boun';
begin
	features := kControlSupportsDataAccess;

	// Let the base class initialization occur
	Result := CallNextEventHandler( inCallRef, inEvent );
  if Result <> noErr then Exit;

	// Extract the initial view bounds from the event
  //TODO!!!
	Result :=GetEventParameter( inEvent, EventParamNamePtr(BounName)^, typeQDRectangle,	nil, sizeof( Rect ), nil, @bounds );
  if Result <> noErr then Exit;

	// Set up this view's feature bits
	Result := SetEventParameter( inEvent, kEventParamControlFeatures, typeUInt32, sizeof( UInt32 ), @features );
	SetControlBounds( inData.view, bounds );
end;

// -----------------------------------------------------------------------------
//	CalendarViewDestruct
// -----------------------------------------------------------------------------
//
function CalendarViewDestruct(inEvent : EventRef; var inData : CalendarViewDataPtr): OSStatus;
begin
  //#pragma unused( inEvent )
	// Clean up any allocated data
	CFRelease( inData^.timeZone );
	FreeMem( inData );
  inData := nil;
	Result := noErr;
end;


// -----------------------------------------------------------------------------
//	CalendarViewDraw
// -----------------------------------------------------------------------------
//

function CalendarViewDraw(inEvent: EventRef; const inData: CalendarViewData): OSStatus;
var
	bounds    : HIRect;
	rowHeight : single;
	colWidth  : single;
	cols      : integer;
	rows      : single;
	drawRect  : HIRect;
	part      : ControlPartCode;
	dayCount  : UInt16;
	rowCount  : single;
	drawData  : CalendarDrawData;
begin
  rows := 0;
  dayCount := 0;
  rowCount := 6 * inData.dayRowRatio + inData.dayNameRowRatio + inData.titleRowRatio;
	
	// Get ready to do the CG drawing boogaloo!
	Result := GetEventParameter( inEvent, kEventParamCGContextRef, typeCGContextRef,	
            nil, sizeof( CGContextRef ), nil, @drawData.context );
  if Result <> noErr then Exit;  

	Result := HIViewGetBounds( inData.view, bounds );

  // highlighting only selected day
	//drawData.hilitePart := GetControlHilite( inData.view );
  drawData.hilitePart := inData.selDay;
	drawData.daysInMonth := inData.daysInMonth;
	drawData.date := inData.date;

	// Figure out how tall a row should be
	rowHeight := bounds.size.height / rowCount;
	colWidth := Round ( bounds.size.width) div 14;	// round here instead of over and over

  inData.drawProc(kControlStructureMetaPart, bounds, drawData);

	drawRect.origin := bounds.origin;
	drawRect.size.height := Round( rowHeight * inData.titleRowRatio);
	drawRect.size.width := colWidth;
  
	inData.drawProc(kCalendarPreviousYearPart, drawRect, drawData);
  inData.labelProc(kCalendarPreviousYearPart, drawRect, drawData);
	drawRect.origin.x := drawRect.origin.x + drawRect.size.width;

	drawRect.size.width := colWidth;
	inData.drawProc( kCalendarPreviousMonthPart, drawRect, drawData);
	inData.labelProc( kCalendarPreviousMonthPart, drawRect, drawData);
	drawRect.origin.x:=drawRect.origin.x+ drawRect.size.width;

	// Draw the month
	drawRect.size.width := 10 * colWidth;
	inData.drawProc( kCalendarMonthPart, drawRect, drawData);
	inData.labelProc( kCalendarMonthPart, drawRect, drawData );
	drawRect.origin.x := drawRect.origin.x + drawRect.size.width;

	drawRect.size.width := colWidth;
	inData.drawProc( kCalendarNextMonthPart, drawRect, drawData);
	inData.labelProc( kCalendarNextMonthPart, drawRect, drawData);
	drawRect.origin.x := drawRect.origin.x + drawRect.size.width;

	drawRect.size.width := colWidth;
	inData.drawProc ( kCalendarNextYearPart, drawRect, drawData);
	inData.labelProc( kCalendarNextYearPart, drawRect, drawData);

	drawRect.origin.y := drawRect.origin.y  + drawRect.size.height;	// on to the next row!
	rows := rows + inData.titleRowRatio;
	drawRect.origin.x := bounds.origin.x;	// reset to leftmost

	// Optionally draw the week day names
	if  inData.dayNameRowRatio <> 0  then begin
		// Draw the weekdays
		drawRect.size.width := 2 * colWidth;
		drawRect.size.height := Int( rowHeight * inData.dayNameRowRatio );
		part := kCalendarSundayNamePart;
    for cols := 0 to 6 do begin
			inData.drawProc (part, drawRect, drawData);
			inData.labelProc(part, drawRect, drawData);
			drawRect.origin.x := drawRect.origin.x+drawRect.size.width;	// on to the next col!
      inc(part);
		end;
		drawRect.origin.y := drawRect.origin.y+drawRect.size.height;	// on to the next row!
		rows := rows + inData.dayNameRowRatio;
	end;

	// Set up to draw the date rows
	drawRect.origin.x := bounds.origin.x;	// reset to leftmost
	drawRect.size.width := 2 * colWidth;
	drawRect.size.height := Round ( rowHeight * inData.dayRowRatio );

	part := 0;
  while rows < rowCount do begin
		for cols := 0 to 6 do begin
			if ( dayCount >= inData.firstDay ) then
				inc(part);
      inc(dayCount);

      if part > inData.daysInMonth
        then inData.drawProc(0, drawRect, drawData)
        else inData.drawProc(part, drawRect, drawData);
      if part > inData.daysInMonth
        then inData.labelProc(0, drawRect, drawData)
        else inData.labelProc(part, drawRect, drawData);
			drawRect.origin.x := drawRect.origin.x + drawRect.size.width;	// on to the next col!
	  end;
		drawRect.origin.x := bounds.origin.x;	// reset to leftmost
		drawRect.origin.y := drawRect.origin.y + drawRect.size.height;	// on to the next row!
    rows := rows + inData.dayRowRatio;
	end;
end;

// -----------------------------------------------------------------------------
//	FindPart
// -----------------------------------------------------------------------------
//
function FindPart(const inBounds: HIRect; const inWhere : HIPoint; const inData : CalendarViewData): ControlPartCode;
var
	part      : ControlPartCode;
	testRect  : HIRect;
	rowHeight : single;
  colWidth  : single;
	rows      : single;
	cols      : integer;
	rowCount  : single;
  dx, dy    : Integer;
begin
  rows := 0;
  rowCount := 6 * inData.dayRowRatio + inData.dayNameRowRatio + inData.titleRowRatio;
	rowHeight := inBounds.size.height / rowCount;
	colWidth := Round(inBounds.size.width / 14 );

	// Part is the month?
	testRect.origin := inBounds.origin;
	testRect.size.height := Round(rowHeight * inData.titleRowRatio );

	testRect.size.width := colWidth;
  if CGRectContainsPoint( testRect, inWhere ) <> 0 then begin
    Result := kCalendarPreviousYearPart;
    Exit;
  end;
	testRect.origin.x :=  testRect.origin.x + testRect.size.width;

	if CGRectContainsPoint( testRect, inWhere ) <> 0 then begin
    Result := kCalendarPreviousMonthPart;
    Exit;
  end;
	testRect.origin.x := testRect.origin.x + testRect.size.width;

	testRect.size.width := 10 * colWidth;
	if CGRectContainsPoint( testRect, inWhere ) <> 0 then begin
    Result := kCalendarMonthPart;
    Exit;
  end;
  testRect.origin.x := testRect.origin.x + testRect.size.width;

	testRect.size.width := colWidth;
	if CGRectContainsPoint( testRect, inWhere ) <> 0 then begin
    Result := kCalendarNextMonthPart;
    Exit;
  end;
	testRect.origin.x := testRect.origin.x + testRect.size.width;

	testRect.size.width := colWidth;
  if CGRectContainsPoint( testRect, inWhere ) <> 0 then begin
    Result := kCalendarNextYearPart;
    Exit;
  end;

	testRect.origin.y := testRect.origin.y + testRect.size.height;
	rows := rows + inData.titleRowRatio;
	testRect.origin.x := inBounds.origin.x;
	
	if ( inData.dayNameRowRatio <> 0 ) then
	begin
		// Part is a weekday?
		testRect.size.height := int( rowHeight * inData.dayNameRowRatio );
		testRect.size.width := inBounds.size.width;

		if CGRectContainsPoint( testRect, inWhere ) <> 0 then 
		begin
			part := kCalendarSundayNamePart;
			testRect.size.width := 2 * colWidth;
			for cols := 0 to 6 do begin
        if CGRectContainsPoint( testRect, inWhere ) <> 0 then begin
          Result := part;
          Exit;
        end;
				testRect.origin.x := testRect.origin.x+testRect.size.width;
				inc(part)
			end;
		end;
		testRect.origin.y := testRect.origin.y + testRect.size.height;
		rows := rows + inData.dayNameRowRatio;
	end;

	testRect.origin.x := inBounds.origin.x;
	testRect.size.height := Round( rowHeight * inData.dayRowRatio );
  testRect.size.width := inBounds.size.width;

  dx := Trunc(inWhere.x / (inBounds.size.width/7));
  dy := Trunc((inWhere.y - testRect.origin.y) / (rowHeight * inData.dayRowRatio));

	// Part is a calendar square?
  part := dy * 7 + dx - inData.firstDay + 1;
  if (part < 0) or (part > inData.daysInMonth) then
    part := kControlNoPart;

	Result := part;
end;



// -----------------------------------------------------------------------------
//	CalendarViewHitTest
// -----------------------------------------------------------------------------
//

function CalendarViewHitTest(inEvent : EventRef; 
  const inData: CalendarViewData): OSStatus;
var
  bounds  :	HIRect;
  where   : HIPoint;
	part    : ControlPartCode;
begin
	Result := GetEventParameter( inEvent, kEventParamMouseLocation, typeHIPoint,
			nil, sizeof( HIPoint ), nil, @where );
  if Result <> noErr then Exit;

	HIViewGetBounds( inData.view, bounds );

	part := FindPart(bounds, where, inData );

	Result := SetEventParameter( inEvent, kEventParamControlPart, typeControlPartCode,
			sizeof( ControlPartCode ), @part ); 
end;

// -----------------------------------------------------------------------------
//	CalendarViewTrack
// -----------------------------------------------------------------------------
//
function CalendarViewTrack(inEvent : EventRef; var inData: CalendarViewData ): OSStatus;
var
	bounds      : HIRect;
	where       : HIPoint;
	part        : ControlPartCode;
	lastPart    : ControlPartCode;
	startHilite : ControlPartCode;
	qdPt        : Point;
	mouseResult : MouseTrackingResult;
	portPixMap  : PixMapHandle;
  tempTime    : CFAbsoluteTime;
	dateChange  : CFGregorianUnits;
begin
	Result := GetEventParameter( inEvent, kEventParamMouseLocation, typeHIPoint,
			niL, sizeof( HIPoint ), nil, @where );
  if Result <> noErr then Exit;

	Result := HIViewGetBounds( inData.view, bounds );
	
	startHilite := GetControlHilite( inData.view );
	
	lastPart := FindPart( bounds, where, inData );
	HiliteControl( inData.view, lastPart );
	
	// Need the port's pixMap's bounds to convert the point
	portPixMap := GetPortPixMap( GetWindowPort( GetControlOwner( inData.view ) ) );

	while ( true ) do
  begin
		part := FindPart( bounds, where, inData );
		if ( lastPart <> part ) then
			HiliteControl( inData.view, part );
		lastPart := part;

    //TODO!!!!!!
		Result := TrackMouseLocation( GrafPtr(-1), qdPt, mouseResult );

		// Need to convert from global
		QDGlobalToLocalPoint( GetWindowPort( GetControlOwner( inData.view ) ), qdPt );
		where.x := qdPt.h - portPixMap^^.bounds.left;
		where.y := qdPt.v - portPixMap^^.bounds.top;
		HIViewConvertPoint( where, nil, inData.view );

		// bail out when the mouse is released
		if ( mouseResult = kMouseTrackingMouseReleased ) then Break;
	end;
	
	// If a day wasn't clicked, revert the highlight to the last highlit day
	if (lastPart < 1) and (lastPart > inData.daysInMonth ) then
		HiliteControl( inData.view, startHilite )
  else if (lastPart >= 1) and (lastPart <= inData.daysInMonth ) then begin
    inData.selDay := lastPart;
   	HIViewSetNeedsDisplay( inData.view, true );
  end;


	if ( lastPart >= kCalendarPreviousYearPart) and (lastPart <= kCalendarNextYearPart ) then
  begin
    FillChar(dateChange, sizeof(dateChange), 0);		

		tempTime := CFGregorianDateGetAbsoluteTime( inData.date, inData.timeZone );

		case lastPart of 
			kCalendarPreviousYearPart:	dateChange.years := -1;
			kCalendarPreviousMonthPart: dateChange.months := -1;
			kCalendarNextMonthPart:    	dateChange.months := 1;
			kCalendarNextYearPart:      dateChange.years := 1;
		end;

		tempTime := CFAbsoluteTimeAddGregorianUnits( tempTime, inData.timeZone, dateChange );
		inData.date := CFAbsoluteTimeGetGregorianDate( tempTime, inData.timeZone );
		
		SetUpDateData( inData );

    if inData.selDay > inData.daysInMonth then
      inData.selDay := inData.daysInMonth;

   	HIViewSetNeedsDisplay( inData.view, true );
  end;

	Result := SetEventParameter( inEvent, kEventParamControlPart, typeControlPartCode,	sizeof( ControlPartCode ), @part ); 
end;

// -----------------------------------------------------------------------------
//	CalendarViewChanged
// -----------------------------------------------------------------------------
//
function CalendarViewChanged(inEvent :EventRef; const inData: CalendarViewData): OSStatus;
begin
  //#pragma unused( inEvent )
	//Status			err = noErr;
	HIViewSetNeedsDisplay( inData.view, true );
	Result := noErr;
end;


// -----------------------------------------------------------------------------
//	CalendarViewGetData
// -----------------------------------------------------------------------------
//

function CalendarViewGetData(inEvent : EventRef; const inData: CalendarViewData): OSStatus;
var
	part    : ControlPartCode;
	tag     : OSType;
	ptr     : Pointer;
	sz      : Size;
	outSize : Size;
begin
	Result := GetEventParameter( inEvent, kEventParamControlPart, typeControlPartCode,	nil, sizeof( ControlPartCode ), nil, @part );
  if Result <> noErr then Exit;

	Result := GetEventParameter( inEvent, kEventParamControlDataTag, typeEnumeration, nil, sizeof( OSType ), nil, @tag );
  if Result <> noErr then Exit;

	Result := GetEventParameter( inEvent, kEventParamControlDataBuffer, typePtr, nil, sizeof( Ptr ), nil, @ptr );
  if Result <> noErr then Exit;

	Result := GetEventParameter( inEvent, kEventParamControlDataBufferSize, typeLongInteger, nil, sizeof( Size ), nil, @sz );
	if Result <> noErr then Exit;

	case tag of
		kControlCalendarTitleRatioTag:
    begin
			if sz = sizeof( single ) then
				PSingle(ptr)^ := inData.titleRowRatio
			else
				Result := errDataSizeMismatch;
			outSize := sizeof( single);
		end;

		kControlCalendarDayNameRatioTag:
    begin
    	if sz = sizeof( single ) then
				PSingle(ptr)^ := inData.dayNameRowRatio
			else
				Result := errDataSizeMismatch;
			outSize := sizeof( single );
    end;

		kControlCalendarDayRatioTag:
    begin
			if ( sz = sizeof( single ) ) then
				PSingle(ptr)^ := inData.dayRowRatio
			else
				Result := errDataSizeMismatch;
			outSize := sizeof( single );
		end;

		kControlCalendarDateTag:
    begin
			if sz = sizeof( CFGregorianDate ) then begin
				CFGregorianDatePtr(ptr)^ := inData.date;
        if (inData.selDay>0) and (inData.selDay<inData.daysInMonth) then
          CFGregorianDatePtr(ptr)^.day := inData.selDay;
			end else
				Result := errDataSizeMismatch;
			outSize := sizeof( CFGregorianDate );
		end;

		kControlCalendarDrawProcTag:
    begin
			if sz =sizeof( CalendarDrawProc ) then
        CalendarDrawProc(Ptr^):=inData.drawProc
			else
				Result := errDataSizeMismatch;
			outSize := sizeof( CalendarDrawProc );
		end;

		kControlCalendarLabelProcTag:
    begin
			if sz = sizeof( CalendarDrawProc ) then
        CalendarDrawProc(Ptr^):=inData.labelProc
			else
				Result := errDataSizeMismatch;
			outSize := sizeof( CalendarDrawProc );
		end;

  else
  	Result := errDataNotSupported;
		outSize := 0;
	end;

	if ( Result = noErr ) then
		Result := SetEventParameter( inEvent, kEventParamControlDataBufferSize, typeLongInteger, sizeof( sz ), @outSize );
end;

// -----------------------------------------------------------------------------
//	CalendarViewSetData
// -----------------------------------------------------------------------------
//

function CalendarViewSetData(inEvent: EventRef;	var inData : CalendarViewData): OSStatus;
var
	part  : ControlPartCode;
	tag   : OSType;
	ptr   : Pointer;
	sz    : Size;
begin
	Result := GetEventParameter( inEvent, kEventParamControlPart, typeControlPartCode,	nil, sizeof( ControlPartCode ), nil, @part );
	if Result <> noErr then Exit;

	Result := GetEventParameter( inEvent, kEventParamControlDataTag, typeEnumeration,	nil, sizeof( OSType ), nil, @tag );
	if Result <> noErr then Exit;

	Result := GetEventParameter( inEvent, kEventParamControlDataBuffer, typePtr, nil, sizeof( Ptr ), nil, @ptr );
	if Result <> noErr then Exit;

	Result := GetEventParameter( inEvent, kEventParamControlDataBufferSize, typeLongInteger, nil, sizeof( sz ), nil, @sz );
	if Result <> noErr then Exit;

	case tag of
		kControlCalendarTitleRatioTag:
			if sz = sizeof( single ) then
        inData.titleRowRatio := PSingle(ptr)^
			else
				Result := errDataSizeMismatch;

		kControlCalendarDayNameRatioTag:
			if sz = sizeof( single ) then
				inData.dayNameRowRatio := PSingle(ptr)^
			else
			  Result := errDataSizeMismatch;

		kControlCalendarDayRatioTag:
			if  sz = sizeof( single ) then
				inData.dayRowRatio := PSingle(ptr )^
			else
				Result := errDataSizeMismatch;

		kControlCalendarDateTag:
			if sz = sizeof(CFGregorianDate ) then
      begin
				inData.date.year :=  CFGregorianDatePtr(ptr)^.year;
				inData.date.month := CFGregorianDatePtr(ptr)^.month;
        inData.selDay := CFGregorianDatePtr(ptr)^.day;
				SetUpDateData( inData );
				HIViewSetNeedsDisplay( inData.view, true );
			end
      else
				Result := errDataSizeMismatch;

		kControlCalendarDrawProcTag:
			if sz = sizeof( CalendarDrawProc) then
				inData.drawProc := CalendarDrawProc(ptr^)
			else
				Result := errDataSizeMismatch;

		kControlCalendarLabelProcTag:
			if sz = sizeof( CalendarDrawProc) then
				inData.labelProc := CalendarDrawProc(ptr^)
			else
				Result := errDataSizeMismatch;
	else
	  Result := errDataNotSupported;
	end;

  if (Result = noErr) and (inData.view<>nil) then
    HIViewSetNeedsDisplay(inData.view, true);
end;


// -----------------------------------------------------------------------------
//	CalendarViewGetRegion
// -----------------------------------------------------------------------------
//

function CalendarViewGetRegion(inEvent: EventRef; const inData : CalendarViewData): OSStatus;
var
	part     : ControlPartCode;
  outShape : HIShapeRef;
  bounds   : HIRect;
begin
	Result := GetEventParameter( inEvent, kEventParamControlPart, typeControlPartCode, nil, sizeof( ControlPartCode ), nil, @part );
	if Result <> noErr then Exit;

  HIViewGetBounds(inData.view, bounds);
  outShape:=HIShapeCreateWithRect(bounds);
  Result := SetEventParameter(inEvent, kEventParamShape, typeHIShapeRef, sizeof(HIShapeRef), @outShape);
  CFRelease(outShape);
end;

// -----------------------------------------------------------------------------
//	CalendarViewHandler
// -----------------------------------------------------------------------------
//	This is the bottleneck for incoming events

function CalendarViewHandler(	inCallRef : EventHandlerCallRef;
  inEvent : EventRef; inUserData : Pointer): OSStatus; mwpascal;
var
	err : OSStatus;
	eventClass : UInt32;
	eventKind  : UInt32;
	data       : CalendarViewDataPtr;
begin
	err := eventNotHandledErr;
  eventClass := GetEventClass( inEvent );
	eventKind := GetEventKind( inEvent );
	data := CalendarViewDataPtr(inUserData);


	case ( eventClass ) of
		kEventClassHIObject:
			case eventKind of
				kEventHIObjectConstruct: 	err := CalendarViewConstruct( inEvent );
				kEventHIObjectInitialize: err := CalendarViewInitialize( inCallRef, inEvent, data^ );
				kEventHIObjectDestruct:  	err := CalendarViewDestruct( inEvent, data ); // don't CallNextEventHandler!
			end;
		kEventClassControl:
			case eventKind of
				kEventControlInitialize:	err := noErr;
				kEventControlDraw:        err := CalendarViewDraw( inEvent, data^ );
				kEventControlHitTest:	    err := CalendarViewHitTest( inEvent, data^ );
				kEventControlTrack:  			err := CalendarViewTrack( inEvent, data^ );
				kEventControlValueFieldChanged,
				kEventControlHiliteChanged:	err := CalendarViewChanged( inEvent, data^ );
				kEventControlGetData:     	err := CalendarViewGetData( inEvent, data^ );
				kEventControlSetData:			  err := CalendarViewSetData( inEvent, data^ );
				kEventControlGetPartRegion: err := CalendarViewGetRegion( inEvent, data^ );
		 end;
	end;
	Result := err;
end;


// -----------------------------------------------------------------------------
//	CalendarViewRegister
// -----------------------------------------------------------------------------
//

var
  sCalendarViewClassRef : HIObjectClassRef = nil;

function CalendarViewRegister: OSStatus;
var
	err : OSStatus;
const
  eventList : array [0..11] of EventTypeSpec = (
    ( eventClass: kEventClassHIObject; eventKind: kEventHIObjectConstruct ),
    ( eventClass: kEventClassHIObject; eventKind: kEventHIObjectInitialize ),
		( eventClass: kEventClassHIObject; eventKind: kEventHIObjectDestruct ),
		( eventClass:	kEventClassControl;  eventKind: kEventControlInitialize ),
		( eventClass:	kEventClassControl;  eventKind: kEventControlDraw ),
		( eventClass:	kEventClassControl;  eventKind: kEventControlHitTest ),
		( eventClass:	kEventClassControl;  eventKind: kEventControlTrack ),
		( eventClass:	kEventClassControl;  eventKind: kEventControlValueFieldChanged ),
		( eventClass:	kEventClassControl;  eventKind: kEventControlHiliteChanged ),
		( eventClass:	kEventClassControl;  eventKind: kEventControlGetData ),
		( eventClass:	kEventClassControl;  eventKind: kEventControlSetData ),
		( eventClass:	kEventClassControl;  eventKind: kEventControlGetPartRegion )
  );
begin
  err := noErr;
	if not Assigned(sCalendarViewClassRef) then begin
		err := HIObjectRegisterSubclass(
			kCalendarViewClassID,		// class ID
			_kHIViewClassID,				// base class ID
			0,						          // option bits
			CalendarViewHandlerUPP,		// construct proc
			length( eventList ),
			@eventList,
			nil,						// construct data,
			@sCalendarViewClassRef );
	end;
	Result:=err;
end;

// -----------------------------------------------------------------------------
//	CalendarViewCreate
// -----------------------------------------------------------------------------
//

function CalendarViewCreate(inWindow : WindowRef; const inBounds : Rect; var outControl: ControlRef): OSStatus;
var
	root  : ControlRef;
	event : EventRef;
const
  BounName : PChar = 'Boun';
begin
	// Make sure this type of view is registered
	Result := CalendarViewRegister;
  if Result <> noErr then Exit;

	// Make the initialization event
	Result := CreateEvent(nil, kEventClassHIObject, kEventHIObjectInitialize, GetCurrentEventTime(), 0, event );
  if Result <> noErr then Exit;
  try
	// Set the bounds into the event
  	Result := SetEventParameter( event, EventParamNamePtr(BounName)^, typeQDRectangle,	sizeof( Rect ), @inBounds );
  	if Result <> noErr then Exit;

  	Result := HIObjectCreate( kCalendarViewClassID, event, outControl );
    if Result <> noErr then Exit;

  	// Get the content root
  	Result := GetRootControl( inWindow, root );
    if Result <> noErr then Exit;

    // - added -
    //HIViewFindByID(root, kHIViewWindowContentID, root);

  	// And stick this view into it
  	Result := HIViewAddSubview( root, outControl );
  finally
    ReleaseEvent(event);
  end;
end;

procedure InitGlobals;
begin
  kCalendarViewClassID := CFSTR('com.apple.CalendarView');
  _kHIViewClassID := CFSTR('com.apple.hiview');

  CalendarViewHandlerUPP := NewEventHandlerUPP(@CalendarViewHandler);
	months[0]  := CFSTR('January');
	months[1]  := CFSTR('February');
	months[2]  := CFSTR('March');
	months[3]  := CFSTR('April');
	months[4]  := CFSTR('May');
	months[5]  := CFSTR('June');
	months[6]  := CFSTR('July');
	months[7]  := CFSTR('August');
	months[8]  := CFSTR('September');
	months[9]  := CFSTR('October');
	months[10] := CFSTR('November');
	months[11] := CFSTR('December');

  dow[0] := CFSTR('Su');
  dow[1] := CFSTR('Mo');
  dow[2] := CFSTR('Tu');
  dow[3] := CFSTR('We');
  dow[4] := CFSTR('Th');
  dow[5] := CFSTR('Fr');
  dow[6] := CFSTR('Sa');
end;

procedure ReleaseGlobals;
begin
  DisposeEventHandlerUPP(CalendarViewHandlerUPP);
end;

function isValidCalendarControl(Calendar: ControlRef): Boolean;
begin
  Result := Assigned(Calendar) and HIObjectIsOfClass(Calendar, kCalendarViewClassID);
end;

function CalendarGetDate(Calendar: ControlRef; var Date: CFGregorianDate): Boolean;
begin
  Result := isValidCalendarControl(Calendar);
  if not Result then Exit;
  Result := GetControlData(Calendar, kControlEntireControl,
    kControlCalendarDateTag, sizeof(Date), @Date, nil) = noErr;
end;

function CalendarSetDate(Calendar: ControlRef; const Date: CFGregorianDate): Boolean;
begin
  Result := isValidCalendarControl(Calendar);
  if not Result then Exit;
  Result := SetControlData(Calendar, kControlEntireControl, kControlCalendarDateTag, sizeof(Date), @Date) = noErr;
end;


initialization
  InitGlobals;

finalization
  ReleaseGlobals;

end.

