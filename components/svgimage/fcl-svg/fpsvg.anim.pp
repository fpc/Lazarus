{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    SMIL timeline: animation elements, interpolation and seeking.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.anim;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}
{$modeswitch advancedrecords}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Math, fpsvg.types, fpsvg.dom, fpsvg.path,
     fpsvg.read, fpsvg.geom;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, math, fpsvg.types, fpsvg.dom, fpsvg.path, fpsvg.read,
     fpsvg.geom;
{$ENDIF FPC_DOTTEDUNITS}

const
  { Maximum timestamps ChangeTimes creates.}
  SVGMaxChangeTimes = 4096;
  { Seconds in a day }
  SVGSecondsPerDay = 86400;

type
  ESVGAnim = class(ESVGError);

  TSVGAnimationKind = (akSet, akAnimate, akColour, akTransform, akMotion);
  TSVGCalcMode = (cmDiscrete, cmLinear, cmPaced, cmSpline);
  TSVGValueKind = (vkDiscrete, vkNumber, vkLength, vkColour, vkNumberList,
                   vkPath);
  TSVGTransformKind = (tkTranslate, tkScale, tkRotate, tkSkewX, tkSkewY);
  { How a motion animation turns what it moves: by a fixed angle, or with a path. }
  TSVGMotionRotate = (mrAngle, mrAuto, mrAutoReverse);

  { Meaning of a begin or end list:
    a time of its own,
    a time taken from another animation,
    a moment of the wall clock. }
  TSVGTimeEntryKind = (teOffset, teSyncBegin, teSyncEnd, teSyncRepeat,
                       teWallClock);

  { One entry of a begin or end list. }
  TSVGTimeEntry = record
    Kind   : TSVGTimeEntryKind;
    // the id of a sync entry
    Base   : TSVGString;
    // seconds, added to the 'when' ?
    Offset : Double;
    // the run a repeat entry waits for
    Count  : Double;
    // a timestamp 
    When   : TDateTime;         
  end;
  TSVGTimeEntryArray = array of TSVGTimeEntry;

  { Restart policy: does a begin restart an active animation. }
  TSVGRestart = (rsAlways, rsWhenNotActive, rsNever);

  { One animation element, resolved against its target. }
  TSVGAnimation = record
    Element       : TSVGElement;       // the animation element itself
    Target        : TSVGElement;
    Kind          : TSVGAnimationKind;
    AttributeName : TSVGString;
    ValueKind     : TSVGValueKind;
    Base          : TSVGString;            // the value the file was read with
    HasBase       : Boolean;           // False when the attribute was absent
    Values        : TStringArray;      // from, to and by folded into this
    KeyTimes      : TSVGDoubleArray;
    KeySplines    : TSVGDoubleArray;   // four numbers per interval
    CalcMode      : TSVGCalcMode;
    Begins        : TSVGDoubleArray;   // the times it becomes active
    Ends          : TSVGDoubleArray;   // the times of its end attribute
    StartsAt      : Double;            // the first of Begins, in seconds
    Duration      : Double;            // negative for indefinite
    Repeats       : Double;            // negative for indefinite
    RepeatDur     : Double;            // 0 when absent, negative for indefinite
    MinDuration   : Double;            // 0 when absent
    MaxDuration   : Double;            // 0 when absent
    Restart       : TSVGRestart;
    Freezes       : Boolean;           // fill="freeze"
    Additive      : Boolean;           // additive="sum"
    Accumulates   : Boolean;           // accumulate="sum"
    TransformKind : TSVGTransformKind; // the function an akTransform writes
    Motion        : TSVGPathMetrics;   // the path an akMotion runs along
    HasMotionPath : Boolean;           // False when it runs over its values
    Rotate        : TSVGMotionRotate;
    RotateAngle   : Double;            // degrees, when Rotate is mrAngle
    // The value this animation gives at that time
    function ValueAt(aSeconds: Double; out aValue: TSVGString): Boolean;
    // Where the last active interval ends. Negative when it never ends.
    function EndsAt: Double;
    // How long the runs of the animation last. Negative when they never end.
    function RepeatDuration: Double;
    // How long the interval beginning at one of the begin times stays active:  Negative when it never ends.
    function ActiveDuration(aIndex: Integer): Double;
    // The same, for an interval beginning at any time.
    function ActiveDurationFrom(aBegin: Double): Double;
    // The interval that covers a time. False when no interval has begun by then.
    function IntervalAt(aSeconds: Double; out aBegin,
      aActive: Double): Boolean;
    // The time of one of the values, from 0 to 1 through one run.
    function KeyTimeAt(aIndex: Integer): Double;
  private
    // The value the list gives a fraction of the way through one run.
    function ValueOfProgress(aProgress: Double): TSVGString;
    // The value with the currentColor keyword read as the colour it stands for on the target.
    function ColourValue(const aValue: TSVGString): TSVGString;
    // Return the interval of the key times a progress falls in. aLocal returns how far into it, from 0 to 1. Returns the last interval when it is past the end.
    function SegmentAt(aProgress: Double; out aLocal: Double): Integer;
    // The transform a motion value represents.
    function MotionTransformOf(const aValue: TSVGString; aProgress: Double): TSVGString;
  end;
  TSVGAnimationArray = array of TSVGAnimation;

  { The animations of one document, and the clock that drives them. }
  TSVGTimeline = class(TObject)
  private
    FDocument: TSVGDocument;
    FAnimations: TSVGAnimationArray;
    FCount: Integer;
    FDuration: Double;
    FWallClockNow: TDateTime;
    function GetAnimation(aIndex: Integer): TSVGAnimation;
    function GetIsAnimated: Boolean;
    procedure Collect(aElement: TSVGElement);
    procedure AddElement(aElement: TSVGElement);
    procedure WriteBase(const aAnimation: TSVGAnimation);
    function IndexOfID(const aID: TSVGString): Integer;
    function EntryTime(const aEntry: TSVGTimeEntry; out aTime: Double): Boolean;
    function SettleTiming(aIndex: Integer; const aBeginEntries,
      aEndEntries: TSVGTimeEntryArray): Boolean;
    procedure ResolveTiming;
    procedure ApplyRestart;
    procedure DropUntimed;
    function IsGroupHead(aIndex: Integer): Boolean;
    function GroupValue(aIndex: Integer; aSeconds: Double;
      out aValue: TSVGString): Boolean;
  public
    // Reads the animations of a document, in document order, and the values its attributes were read with
    constructor Create(aDocument: TSVGDocument); overload;
    // The same, with the moment the document clock starts at.
    constructor Create(aDocument: TSVGDocument;  aWallClockNow: TDateTime); overload;
    // Seek to a moment in the timeline.
    procedure Seek(aSeconds: Double);
    // Puts every animated attribute back to the value it was read with.
    procedure Reset;
    // The times at which anything changes in increasing order. At most SVGMaxChangeTimes elements.
    function ChangeTimes: TSVGDoubleArray;
    // The document the animations were read from.
    property Document: TSVGDocument read FDocument;
    // The moment the document clock starts at, zero when the timeline was
    // built without one.
    property WallClockNow: TDateTime read FWallClockNow;
    // True when the document declares an animation at all.
    property IsAnimated: Boolean read GetIsAnimated;
    // Where the last animation ends. Negative when one never ends.
    property Duration: Double read FDuration;
    // Number of animations the document declares.
    property Count: Integer read FCount;
    // One of the animations, in document order.
    property Animations[aIndex: Integer]: TSVGAnimation read GetAnimation;
  end;

// The value a fraction of the way from one to the other.
function SVGInterpolate(aKind: TSVGValueKind; const aFrom, aTo: TSVGString;
  aAt: Double): TSVGString;
// The kind of value an attribute contains.
function SVGValueKindOf(const aAttribute: TSVGString): TSVGValueKind;
// The colour the currentColor keyword stands for on an element
function SVGCurrentColorOf(aElement: TSVGElement): TSVGColor;
// Reads an SMIL clock value: 3, 3s, 250ms, 01:30, 1.5min includingleading plus or minus. Returns false when the text is not a clock value.
function TryStrToSVGClock(const aText: TSVGString; out aSeconds: Double): Boolean;
// Reads one entry of a begin or end list
function TryStrToSVGTimeEntry(const aText: TSVGString; out aEntry: TSVGTimeEntry): Boolean;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ELSE FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ENDIF FPC_DOTTEDUNITS}

type
  { One command of a path: its letter, and the numbers after it. }
  TSVGPathStep = record
    Command : AnsiChar;
    Args    : TSVGDoubleArray;
  end;
  TSVGPathStepArray = array of TSVGPathStep;

const
  { The attributes whose values can interpolate }
  ColourKeyword = 'currentColor';
  ColorAttribute = 'color';
  ColourAttributes = ';color;fill;flood-color;lighting-color;stop-color;stroke;';
  LengthAttributes =
    ';baseline-shift;cx;cy;dx;dy;font-size;fx;fy;height;'
    + 'letter-spacing;markerHeight;markerWidth;offset;r;refX;refY;rx;ry;'
    + 'startOffset;stroke-dashoffset;stroke-width;textLength;width;'
    + 'word-spacing;x;x1;x2;y;y1;y2;';
  { The <number> of the filter primitives }
  NumberAttributes = ';amplitude;azimuth;bias;diffuseConstant;divisor;'
    + 'elevation;exponent;fill-opacity;flood-opacity;intercept;k1;k2;k3;'
    + 'k4;limitingConeAngle;opacity;pathLength;pointsAtX;pointsAtY;'
    + 'pointsAtZ;scale;seed;slope;specularConstant;specularExponent;'
    + 'stop-opacity;stroke-miterlimit;stroke-opacity;surfaceScale;z;';
  XLinkPrefix = 'xlink:';
  ListAttributes = ';baseFrequency;filterRes;kernelMatrix;'
    + 'kernelUnitLength;order;points;radius;rotate;stdDeviation;'
    + 'stroke-dasharray;tableValues;values;viewBox;';
  PathAttributes = ';d;';

  TransformNames: array[TSVGTransformKind] of TSVGString =
    ('translate', 'scale', 'rotate', 'skewX', 'skewY');
  { How many arguments a transform function expects }
  TransformArity: array[TSVGTransformKind] of Integer = (2, 2, 3, 1, 1);

// The value a fraction of the way between two numbers.
function Interpolate(aFrom, aTo, aAt: Double): Double;

begin
  Result := aFrom + (aTo - aFrom) * aAt;
end;


// The value of a property in an inline style
function InlineStyleValue(const aStyle, aName: TSVGString): TSVGString;

var
  lAt, lStop, lColon: Integer;
  lPart: TSVGString;

begin
  Result := '';
  lAt := 1;
  while lAt <= Length(aStyle) do
    begin
    lStop := lAt;
    while (lStop <= Length(aStyle)) and (aStyle[lStop] <> ';') do
      Inc(lStop);
    lPart := Copy(aStyle, lAt, lStop - lAt);
    lColon := Pos(':', lPart);
    if (lColon > 0)
       and SameText(Trim(Copy(lPart, 1, lColon - 1)), aName) then
      Result := Trim(Copy(lPart, lColon + 1, Length(lPart)));
    lAt := lStop + 1;
    end;
end;


function SVGCurrentColorOf(aElement: TSVGElement): TSVGColor;

var
  lNode: TSVGElement;
  lText: TSVGString;
  lColour: TSVGColor;

begin
  Result := TSVGColor.Black;
  lNode := aElement;
  while lNode <> nil do
    begin
    lText := Trim(InlineStyleValue(lNode.AttributeDef('style', ''),
      ColorAttribute));
    if lText = '' then
      lText := Trim(lNode.AttributeDef(ColorAttribute, ''));
    if (lText <> '') and not SameText(lText, ColourKeyword)
       and not SameText(lText, 'inherit') then
      begin
      lColour := TSVGColor.Transparent;
      if lColour.TryParse(lText) then
        Exit(lColour);
      end;
    lNode := lNode.Parent;
    end;
end;


// True when two lists of lengths have an equal amount of elements.
function SameLengthUnits(const aFirst, aSecond: TSVGLengthArray): Boolean;

var
  I: Integer;

begin
  Result := False;
  if (Length(aFirst) = 0) or (Length(aFirst) <> Length(aSecond)) then
    Exit;
  for I := 0 to High(aFirst) do
    if aFirst[I].LengthUnit <> aSecond[I].LengthUnit then
      Exit;
  Result := True;
end;


// The lengths written out, separated by spaces.
function LengthListToText(const aValues: TSVGLengthArray): TSVGString;

var
  I: Integer;

begin
  Result := '';
  for I := 0 to High(aValues) do
    begin
    if I > 0 then
      Result := Result + ' ';
    Result := Result + aValues[I].ToString;
    end;
end;


// Number of arguments for a path command, -1 when the letter is not one.
function PathArgCount(aCommand: AnsiChar): Integer;

begin
  case UpCase(aCommand) of
    'Z': Result := 0;
    'H', 'V': Result := 1;
    'M', 'L', 'T': Result := 2;
    'S', 'Q': Result := 4;
    'C': Result := 6;
    'A': Result := 7;
  else
    Result := -1;
  end;
end;


// True when the argument at that index of an arc is one of its flags.
function IsArcFlag(aCommand: AnsiChar; aIndex: Integer): Boolean;

begin
  Result := (UpCase(aCommand) = 'A') and (aIndex >= 3) and (aIndex <= 4);
end;


// The commands of a d attribute, each with the numbers after it. False when the text is not the path grammar.
function TryReadPathSteps(const aText: TSVGString;
  out aSteps: TSVGPathStepArray): Boolean;

var
  lScanner: TSVGScanner;
  lCommand: AnsiChar;
  lCount, lArgs, I: Integer;
  lValue: Double;
  lFlag: Boolean;

begin
  Result := False;
  aSteps := nil;
  lCount := 0;
  lCommand := #0;
  lScanner := TSVGScanner.Create(aText);
  SetLength(aSteps, 8);
  repeat
    lScanner.SkipWSPComma;
    if lScanner.AtEnd then
      Break;
    if PathArgCount(lScanner.Current) >= 0 then
      begin
      lCommand := lScanner.Current;
      Inc(lScanner.Pos);
      end
    else if lCommand = #0 then
      Exit
    else if UpCase(lCommand) = 'M' then
      // The numbers after a moveto draw lines, of the same hand as it.
      lCommand := AnsiChar(Ord(lCommand) - Ord('M') + Ord('L'))
    else if PathArgCount(lCommand) = 0 then
      Exit;
    lArgs := PathArgCount(lCommand);
    if lCount = Length(aSteps) then
      SetLength(aSteps, lCount * 2);
    aSteps[lCount].Command := lCommand;
    SetLength(aSteps[lCount].Args, lArgs);
    for I := 0 to lArgs - 1 do
      begin
      lScanner.SkipWSPComma;
      if IsArcFlag(lCommand, I) then
        begin
        if not lScanner.ScanFlag(lFlag) then
          Exit;
        if lFlag then
          aSteps[lCount].Args[I] := 1
        else
          aSteps[lCount].Args[I] := 0;
        end
      else
        begin
        if not lScanner.ScanNumber(lValue) then
          Exit;
        aSteps[lCount].Args[I] := lValue;
        end;
      end;
    Inc(lCount);
  until False;
  SetLength(aSteps, lCount);
  Result := lCount > 0;
end;


// True when two paths hold the same commands in the same order.
function SamePathSteps(const aFirst, aSecond: TSVGPathStepArray): Boolean;

var
  I: Integer;

begin
  Result := False;
  if Length(aFirst) <> Length(aSecond) then
    Exit;
  for I := 0 to High(aFirst) do
    if aFirst[I].Command <> aSecond[I].Command then
      Exit;
  Result := Length(aFirst) > 0;
end;


// The commands written back out, each with its numbers after it.
function PathStepsToText(const aSteps: TSVGPathStepArray): TSVGString;

var
  I, J: Integer;

begin
  Result := '';
  for I := 0 to High(aSteps) do
    begin
    if I > 0 then
      Result := Result + ' ';
    Result := Result + aSteps[I].Command;
    for J := 0 to High(aSteps[I].Args) do
      Result := Result + ' ' + SVGFormatFloat(aSteps[I].Args[J]);
    end;
end;


// The numbers written out, separated by spaces.
function FormatNumbers(const aValues: TSVGDoubleArray): TSVGString;

var
  I: Integer;

begin
  Result := '';
  for I := 0 to High(aValues) do
    begin
    if I > 0 then
      Result := Result + ' ';
    Result := Result + SVGFormatFloat(aValues[I]);
    end;
end;


function SVGTransformKindOf(const aType: TSVGString): TSVGTransformKind;

var
  lName: TSVGString;
  K: TSVGTransformKind;

begin
  Result := tkTranslate;
  lName := Trim(aType);
  for K := Low(TSVGTransformKind) to High(TSVGTransformKind) do
    if lName = TransformNames[K] then
      Exit(K);
end;


// The arguments of one transform value, filled out to the count the function expects.
// Empty when the text is not a list of numbers of a length the function expects.
function TransformArguments(aKind: TSVGTransformKind;
  const aText: TSVGString): TSVGString;

var
  lValues: TSVGDoubleArray;

begin
  Result := '';
  if not TryStrToSVGNumberList(aText, lValues) then
    Exit;
  if Length(lValues) = 1 then
    case aKind of
      tkTranslate:
        begin
        SetLength(lValues, 2);
        lValues[1] := 0;
        end;
      tkScale:
        begin
        SetLength(lValues, 2);
        lValues[1] := lValues[0];
        end;
      tkRotate:
        begin
        SetLength(lValues, 3);
        lValues[1] := 0;
        lValues[2] := 0;
        end;
    end;
  if Length(lValues) <> TransformArity[aKind] then
    Exit;
  Result := FormatNumbers(lValues);
end;


// The arguments of a transform function at rest
function TransformZero(aKind: TSVGTransformKind): TSVGString;

var
  lValues: TSVGDoubleArray;
  I: Integer;

begin
  SetLength(lValues, TransformArity[aKind]);
  for I := 0 to High(lValues) do
    lValues[I] := 0;
  Result := FormatNumbers(lValues);
end;


// A 16-bit channel rounded to the nearest 8-bit value.
function ChannelByte(aValue: Double): Byte;

begin
  Result := Round(SVGClamp(aValue / 257, 0, 255));
end;


// The colour a fraction of the way from one to the other.
function BlendColors(const aFrom, aTo: TSVGColor; aAt: Double): TSVGColor;

begin
  Result := TSVGColor.FromBytes(
    ChannelByte(Interpolate(aFrom.Red, aTo.Red, aAt)),
    ChannelByte(Interpolate(aFrom.Green, aTo.Green, aAt)),
    ChannelByte(Interpolate(aFrom.Blue, aTo.Blue, aAt)),
    ChannelByte(Interpolate(aFrom.Alpha, aTo.Alpha, aAt)));
end;


// Splits text on a separator, trimming every entry.
function SplitOn(const aText: TSVGString; aSeparator: AnsiChar): TStringArray;

var
  lCount, lStart, I: Integer;

begin
  Result := nil;
  if Trim(aText) = '' then
    Exit;
  SetLength(Result, 4);
  lCount := 0;
  lStart := 1;
  for I := 1 to Length(aText) + 1 do
    if (I > Length(aText)) or (aText[I] = aSeparator) then
      begin
      if lCount = Length(Result) then
        SetLength(Result, lCount * 2);
      Result[lCount] := Trim(Copy(aText, lStart, I - lStart));
      Inc(lCount);
      lStart := I + 1;
      end;
  SetLength(Result, lCount);
end;


// The entries of a semicolon separated attribute
function SplitValueList(const aText: TSVGString): TStringArray;

begin
  Result := SplitOn(aText, ';');
  while (Length(Result) > 0) and (Result[High(Result)] = '') do
    SetLength(Result, Length(Result) - 1);
end;


function TryStrToSVGClock(const aText: TSVGString; out aSeconds: Double): Boolean;

var
  lText, lUnit: TSVGString;
  lFields: TStringArray;
  lScanner: TSVGScanner;
  lSign, lValue, lScale: Double;
  I: Integer;

begin
  aSeconds := 0;
  Result := False;
  lText := Trim(aText);
  if lText = '' then
    Exit;
  lSign := 1;
  if lText[1] in ['+', '-'] then
    begin
    if lText[1] = '-' then
      lSign := -1;
    lText := Copy(lText, 2, Length(lText) - 1);
    if lText = '' then
      Exit;
    end;
  if Pos(':', lText) > 0 then
    begin
    lFields := SplitOn(lText, ':');
    if (Length(lFields) < 2) or (Length(lFields) > 3) then
      Exit;
    for I := 0 to High(lFields) do
      begin
      if not TryStrToSVGNumber(lFields[I], lValue) then
        Exit;
      if lValue < 0 then
        Exit;
      // Only the seconds of a clock value may have a fractional part.
      if (I < High(lFields)) and (Frac(lValue) <> 0) then
        Exit;
      aSeconds := aSeconds * 60 + lValue;
      end;
    aSeconds := aSeconds * lSign;
    Exit(True);
    end;
  lScanner := TSVGScanner.Create(lText);
  if not lScanner.ScanNumber(lValue) then
    Exit;
  lUnit := lScanner.ScanName;
  if not lScanner.AtEnd then
    Exit;
  if (lUnit = '') or (lUnit = 's') then
    lScale := 1
  else if lUnit = 'ms' then
    lScale := 0.001
  else if lUnit = 'min' then
    lScale := 60
  else if lUnit = 'h' then
    lScale := 3600
  else
    Exit;
  aSeconds := lSign * lValue * lScale;
  Result := True;
end;


// Convert a ISO 8601 date and time to a TDateTime value
function TryStrToSVGWallClock(const aText: TSVGString;
  out aWhen: TDateTime): Boolean;

var
  lText: TSVGString;
  lNumbers: TSVGDoubleArray;
  lDate, lTime, lZone: TDateTime;
  lAt, lSign, lYear, lMonth, lDay: Integer;
  lSeconds: Double;

begin
  aWhen := 0;
  Result := False;
  lText := Trim(aText);
  lZone := 0;
  lSign := 0;
  // The zone comes last, and is read off before the rest.
  if (lText <> '') and (UpCase(lText[Length(lText)]) = 'Z') then
    SetLength(lText, Length(lText) - 1)
  else
    begin
    lAt := Length(lText);
    while (lAt > 1) and not (lText[lAt] in ['+', '-']) do
      Dec(lAt);
    // A date is written with hyphens of its own, so a sign counts as a
    // zone only where a time has been written before it.
    if (lAt > 1) and (lText[lAt] in ['+', '-']) and (Pos('T', lText) > 0)
       and (lAt > Pos('T', lText)) then
      begin
      if lText[lAt] = '-' then
        lSign := -1
      else
        lSign := 1;
      if not TryStrToSVGNumberList(StringReplace(
               Copy(lText, lAt + 1, Length(lText)), ':', ' ', [rfReplaceAll]),
               lNumbers) or (Length(lNumbers) <> 2) then
        Exit;
      lZone := lSign * (lNumbers[0] / 24 + lNumbers[1] / (24 * 60));
      SetLength(lText, lAt - 1);
      end;
    end;
  lAt := Pos('T', lText);
  lTime := 0;
  if lAt > 0 then
    begin
    if not TryStrToSVGNumberList(StringReplace(
             Copy(lText, lAt + 1, Length(lText)), ':', ' ', [rfReplaceAll]),
             lNumbers)
       or (Length(lNumbers) < 2) or (Length(lNumbers) > 3) then
      Exit;
    lSeconds := 0;
    if Length(lNumbers) = 3 then
      lSeconds := lNumbers[2];
    if (lNumbers[0] < 0) or (lNumbers[0] > 24) or (lNumbers[1] < 0)
       or (lNumbers[1] > 59) or (lSeconds < 0) or (lSeconds >= 60) then
      Exit;
    lTime := (lNumbers[0] + lNumbers[1] / 60 + lSeconds / 3600) / 24;
    SetLength(lText, lAt - 1);
    end;
  if not TryStrToSVGNumberList(StringReplace(lText, '-', ' ',
           [rfReplaceAll]), lNumbers) or (Length(lNumbers) <> 3) then
    Exit;
  lYear := Round(lNumbers[0]);
  lMonth := Round(lNumbers[1]);
  lDay := Round(lNumbers[2]);
  if (lYear < 1) or (lYear > 9999) or (lMonth < 1) or (lMonth > 12)
     or (lDay < 1) or (lDay > 31) then
    Exit;
  if not TryEncodeDate(Word(lYear), Word(lMonth), Word(lDay), lDate) then
    Exit;
  aWhen := lDate + lTime - lZone;
  Result := True;
end;


function TryStrToSVGTimeEntry(const aText: TSVGString;
  out aEntry: TSVGTimeEntry): Boolean;

var
  lText, lTail: TSVGString;
  lAt, lStop: Integer;
  lSign: Double;

begin
  aEntry.Kind := teOffset;
  aEntry.Base := '';
  aEntry.Offset := 0;
  aEntry.Count := 0;
  aEntry.When := 0;
  Result := False;
  lText := Trim(aText);
  if lText = '' then
    Exit;
  lTail := '';
  lAt := Pos('.begin', lText);
  if lAt > 0 then
    begin
    aEntry.Kind := teSyncBegin;
    lTail := Copy(lText, lAt + 6, Length(lText));
    end
  else if Pos('.repeat(', lText) > 0 then
    begin
    lAt := Pos('.repeat(', lText);
    lStop := Pos(')', lText);
    if lStop < lAt + 8 then
      Exit;
    if not TryStrToSVGNumber(Copy(lText, lAt + 8, lStop - lAt - 8),
                             aEntry.Count) or (aEntry.Count < 1) then
      Exit;
    aEntry.Kind := teSyncRepeat;
    lTail := Copy(lText, lStop + 1, Length(lText));
    end
  else if Copy(lText, 1, 10) = 'wallclock(' then
    begin
    lStop := Pos(')', lText);
    if lStop < 11 then
      Exit;
    if not TryStrToSVGWallClock(Copy(lText, 11, lStop - 11), aEntry.When) then
      Exit;
    aEntry.Kind := teWallClock;
    Exit(True);
    end
  else
    begin
    lAt := Pos('.end', lText);
    if lAt = 0 then
      Exit(TryStrToSVGClock(lText, aEntry.Offset));
    aEntry.Kind := teSyncEnd;
    lTail := Copy(lText, lAt + 4, Length(lText));
    end;
  aEntry.Base := Copy(lText, 1, lAt - 1);
  if aEntry.Base = '' then
    Exit;
  lTail := Trim(lTail);
  if lTail = '' then
    Exit(True);
  lSign := 1;
  if lTail[1] = '-' then
    lSign := -1
  else if lTail[1] <> '+' then
    Exit;
  if not TryStrToSVGClock(Trim(Copy(lTail, 2, Length(lTail))),
                          aEntry.Offset) then
    Exit;
  aEntry.Offset := lSign * aEntry.Offset;
  Result := True;
end;


// The entries of a begin or end list that this can time
function ReadTimeEntries(aElement: TSVGElement; const aName: TSVGString;
  aDefaultZero: Boolean): TSVGTimeEntryArray;

var
  lTexts: TStringArray;
  lEntry: TSVGTimeEntry;
  lCount, I: Integer;

begin
  Result := nil;
  lTexts := SplitValueList(aElement.Attributes[aName]);
  if Length(lTexts) = 0 then
    begin
    if not aDefaultZero then
      Exit;
    SetLength(Result, 1);
    Result[0].Kind := teOffset;
    Result[0].Base := '';
    Result[0].Offset := 0;
    Exit;
    end;
  SetLength(Result, Length(lTexts));
  lCount := 0;
  for I := 0 to High(lTexts) do
    if TryStrToSVGTimeEntry(lTexts[I], lEntry) then
      begin
      Result[lCount] := lEntry;
      Inc(lCount);
      end;
  SetLength(Result, lCount);
end;


function SVGValueKindOf(const aAttribute: TSVGString): TSVGValueKind;

var
  lName: TSVGString;

begin
  lName := ';' + aAttribute + ';';
  if Pos(lName, ColourAttributes) > 0 then
    Result := vkColour
  else if Pos(lName, LengthAttributes) > 0 then
    Result := vkLength
  else if Pos(lName, NumberAttributes) > 0 then
    Result := vkNumber
  else if Pos(lName, ListAttributes) > 0 then
    Result := vkNumberList
  else if Pos(lName, PathAttributes) > 0 then
    Result := vkPath
  else
    Result := vkDiscrete;
end;


function SVGInterpolate(aKind: TSVGValueKind; const aFrom, aTo: TSVGString;
  aAt: Double): TSVGString;

var
  lFromNumber, lToNumber: Double;
  lFromLength, lToLength: TSVGLength;
  lFromColor, lToColor: TSVGColor;
  lFromList, lToList: TSVGDoubleArray;
  lFromLengths, lToLengths: TSVGLengthArray;
  lFromSteps, lToSteps: TSVGPathStepArray;
  I, J: Integer;

begin
  if aAt <= 0 then
    Exit(aFrom);
  if aAt >= 1 then
    Exit(aTo);
  case aKind of
    vkNumber:
      if TryStrToSVGNumber(aFrom, lFromNumber)
         and TryStrToSVGNumber(aTo, lToNumber) then
        Exit(SVGFormatFloat(Interpolate(lFromNumber, lToNumber, aAt)));
    vkLength:
      begin
      lFromLength := TSVGLength.Zero;
      lToLength := TSVGLength.Zero;
      if lFromLength.TryParse(aFrom) and lToLength.TryParse(aTo)
         and (lFromLength.LengthUnit = lToLength.LengthUnit) then
        begin
        lFromLength.Value := Interpolate(lFromLength.Value, lToLength.Value, aAt);
        Exit(lFromLength.ToString);
        end;
      if TryStrToSVGLengthList(aFrom, lFromLengths)
         and TryStrToSVGLengthList(aTo, lToLengths)
         and SameLengthUnits(lFromLengths, lToLengths) then
        begin
        for I := 0 to High(lFromLengths) do
          lFromLengths[I].Value := Interpolate(lFromLengths[I].Value,
            lToLengths[I].Value, aAt);
        Exit(LengthListToText(lFromLengths));
        end;
      end;
    vkColour:
      begin
      lFromColor := TSVGColor.Transparent;
      lToColor := TSVGColor.Transparent;
      if lFromColor.TryParse(aFrom) and lToColor.TryParse(aTo) then
        Exit(BlendColors(lFromColor, lToColor, aAt).ToString);
      end;
    vkNumberList:
      if TryStrToSVGNumberList(aFrom, lFromList)
         and TryStrToSVGNumberList(aTo, lToList)
         and (Length(lFromList) > 0)
         and (Length(lFromList) = Length(lToList)) then
        begin
        for I := 0 to High(lFromList) do
          lFromList[I] := Interpolate(lFromList[I], lToList[I], aAt);
        Exit(FormatNumbers(lFromList));
        end;
    vkPath:
      if TryReadPathSteps(aFrom, lFromSteps)
         and TryReadPathSteps(aTo, lToSteps)
         and SamePathSteps(lFromSteps, lToSteps) then
        begin
        for I := 0 to High(lFromSteps) do
          for J := 0 to High(lFromSteps[I].Args) do
            // A flag takes the value of the nearer end.
            if IsArcFlag(lFromSteps[I].Command, J) then
              begin
              if aAt >= 0.5 then
                lFromSteps[I].Args[J] := lToSteps[I].Args[J];
              end
            else
              lFromSteps[I].Args[J] := Interpolate(lFromSteps[I].Args[J],
                lToSteps[I].Args[J], aAt);
        Exit(PathStepsToText(lFromSteps));
        end;
  end;
  if aAt < 0.5 then
    Result := aFrom
  else
    Result := aTo;
end;


// The value with a number of copies of one step added to it.
function SVGAccumulate(aKind: TSVGValueKind; const aValue, aStep: TSVGString;
  aTimes: Integer; out aResult: TSVGString): Boolean;

var
  lNumber, lStepNumber: Double;
  lLength, lStepLength: TSVGLength;
  lColor, lStepColor: TSVGColor;
  lList, lStepList: TSVGDoubleArray;
  lLengths, lStepLengths: TSVGLengthArray;
  lSteps, lStepSteps: TSVGPathStepArray;
  I, J: Integer;

begin
  aResult := '';
  Result := False;
  case aKind of
    vkNumber:
      begin
      Result := TryStrToSVGNumber(aValue, lNumber)
            and TryStrToSVGNumber(aStep, lStepNumber);
      if Result then
        aResult := SVGFormatFloat(lNumber + aTimes * lStepNumber);
      end;
    vkLength:
      begin
      lLength := TSVGLength.Zero;
      lStepLength := TSVGLength.Zero;
      Result := lLength.TryParse(aValue) and lStepLength.TryParse(aStep)
            and (lLength.LengthUnit = lStepLength.LengthUnit);
      if Result then
        begin
        lLength.Value := lLength.Value + aTimes * lStepLength.Value;
        aResult := lLength.ToString;
        Exit;
        end;
      Result := TryStrToSVGLengthList(aValue, lLengths)
            and TryStrToSVGLengthList(aStep, lStepLengths)
            and SameLengthUnits(lLengths, lStepLengths);
      if Result then
        begin
        for I := 0 to High(lLengths) do
          lLengths[I].Value := lLengths[I].Value
                             + aTimes * lStepLengths[I].Value;
        aResult := LengthListToText(lLengths);
        end;
      end;
    vkColour:
      begin
      lColor := TSVGColor.Transparent;
      lStepColor := TSVGColor.Transparent;
      Result := lColor.TryParse(aValue) and lStepColor.TryParse(aStep);
      if Result then
        aResult := TSVGColor.FromBytes(
          ChannelByte(lColor.Red + aTimes * lStepColor.Red),
          ChannelByte(lColor.Green + aTimes * lStepColor.Green),
          ChannelByte(lColor.Blue + aTimes * lStepColor.Blue),
          ChannelByte(lColor.Alpha)).ToString;
      end;
    vkNumberList:
      begin
      Result := TryStrToSVGNumberList(aValue, lList)
            and TryStrToSVGNumberList(aStep, lStepList)
            and (Length(lList) > 0) and (Length(lList) = Length(lStepList));
      if Result then
        begin
        for I := 0 to High(lList) do
          lList[I] := lList[I] + aTimes * lStepList[I];
        aResult := FormatNumbers(lList);
        end;
      end;
    vkPath:
      begin
      Result := TryReadPathSteps(aValue, lSteps)
            and TryReadPathSteps(aStep, lStepSteps)
            and SamePathSteps(lSteps, lStepSteps);
      if Result then
        begin
        for I := 0 to High(lSteps) do
          for J := 0 to High(lSteps[I].Args) do
            // A flag keeps the value it starts with.
            if not IsArcFlag(lSteps[I].Command, J) then
              lSteps[I].Args[J] := lSteps[I].Args[J]
                                 + aTimes * lStepSteps[I].Args[J];
        aResult := PathStepsToText(lSteps);
        end;
      end;
  end;
end;


// The sum of two values of a kind. False when the kind cannot be added.
function SVGAddValues(aKind: TSVGValueKind; const aFirst, aSecond: TSVGString;
  out aValue: TSVGString): Boolean;

begin
  Result := SVGAccumulate(aKind, aFirst, aSecond, 1, aValue);
end;


// The distance between two values of a kind. False when the kind has no distance
function SVGValueDistance(aKind: TSVGValueKind; const aFrom, aTo: TSVGString;
  out aDistance: Double): Boolean;

var
  lFromNumber, lToNumber: Double;
  lFromLength, lToLength: TSVGLength;
  lFromColor, lToColor: TSVGColor;
  lFromList, lToList: TSVGDoubleArray;
  lFromLengths, lToLengths: TSVGLengthArray;
  lFromSteps, lToSteps: TSVGPathStepArray;
  lSum: Double;
  I, J: Integer;

begin
  aDistance := 0;
  Result := False;
  case aKind of
    vkNumber:
      begin
      Result := TryStrToSVGNumber(aFrom, lFromNumber)
            and TryStrToSVGNumber(aTo, lToNumber);
      if Result then
        aDistance := Abs(lToNumber - lFromNumber);
      end;
    vkLength:
      begin
      lFromLength := TSVGLength.Zero;
      lToLength := TSVGLength.Zero;
      Result := lFromLength.TryParse(aFrom) and lToLength.TryParse(aTo)
            and (lFromLength.LengthUnit = lToLength.LengthUnit);
      if Result then
        begin
        aDistance := Abs(lToLength.Value - lFromLength.Value);
        Exit;
        end;
      Result := TryStrToSVGLengthList(aFrom, lFromLengths)
            and TryStrToSVGLengthList(aTo, lToLengths)
            and SameLengthUnits(lFromLengths, lToLengths);
      if Result then
        begin
        lSum := 0;
        for I := 0 to High(lFromLengths) do
          lSum := lSum + Sqr(lToLengths[I].Value - lFromLengths[I].Value);
        aDistance := Sqrt(lSum);
        end;
      end;
    vkColour:
      begin
      lFromColor := TSVGColor.Transparent;
      lToColor := TSVGColor.Transparent;
      Result := lFromColor.TryParse(aFrom) and lToColor.TryParse(aTo);
      if Result then
        aDistance := Sqrt(Sqr(1.0 * lToColor.Red - lFromColor.Red)
                        + Sqr(1.0 * lToColor.Green - lFromColor.Green)
                        + Sqr(1.0 * lToColor.Blue - lFromColor.Blue)) / 257;
      end;
    vkNumberList:
      begin
      Result := TryStrToSVGNumberList(aFrom, lFromList)
            and TryStrToSVGNumberList(aTo, lToList)
            and (Length(lFromList) > 0)
            and (Length(lFromList) = Length(lToList));
      if Result then
        begin
        lSum := 0;
        for I := 0 to High(lFromList) do
          lSum := lSum + Sqr(lToList[I] - lFromList[I]);
        aDistance := Sqrt(lSum);
        end;
      end;
    vkPath:
      begin
      Result := TryReadPathSteps(aFrom, lFromSteps)
            and TryReadPathSteps(aTo, lToSteps)
            and SamePathSteps(lFromSteps, lToSteps);
      if Result then
        begin
        lSum := 0;
        for I := 0 to High(lFromSteps) do
          for J := 0 to High(lFromSteps[I].Args) do
            if not IsArcFlag(lFromSteps[I].Command, J) then
              lSum := lSum + Sqr(lToSteps[I].Args[J]
                             - lFromSteps[I].Args[J]);
        aDistance := Sqrt(lSum);
        end;
      end;
  end;
end;


// The key times that space values by the distance between them, which is
// what calcMode="paced" asks for. Nil when a distance cannot be measured,
// or every value is at the same place.
function PacedKeyTimes(aKind: TSVGValueKind;
  const aValues: TStringArray): TSVGDoubleArray;

var
  lTimes: TSVGDoubleArray;
  lStep, lRun: Double;
  I: Integer;

begin
  Result := nil;
  if Length(aValues) < 2 then
    Exit;
  SetLength(lTimes, Length(aValues));
  lTimes[0] := 0;
  lRun := 0;
  for I := 1 to High(aValues) do
    begin
    if not SVGValueDistance(aKind, aValues[I - 1], aValues[I], lStep) then
      Exit;
    lRun := lRun + lStep;
    lTimes[I] := lRun;
    end;
  if lRun <= 0 then
    Exit;
  for I := 1 to High(lTimes) do
    lTimes[I] := lTimes[I] / lRun;
  lTimes[High(lTimes)] := 1;
  Result := lTimes;
end;


// Transforms a time (0..1) into a fraction (0..1) of the change between an
// interval's two values, by reading the y of the keySplines Bezier at x =
// the time.
function SplineEase(aX1, aY1, aX2, aY2, aAt: Double): Double;

const
  { Halvings of the interval the curve is solved over. Thirty of them
    place the time to within a thousand millionth. }
  Iterations = 30;

var
  lLow, lHigh, lTime, lX, lOneLess: Double;
  I: Integer;

begin
  if aAt <= 0 then
    Exit(0);
  if aAt >= 1 then
    Exit(1);
  lLow := 0;
  lHigh := 1;
  lTime := aAt;
  for I := 1 to Iterations do
    begin
    lOneLess := 1 - lTime;
    lX := 3 * lOneLess * lOneLess * lTime * aX1
        + 3 * lOneLess * lTime * lTime * aX2
        + lTime * lTime * lTime;
    if lX < aAt then
      lLow := lTime
    else
      lHigh := lTime;
    lTime := (lLow + lHigh) / 2;
    end;
  lOneLess := 1 - lTime;
  Result := 3 * lOneLess * lOneLess * lTime * aY1
          + 3 * lOneLess * lTime * lTime * aY2
          + lTime * lTime * lTime;
end;


// The key splines an element declares: four numbers for each interval between two values. 
function ReadKeySplines(aElement: TSVGElement;
  aValueCount: Integer): TSVGDoubleArray;

var
  lTexts: TStringArray;
  lSplines, lNumbers: TSVGDoubleArray;
  I, J: Integer;

begin
  Result := nil;
  lTexts := SplitValueList(aElement.Attributes['keySplines']);
  if (aValueCount < 2) or (Length(lTexts) <> aValueCount - 1) then
    Exit;
  SetLength(lSplines, 4 * Length(lTexts));
  for I := 0 to High(lTexts) do
    begin
    if not TryStrToSVGNumberList(lTexts[I], lNumbers)
       or (Length(lNumbers) <> 4) then
      Exit;
    if (lNumbers[0] < 0) or (lNumbers[0] > 1)
       or (lNumbers[2] < 0) or (lNumbers[2] > 1) then
      Exit;
    for J := 0 to 3 do
      lSplines[4 * I + J] := lNumbers[J];
    end;
  Result := lSplines;
end;


// The coordinate pair of a motion value, as two numbers. Empty when the text is not one pair.
function MotionPoint(const aText: TSVGString): TSVGString;

var
  lValues: TSVGDoubleArray;

begin
  Result := '';
  if TryStrToSVGNumberList(aText, lValues) and (Length(lValues) = 2) then
    Result := FormatNumbers(lValues);
end;


// The d of the path referenced by an mpath child of aElement. Empty when there is no mpath
function MotionPathReference(aElement: TSVGElement): TSVGString;

var
  lChild, lTarget: TSVGElement;

begin
  Result := '';
  lChild := aElement.FindChildElement('mpath');
  if (lChild = nil) or (aElement.Document = nil) then
    Exit;
  lTarget := aElement.Document.ResolveReference(SVGHRefOf(lChild));
  if (lTarget <> nil) and (lTarget.TagName = 'path') then
    Result := lTarget.Attributes['d'];
end;


// The path a motion animation runs along, measured. False when it has none: it then runs over its values as points.
function MotionMetricsOf(const aPathText: TSVGString;
  const aPoints: TStringArray; out aMetrics: TSVGPathMetrics): Boolean;

var
  lPath: TSVGPath;
  lPoints: TSVGPointArray;
  lValues: TSVGDoubleArray;
  I: Integer;

begin
  Result := False;
  lPath := TSVGPath.Create;
  try
    if Trim(aPathText) <> '' then
      begin
      if not lPath.TryParse(aPathText) then
        Exit;
      end
    else
      begin
      if Length(aPoints) < 2 then
        Exit;
      SetLength(lPoints, Length(aPoints));
      for I := 0 to High(aPoints) do
        begin
        if not TryStrToSVGNumberList(aPoints[I], lValues)
           or (Length(lValues) <> 2) then
          Exit;
        lPoints[I] := TSVGPoint.Create(lValues[0], lValues[1]);
        end;
      lPath.AddPolygon(lPoints, False);
      end;
    aMetrics := TSVGPathMetrics.Create(lPath, SVGDefaultFlatness);
    Result := aMetrics.TotalLength > 0;
  finally
    lPath.Free;
  end;
end;


{ Read a motion animation:
  the path of an mpath child or of a path attribute or points.
  The values become the key points along a path when there is one.
  False when the element contains neither a path nor points. }
function ReadMotion(aElement: TSVGElement;
  var aAnimation: TSVGAnimation): Boolean;

var
  lPathText, lText: TSVGString;
  lKeyPoints: TStringArray;
  lNumber: Double;
  I: Integer;

begin
  Result := False;
  lText := Trim(aElement.Attributes['rotate']);
  if lText = 'auto' then
    aAnimation.Rotate := mrAuto
  else if lText = 'auto-reverse' then
    aAnimation.Rotate := mrAutoReverse
  else if not TryStrToSVGNumber(lText, aAnimation.RotateAngle) then
    aAnimation.RotateAngle := 0;
  for I := 0 to High(aAnimation.Values) do
    begin
    aAnimation.Values[I] := MotionPoint(aAnimation.Values[I]);
    if aAnimation.Values[I] = '' then
      Exit;
    end;
  lPathText := MotionPathReference(aElement);
  if Trim(lPathText) = '' then
    lPathText := aElement.Attributes['path'];
  lKeyPoints := SplitValueList(aElement.Attributes['keyPoints']);
  if (Trim(lPathText) <> '') or (Length(lKeyPoints) > 1) then
    begin
    if not MotionMetricsOf(lPathText, aAnimation.Values, aAnimation.Motion) then
      Exit;
    aAnimation.HasMotionPath := True;
    { Along a path the values are fractions of the path length rather than points:
      the key points when the element specifies them,
      and the whole path it when it does not. }
    aAnimation.ValueKind := vkNumber;
    if Length(lKeyPoints) > 1 then
      begin
      aAnimation.Values := lKeyPoints;
      for I := 0 to High(aAnimation.Values) do
        if not TryStrToSVGNumber(aAnimation.Values[I], lNumber) then
          Exit;
      end
    else
      begin
      SetLength(aAnimation.Values, 2);
      aAnimation.Values[0] := '0';
      aAnimation.Values[1] := '1';
      end;
    end;
  Result := Length(aAnimation.Values) > 0;
end;


{ The "from", "to" and "by" values, and how the animation executes them:
  a "by" animation of a transform starts from the function at rest
  and adds to whatever is under it, and a "to" animation does not accumulate.
  Empty when the element gives no values that can be run.}
function FoldValues(aElement: TSVGElement; aKind: TSVGAnimationKind;
  aTransformKind: TSVGTransformKind; aValueKind: TSVGValueKind;
  const aBase: TSVGString; aHasBase: Boolean;
  out aAdds, aToAnimation: Boolean): TStringArray;

var
  lFrom, lTo, lBy, lSum: TSVGString;

begin
  Result := nil;
  aAdds := False;
  aToAnimation := False;
  lFrom := Trim(aElement.Attributes['from']);
  lTo := Trim(aElement.Attributes['to']);
  lBy := Trim(aElement.Attributes['by']);
  if aKind = akTransform then
    begin
    // from, to and by hold the arguments of the transform function.
    // Shorthand, such as a single number for scale, is expanded to the full argument list
    if lFrom <> '' then
      begin
      lFrom := TransformArguments(aTransformKind, lFrom);
      if lFrom = '' then
        Exit;
      end;
    if lTo <> '' then
      begin
      lTo := TransformArguments(aTransformKind, lTo);
      if lTo = '' then
        Exit;
      end;
    if lBy <> '' then
      begin
      lBy := TransformArguments(aTransformKind, lBy);
      if lBy = '' then
        Exit;
      end;
    end;
  aToAnimation := (lTo <> '') and (lFrom = '') and (lBy = '');
  aAdds := (aKind = akTransform) and (lBy <> '') and (lFrom = '');
  if aKind = akSet then
    begin
    if lTo <> '' then
      begin
      SetLength(Result, 1);
      Result[0] := lTo;
      end;
    Exit;
    end;
  if (lFrom <> '') and (lTo <> '') then
    begin
    SetLength(Result, 2);
    Result[0] := lFrom;
    Result[1] := lTo;
    end
  else if (lFrom <> '') and (lBy <> '') then
    begin
    if not SVGAddValues(aValueKind, lFrom, lBy, lSum) then
      Exit;
    SetLength(Result, 2);
    Result[0] := lFrom;
    Result[1] := lSum;
    end
  else if (lTo <> '') and aHasBase then
    begin
    SetLength(Result, 2);
    Result[0] := aBase;
    Result[1] := lTo;
    end
  else if lTo <> '' then
    begin
    SetLength(Result, 1);
    Result[0] := lTo;
    end
  else if (lBy <> '') and aHasBase then
    begin
    if not SVGAddValues(aValueKind, aBase, lBy, lSum) then
      Exit;
    SetLength(Result, 2);
    Result[0] := aBase;
    Result[1] := lSum;
    end
  else if lFrom <> '' then
    begin
    SetLength(Result, 1);
    Result[0] := lFrom;
    end;
end;


// Reads the element's keyTimes: 
// Nil unless there is one time per value, the times do not decrease, they
// all lie between 0 and 1, and the first is 0.
function ReadKeyTimes(aElement: TSVGElement;
  aValueCount: Integer): TSVGDoubleArray;

var
  lTexts: TStringArray;
  lTimes: TSVGDoubleArray;
  I: Integer;

begin
  Result := nil;
  lTexts := SplitValueList(aElement.Attributes['keyTimes']);
  if (Length(lTexts) = 0) or (Length(lTexts) <> aValueCount) then
    Exit;
  SetLength(lTimes, aValueCount);
  for I := 0 to aValueCount - 1 do
    begin
    if not TryStrToSVGNumber(lTexts[I], lTimes[I]) then
      Exit;
    if (lTimes[I] < 0) or (lTimes[I] > 1) then
      Exit;
    if (I > 0) and (lTimes[I] < lTimes[I - 1]) then
      Exit;
    end;
  if lTimes[0] <> 0 then
    Exit;
  Result := lTimes;
end;


// The element an animation applies to: the one in href if present, or the parent
function AnimationTarget(aElement: TSVGElement): TSVGElement;

var
  lHRef: TSVGString;

begin
  lHRef := SVGHRefOf(aElement);
  if (lHRef <> '') and (aElement.Document <> nil) then
    Result := aElement.Document.ResolveReference(lHRef)
  else
    Result := aElement.Parent;
end;


// Reads one animation element against its target. False when the element is not an animation, or does not reference an animationg.
function TryReadAnimation(aElement: TSVGElement; out aAnimation: TSVGAnimation): Boolean;

var
  lTag, lText, lUnder: TSVGString;
  lNumber: Double;
  lPaced: TSVGDoubleArray;
  lHasUnder, lAdds, lToAnimation: Boolean;
  I: Integer;

begin
  Result := False;
  lTag := aElement.TagName;
  if lTag = 'set' then
    aAnimation.Kind := akSet
  else if lTag = 'animate' then
    aAnimation.Kind := akAnimate
  else if lTag = 'animateColor' then
    aAnimation.Kind := akColour
  else if lTag = 'animateTransform' then
    aAnimation.Kind := akTransform
  else if lTag = 'animateMotion' then
    aAnimation.Kind := akMotion
  else
    Exit;
  aAnimation.HasMotionPath := False;
  aAnimation.Rotate := mrAngle;
  aAnimation.RotateAngle := 0;
  aAnimation.KeySplines := nil;
  aAnimation.Target := AnimationTarget(aElement);
  aAnimation.AttributeName := Trim(aElement.Attributes['attributeName']);
  // The reader keeps an attribute of the XLink namespace under its local
  // name, and an attributeName naming one reaches that same attribute.
  if Copy(aAnimation.AttributeName, 1, Length(XLinkPrefix)) = XLinkPrefix then
    aAnimation.AttributeName := Copy(aAnimation.AttributeName,
      Length(XLinkPrefix) + 1, Length(aAnimation.AttributeName));
  // A motion animation moves the element itself, which reaches the
  // document as a transform outside the one the element is written with.
  if aAnimation.Kind = akMotion then
    aAnimation.AttributeName := 'transform';
  if (aAnimation.Target = nil) or (aAnimation.AttributeName = '') then
    Exit;
  aAnimation.TransformKind := SVGTransformKindOf(aElement.Attributes['type']);
  case aAnimation.Kind of
    akColour:
      aAnimation.ValueKind := vkColour;
    akTransform, akMotion:
      aAnimation.ValueKind := vkNumberList;
  else
    aAnimation.ValueKind := SVGValueKindOf(aAnimation.AttributeName);
  end;
  aAnimation.HasBase := aAnimation.Target.HasAttribute(aAnimation.AttributeName);
  aAnimation.Base := aAnimation.Target.Attributes[aAnimation.AttributeName];
  // A transform animation writes a function of its own, so what a to or
  // a by animation of one starts from is that function at rest rather
  // than the transform list under it.
  lUnder := aAnimation.Base;
  lHasUnder := aAnimation.HasBase;
  if aAnimation.Kind = akTransform then
    begin
    lUnder := TransformZero(aAnimation.TransformKind);
    lHasUnder := True;
    end
  else if aAnimation.Kind = akMotion then
    begin
    // A motion is an offset from where the element stands, so a to or a
    // by animation of one starts from no offset at all.
    lUnder := '0 0';
    lHasUnder := True;
    end;
  aAnimation.Values := SplitValueList(aElement.Attributes['values']);
  lAdds := False;
  lToAnimation := False;
  if Length(aAnimation.Values) = 0 then
    aAnimation.Values := FoldValues(aElement, aAnimation.Kind,
      aAnimation.TransformKind, aAnimation.ValueKind, lUnder, lHasUnder,
      lAdds, lToAnimation);
  if aAnimation.Kind = akMotion then
    begin
    if not ReadMotion(aElement, aAnimation) then
      Exit;
    end
  else
    begin
    if Length(aAnimation.Values) = 0 then
      Exit;
    if aAnimation.Kind = akTransform then
      for I := 0 to High(aAnimation.Values) do
        begin
        aAnimation.Values[I] := TransformArguments(aAnimation.TransformKind,
          aAnimation.Values[I]);
        if aAnimation.Values[I] = '' then
          Exit;
        end;
    end;
  // A set holds one value and takes neither attribute.
  aAnimation.Additive := (aAnimation.Kind <> akSet)
    and (lAdds or (Trim(aElement.Attributes['additive']) = 'sum'));
  aAnimation.Accumulates := (aAnimation.Kind <> akSet) and not lToAnimation
    and (Trim(aElement.Attributes['accumulate']) = 'sum');
  lText := Trim(aElement.Attributes['calcMode']);
  if aAnimation.Kind = akSet then
    aAnimation.CalcMode := cmDiscrete
  else if lText = 'discrete' then
    aAnimation.CalcMode := cmDiscrete
  else if lText = 'paced' then
    aAnimation.CalcMode := cmPaced
  else if lText = 'spline' then
    aAnimation.CalcMode := cmSpline
  else if lText = 'linear' then
    aAnimation.CalcMode := cmLinear
  else if aAnimation.Kind = akMotion then
    aAnimation.CalcMode := cmPaced
  else
    aAnimation.CalcMode := cmLinear;
  aAnimation.Element := aElement;
  aAnimation.Begins := nil;
  aAnimation.Ends := nil;
  aAnimation.StartsAt := 0;
  aAnimation.KeyTimes := ReadKeyTimes(aElement, Length(aAnimation.Values));
  if aAnimation.CalcMode = cmSpline then
    aAnimation.KeySplines := ReadKeySplines(aElement,
      Length(aAnimation.Values));
  // Pacing by distance is worked out from the values, and stands in for
  // any key times the element gives.
  if aAnimation.CalcMode = cmPaced then
    begin
    lPaced := PacedKeyTimes(aAnimation.ValueKind, aAnimation.Values);
    if lPaced <> nil then
      aAnimation.KeyTimes := lPaced;
    end;
  aAnimation.Duration := -1;
  if TryStrToSVGClock(aElement.Attributes['dur'], lNumber) and (lNumber > 0) then
    aAnimation.Duration := lNumber;
  aAnimation.RepeatDur := 0;
  lText := Trim(aElement.Attributes['repeatDur']);
  if lText = 'indefinite' then
    aAnimation.RepeatDur := -1
  else if TryStrToSVGClock(lText, lNumber) and (lNumber > 0) then
    aAnimation.RepeatDur := lNumber;
  aAnimation.Repeats := 1;
  lText := Trim(aElement.Attributes['repeatCount']);
  if lText = 'indefinite' then
    aAnimation.Repeats := -1
  else if TryStrToSVGNumber(lText, lNumber) and (lNumber > 0) then
    aAnimation.Repeats := lNumber
  else if aAnimation.RepeatDur <> 0 then
    // With a repeat duration and no count, the runs go on for as long as
    // that duration allows.
    aAnimation.Repeats := -1;
  aAnimation.MinDuration := 0;
  if TryStrToSVGClock(aElement.Attributes['min'], lNumber)
     and (lNumber > 0) then
    aAnimation.MinDuration := lNumber;
  aAnimation.MaxDuration := 0;
  if TryStrToSVGClock(aElement.Attributes['max'], lNumber)
     and (lNumber > 0) then
    aAnimation.MaxDuration := lNumber;
  lText := Trim(aElement.Attributes['restart']);
  if lText = 'never' then
    aAnimation.Restart := rsNever
  else if lText = 'whenNotActive' then
    aAnimation.Restart := rsWhenNotActive
  else
    aAnimation.Restart := rsAlways;
  aAnimation.Freezes := Trim(aElement.Attributes['fill']) = 'freeze';
  Result := True;
end;


// Appends a change time, dropping anything before the start of the clock.
procedure AppendChangeTime(var aTimes: TSVGDoubleArray; var aCount: Integer;
  aAt: Double);

begin
  if aAt < 0 then
    Exit;
  if aCount = Length(aTimes) then
    SetLength(aTimes, 8 + aCount * 2);
  aTimes[aCount] := aAt;
  Inc(aCount);
end;


// True when two lists hold the same times.
function SameTimes(const aFirst, aSecond: TSVGDoubleArray): Boolean;

var
  I: Integer;

begin
  Result := Length(aFirst) = Length(aSecond);
  if not Result then
    Exit;
  for I := 0 to High(aFirst) do
    if aFirst[I] <> aSecond[I] then
      Exit(False);
end;


// The times in increasing order, with the duplicates dropped.
function SortedTimes(const aTimes: TSVGDoubleArray): TSVGDoubleArray;

var
  lSorted: TSVGDoubleArray;
  lValue: Double;
  I, J, lCount: Integer;

begin
  lSorted := Copy(aTimes, 0, Length(aTimes));
  for I := 1 to High(lSorted) do
    begin
    lValue := lSorted[I];
    J := I - 1;
    while (J >= 0) and (lSorted[J] > lValue) do
      begin
      lSorted[J + 1] := lSorted[J];
      Dec(J);
      end;
    lSorted[J + 1] := lValue;
    end;
  lCount := 0;
  for I := 0 to High(lSorted) do
    if (lCount = 0) or (lSorted[I] - lSorted[lCount - 1] > 1e-9) then
      begin
      lSorted[lCount] := lSorted[I];
      Inc(lCount);
      end;
  SetLength(lSorted, lCount);
  Result := lSorted;
end;


{ TSVGAnimation }

function TSVGAnimation.KeyTimeAt(aIndex: Integer): Double;

var
  lCount: Integer;

begin
  lCount := Length(Values);
  if (lCount > 0) and (Length(KeyTimes) = lCount) then
    Exit(KeyTimes[aIndex]);
  if lCount < 2 then
    Exit(0);
  if CalcMode = cmDiscrete then
    Result := aIndex / lCount
  else
    Result := aIndex / (lCount - 1);
end;


function TSVGAnimation.SegmentAt(aProgress: Double; out aLocal: Double): Integer;

var
  lCount, I: Integer;
  lLow, lHigh: Double;

begin
  aLocal := 0;
  Result := 0;
  lCount := Length(Values);
  if lCount < 2 then
    Exit;
  for I := lCount - 2 downto 0 do
    if aProgress >= KeyTimeAt(I) then
      begin
      Result := I;
      Break;
      end;
  lLow := KeyTimeAt(Result);
  lHigh := KeyTimeAt(Result + 1);
  if lHigh > lLow then
    aLocal := SVGClamp((aProgress - lLow) / (lHigh - lLow), 0, 1)
  else
    aLocal := 1;
end;


function TSVGAnimation.ColourValue(const aValue: TSVGString): TSVGString;

begin
  Result := aValue;
  if (ValueKind = vkColour) and (Target <> nil)
     and SameText(Trim(aValue), ColourKeyword) then
    Result := SVGCurrentColorOf(Target).ToString;
end;


function TSVGAnimation.ValueOfProgress(aProgress: Double): TSVGString;

var
  lCount, lIndex, I: Integer;
  lLocal: Double;

begin
  lCount := Length(Values);
  if lCount = 0 then
    Exit('');
  if lCount = 1 then
    Exit(ColourValue(Values[0]));
  if CalcMode = cmDiscrete then
    begin
    lIndex := 0;
    for I := lCount - 1 downto 0 do
      if aProgress >= KeyTimeAt(I) then
        begin
        lIndex := I;
        Break;
        end;
    Exit(ColourValue(Values[lIndex]));
    end;
  lIndex := SegmentAt(aProgress, lLocal);
  if (CalcMode = cmSpline) and (Length(KeySplines) = 4 * (lCount - 1)) then
    lLocal := SplineEase(KeySplines[4 * lIndex], KeySplines[4 * lIndex + 1],
      KeySplines[4 * lIndex + 2], KeySplines[4 * lIndex + 3], lLocal);
  Result := SVGInterpolate(ValueKind, ColourValue(Values[lIndex]),
    ColourValue(Values[lIndex + 1]), lLocal);
end;


function TSVGAnimation.MotionTransformOf(const aValue: TSVGString;
  aProgress: Double): TSVGString;

var
  lPoint, lTangent: TSVGPoint;
  lNumbers: TSVGDoubleArray;
  lIndex: Integer;
  lFraction, lLocal, lAngle: Double;

begin
  Result := '';
  lTangent := TSVGPoint.Create(1, 0);
  if HasMotionPath then
    begin
    if not TryStrToSVGNumber(aValue, lFraction) then
      Exit;
    if not Motion.PlaceAt(SVGClamp(lFraction, 0, 1) * Motion.TotalLength,
                          lPoint, lTangent) then
      Exit;
    end
  else
    begin
    if not TryStrToSVGNumberList(aValue, lNumbers)
       or (Length(lNumbers) <> 2) then
      Exit;
    lPoint := TSVGPoint.Create(lNumbers[0], lNumbers[1]);
    if Rotate <> mrAngle then
      begin
      lIndex := SegmentAt(aProgress, lLocal);
      if (Length(Values) > lIndex + 1)
         and TryStrToSVGNumberList(Values[lIndex], lNumbers)
         and (Length(lNumbers) = 2) then
        begin
        lTangent := TSVGPoint.Create(-lNumbers[0], -lNumbers[1]);
        if TryStrToSVGNumberList(Values[lIndex + 1], lNumbers)
           and (Length(lNumbers) = 2) then
          lTangent := TSVGPoint.Create(lTangent.X + lNumbers[0],
            lTangent.Y + lNumbers[1]);
        end;
      end;
    end;
  Result := 'translate(' + SVGFormatFloat(lPoint.X) + ' '
    + SVGFormatFloat(lPoint.Y) + ')';
  if (Rotate = mrAngle) and (RotateAngle = 0) then
    Exit;
  case Rotate of
    mrAuto:
      lAngle := RadToDeg(ArcTan2(lTangent.Y, lTangent.X));
    mrAutoReverse:
      lAngle := RadToDeg(ArcTan2(lTangent.Y, lTangent.X)) + 180;
  else
    lAngle := RotateAngle;
  end;
  Result := Result + ' rotate(' + SVGFormatFloat(lAngle) + ')';
end;


function TSVGAnimation.RepeatDuration: Double;

begin
  Result := -1;
  if (Duration > 0) and (Repeats >= 0) then
    Result := Duration * Repeats;
  if RepeatDur > 0 then
    if (Result < 0) or (RepeatDur < Result) then
      Result := RepeatDur;
end;


function TSVGAnimation.ActiveDurationFrom(aBegin: Double): Double;

var
  I: Integer;

begin
  Result := RepeatDuration;
  for I := 0 to High(Ends) do
    if Ends[I] >= aBegin then
      begin
      if (Result < 0) or (Ends[I] - aBegin < Result) then
        Result := Ends[I] - aBegin;
      Break;
      end;
  // A min above a max leaves both unread, which is what SVG asks for.
  if (MinDuration > 0) and (MaxDuration > 0)
     and (MinDuration > MaxDuration) then
    Exit;
  if (MaxDuration > 0) and ((Result < 0) or (Result > MaxDuration)) then
    Result := MaxDuration;
  if (MinDuration > 0) and (Result >= 0) and (Result < MinDuration) then
    Result := MinDuration;
end;


function TSVGAnimation.ActiveDuration(aIndex: Integer): Double;

begin
  Result := -1;
  if (aIndex < 0) or (aIndex > High(Begins)) then
    Exit;
  Result := ActiveDurationFrom(Begins[aIndex]);
end;


function TSVGAnimation.IntervalAt(aSeconds: Double; out aBegin,
  aActive: Double): Boolean;

var
  I: Integer;

begin
  aBegin := 0;
  aActive := -1;
  Result := False;
  // A begin that has passed starts the animation over, so the last of
  // them is the interval that time falls in.
  for I := High(Begins) downto 0 do
    if aSeconds >= Begins[I] then
      begin
      aBegin := Begins[I];
      aActive := ActiveDuration(I);
      Exit(True);
      end;
end;


function TSVGAnimation.ValueAt(aSeconds: Double; out aValue: TSVGString): Boolean;

var
  lBegin, lLocal, lActive, lRepeat, lProgress: Double;
  lRuns: Integer;
  lFrozen: Boolean;
  lPiled: TSVGString;

begin
  aValue := '';
  Result := False;
  if Length(Values) = 0 then
    Exit;
  if not IntervalAt(aSeconds, lBegin, lActive) then
    Exit;
  lLocal := aSeconds - lBegin;
  lFrozen := False;
  if (lActive >= 0) and (lLocal >= lActive) then
    begin
    if not Freezes then
      Exit;
    lLocal := lActive;
    lFrozen := True;
    end;
  // A min that stretches the interval past the runs holds the last value
  // for the rest of it.
  lRepeat := RepeatDuration;
  if (lRepeat >= 0) and (lLocal > lRepeat) then
    begin
    lLocal := lRepeat;
    lFrozen := True;
    end;
  lRuns := 0;
  lProgress := 0;
  if Duration <= 0 then
    // Under an indefinite duration the first value stands for the whole
    // of the run.
    aValue := Values[0]
  else
    begin
    lProgress := Frac(lLocal / Duration);
    lRuns := Trunc(lLocal / Duration);
    if lFrozen and (lProgress = 0) and (lRuns > 0) then
      begin
      lProgress := 1;
      Dec(lRuns);
      end;
    aValue := ValueOfProgress(lProgress);
    end;
  if Accumulates and (lRuns > 0)
     and SVGAccumulate(ValueKind, aValue, ColourValue(Values[High(Values)]),
       lRuns, lPiled) then
    aValue := lPiled;
  if Kind = akTransform then
    aValue := TransformNames[TransformKind] + '(' + aValue + ')'
  else if Kind = akMotion then
    begin
    aValue := MotionTransformOf(aValue, lProgress);
    if aValue = '' then
      Exit;
    end;
  Result := True;
end;


function TSVGAnimation.EndsAt: Double;

var
  lActive: Double;

begin
  Result := -1;
  if Length(Begins) = 0 then
    Exit;
  lActive := ActiveDuration(High(Begins));
  if lActive < 0 then
    Exit;
  Result := Begins[High(Begins)] + lActive;
end;


{ TSVGTimeline }

constructor TSVGTimeline.Create(aDocument: TSVGDocument);

begin
  Create(aDocument, 0);
end;


constructor TSVGTimeline.Create(aDocument: TSVGDocument;
  aWallClockNow: TDateTime);

var
  I: Integer;
  lEnd: Double;

begin
  inherited Create;
  FDocument := aDocument;
  FWallClockNow := aWallClockNow;
  FDuration := 0;
  if (aDocument <> nil) and (aDocument.Root <> nil) then
    Collect(aDocument.Root);
  SetLength(FAnimations, FCount);
  ResolveTiming;
  ApplyRestart;
  DropUntimed;
  for I := 0 to FCount - 1 do
    begin
    lEnd := FAnimations[I].EndsAt;
    if lEnd < 0 then
      begin
      FDuration := -1;
      Exit;
      end;
    if lEnd > FDuration then
      FDuration := lEnd;
    end;
end;


procedure TSVGTimeline.Collect(aElement: TSVGElement);

var
  I: Integer;

begin
  AddElement(aElement);
  for I := 0 to aElement.ChildCount - 1 do
    if aElement[I] is TSVGElement then
      Collect(TSVGElement(aElement[I]));
end;


procedure TSVGTimeline.AddElement(aElement: TSVGElement);

var
  lAnimation: TSVGAnimation;

begin
  if not TryReadAnimation(aElement, lAnimation) then
    Exit;
  if FCount = Length(FAnimations) then
    SetLength(FAnimations, 8 + FCount * 2);
  FAnimations[FCount] := lAnimation;
  Inc(FCount);
end;


// The animation whose element goes by that id, or -1 when no animation
// of the document does.
function TSVGTimeline.IndexOfID(const aID: TSVGString): Integer;

var
  I: Integer;

begin
  Result := -1;
  if aID = '' then
    Exit;
  for I := 0 to FCount - 1 do
    if (FAnimations[I].Element <> nil)
       and (FAnimations[I].Element.ID = aID) then
      Exit(I);
end;


// The time one entry of a begin or end list names. False when it names a
// moment nothing can be worked out for: an animation the document does
// not hold, one whose own begin could not be worked out, one whose first
// interval never ends, a run it never reaches, or the wall clock where
// the timeline was built without one.
function TSVGTimeline.EntryTime(const aEntry: TSVGTimeEntry;
  out aTime: Double): Boolean;

var
  lIndex: Integer;
  lActive, lReached: Double;

begin
  aTime := aEntry.Offset;
  if aEntry.Kind = teOffset then
    Exit(True);
  Result := False;
  if aEntry.Kind = teWallClock then
    begin
    if FWallClockNow = 0 then
      Exit;
    aTime := (aEntry.When - FWallClockNow) * SVGSecondsPerDay;
    Exit(True);
    end;
  lIndex := IndexOfID(aEntry.Base);
  if (lIndex < 0) or (Length(FAnimations[lIndex].Begins) = 0) then
    Exit;
  case aEntry.Kind of
    teSyncBegin:
      aTime := FAnimations[lIndex].Begins[0] + aEntry.Offset;
    teSyncRepeat:
      begin
      if FAnimations[lIndex].Duration <= 0 then
        Exit;
      lReached := aEntry.Count * FAnimations[lIndex].Duration;
      lActive := FAnimations[lIndex].ActiveDuration(0);
      if (lActive >= 0) and (lReached > lActive) then
        Exit;
      aTime := FAnimations[lIndex].Begins[0] + lReached + aEntry.Offset;
      end;
  else
    lActive := FAnimations[lIndex].ActiveDuration(0);
    if lActive < 0 then
      Exit;
    aTime := FAnimations[lIndex].Begins[0] + lActive + aEntry.Offset;
  end;
  Result := True;
end;


// Works out the begin and end times one animation has now, from the
// times the others have reached. True when they came out other than they
// were, so that another pass over all of them is worth making.
function TSVGTimeline.SettleTiming(aIndex: Integer; const aBeginEntries,
  aEndEntries: TSVGTimeEntryArray): Boolean;

var
  lBegins, lEnds: TSVGDoubleArray;
  lTime: Double;
  lCount, I: Integer;

begin
  SetLength(lBegins, Length(aBeginEntries));
  lCount := 0;
  for I := 0 to High(aBeginEntries) do
    if EntryTime(aBeginEntries[I], lTime) then
      begin
      lBegins[lCount] := lTime;
      Inc(lCount);
      end;
  SetLength(lBegins, lCount);
  SetLength(lEnds, Length(aEndEntries));
  lCount := 0;
  for I := 0 to High(aEndEntries) do
    if EntryTime(aEndEntries[I], lTime) then
      begin
      lEnds[lCount] := lTime;
      Inc(lCount);
      end;
  SetLength(lEnds, lCount);
  lBegins := SortedTimes(lBegins);
  lEnds := SortedTimes(lEnds);
  Result := not SameTimes(FAnimations[aIndex].Begins, lBegins)
         or not SameTimes(FAnimations[aIndex].Ends, lEnds);
  if not Result then
    Exit;
  FAnimations[aIndex].Begins := lBegins;
  FAnimations[aIndex].Ends := lEnds;
  if Length(lBegins) > 0 then
    FAnimations[aIndex].StartsAt := lBegins[0];
end;


procedure TSVGTimeline.ResolveTiming;

var
  lBeginEntries, lEndEntries: array of TSVGTimeEntryArray;
  lChanged: Boolean;
  lPass, I: Integer;

begin
  if FCount = 0 then
    Exit;
  SetLength(lBeginEntries, FCount);
  SetLength(lEndEntries, FCount);
  for I := 0 to FCount - 1 do
    begin
    lBeginEntries[I] := ReadTimeEntries(FAnimations[I].Element, 'begin', True);
    lEndEntries[I] := ReadTimeEntries(FAnimations[I].Element, 'end', False);
    end;
  // An entry referring to another animation resolves once that one has a time,
  // so the pass is made again for as long as one of them moves.
  lPass := 0;
  repeat
    lChanged := False;
    for I := 0 to FCount - 1 do
      if SettleTiming(I, lBeginEntries[I], lEndEntries[I]) then
        lChanged := True;
    Inc(lPass);
  until not lChanged or (lPass > FCount + 1);
end;


// Drops the begin times that restart forbids
procedure TSVGTimeline.ApplyRestart;

var
  lKept: TSVGDoubleArray;
  lActive: Double;
  I, J, lCount: Integer;

begin
  for I := 0 to FCount - 1 do
    begin
    if (FAnimations[I].Restart = rsAlways)
       or (Length(FAnimations[I].Begins) < 2) then
      Continue;
    lKept := Copy(FAnimations[I].Begins, 0, Length(FAnimations[I].Begins));
    lCount := 1;
    if FAnimations[I].Restart = rsWhenNotActive then
      for J := 1 to High(FAnimations[I].Begins) do
        begin
        lActive := FAnimations[I].ActiveDurationFrom(lKept[lCount - 1]);
        if lActive < 0 then
          Break;
        if FAnimations[I].Begins[J] >= lKept[lCount - 1] + lActive then
          begin
          lKept[lCount] := FAnimations[I].Begins[J];
          Inc(lCount);
          end;
        end;
    SetLength(lKept, lCount);
    FAnimations[I].Begins := lKept;
    end;
end;


// Drops the animations that never begin
procedure TSVGTimeline.DropUntimed;

var
  I, lKept: Integer;

begin
  lKept := 0;
  for I := 0 to FCount - 1 do
    if Length(FAnimations[I].Begins) > 0 then
      begin
      if lKept <> I then
        FAnimations[lKept] := FAnimations[I];
      Inc(lKept);
      end;
  FCount := lKept;
  SetLength(FAnimations, FCount);
end;


function TSVGTimeline.GetAnimation(aIndex: Integer): TSVGAnimation;

begin
  if (aIndex < 0) or (aIndex >= FCount) then
    raise ESVGAnim.CreateFmt(SErrAnimationIndexOutOfRange, [aIndex]);
  Result := FAnimations[aIndex];
end;


function TSVGTimeline.GetIsAnimated: Boolean;

begin
  Result := FCount > 0;
end;


procedure TSVGTimeline.WriteBase(const aAnimation: TSVGAnimation);

begin
  if aAnimation.HasBase then
    aAnimation.Target.Attributes[aAnimation.AttributeName] := aAnimation.Base
  else
    aAnimation.Target.RemoveAttribute(aAnimation.AttributeName);
end;


// True when no animation before aIndex writes the same attribute of the Same element as aIndex writes.
function TSVGTimeline.IsGroupHead(aIndex: Integer): Boolean;

var
  I: Integer;

begin
  for I := 0 to aIndex - 1 do
    if (FAnimations[I].Target = FAnimations[aIndex].Target)
       and (FAnimations[I].AttributeName = FAnimations[aIndex].AttributeName) then
      Exit(False);
  Result := True;
end;


// One value on top of another: a transform is concatenated with what it
// runs over, and any other kind is added to it
function ValueOver(const aAnimation: TSVGAnimation;
  const aUnder, aValue: TSVGString): TSVGString;

begin
  if Trim(aUnder) = '' then
    Exit(aValue);
  if aAnimation.Kind = akTransform then
    Exit(aUnder + ' ' + aValue);
  if not SVGAddValues(aAnimation.ValueKind, aUnder, aValue, Result) then
    Result := aValue;
end;


// The value the animations of one group give at a certain time
function TSVGTimeline.GroupValue(aIndex: Integer; aSeconds: Double;
  out aValue: TSVGString): Boolean;

var
  I: Integer;
  lOne, lUnder, lMotion, lInner: TSVGString;
  lHasMotion, lHasInner: Boolean;

begin
  aValue := '';
  Result := False;
  lMotion := '';
  lInner := '';
  lHasMotion := False;
  lHasInner := False;
  for I := aIndex to FCount - 1 do
    begin
    if (FAnimations[I].Target <> FAnimations[aIndex].Target)
       or (FAnimations[I].AttributeName <> FAnimations[aIndex].AttributeName) then
      Continue;
    if not FAnimations[I].ValueAt(aSeconds, lOne) then
      Continue;
    Result := True;
    if FAnimations[I].Kind = akMotion then
      begin
      if FAnimations[I].Additive and lHasMotion then
        lMotion := lMotion + ' ' + lOne
      else
        lMotion := lOne;
      lHasMotion := True;
      Continue;
      end;
    if not FAnimations[I].Additive then
      lInner := lOne
    else
      begin
      if lHasInner then
        lUnder := lInner
      else if FAnimations[aIndex].HasBase then
        lUnder := FAnimations[aIndex].Base
      else
        lUnder := '';
      lInner := ValueOver(FAnimations[I], lUnder, lOne);
      end;
    lHasInner := True;
    end;
  if not Result then
    Exit;
  // The element keeps the transform it was read with under a motion that
  // no animation of the attribute replaces.
  if not lHasInner then
    lInner := FAnimations[aIndex].Base;
  if Trim(lMotion) = '' then
    aValue := lInner
  else if Trim(lInner) = '' then
    aValue := lMotion
  else
    aValue := lMotion + ' ' + lInner;
end;


procedure TSVGTimeline.Seek(aSeconds: Double);

var
  I, lPass: Integer;
  lValue: TSVGString;

begin
  // The colour of an element is written before anything else, as the
  // cascade computes it first: an animation of another attribute may
  // name currentColor, which stands for what color holds at that moment.
  for lPass := 0 to 1 do
    for I := 0 to FCount - 1 do
      begin
      if not IsGroupHead(I) then
        Continue;
      if (FAnimations[I].AttributeName = ColorAttribute) <> (lPass = 0) then
        Continue;
      if GroupValue(I, aSeconds, lValue) then
        FAnimations[I].Target.Attributes[FAnimations[I].AttributeName] := lValue
      else
        WriteBase(FAnimations[I]);
      end;
end;


procedure TSVGTimeline.Reset;

var
  I: Integer;

begin
  for I := 0 to FCount - 1 do
    WriteBase(FAnimations[I]);
end;


function TSVGTimeline.ChangeTimes: TSVGDoubleArray;

var
  lTimes: TSVGDoubleArray;
  lCount, I, J, K, lStep: Integer;
  lBegin, lAt, lActive: Double;

begin
  Result := nil;
  if FCount = 0 then
    Exit;
  lTimes := nil;
  lCount := 0;
  AppendChangeTime(lTimes, lCount, 0);
  for I := 0 to FCount - 1 do
    for K := 0 to High(FAnimations[I].Begins) do
      begin
      lBegin := FAnimations[I].Begins[K];
      lActive := FAnimations[I].ActiveDuration(K);
      AppendChangeTime(lTimes, lCount, lBegin);
      if lActive >= 0 then
        AppendChangeTime(lTimes, lCount, lBegin + lActive);
      if (FAnimations[I].CalcMode <> cmDiscrete)
         or (FAnimations[I].Duration <= 0) or (lActive < 0)
         or (Length(FAnimations[I].Values) < 2) then
        Continue;
      lStep := 0;
      while (lStep * FAnimations[I].Duration < lActive)
            and (lCount < SVGMaxChangeTimes) do
        begin
        for J := 1 to Length(FAnimations[I].Values) - 1 do
          begin
          lAt := (lStep + FAnimations[I].KeyTimeAt(J))
               * FAnimations[I].Duration;
          if lAt < lActive then
            AppendChangeTime(lTimes, lCount, lBegin + lAt);
          end;
        Inc(lStep);
        end;
      end;
  SetLength(lTimes, lCount);
  Result := SortedTimes(lTimes);
end;


end.
