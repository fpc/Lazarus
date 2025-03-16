unit IdeDebuggerWatchResUtils;

{$mode objfpc}{$H+}
{$IFDEF INLINE_OFF}{ $INLINE OFF}{$ENDIF}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, IdeDebuggerWatchResult, LazDebuggerIntf,
  IdeDebuggerWatchValueIntf;

type

  PStringBuilderPart = ^TStringBuilderPart;

  { TStringBuilderPart }

  TStringBuilderPart = record// object
  private type
    PHeader = ^THeader;
    THeader = record
      FTotalLen, FCount: Integer;
      FPrefix, FPostfix, FSeparator: String;
    end;
  private const
    HD_SIZE = sizeof(THeader);
  private
    FData: Pointer;
    FType: (sbfString, sbfStringList, sbfPartList);
    function GetAsString: String; inline;
    function GetStringCount: Integer; inline;
    function GetStrings(AnIndex: Integer): String; inline;
    function GetPartCount: Integer; inline;
    function GetParts(AnIndex: Integer): TStringBuilderPart; inline;
    function GetPartsAsString(AnIndex: Integer): String; inline;
    function GetPartsTotalLen(AnIndex: Integer): Integer; inline;
    function GetPostfix: String; inline;
    function GetPrefix: String; inline;
    function GetSeparator: String; inline;
    function GetRawPartsPtr(AnIndex: Integer): PStringBuilderPart; inline;
    function GetTotalLen: integer; inline;
    procedure SetAsString(const AValue: String); inline;
    procedure SetStringCount(AValue: Integer); inline;
    procedure SetStrings(AnIndex: Integer; const AValue: String); inline;
    procedure SetPartCount(AValue: Integer); inline;
    procedure SetParts(AnIndex: Integer; AValue: TStringBuilderPart); inline;
    procedure SetPartsAsString(AnIndex: Integer; const AValue: String); inline;
    procedure SetPostfix(const AValue: String); inline;
    procedure SetPrefix(const AValue: String); inline;
    procedure SetSeparator(const AValue: String); inline;
    procedure SetRawAsString(const AValue: String); inline;
    procedure SetRawStringCount(AValue: Integer); inline;
    procedure SetRawStrings(AnIndex: Integer; const AValue: String); inline;
    procedure SetRawPartCount(AValue: Integer); inline;
    procedure SetRawParts(AnIndex: Integer; const AValue: TStringBuilderPart); inline;
    procedure SetRawPartsAsString(AnIndex: Integer; const AValue: String);
    procedure SetRawPostfix(const AValue: String); inline;
    procedure SetRawPrefix(const AValue: String); inline;
    procedure SetRawSeparator(const AValue: String); inline;
    procedure DoFreeAll;
    procedure WriteTo(var ADest: PChar);
  public
    procedure Init; inline; // Similar to "RawAsString := ''"
    procedure FreeAll; inline;
    function GetFullString: String;
    property TotalLen: integer read GetTotalLen;

  public
    (* * Init: New TStringBuilderPart must be initialized with "Init"
       * Strings[], Parts[], PartsAsString:
         - Must only be accessed when the builder has been set to the matching
           list type using StringCount or PartCount (index must be in range / no checks)
       * Parts[]:
         - Must not be changed inline.
         - They must be assigned a new TStringBuilderPart that has its final value
         - Once assigned changes can be made using PartAsString, AppendToPart, PrependToPart
       * Prefix, PostFix, Separator:
         - Must only be accessed when the builder is a list
    *)

    (* *** Single String sbfString *** *)
    property AsString: String                                read GetAsString    write SetAsString;

    (* *** List of String sbfStringList *** *)
    property  StringCount: Integer                           read GetStringCount write SetStringCount;
    procedure ChangeStringCount(ANewCount: Integer; ATrim: Boolean = False);
    property  Strings[AnIndex: Integer]: String              read GetStrings     write SetStrings;

    (* *** List of embedded Parts (sub-builders) sbfPartList *** *)
    property  PartCount: Integer                             read GetPartCount   write SetPartCount;
    procedure ChangePartCount(ANewCount: Integer; ATrim: Boolean = False);
    property  Parts[AnIndex: Integer]: TStringBuilderPart    read GetParts       write SetParts;
    // Set the "AsString" of a part that is already in the list
    property  PartsAsString[AnIndex: Integer]: String        read GetPartsAsString write SetPartsAsString;
    property  PartsTotalLen[AnIndex: Integer]: Integer       read GetPartsTotalLen;

    // Append/Prepend to a part (either to the string, or Pre/FPostFix
    procedure PrependToPart(AnIndex: Integer; const AValue: String);
    procedure AppendToPart(AnIndex: Integer; const AValue: String);

    (* *** List ... *** *)
    property Prefix: String    read GetPrefix    write SetPrefix;
    property Postfix: String   read GetPostfix   write SetPostfix;
    property Separator: String read GetSeparator write SetSeparator;

  public
    (* *** RAW ACCESS ***
       * All data must be user initialized
         - The following methods will initialize data. (They are "write once". They must not be used to modify data):
           Init, RawAsString, RawPartCount, RawStringCount, RawPartsAsString
       * Once data/type (string, string-list, part-list) has been set/chosen, it must not be changed anymore
       * RawParts:
         - Each Part in "RawParts" must be either
           - assigned once (with a part that has its data already)
           - set with "PartAsString"
         - Once a sub-part has been added, it must not be replaced
         - Once a sub-part has been added, it must not be modified anymore
           (except with Prepend/AppendToPart)
         - Once a sub-part has been added, it is owned and must not be freed by itself
         - Once a value has been set (subvalue, post/Rawprefix, ...), it must not be changed
         - Change...Count is only to cut off uninitialized entries. (data will not be freed, if it had been assigned)
         - Before "FreeAll": All strings/Rawparts must have been initialized (use RawChangeCount to cut off uninitialized entries)
      *** Ptr / Pointer to String ***
      * Using the builders string variable (via pointer) allows the compiler to reduce temporary string vars
    *)
    (* *** Single String sbfString *** *)
    property  RawAsString: String                               read GetAsString    write SetRawAsString;
    function  RawAsStringPtr: PString;
    (* *** List of String sbfStringList *** *)
    property  RawStringCount: Integer                           read GetStringCount write SetRawStringCount;
    procedure RawChangeStringCount(ANewCount: Integer);
    property  RawStrings[AnIndex: Integer]: String              read GetStrings     write SetRawStrings;
    (* *** List of embedded RawParts (sub-builders) sbfPartList *** *)
    property  RawPartCount: Integer                             read GetPartCount   write SetRawPartCount;
    procedure RawChangePartCount(ANewCount: Integer);
    property  RawParts[AnIndex: Integer]: TStringBuilderPart    read GetParts       write SetRawParts;
    property  RawPartsPtr[AnIndex: Integer]: PStringBuilderPart read GetRawPartsPtr;
    // Set the "RawAsString" of a part that is already in the list (if it has no value yet)
    property  RawPartsAsString[AnIndex: Integer]: String        read GetPartsAsString write SetRawPartsAsString;
    (* *** List ... *** *)
    property RawPrefix: String    read GetPrefix    write SetRawPrefix;
    property RawPostfix: String   read GetPostfix   write SetRawPostfix;
    property RawSeparator: String read GetSeparator write SetRawSeparator;
  end;

function ExtractProcResFromMethod(AMethodRes: TWatchResultData): TWatchResultData;
function ExtractInstanceResFromMethod(AMethodRes: TWatchResultData): TWatchResultData;

implementation

function ExtractProcResFromMethod(AMethodRes: TWatchResultData
  ): TWatchResultData;
begin
  Result := nil;
  if (AMethodRes <> nil) and
     (AMethodRes.StructType = dstRecord) and
     (AMethodRes.FieldCount = 2) and
     (LowerCase(AMethodRes.Fields[0].FieldName) = 'proc') and
     (AMethodRes.Fields[0].Field <> nil) and
     (AMethodRes.Fields[0].Field.ValueKind in [rdkFunction, rdkProcedure, rdkFunctionRef, rdkProcedureRef]) and
     (LowerCase(AMethodRes.Fields[1].FieldName) = 'self') and
     (AMethodRes.Fields[1].Field <> nil) and
     (AMethodRes.Fields[1].Field.ValueKind = rdkStruct)
  then
    Result := AMethodRes.Fields[0].Field;
end;

function ExtractInstanceResFromMethod(AMethodRes: TWatchResultData
  ): TWatchResultData;
begin
  Result := nil;
  if (AMethodRes <> nil) and
     (AMethodRes.StructType = dstRecord) and
     (AMethodRes.FieldCount = 2) and
     (LowerCase(AMethodRes.Fields[0].FieldName) = 'proc') and
     (AMethodRes.Fields[0].Field <> nil) and
     (AMethodRes.Fields[0].Field.ValueKind in [rdkFunction, rdkProcedure, rdkFunctionRef, rdkProcedureRef]) and
     (LowerCase(AMethodRes.Fields[1].FieldName) = 'self') and
     (AMethodRes.Fields[1].Field <> nil) and
     (AMethodRes.Fields[1].Field.ValueKind = rdkStruct)
  then
    Result := AMethodRes.Fields[1].Field;
end;

{ TStringBuilderPart }

function TStringBuilderPart.GetAsString: String;
begin
  if FType = sbfString then
    Result := String(FData)
  else
    Result := '';
end;

function TStringBuilderPart.GetStringCount: Integer;
begin
  if (FType = sbfStringList) and (FData <> nil) then
    Result := PHeader(FData)^.FCount
  else
    Result := 0;
end;

function TStringBuilderPart.GetStrings(AnIndex: Integer): String;
begin
  Result := PString(FData + HD_SIZE)[AnIndex];
end;

function TStringBuilderPart.GetPartCount: Integer;
begin
  if (FType = sbfPartList) and (FData <> nil) then
    Result := PHeader(FData)^.FCount
  else
    Result := 0;
end;

function TStringBuilderPart.GetParts(AnIndex: Integer): TStringBuilderPart;
begin
  Result := PStringBuilderPart(FData + HD_SIZE)[AnIndex];
end;

function TStringBuilderPart.GetPartsAsString(AnIndex: Integer): String;
var
  p: PStringBuilderPart;
begin
  p := @PStringBuilderPart(FData + HD_SIZE)[AnIndex];
  if p^.FType = sbfString then
    Result := String(p^.FData)
  else
    Result := '';
end;

function TStringBuilderPart.GetPartsTotalLen(AnIndex: Integer): Integer;
var
  p: PStringBuilderPart;
begin
  p := @PStringBuilderPart(FData + HD_SIZE)[AnIndex];
  if p^.FType = sbfString then
    Result := Length(String(p^.FData))
  else
    Result := PHeader(p^.FData)^.FTotalLen;
end;

function TStringBuilderPart.GetPostfix: String;
begin
  if (FType <> sbfString) and (FData <> nil) then
    Result := PHeader(FData)^.FPostfix
  else
    Result := '';
end;

function TStringBuilderPart.GetPrefix: String;
begin
  if (FType <> sbfString) and (FData <> nil) then
    Result := PHeader(FData)^.FPostfix
  else
    Result := '';
end;

function TStringBuilderPart.GetSeparator: String;
begin
  if (FType <> sbfString) and (FData <> nil) then
    Result := PHeader(FData)^.FSeparator
  else
    Result := '';
end;

function TStringBuilderPart.GetRawPartsPtr(AnIndex: Integer): PStringBuilderPart;
begin
  Result := @PStringBuilderPart(FData + HD_SIZE)[AnIndex];
end;

function TStringBuilderPart.GetTotalLen: integer;
begin
  if (FType <> sbfString) and (FData <> nil) then
    Result := PHeader(FData)^.FTotalLen
  else
    Result := Length(string(FData));
end;

procedure TStringBuilderPart.SetAsString(const AValue: String);
begin
  if FType <> sbfString then FreeAll;
  FType := sbfString;
  String(FData) := AValue;
end;

procedure TStringBuilderPart.SetStringCount(AValue: Integer);
begin
  if FType = sbfStringList then begin
    ChangeStringCount(AValue, PHeader(FData)^.FCount > 2 * AValue);
  end
  else begin
    if FData <> nil then
      FreeAll;
    FType := sbfStringList;
    FData := AllocMem(HD_SIZE + AValue * SizeOf(string));
    PHeader(FData)^.FCount := AValue;
  end;
end;

procedure TStringBuilderPart.SetStrings(AnIndex: Integer; const AValue: String);
var
  ps: PString;
begin
  ps := @PString(FData + HD_SIZE)[AnIndex];
  PHeader(FData)^.FTotalLen := PHeader(FData)^.FTotalLen - Length(ps^) + Length(AValue);
  ps^ := AValue;
end;

procedure TStringBuilderPart.SetPartCount(AValue: Integer);
begin
  if FType = sbfPartList then begin
    ChangePartCount(AValue, PHeader(FData)^.FCount > 2 * AValue);
  end
  else begin
    if FData <> nil then
      FreeAll;
    FType := sbfPartList;
    FData := AllocMem(HD_SIZE + AValue * SizeOf(TStringBuilderPart));
    PHeader(FData)^.FCount := AValue;
  end;
end;

procedure TStringBuilderPart.SetParts(AnIndex: Integer; AValue: TStringBuilderPart);
var
  pp: PStringBuilderPart;
begin
  pp := @PStringBuilderPart(FData + HD_SIZE)[AnIndex];
  PHeader(FData)^.FTotalLen := PHeader(FData)^.FTotalLen - pp^.TotalLen + AValue.TotalLen;
  pp^ := AValue;
end;

procedure TStringBuilderPart.SetPartsAsString(AnIndex: Integer; const AValue: String);
var
  p: PStringBuilderPart;
  t: Integer;
begin
  p := @PStringBuilderPart(FData + HD_SIZE)[AnIndex];
  t := PHeader(FData)^.FTotalLen;
  if p^.FType <> sbfString then begin
    t := t - + p^.TotalLen;
    p^.FreeAll;
  end
  else
    t := t - Length(String(p^.FData));

  p^.FType := sbfString;
  String(p^.FData) := AValue;
  t := t + Length(AValue);
  PHeader(FData)^.FTotalLen := t;
end;

procedure TStringBuilderPart.SetPostfix(const AValue: String);
begin
  PHeader(FData)^.FTotalLen := PHeader(FData)^.FTotalLen - Length(PHeader(FData)^.FPostfix) + Length(AValue);
  PHeader(FData)^.FPostfix := AValue;
end;

procedure TStringBuilderPart.SetPrefix(const AValue: String);
begin
  PHeader(FData)^.FTotalLen := PHeader(FData)^.FTotalLen - Length(PHeader(FData)^.FPrefix) + Length(AValue);
  PHeader(FData)^.FPrefix := AValue;
end;

procedure TStringBuilderPart.SetSeparator(const AValue: String);
begin
  PHeader(FData)^.FTotalLen := PHeader(FData)^.FTotalLen + (Length(AValue) - Length(PHeader(FData)^.FSeparator)) * (PHeader(FData)^.FCount - 1);
  PHeader(FData)^.FSeparator := AValue;
end;

procedure TStringBuilderPart.SetRawAsString(const AValue: String);
begin
  FType := sbfString;
  FData := nil;
  String(FData) := AValue;
end;

procedure TStringBuilderPart.SetRawStringCount(AValue: Integer);
begin
  FType := sbfStringList;
  FData := GetMem(HD_SIZE + AValue * SizeOf(string));
  FillChar(FData^,HD_SIZE,0);
  PHeader(FData)^.FCount := AValue;
end;

procedure TStringBuilderPart.SetRawStrings(AnIndex: Integer; const AValue: String);
begin
  PString(FData + HD_SIZE)[AnIndex] := AValue;
  PHeader(FData)^.FTotalLen := PHeader(FData)^.FTotalLen + Length(AValue);
end;

procedure TStringBuilderPart.SetRawPartCount(AValue: Integer);
begin
  FType := sbfPartList;
  FData := GetMem(HD_SIZE + AValue * SizeOf(TStringBuilderPart));
  FillChar(FData^,HD_SIZE,0);
  PHeader(FData)^.FCount := AValue;
end;

procedure TStringBuilderPart.SetRawParts(AnIndex: Integer; const AValue: TStringBuilderPart);
begin
  PStringBuilderPart(FData + HD_SIZE)[AnIndex] := AValue;
  PHeader(FData)^.FTotalLen := PHeader(FData)^.FTotalLen + AValue.TotalLen;
end;

procedure TStringBuilderPart.SetRawPartsAsString(AnIndex: Integer; const AValue: String);
var
  p: PStringBuilderPart;
begin
  p := @PStringBuilderPart(FData + HD_SIZE)[AnIndex];
  p^.FType := sbfString;
  p^.FData := nil;
  String(p^.FData) := AValue;
  PHeader(FData)^.FTotalLen := PHeader(FData)^.FTotalLen + Length(AValue);
end;

procedure TStringBuilderPart.SetRawPostfix(const AValue: String);
begin
  PHeader(FData)^.FPostfix := AValue;
  PHeader(FData)^.FTotalLen := PHeader(FData)^.FTotalLen + Length(AValue);
end;

procedure TStringBuilderPart.SetRawPrefix(const AValue: String);
begin
  PHeader(FData)^.FPrefix := AValue;
  PHeader(FData)^.FTotalLen := PHeader(FData)^.FTotalLen + Length(AValue);
end;

procedure TStringBuilderPart.SetRawSeparator(const AValue: String);
begin
  PHeader(FData)^.FSeparator := AValue;
  PHeader(FData)^.FTotalLen := PHeader(FData)^.FTotalLen + Length(AValue) * (PHeader(FData)^.FCount - 1);
end;

procedure TStringBuilderPart.DoFreeAll;
var
  ps: PString;
  pp: PStringBuilderPart;
  i: Integer;
begin
  case FType of
    //sbfString:
    //    Finalize(String(FData));
    sbfStringList: begin
        Finalize(PHeader(FData)^.FPrefix);
        Finalize(PHeader(FData)^.FPostfix);
        Finalize(PHeader(FData)^.FSeparator);
        ps := PString(FData + HD_SIZE);
        for i := 0 to PHeader(FData)^.FCount - 1 do begin
          Finalize(ps^);
          inc(ps);
        end;
        Freemem(FData);
      end;
    sbfPartList: begin
        Finalize(PHeader(FData)^.FPrefix);
        Finalize(PHeader(FData)^.FPostfix);
        Finalize(PHeader(FData)^.FSeparator);
        pp := PStringBuilderPart(FData + HD_SIZE);
        for i := 0 to PHeader(FData)^.FCount - 1 do begin
          if pp^.FType = sbfString then
            Finalize(String(pp^.FData))
          else
          //if pp^.FData <> nil then
            pp^.DoFreeAll;
          inc(pp);
        end;
        Freemem(FData);
      end;
  end;
end;

procedure TStringBuilderPart.WriteTo(var ADest: PChar);
var
  ps: PString;
  pp: PStringBuilderPart;
  i, c: Integer;
  txt: PChar;
  len: Integer;
begin
  if FData = nil then
    exit;

  case FType of
    sbfString: begin
        len := Length(String(FData));
        move(FData^, ADest^, len);
        inc(ADest, len);
      end;
    sbfStringList: begin
        len := Length(PHeader(FData)^.FPrefix);
        if len > 0 then begin
          move(PChar(PHeader(FData)^.FPrefix)^, ADest^, len);
          inc(ADest, len)
        end;
        len := Length(PHeader(FData)^.FSeparator);
        ps := PString(FData + HD_SIZE);
        c := PHeader(FData)^.FCount;
        if (c > 1) and (len > 0) then begin
          txt := PChar(PHeader(FData)^.FSeparator);
          for i := c - 2 downto 0 do begin
            move(pchar(ps^)^, ADest^, Length(String(ps^)));
            inc(ADest, Length(ps^));
            inc(ps);
            move(txt^, ADest^, len);
            inc(ADest, len)
          end;
          move(pchar(ps^)^, ADest^, Length(String(ps^)));
          inc(ADest, Length(ps^));
        end
        else begin
          for i := c - 1 downto 0 do begin
            move(pchar(ps^)^, ADest^, Length(String(ps^)));
            inc(ADest, Length(ps^));
            inc(ps);
          end;
        end;
        len := Length(PHeader(FData)^.FPostfix);
        if len > 0 then begin
          move(PChar(PHeader(FData)^.FPostfix)^, ADest^, len);
          inc(ADest, len)
        end;
      end;
    sbfPartList: begin
        len := Length(PHeader(FData)^.FPrefix);
        if len > 0 then begin
          move(PChar(PHeader(FData)^.FPrefix)^, ADest^, len);
          inc(ADest, len)
        end;
        len := Length(PHeader(FData)^.FSeparator);
        pp := PStringBuilderPart(FData + HD_SIZE);
        c := PHeader(FData)^.FCount;
        if (c > 1) and (len > 0) then begin
          txt := PChar(PHeader(FData)^.FSeparator);
          for i := c - 2 downto 0 do begin
            pp^.WriteTo(ADest);
            inc(pp);
            move(txt^, ADest^, len);
            inc(ADest, len)
          end;
          pp^.WriteTo(ADest);
        end
        else begin
          for i := c- 1 downto 0 do begin
            pp^.WriteTo(ADest);
            inc(pp);
          end;
        end;
        len := Length(PHeader(FData)^.FPostfix);
        if len > 0 then begin
          move(PChar(PHeader(FData)^.FPostfix)^, ADest^, len);
          inc(ADest, len)
        end;
      end;
  end;
end;

procedure TStringBuilderPart.Init;
begin
  FData := Nil;
end;

procedure TStringBuilderPart.FreeAll;
begin
  if FData = nil then
    exit;

  if FType = sbfString then
    Finalize(String(FData))
  else
    DoFreeAll;
  FData := nil;
end;

function TStringBuilderPart.GetFullString: String;
var
  p: PChar;
begin
  Result := '';
  if FData = nil then
    exit();
  if FType = sbfString then
    exit(string(FData));
  if PHeader(FData)^.FTotalLen = 0 then
    exit;

  SetLength(Result, PHeader(FData)^.FTotalLen);
  p := pchar(Result);
  WriteTo(p);
  assert(p = Pchar(Result)+Length(Result), 'TStringBuilderPart.GetFullString: p = Pchar(Result)+Length(Result)');
end;

procedure TStringBuilderPart.ChangeStringCount(ANewCount: Integer; ATrim: Boolean);
var
  i, t: Integer;
  ps: PString;
begin
  t := PHeader(FData)^.FTotalLen;
  ps := @PString(FData + HD_SIZE)[ANewCount];
  for i := ANewCount to PHeader(FData)^.FCount -1 do begin
    t := t - Length(ps^);
    ps^ := '';
    inc(ps);
  end;
  if ATrim or (ANewCount > PHeader(FData)^.FCount) then
    FData := ReAllocMem(FData, HD_SIZE + ANewCount * SizeOf(string));
  t := t + Length(PHeader(FData)^.FSeparator) * (ANewCount - PHeader(FData)^.FCount);
  PHeader(FData)^.FTotalLen := t;
  PHeader(FData)^.FCount := ANewCount;
end;

procedure TStringBuilderPart.ChangePartCount(ANewCount: Integer; ATrim: Boolean);
var
  i, t: Integer;
  pp: PStringBuilderPart;
begin
  t := PHeader(FData)^.FTotalLen;
  pp := @PStringBuilderPart(FData + HD_SIZE)[ANewCount];
  for i := ANewCount to PHeader(FData)^.FCount -1 do begin
    t := t - pp^.TotalLen;
    pp^.FreeAll;
    inc(pp);
  end;
  if ATrim or (ANewCount > PHeader(FData)^.FCount) then
    FData := ReAllocMem(FData, HD_SIZE + ANewCount * SizeOf(TStringBuilderPart));
  t := t + Length(PHeader(FData)^.FSeparator) * (ANewCount - PHeader(FData)^.FCount);
  PHeader(FData)^.FTotalLen := t;
  PHeader(FData)^.FCount := ANewCount;
end;

procedure TStringBuilderPart.PrependToPart(AnIndex: Integer; const AValue: String);
var
  p: PStringBuilderPart;
begin
  p := @PStringBuilderPart(FData + HD_SIZE)[AnIndex];
  case p^.FType of
    sbfString: String(p^.FData) := AValue + String(p^.FData);
    sbfStringList,
    sbfPartList: begin
      PHeader(p^.FData)^.FPrefix := AValue + PHeader(p^.FData)^.FPrefix;
      PHeader(p^.FData)^.FTotalLen := PHeader(p^.FData)^.FTotalLen + Length(AValue);
    end;
  end;
  PHeader(FData)^.FTotalLen := PHeader(FData)^.FTotalLen + Length(AValue);
end;

procedure TStringBuilderPart.AppendToPart(AnIndex: Integer; const AValue: String);
var
  p: PStringBuilderPart;
begin
  p := @PStringBuilderPart(FData + HD_SIZE)[AnIndex];
  case p^.FType of
    sbfString: String(p^.FData) := String(p^.FData) + AValue;
    sbfStringList,
    sbfPartList: begin
      PHeader(p^.FData)^.FPostfix := PHeader(p^.FData)^.FPostfix + AValue;
      PHeader(p^.FData)^.FTotalLen := PHeader(p^.FData)^.FTotalLen + Length(AValue);
    end;
  end;
  PHeader(FData)^.FTotalLen := PHeader(FData)^.FTotalLen + Length(AValue);
end;

function TStringBuilderPart.RawAsStringPtr: PString;
begin
  FType := sbfString;
  FData := nil;
Result := PString(@FData )
end;

procedure TStringBuilderPart.RawChangeStringCount(ANewCount: Integer);
begin
  if ANewCount > PHeader(FData)^.FCount then
    FData := ReAllocMem(FData, HD_SIZE + ANewCount * SizeOf(string));
  PHeader(FData)^.FTotalLen := PHeader(FData)^.FTotalLen + Length(PHeader(FData)^.FSeparator) * (ANewCount - PHeader(FData)^.FCount);
  PHeader(FData)^.FCount := ANewCount;
end;

procedure TStringBuilderPart.RawChangePartCount(ANewCount: Integer);
begin
  if ANewCount > PHeader(FData)^.FCount then
    FData := ReAllocMem(FData, HD_SIZE + ANewCount * SizeOf(TStringBuilderPart));
  PHeader(FData)^.FTotalLen := PHeader(FData)^.FTotalLen + Length(PHeader(FData)^.FSeparator) * (ANewCount - PHeader(FData)^.FCount);
  PHeader(FData)^.FCount := ANewCount;
end;

end.

