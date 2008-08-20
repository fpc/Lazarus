{ $Id$
                  ---------------------------------------
                  carbonclipboard.pp  -  Carbon clipboard
                  ---------------------------------------

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CarbonClipboard;

{$mode objfpc}{$H+}

interface

// debugging defines
{$I carbondebug.inc}

uses
 // rtl+ftl
  Types, Classes, SysUtils, Math, Contnrs,
 // carbon bindings
  MacOSAll,
 // LCL
  LCLProc, LCLType, Graphics, GraphType;

type

  { TCarbonClipboard }

  TCarbonClipboard = class
    FOwnerShips: Integer;
  private
    FPasteboards: Array [TClipboardType] of PasteboardRef;
    FFormats: TList; // list of CFStringRef UTIs
    FOnClipboardRequest: Array [TClipboardType] of TClipboardRequestEvent;
    
    function FindFormat(const UTI: CFStringRef): TClipboardFormat;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure CheckOwnerShip;
    function Clear(ClipboardType: TClipboardType): Boolean;
    function FormatToMimeType(FormatID: TClipboardFormat): String;
    function GetData(ClipboardType: TClipboardType; FormatID: TClipboardFormat;
      Stream: TStream): Boolean;
    function GetFormats(ClipboardType: TClipboardType; var Count: Integer;
      var List: PClipboardFormat): Boolean;
    function GetOwnerShip(ClipboardType: TClipboardType;
      OnRequestProc: TClipboardRequestEvent; FormatCount: Integer;
      Formats: PClipboardFormat): Boolean;
    function RegisterFormat(const AMimeType: String): TClipboardFormat;
  public
    property OwnerShips: Integer read FOwnerShips;
  end;

var
  ClipboardTypeToPasteboard: Array [TClipboardType] of CFStringRef =
  (
{ctPrimarySelection  } kPasteboardUniqueName, // local application pasteboard
{ctSecondarySelection} nil,                   // Find pasteboard
{ctClipboard         } nil                    // standard global pasteboard
  );
  Clipboard: TCarbonClipboard;


implementation

uses CarbonProc, CarbonDbgConsts;

{ TCarbonClipboard }

{------------------------------------------------------------------------------
  Method:  TCarbonClipboard.FindFormat
  Params:  UTI
  Returns: The corresponding registered format identifier
 ------------------------------------------------------------------------------}
function TCarbonClipboard.FindFormat(const UTI: CFStringRef): TClipboardFormat;
var
  I: Integer;
begin
  for I := 1 to FFormats.Count - 1 do
  begin
    if UTTypeEqual(UTI, CFStringRef(FFormats[I])) then
    begin
      Result := I;
      Exit;
    end;
  end;
  
  Result := 0;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonClipboard.Create
 ------------------------------------------------------------------------------}
constructor TCarbonClipboard.Create;
var
  T: TClipboardType;
begin
  for T := Low(TClipboardType) to High(TClipboardType) do
  begin
    OSError(
      PasteboardCreate(ClipboardTypeToPasteboard[T], FPasteboards[T]),
      Self, SCreate, 'PasteboardCreate', ClipboardTypeName[T]);

    FOnClipboardRequest[T] := nil;
  end;
  FOwnerShips := 0;
  
  FFormats := TList.Create;
  
  FFormats.Add(nil); // add default supported text formats
  FFormats.Add(kUTTypePlainText);
  FFormats.Add(kUTTypeUTF8PlainText);
  FFormats.Add(kUTTypeUTF16PlainText);
  
  RegisterFormat(PredefinedClipboardMimeTypes[pcfText]);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonClipboard.Destroy
 ------------------------------------------------------------------------------}
destructor TCarbonClipboard.Destroy;
var
  T: TClipboardType;
  I: Integer;
  S: CFStringRef;
begin
  for I := 4 to FFormats.Count - 1 do // 0..3 are predefined
  begin
    S := FFormats[I];
    FreeCFString(S);
  end;
  
  FFormats.Free;
  
  for T := Low(TClipboardType) to High(TClipboardType) do
    CFRelease(FPasteboards[T]);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonClipboard.CheckOwnerShip

  Checks the ownership
 ------------------------------------------------------------------------------}
procedure TCarbonClipboard.CheckOwnerShip;
var
  T: TClipboardType;
begin
  for T := Low(TClipboardType) to High(TClipboardType) do
  begin
    if FOnClipboardRequest[T] = nil then Continue;
    if (PasteboardSynchronize(FPasteboards[T]) and
      kPasteboardClientIsOwner) = 0 then
    begin  // inform LCL about ownership lost
      Dec(FOwnerShips);
      FOnClipboardRequest[T](0, nil);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonClipboard.Clear
  Params:  ClipboardType - Clipboard type
  Returns: If the function succeeds
  
  Clears the specified clipboard and gets ownership
 ------------------------------------------------------------------------------}
function TCarbonClipboard.Clear(ClipboardType: TClipboardType): Boolean;
var
  Pasteboard: PasteboardRef;
begin
  Result := False;
  Pasteboard := FPasteboards[ClipboardType];
  
  if OSError(PasteboardClear(Pasteboard), Self, 'Clear', 'PasteboardClear') then Exit;
  PasteboardSynchronize(Pasteboard);
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonClipboard.FormatToMimeType
  Params:  FormatID - A registered format identifier (0 is invalid)
  Returns: The corresponding mime type as string
 ------------------------------------------------------------------------------}
function TCarbonClipboard.FormatToMimeType(FormatID: TClipboardFormat): String;
var
  S: CFStringRef;
begin
  if (FormatID > 0) and (FormatID < FFormats.Count) then
  begin
    S := UTTypeCopyPreferredTagWithClass(CFStringRef(FFormats[FormatID]), kUTTagClassMIMEType);
    try
      Result := CFStringToStr(S);
    finally
      FreeCFString(S);
    end;
  end
  else
    Result := '';
end;

{------------------------------------------------------------------------------
  Method:  TCarbonClipboard.GetData
  Params:  ClipboardType - Clipboard type
           FormatID      - A registered format identifier (0 is invalid)
           Stream        - If format is available, it will be appended to this
                           stream
  Returns: If the function succeeds
 ------------------------------------------------------------------------------}
function TCarbonClipboard.GetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): Boolean;
var
  Pasteboard: PasteboardRef;
  I: Integer;
  UTI, CFString: CFStringRef;
  Encoding: CFStringEncoding;
  Flavors: CFArrayRef;
  FlavorData: CFDataRef;
  Count: ItemCount;
  ID: PasteboardItemID;
  S: String;
const
  SName = 'GetData';
  
  function HasFormat(Format: CFStringRef): Boolean;
  var
    FlavorCount: CFIndex;
    J: Integer;
  begin
    Result := False;
    FlavorCount := CFArrayGetCount(Flavors);
    
    for J := 0 to FlavorCount - 1 do
      if UTTypeEqual(Format, CFArrayGetValueAtIndex(Flavors, J)) then
      begin
        //DebugLn('Has UTI ' + CFStringToStr(Format));
        Result := True;
        Break;
      end;
  end;
  
begin
  Result := False;
  
  if not ((FormatID > 0) and (FormatID < FFormats.Count)) then
  begin
    DebugLn('TCarbonClipboard.GetData Error: Invalid Format ' + DbgS(FormatID) + ' specified!');
    Exit;
  end;
  
  Pasteboard := FPasteboards[ClipboardType];

  PasteboardSynchronize(Pasteboard);
  if OSError(PasteboardGetItemCount(Pasteboard, Count), Self, SName,
    'PasteboardGetItemCount') then Exit;
  if Count < 1 then Exit;
  
  for I := 1 to Count do
  begin
    if OSError(PasteboardGetItemIdentifier(Pasteboard, I, ID), Self, SName,
      'PasteboardGetItemIdentifier') then Continue;
    if OSError(PasteboardCopyItemFlavors(Pasteboard, ID, Flavors), Self, SName,
      'PasteboardCopyItemFlavors') then Continue;
      
    UTI := FFormats[FormatID];
    if FormatID = 1 then
    begin
      if HasFormat(FFormats[2]) then UTI := FFormats[2]   // check UTF-8 text
      else
        if HasFormat(FFormats[3]) then UTI := FFormats[3] // check UTF-16 text
        else
          if not HasFormat(UTI) then Exit;               // check plain text
    end
    else
      if not HasFormat(UTI) then Exit;

    //DebugLn('TCarbonClipboard.GetData Paste FlavorType: ' + CFStringToStr(UTI));

    if OSError(PasteboardCopyItemFlavorData(Pasteboard, ID, UTI, FlavorData),
      Self, SGetData, 'PasteboardCopyItemFlavorData') then Continue;
    try
      if CFDataGetLength(FlavorData) = 0 then
      begin
        Result := True;
        Exit;
      end;
      //DebugLn('TCarbonClipboard.GetData Paste FlavordataLength: ' + DbgS(CFDataGetLength(FlavorData)));

      if FormatID = 1 then
      begin
        if UTI =  FFormats[2] then // UTF-8 text
            Encoding := kCFStringEncodingUTF8;
        if UTI =  FFormats[3] then // UTF-16 text
            Encoding := kCFStringEncodingUTF16;
        if UTI =  FFormats[1] then // plain text
            Encoding := CFStringGetSystemEncoding;
        
        CreateCFString(FlavorData, Encoding, CFString);
        try
          S := CFStringtoStr(CFString);
          Stream.Write(S[1], Length(S));
        finally
          FreeCFString(CFString);
        end;
      end
      else
        Stream.Write(CFDataGetBytePtr(FlavorData)^, CFDataGetLength(FlavorData));
    finally
      CFRelease(FlavorData);
    end;

    Result := True;
    Exit;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonClipboard.GetFormats
  Params:  ClipboardType - The type of clipboard operation
           Count         - The number of clipboard formats
           List          - Pointer to an array of supported formats
                           (you must free it yourself)
  Returns: If the function succeeds
 ------------------------------------------------------------------------------}
function TCarbonClipboard.GetFormats(ClipboardType: TClipboardType;
  var Count: Integer; var List: PClipboardFormat): Boolean;
var
  Pasteboard: PasteboardRef;
  I, J: Integer;
  Flavors: CFArrayRef;
  UTI: CFStringRef;
  FlavorCount: CFIndex;
  FormatID: TClipboardFormat;
  C: ItemCount;
  ID: PasteboardItemID;
  Formats: TList;
const
  SName = 'GetFormats';
begin
  Result := False;

  Pasteboard := FPasteboards[ClipboardType];

  PasteboardSynchronize(Pasteboard);
  if OSError(PasteboardGetItemCount(Pasteboard, C), Self, SName,
    'PasteboardGetItemCount') then Exit;
  if C < 1 then Exit;
  
  Formats := TList.Create;
  try
    for I := 1 to C do
    begin
      if OSError(PasteboardGetItemIdentifier(Pasteboard, I, ID), Self, SName,
        'PasteboardGetItemIdentifier') then Continue;
      if OSError(PasteboardCopyItemFlavors(Pasteboard, ID, Flavors), Self, SName,
        'PasteboardCopyItemFlavors') then Continue;

      FlavorCount := CFArrayGetCount(Flavors);
      for J := 0 to FlavorCount - 1 do
      begin
        UTI := CFArrayGetValueAtIndex(Flavors, J);
        //DebugLn('TCarbonClipboard.GetFormats ' + CFStringToStr(UTI));

        FormatID := FindFormat(UTI);
        if FormatID = 0 then
          FormatID := FFormats.Add(UTI);
        if FormatID < 4 then // if it is text format, add plain text format
          if Formats.IndexOf(Pointer(1)) = -1 then Formats.Add(Pointer(1));


        if Formats.IndexOf(Pointer(FormatID)) = -1 then
        begin
          //DebugLn('TCarbonClipboard.GetFormats ' + FormatToMimeType(FormatID) +
          //  ' ' + CFStringToStr(UTI));
          Formats.Add(Pointer(FormatID));
        end;
      end;
    end;

    Count := Formats.Count;
    GetMem(List, Count * SizeOf(TClipboardFormat));
    for I := 0 to Count - 1 do List[i] := TClipboardFormat(Formats[I]);
  finally
    Formats.Free;
  end;
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonClipboard.GetOwnerShip
  Params:  ClipboardType - Type of clipboard
           OnRequestProc - TClipboardRequestEvent is defined in LCLType.pp
                           If OnRequestProc is nil the onwership will end.
           FormatCount   - Number of formats
           Formats       - Array of TClipboardFormat. The supported formats the
                           owner provides.

  Returns: If the function succeeds

  Sets the supported formats and requests ownership for the clipboard.
  The OnRequestProc is used to get the data from the LCL and to put it on the
  clipboard.
  If someone else requests the ownership, the OnRequestProc will be executed
  with the invalid FormatID 0 to notify the old owner of the lost of ownership.
 ------------------------------------------------------------------------------}
function TCarbonClipboard.GetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent; FormatCount: Integer;
  Formats: PClipboardFormat): Boolean;
  
  procedure AddData(Format: CFStringRef; CFData: CFDataRef);
  begin
    if CFData = nil then Exit;
    //DebugLn('Add Data ' + CFStringToStr(Format));
    
    OSError(PasteboardPutItemFlavor(FPasteboards[ClipboardType],
          PasteboardItemID(1), Format, CFData, 0),
        Self, 'GetOwnerShip', 'PasteboardPutItemFlavor');
  end;
    
  procedure PutOnClipboard;
  var
    DataStream: TStringStream;
    I: Integer;
    CFString: CFStringRef;
  begin
    DataStream := TStringStream.Create('');

    for I := 0 to FormatCount - 1 do
    begin
      if not ((Formats[I] > 0) and (Formats[I] < FFormats.Count)) then
      begin
        DebugLn('TCarbonClipboard.GetOwnerShip Error: Invalid Format ' + DbgS(Formats[I]) + ' specified!');
        Continue;
      end;
      
      DataStream.Size := 0;
      DataStream.Position := 0;
      FOnClipBoardRequest[ClipboardType](Formats[I], DataStream);

      if Formats[I] = 1 then // add more unicode and mac text formats
      begin
        CreateCFString(DataStream.DataString, CFString);
        try
          // UTF-8 text
          AddData(FFormats[2], CFStringToData(CFString, kCFStringEncodingUTF8));
          // UTF-16 text
          AddData(FFormats[3], CFStringToData(CFString, kCFStringEncodingUTF16));
          // plain text
          AddData(FFormats[1], CFStringToData(CFString, CFStringGetSystemEncoding));
        finally
          FreeCFString(CFString);
        end;
      end
      else
        AddData(FFormats[Formats[I]], CFDataCreate(nil, @DataStream.DataString[1],
          DataStream.Size));
    end;

    DataStream.Free;
  end;
  
begin
  Result := False;
  //DebugLn('TCarbonClipboard.GetOwnerShip');

  if (FormatCount = 0) or (OnRequestProc = nil) then
  begin
    // The LCL indicates it doesn't have the clipboard data anymore
    // and the interface can't use the OnRequestProc anymore.
    FOnClipboardRequest[ClipboardType] := nil;
    Dec(FOwnerShips);
  end
  else
  begin
    // clear OnClipBoardRequest to prevent destroying the LCL clipboard,
    // when emptying the clipboard
    FOnClipboardRequest[ClipboardType] := nil;
    if not Clear(ClipboardType) then Exit;
    
    Inc(FOwnerShips);
    FOnClipboardRequest[ClipboardType] := OnRequestProc;
    PutOnClipboard;
  end;
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonClipboard.RegisterFormat
  Params:  AMimeType - A string (usually a MIME type) identifying a new format
                       type to register
  Returns: The registered Format identifier (TClipboardFormat)
 ------------------------------------------------------------------------------}
function TCarbonClipboard.RegisterFormat(const AMimeType: String): TClipboardFormat;
var
  UTI, M: CFStringRef;
begin
  CreateCFString(AMimeType, M);
  try
    UTI := UTTypeCreatePreferredIdentifierForTag(kUTTagClassMIMEType, M, nil);
  finally
    FreeCFString(M);
  end;
  
  Result := FindFormat(UTI);
  if Result = 0 then
  begin
    //DebugLn('TCarbonClipboard.RegisterFormat ' + AMimeType + ' ' + CFStringToStr(UTI));
    Result := FFormats.Add(UTI);
  end
  else
    FreeCFString(UTI);
end;

initialization

  CreateCFString('com.apple.pasteboard.find', ClipboardTypeToPasteboard[ctSecondarySelection]);
  CreateCFString('com.apple.pasteboard.clipboard', ClipboardTypeToPasteboard[ctClipboard]);
  Clipboard := TCarbonClipboard.Create;

finalization

  FreeAndNil(Clipboard);
  FreeCFString(ClipboardTypeToPasteboard[ctSecondarySelection]);
  FreeCFString(ClipboardTypeToPasteboard[ctClipboard]);


end.
