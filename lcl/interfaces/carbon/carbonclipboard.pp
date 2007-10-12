{ $Id$
                  ---------------------------------------
                  carbonclipboard.pp  -  Carbon clipboard
                  ---------------------------------------

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  FPCMacOSAll,
 // LCL
  LCLProc, LCLType, Graphics, GraphType;

type

  { TCarbonClipboard }

  TCarbonClipboard = class
  private
    FPasteboards: Array [TClipboardType] of PasteboardRef;
    FFormats: TList; // list of CFStringRef UTIs, 1 is reserved for text/plain
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
  end;

var
  ClipboardTypeToPasteboard: Array [TClipboardType] of CFStringRef =
  (
{ctPrimarySelection  } kPasteboardUniqueName, // local application pasteboard
{ctSecondarySelection} nil,       // Find pasteboard
{ctClipboard         } nil   // standard global pasteboard
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
    if UTTypeConformsTo(UTI, CFStringRef(FFormats[I])) then
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
  FFormats := TList.Create;
  FFormats.Add(nil);
  
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
  for I := 0 to FFormats.Count - 1 do
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
    if FormatID = 1 then
    begin
      Result := PredefinedClipboardMimeTypes[pcfText];
      Exit;
    end;
    
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
  I, J: Integer;
  L: SizeUInt;
  Flavors: CFArrayRef;
  UTI: CFStringRef;
  FlavorCount: CFIndex;
  FlavorData: CFDataRef;
  Count: ItemCount;
  ID: PasteboardItemID;
  S: String;
const
  SName = 'GetData';
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
      
    FlavorCount := CFArrayGetCount(Flavors);
    for J := 0 to FlavorCount - 1 do
    begin
      UTI := CFArrayGetValueAtIndex(Flavors, J);
      //DebugLn('TCarbonClipboard.GetData FlavorType: ' + CFStringToStr(UTI) +
      //  ' ' + CFStringToStr(FFormats[FormatID]));
      if UTTypeConformsTo(FFormats[FormatID], UTI) then
      begin
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

          if FormatID = 1 then // convert text/plain UTF-16 to UTF-8
          begin
            SetLength(S, (CFDataGetLength(FlavorData) div 2) * 3);
            if ConvertUTF16ToUTF8(PChar(S), Length(S) + 1,
              PWideChar(CFDataGetBytePtr(FlavorData)), CFDataGetLength(FlavorData) div 2,
              [toInvalidCharToSymbol], L) <> trNoError then Exit;

            SetLength(S, L - 1);
            Stream.Write(S[1], L - 1);
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

        FormatID := FindFormat(UTI);
        if FormatID = 0 then
          FormatID := FFormats.Add(UTI);

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
  
  procedure PutOnClipboard;
  var
    Data: CFDataRef;
    UTI: CFStringRef;
    DataStream: TMemoryStream;
    I: Integer;
    L: SizeUInt;
    W: WideString;
  begin
    DataStream := TMemoryStream.Create;

    for I := 0 to FormatCount - 1 do
    begin
      DataStream.Size := 0;
      DataStream.Position := 0;

      if not ((Formats[I] > 0) and (Formats[I] < FFormats.Count)) then
      begin
        DebugLn('TCarbonClipboard.GetOwnerShip Error: Invalid Format ' + DbgS(Formats[I]) + ' specified!');
        Continue;
      end;
      
      UTI := CFStringRef(FFormats[Formats[I]]);
      FOnClipBoardRequest[ClipboardType](Formats[I], DataStream);
      
      if Formats[I] = 1 then // convert plain/text UTF-8 to UTF-16
      begin
        SetLength(W, DataStream.Size);
        if ConvertUTF8ToUTF16(PWideChar(W), Length(W) + 1, PChar(DataStream.Memory),
          DataStream.Size, [toInvalidCharToSymbol], L) <> trNoError then Exit;
          
        SetLength(W, L - 1);
        Data := CFDataCreate(nil, PChar(W), (L - 1) * 2);
      end
      else
        Data := CFDataCreate(nil, DataStream.Memory, DataStream.Size);

      OSError(PasteboardPutItemFlavor(FPasteboards[ClipboardType],
          PasteboardItemID(1), UTI, Data, 0),
        Self, 'GetOwnerShip', 'PasteboardPutItemFlavor');
    end;

    DataStream.Free;
  end;
  
begin
  Result := False;
  DebugLn('TCarbonClipboard.GetOwnerShip');

  if (FormatCount = 0) or (OnRequestProc = nil) then
  begin
    // The LCL indicates it doesn't have the clipboard data anymore
    // and the interface can't use the OnRequestProc anymore.
    FOnClipboardRequest[ClipboardType] := nil;
  end
  else
  begin
    // clear OnClipBoardRequest to prevent destroying the LCL clipboard,
    // when emptying the clipboard
    FOnClipboardRequest[ClipboardType] := nil;
    if not Clear(ClipboardType) then Exit;
    
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
  if AMimeType = PredefinedClipboardMimeTypes[pcfText] then
    CreateCFString('public.utf16-plain-text', UTI)
  else
  begin
    CreateCFString(AMimeType, M);
    try
      UTI := UTTypeCreatePreferredIdentifierForTag(kUTTagClassMIMEType, M, nil);
    finally
      FreeCFString(M);
    end;
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

  Clipboard.Free;
  FreeCFString(ClipboardTypeToPasteboard[ctSecondarySelection]);
  FreeCFString(ClipboardTypeToPasteboard[ctClipboard]);


end.
