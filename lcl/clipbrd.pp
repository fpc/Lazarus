{
 /***************************************************************************
                               Clipbrd.pp
                             -------------------
                             Component Library Clipboard Controls
                   Initial Revision  : Sat Feb 26 2000


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{
@created(26-Feb-2000)
@lastmod(12-Nov-2001)

    @abstract(This is the clipboard class for Copy/Paste functions)
    Introduced by Shane Miller <smiller@lakefield.net>
    Rewrite done by Hongli Lai <hongli@telekabel.nl>
    Rewrite done by Mattias Gaertner <gaertner@informatik.uni-koeln.de>

Clipboard unit.
For Copying and Pasting.  You know what it's for!  Why am I explaining it?  :-)


  The clipboard object encapsulates the Windows clipboard and the three
  standard Gtk selections. For each of the three clipboards/selections there is
  an object: PrimarySelection, SecondarySelection and Clipboard.
  There is no difference between the three objects except their type.
  
  Brief explanation of TClipboard:

  AddFormat:
    Use these functions to add data to the supported formats.
  Assign:
    Add the data to the clipboard with the corresponding FormatID.
  Clear:
    Clears the clipboard.
  FindPictureFormatID
    Search the first FormatID that is a registered TGraphic.
  GetComponent
    Read a component from clipboard
  GetFormat
    Read data from clipboard
  SupportedFormats
    Fills a TStrings list with the supported mime type.
  SupportedFormats
    Returns an array of suupported formats. You must free the memory with
    FreeMem.
  GetTextBuf
    Fetch text from clipboard, if supported.
  HasFormat
    Look up if the format is supported. If Format is the TPicture format
    (CF_PICTURE) all registered graphics formats are tested.
  HasPictureFormat
    Returns true if FindPictureFormatID<>0
  SetComponent
    Write a component to the clipboard.
  SetFormat
    Clears the clipboard and adds the data.
  SetSupportedFormats
    Set all supported formats at once. All data will be empty. This procedure
    is useful if setting the OnRequest event to put the data on the fly.
    Example: Using the PrimarySelection from synedit.pp
      procedure TCustomSynEdit.AquirePrimarySelection;
      var
        FormatList: TClipboardFormat;
      begin
        if (not SelAvail) 
        or (PrimarySelection.OnRequest=@PrimarySelectionRequest) then exit;
        FormatList:=CF_TEXT;
        PrimarySelection.SetSupportedFormats(1,@FormatList);
        PrimarySelection.OnRequest:=@PrimarySelectionRequest;
      end;
      
  SetTextBuf
    Add text to the clipboard
  AsText
    Get text from or set text to the clipboard.
  ClipboardType
    The type of the clipboard object. For example:
      PrimarySelection.ClipboardType = ctPrimarySelection
  FormatCount
    Number of supported formats
  Formats
    You can read the formats with this property one by one. But this will result
    in many requests, which can be very slow (especially on terminals).
    Better use "SupportedFormats".
  OnRequest
    If the clipboard has the ownership, each time data is requested by the
    application or another application from the clipboard this event will be
    called. There is one special case: If the clipboard looses ownership the
    OnRequest event will be called with FormatID=0.
    This event will be erased on lost of ownership.
    If the OnRequest event was already set before, the prior method will be
    called with FormatID=0 to be notified of the loss.
  
  
  For mime types see:
    http://www.iana.org/assignments/media-types
}

unit Clipbrd;

{$MODE Objfpc}{$H+}

interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
  Classes, SysUtils, fasthtmlparser,
  // LCL
  LCLType, LCLIntf, LResources, Graphics,
  // LazUtils
  FPCAdds, LazUTF8, LazLoggerBase;

{ for delphi compatibility:

  In Delphi there are 4 predefined constants, but the LCL has only dynamic values.
  
  CF_TEXT = 1;
  CF_BITMAP = 2;
  CF_METAFILEPICT = 3;

  CF_OBJECT = 230
}
function CF_Text: TClipboardFormat;
function CF_Bitmap: TClipboardFormat;
function CF_Picture: TClipboardFormat;
function CF_MetaFilePict: TClipboardFormat;
function CF_Object: TClipboardFormat;
function CF_Component: TClipboardFormat;
function CF_HTML: TClipboardformat;

type
  TClipboardData = record
    FormatID: TClipboardFormat;
    Stream: TMemoryStream;
  end;

  { TClipboard }

  TClipboard = Class(TPersistent)
  private
    FAllocated: Boolean;    // = has ownership
    FClipboardType: TClipboardType;
    FCount: integer;        // # formats of cached clipboard data
    FData: ^TClipboardData; // cached clipboard data
    FSupportedFormatsChanged: boolean;
    FOnRequest: TClipboardRequestEvent;
    FOpenRefCount: Integer; // reference count for Open and Close (not used yet)
    procedure AssignGraphic(Source: TGraphic);
    procedure AssignGraphic(Source: TGraphic; FormatID: TClipboardFormat);
    procedure AssignPicture(Source: TPicture);
    function AssignToGraphic(Dest: TGraphic): boolean;
    function AssignToGraphic(Dest: TGraphic; FormatID: TClipboardFormat): boolean;
    //procedure AssignToMetafile(Dest: TMetafile);
    procedure AssignToPicture(Dest: TPicture);
    function GetAsText: string;
    function GetFormatCount: Integer;
    function GetFormats(Index: Integer): TClipboardFormat;
    function GetOwnerShip: boolean;
    function IndexOfCachedFormatID(FormatID: TClipboardFormat; 
      CreateIfNotExists: boolean): integer;
    procedure InternalOnRequest(const RequestedFormatID: TClipboardFormat;
      AStream: TStream);
    procedure SetAsText(const Value: string);
    function SetBuffer(FormatID: TClipboardFormat;
                       var Buffer; Size: Integer): Boolean;
    procedure SetOnRequest(AnOnRequest: TClipboardRequestEvent);
    procedure BeginUpdate;
    function EndUpdate: Boolean;
    function IsUpdating: Boolean;
    function CanReadFromInterface: Boolean;
    function CanReadFromCache: Boolean;
    procedure OnDefaultFindClass(Reader: TReader; const AClassName: string;
                                 var ComponentClass: TComponentClass);
  public
    function AddFormat(FormatID: TClipboardFormat; Stream: TStream): Boolean;
    function AddFormat(FormatID: TClipboardFormat; var Buffer; Size: Integer): Boolean;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Clear;
    procedure Close;
    constructor Create;
    constructor Create(AClipboardType: TClipboardType);
    destructor Destroy; override;
    function FindPictureFormatID: TClipboardFormat;
    function FindFormatID(const FormatName: string): TClipboardFormat;
    //function GetAsHandle(Format: integer): THandle;
    function GetAsHtml(ExtractFragmentOnly: Boolean): String;
    function GetComponent(Owner, Parent: TComponent): TComponent;
    procedure GetComponent(var RootComponent: TComponent;
                          OnFindComponentClass: TFindComponentClassEvent;
                          Owner: TComponent = nil;
                          Parent: TComponent = nil);
    procedure GetComponentAsText(var RootComponent: TComponent;
                                 OnFindComponentClass: TFindComponentClassEvent;
                                 Owner: TComponent = nil;
                                 Parent: TComponent = nil);
    function GetFormat(FormatID: TClipboardFormat; Stream: TStream): Boolean;
    procedure SupportedFormats(List: TStrings);
    procedure SupportedFormats(var AFormatCount: integer;
                               var FormatList: PClipboardFormat);
    function GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
    function HasFormat(FormatID: TClipboardFormat): Boolean;
    function HasFormatName(const FormatName: string): Boolean;
    function HasPictureFormat: boolean;
    procedure Open;
    //procedure SetAsHandle(Format: integer; Value: THandle);
    procedure SetAsHtml(Html: String);
    procedure SetAsHtml(Html: String; const PlainText: String);
    function SetComponent(Component: TComponent): Boolean;
    function SetComponentAsText(Component: TComponent): Boolean;
    function SetFormat(FormatID: TClipboardFormat; Stream: TStream): Boolean;
    function SetSupportedFormats(AFormatCount: integer;
                                  FormatList: PClipboardFormat): Boolean;
    procedure SetTextBuf(Buffer: PChar);
    property AsText: string read GetAsText write SetAsText;
    property ClipboardType: TClipboardType read FClipboardType;
    property FormatCount: Integer read GetFormatCount;
    property Formats[Index: Integer]: TClipboardFormat read GetFormats;
    property OnRequest: TClipboardRequestEvent read FOnRequest write SetOnRequest;
  end;


function Clipboard: TClipboard;
function SetClipboard(NewClipboard: TClipboard): TClipboard;
function PrimarySelection: TClipboard;
function SecondarySelection: TClipboard;
function Clipboard(ClipboardType: TClipboardType): TClipboard;
function SetClipboard(ClipboardType: TClipboardType;
  NewClipboard: TClipboard): TClipboard;
procedure FreeAllClipboards;

function RegisterClipboardFormat(const Format: string): TClipboardFormat;


implementation

var
  FClipboards: array[TClipboardType] of TClipboard;

{$I clipbrd.inc}

function RegisterClipboardFormat(const Format: string): TClipboardFormat;
begin
  Result:=ClipboardRegisterFormat(Format);
end;

function Clipboard: TClipboard;
begin
  Result:=Clipboard(ctClipboard);
end;

function SetClipboard(NewClipboard: TClipboard): TClipboard;
begin
  Result:=SetClipboard(ctClipboard,NewClipboard);
end;

function PrimarySelection: TClipboard;
begin
  Result:=Clipboard(ctPrimarySelection);
end;

function SecondarySelection: TClipboard;
begin
  Result:=Clipboard(ctSecondarySelection);
end;

function Clipboard(ClipboardType: TClipboardType): TClipboard;
begin
  if not Assigned(FClipboards[ClipboardType]) then
     FClipboards[ClipboardType] := TClipboard.Create(ClipboardType);
  Result := FClipboards[ClipboardType];
end;

function SetClipboard(ClipboardType: TClipboardType;
  NewClipboard: TClipboard): TClipboard;
begin
  if Assigned(FClipboards[ClipboardType]) then
  begin
     FClipboards[ClipboardType].Free;
     FClipboards[ClipboardType] := nil;
  end;
  FClipboards[ClipboardType] := NewClipboard;
  Result := FClipboards[ClipboardType];
end;

function CF_Text: TClipboardFormat;
begin
  Result:=PredefinedClipboardFormat(pcfText);
end;

function CF_Bitmap: TClipboardFormat;
begin
  Result:=PredefinedClipboardFormat(pcfBitmap);
end;

function CF_Picture: TClipboardFormat;
begin
  Result:=PredefinedClipboardFormat(pcfPicture);
end;

function CF_MetaFilePict: TClipboardFormat;
begin
  Result:=PredefinedClipboardFormat(pcfMetaFilePict);
end;

function CF_Object: TClipboardFormat;
begin
  Result:=PredefinedClipboardFormat(pcfObject);
end;

function CF_Component: TClipboardFormat;
begin
  Result:=PredefinedClipboardFormat(pcfComponent);
end;

procedure FreeAllClipboards;
var AClipboardType: TClipboardType;
begin
  for AClipboardType:=Low(TClipboardType) to High(TClipboardType) do
    FreeAndNil(FClipboards[AClipboardType]);
end;

procedure LoadGraphicFromClipboardFormat(Dest: TGraphic;
  ClipboardType: TClipboardType; FormatID: TClipboardFormat);
begin
  Clipboard(ClipboardType).AssignToGraphic(Dest,FormatID);
end;

procedure SaveGraphicToClipboardFormat(Src: TGraphic;
  ClipboardType: TClipboardType; FormatID: TClipboardFormat);
begin
  Clipboard(ClipboardType).AssignGraphic(Src,FormatID);
end;

//-----------------------------------------------------------------------------

procedure InternalInit;
var
  AClipboardType: TClipboardType;
begin
  OnLoadGraphicFromClipboardFormat:=@LoadGraphicFromClipboardFormat;
  OnSaveGraphicToClipboardFormat:=@SaveGraphicToClipboardFormat;
  OnLoadSaveClipBrdGraphicValid:=true;

  for AClipboardType:=Low(TClipboardType) to High(TClipboardType) do
    FClipboards[AClipboardType]:=nil;
end;

procedure InternalFinal;
begin
  OnLoadSaveClipBrdGraphicValid:=false;
  FreeAllClipboards;
end;

initialization
  InternalInit;
  
finalization
  InternalFinal;

end.
