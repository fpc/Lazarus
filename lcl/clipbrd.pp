{
 /***************************************************************************
                               Clipbrd.pp
                             -------------------
                             Component Library Clipboard Controls
                   Initial Revision  : Sat Feb 26 2000


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
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
    Clear cache and supported format list.
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
    http://www.isi.edu/in-notes/iana/assignments/media-types/media-types

  ToDo:
    - Better description
    - graphic formats

}

unit Clipbrd;

{$MODE Objfpc}{$H+}

interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
  Classes, SysUtils, LCLType, LCLLinux, GraphType, Graphics;

type  
  TClipboardData = record
    FormatID: TClipboardFormat;
    Stream: TMemoryStream;
  end;


{ for delphi compatibility:

  In Delphi there are 4 predefined constants, but the LCL has only dynamic
  values.
  
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


type
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
    procedure AssignPicture(Source: TPicture);
    procedure AssignToBitmap(Dest: TBitmap);
    procedure AssignToPixmap(Dest: TPixmap);
    procedure AssignToIcon(Dest: TIcon);
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
    procedure SetBuffer(FormatID: TClipboardFormat; var Buffer; Size: Integer);
    procedure SetOnRequest(AnOnRequest: TClipboardRequestEvent);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    function AddFormat(FormatID: TClipboardFormat; Stream: TStream): Boolean;
    function AddFormat(FormatID: TClipboardFormat; var Buffer; Size: Integer): Boolean;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure Close; // dummy for delphi compatibility only
    constructor Create;
    constructor Create(AClipboardType: TClipboardType);
    destructor Destroy; override;
    function FindPictureFormatID: TClipboardFormat;
    //function GetAsHandle(Format: integer): THandle;
    function GetComponent(Owner, Parent: TComponent): TComponent;
    function GetFormat(FormatID: TClipboardFormat; Stream: TStream): Boolean;
    procedure SupportedFormats(List: TStrings);
    procedure SupportedFormats(var AFormatCount: integer;
                    var FormatList: PClipboardFormat);
    function GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
    function HasFormat(FormatID: TClipboardFormat): Boolean;
    function HasPictureFormat: boolean;
    procedure Open; // dummy for delphi compatibility only
    //procedure SetAsHandle(Format: integer; Value: THandle);
    procedure SetComponent(Component: TComponent);
    procedure SetFormat(FormatID: TClipboardFormat; Stream: TStream);
    procedure SetSupportedFormats(AFormatCount: integer;
                    FormatList: PClipboardFormat);
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
  Result:=PredefinedClipboardFormat(pcfDelphiText);
end;

function CF_Bitmap: TClipboardFormat;
begin
  Result:=PredefinedClipboardFormat(pcfDelphiBitmap);
end;

function CF_Picture: TClipboardFormat;
begin
  Result:=PredefinedClipboardFormat(pcfDelphiPicture);
end;

function CF_MetaFilePict: TClipboardFormat;
begin
  Result:=PredefinedClipboardFormat(pcfDelphiMetaFilePict);
end;

function CF_Object: TClipboardFormat;
begin
  Result:=PredefinedClipboardFormat(pcfDelphiObject);
end;

function CF_Component: TClipboardFormat;
begin
  Result:=PredefinedClipboardFormat(pcfDelphiComponent);
end;

procedure FreeAllClipboards;
var AClipboardType: TClipboardType;
begin
  for AClipboardType:=Low(TClipboardType) to High(TClipboardType) do
    FreeAndNil(FClipboards[AClipboardType]);
end;

//-----------------------------------------------------------------------------

procedure InternalInit;
var
  AClipboardType: TClipboardType;
begin
  for AClipboardType:=Low(TClipboardType) to High(TClipboardType) do
    FClipboards[AClipboardType]:=nil;
end;

procedure InternalFinal;
begin
  FreeAllClipboards;
end;

initialization
  InternalInit;
  
finalization
  InternalFinal;

end.

{
  $Log$
  Revision 1.10  2002/10/24 10:05:50  lazarus
  MG: broke graphics.pp <-> clipbrd.pp circle

  Revision 1.9  2002/05/10 06:05:48  lazarus
  MG: changed license to LGPL

  Revision 1.8  2002/03/11 23:22:46  lazarus
  MG: added TPicture clipboard support

  Revision 1.7  2002/03/09 11:55:13  lazarus
  MG: fixed class method completion

  Revision 1.6  2002/02/03 00:24:00  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.5  2001/11/12 19:30:00  lazarus
  MG: added try excepts for clipboard

  Revision 1.3  2001/06/15 10:31:05  lazarus
  MG: set longstrings as default

  Revision 1.2  2001/02/16 19:13:30  lazarus
  Added some functions
  Shane

  Revision 1.1  2000/07/13 10:28:23  michael
  + Initial import

}
