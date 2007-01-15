{  $Id$  }
{
 /***************************************************************************
                              intfgraphics.pp
                              ---------------

 ***************************************************************************/

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

  Author: Mattias Gaertner

  Abstract:
    Classes and functions for easy handling of raw images (interface images).
}
unit IntfGraphics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpImage, FPReadBMP, BMPComn, FPCAdds, AvgLvlTree, LCLType,
  LCLProc, GraphType, LCLIntf;

type
  { TLazIntfImage }
  { This descendent of TFPCustomImage stores its image data as raw images and
    is therefore able to directly interchange images with the LCL interfaces.

    Usage examples:

    1. Loading a .xpm file into a TBitmap:

      var
        BmpHnd,MaskHnd: HBitmap;
        Bitmap1: TBitmap;
        IntfImg1: TLazIntfImage;
        Reader: TLazReaderXPM;
      begin
        // create a bitmap (or use an existing one)
        Bitmap1:=TBitmap.Create;
        // create the raw image
        IntfImg1:=TLazIntfImage.Create(0,0);
        // get the description for the current screen (bitsperpixel, depth, ...)
        IntfImg1.GetDescriptionFromDevice(0);
        // create the XPM reader
        Reader:=TLazReaderXPM.Create;
        // load the image
        IntfImg1.LoadFromFile('filename.xpm',Reader);
        // create the bitmap handles
        IntfImg1.CreateBitmap(BmpHnd,MaskHnd);
        // apply handles to the Bitmap1
        Bitmap1.Handle:=BmpHnd;
        Bitmap1.MaskHandle:=MaskHnd;
        // clean up
        Reader.Free;
        IntfImg1.Free;
        // do something with the Bitmap1
        ...
      end;


    2. Saving a TBitmap to a .xpm file:

      var
        BmpHnd,MaskHnd: HBitmap;
        Bitmap1: TBitmap;
        IntfImg1: TLazIntfImage;
        Writer: TLazWriterXPM;
      begin
        ...
        // create the raw image
        IntfImg1:=TLazIntfImage.Create(0,0);
        // load the raw image from the bitmap handles
        IntfImg1.LoadFromBitmap(Bitmap1.Handle,Bitmap1.MaskHandle);
        // create the XPM writer
        Writer:=TLazWriterXPM.Create;
        // save image to file
        IntfImg1.SaveToFile('filename.xpm',Writer);
        // clean up
        Writer.Free;
        IntfImg1.Free;
        ...
      end;
    }

  TOnGetLazIntfImagePixel = procedure(x, y: integer; var Color: TFPColor)
                            of object;
  TOnSetLazIntfImagePixel = procedure(x, y: integer; const Color: TFPColor)
                            of object;

  TOnReadRawImageBits = procedure(TheData: PByte;
    const Position: TRawImagePosition;
    Prec, Shift: cardinal; var Bits: word);

  TOnWriteRawImageBits = procedure(TheData: PByte;
    const Position: TRawImagePosition;
    Prec, Shift: cardinal; Bits: word);


  { TLazIntfImage }

  TLazIntfImage = class(TFPCustomImage)
  private
    FAutoCreateMask: boolean;
    FDataDescription: TRawImageDescription;
    FPixelData: PByte;
    FPixelDataSize: PtrUInt;
    FMaskData: PByte;
    FMaskDataSize: PtrUInt;
    FLineStarts: PRawImagePosition;
    FMaskLineStarts: PRawImagePosition;
    FUpdateCount: integer;
    fCreateAllDataNeeded: boolean;
    FGetSetColorFunctionsUpdateNeeded: boolean;
    FReadRawImageBits: TOnReadRawImageBits;
    FWriteRawImageBits: TOnWriteRawImageBits;
    FAlphaReadRawImageBits: TOnReadRawImageBits;
    FAlphaWriteRawImageBits: TOnWriteRawImageBits;
    function GetTColors(x, y: integer): TGraphicsColor;
    procedure SetAutoCreateMask(const AValue: boolean);
    procedure SetTColors(x, y: integer; const AValue: TGraphicsColor);
  protected
    OnGetInternalColor: TOnGetLazIntfImagePixel;
    OnSetInternalColor: TOnSetLazIntfImagePixel;
    procedure SetUsePalette (Value: boolean); override;
    procedure SetInternalColor(x, y: integer; const Value: TFPColor); override;
    function GetInternalColor(x, y: integer): TFPColor; override;
    procedure SetInternalPixel (x,y:integer; Value:integer); override;
    function GetInternalPixel (x,y:integer) : integer; override;
    procedure FreeAllData; virtual;
    procedure FreePixelData; virtual;
    procedure FreeMaskData; virtual;
    procedure CreateDataAndLineStarts(var Data: Pointer; var DataSize: PtrUInt;
                                      var TheLineStarts: PRawImagePosition;
                                      TheBitsPerPixel: cardinal;
                                      TheLineEnd: TRawImageLineEnd); virtual;
    procedure SetDataDescription(const NewDescription: TRawImageDescription); virtual;
    procedure ChooseGetSetColorFunctions; virtual;
    procedure ChooseRawBitsProc(BitsPerPixel: cardinal;
                                ByteOrder: TRawImageByteOrder;
                                BitOrder: TRawImageBitOrder;
                                var ProcReadRawImageBits: TOnReadRawImageBits;
                                var ProcWriteRawImageBits: TOnWriteRawImageBits);
    // get color functions
    procedure GenericGetColor(x, y: integer; var Value: TFPColor);
    procedure GetColor_NoPalette_RGBA_Alpha_Sep_Mask(x, y: integer; var Value: TFPColor);
    procedure GetColor_NoPalette_RGBA_Alpha_Sep_NoMask(x, y: integer; var Value: TFPColor);
    procedure GetColor_NoPalette_RGBA_Alpha_NoSep(x, y: integer; var Value: TFPColor);
    procedure GetColor_NoPalette_RGBA_NoAlpha(x, y: integer; var Value: TFPColor);
    procedure GetColor_NoPalette_Gray(x, y: integer; var Value: TFPColor);
    procedure GetColor_NULL(x, y: integer; var Value: TFPColor);
    // set color functions
    procedure GenericSetColor(x, y: integer; const Value: TFPColor);
    procedure SetColor_NoPalette_RGBA_Alpha_Sep_Mask(x, y: integer; const Value: TFPColor);
    procedure SetColor_NoPalette_RGBA_Alpha_NoSep(x, y: integer; const Value: TFPColor);
    procedure SetColor_NoPalette_RGBA_NoAlpha(x, y: integer; const Value: TFPColor);
    procedure SetColor_NoPalette_Gray(x, y: integer; const Value: TFPColor);
    procedure SetColor_NULL(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_R8G8B8_A1_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_R8G8B8_A1_BIO_TTB_RBO(x, y: integer; const Value: TFPColor);
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SetSize(AWidth, AHeight: integer); override;
    function CheckDescription(const ADescription: TRawImageDescription;
                              ExceptionOnError: boolean): boolean; virtual;
    procedure GetDescriptionFromDevice(DC: HDC); virtual;
    procedure GetDescriptionFromBitmap(Bitmap: HBitmap); virtual;
    procedure Set_BPP24_B8G8R8_A1_BIO_TTB(NewWidth, NewHeight: integer);
    procedure Set_BPP32_B8G8R8_A1_BIO_TTB(NewWidth, NewHeight: integer);
    procedure LoadFromDevice(DC: HDC); virtual;
    procedure LoadFromBitmap(Bitmap, MaskBitmap: HBitmap; AWidth: integer = -1; AHeight: integer = -1); virtual;
    procedure CreateBitmap(var Bitmap, MaskBitmap: HBitmap;
                           AlwaysCreateMask: boolean); virtual;
    procedure SetRawImage(const RawImage: TRawImage); virtual;
    procedure GetRawImage(out RawImage: TRawImage); virtual;
    procedure FillPixels(const Color: TFPColor); virtual;
    procedure CopyPixels(Src: TFPCustomImage); virtual;
    procedure GetXYDataPostion(x, y: integer; var Position: TRawImagePosition);
    procedure GetXYMaskPostion(x, y: integer; var Position: TRawImagePosition);
    function GetDataLineStart(y: integer): Pointer;// similar to Delphi TBitmap.ScanLine. Only works with byte aligned lines.
    procedure CreateAllData; virtual;
    procedure CreatePixelData; virtual;
    procedure CreateMaskData; virtual;
    function HasTransparency: boolean; virtual;
  public
    property PixelData: PByte read FPixelData;
    property MaskData: PByte read FMaskData;
    property DataDescription: TRawImageDescription read FDataDescription
                                                   write SetDataDescription;
    property AutoCreateMask: boolean read FAutoCreateMask write SetAutoCreateMask;
    property TColors[x,y: integer]: TGraphicsColor read GetTColors write SetTColors;
  end;
  
  
  { TLazIntfImageMask }
  
  TLazIntfImageMask = class(TFPCustomImage)
  private
    FImage: TLazIntfImage;
  protected
    procedure SetInternalColor(x, y: integer; const Value: TFPColor); override;
    function GetInternalColor(x, y: integer): TFPColor; override;
    procedure SetInternalPixel (x,y:integer; Value:integer); override;
    function GetInternalPixel (x,y:integer) : integer; override;
  public
    constructor CreateWithImage(TheImage: TLazIntfImage); virtual;
    property Image: TLazIntfImage read FImage;
  end;


  { TLazAVLPalette }
  { This descendent of TFPPalette uses a AVL tree for speed up. }

  TLazAVLPalette = class(TFPPalette)
  protected
    FAVLPalette: TAvgLvlTree; // tree of PLazAVLPaletteEntry 'color to index'
    FAVLNodes: PAvgLvlTreeNode;// 'index to node' array
    procedure SetCount(NewCount: integer); override;
    procedure SetColor(Index: integer; const NewColor: TFPColor); override;
    function CompareEntries(Index1, Index2: integer): integer;
    function CompareColorWithEntries(const AColor: TFPColor;
                                     Index: integer): integer;
    procedure EnlargeData; override;
  public
    destructor Destroy; override;
    function IndexOf(const AColor: TFPColor): integer; override;
    function Add(const NewColor: TFPColor): integer; override;
    procedure CheckConsistency; virtual;
  end;


  { TArrayNodesTree }

  PArrayNode = ^TArrayNode;
  TArrayNode = class
  public
    Parent: TArrayNode;
    Value: integer;
    Childs: PArrayNode;
    StartValue: integer;
    Capacity: integer;
    Data: Pointer;
    constructor Create;
    destructor Destroy; override;
    procedure DeleteChilds;
    procedure UnbindFromParent;
    procedure CreateChildNode(ChildValue: integer);
    function GetChildNode(ChildValue: integer;
                          CreateIfNotExists: boolean): TArrayNode;
    procedure Expand(ValueToInclude: integer);
    function FindPrevSibling: TArrayNode;
    function FindNextSibling: TArrayNode;
    function FindNext: TArrayNode;
    function FindPrev: TArrayNode;
    function FindFirstChild: TArrayNode;
    function FindLastChild: TArrayNode;
    function FindLastSubChild: TArrayNode;
    function FindFirstSibling: TArrayNode;
    function FindLastSibling: TArrayNode;
    procedure ConsistencyCheck;
  end;

  TArrayNodesTree = class
  public
    Root: TArrayNode;
    function FindNode(Path: PInteger; Count: integer): TArrayNode;
    function FindData(Path: PInteger; Count: integer): Pointer;
    function SetNode(Path: PInteger; Count: integer;
                     Data: Pointer): TArrayNode;
    procedure Delete(Node: TArrayNode);
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    procedure ConsistencyCheck;
  end;


  { TLazReaderXPM }
  { This is a FPImage reader for xpm images. }

  TLazReaderXPM = class(TFPCustomImageReader)
  private
    FWidth: Integer;
    FHeight: Integer;
    FColorCount: Integer;
    FCharsPerPixel: Integer;
    fXHot: Integer;
    fYHot: Integer;
    FPixelToColorTree: TArrayNodesTree;
  protected
    procedure ClearPixelToColorTree;
    procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
    function  InternalCheck(Str: TStream): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;


  { TLazWriterXPM }
  { This is a FPImage writer for xpm images. }

  TLazWriterXPM = class(TFPCustomImageWriter)
  private
    FNibblesPerSample: word;
    FRightShiftSample: cardinal;
    procedure SetNibblesPerSample(const AValue: word);
  protected
    procedure InternalWrite(Str: TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
    property NibblesPerSample: word read FNibblesPerSample
                                    write SetNibblesPerSample;
  end;
  
  
  { TLazReaderBMP }
  { This is an imroved FPImage writer for bmp images. }
  TLazReaderBMP = class (TFPCustomImageReader)
  Private
    FUseLeftBottomAsTransparent: Boolean;
    Procedure FreeBufs;      // Free (and nil) buffers.
  protected
    ReadSize: Integer;       // Size (in bytes) of 1 scanline.
    BFI: TBitMapInfoHeader;  // The header as read from the stream.
    FPalette: PFPcolor;      // Buffer with Palette entries.
    FBitsPerPixel: Integer;  // bits per pixel (1, 4, 8, 15, 16, 24, 32)
    FTransparentColor: TFPColor; // color which should be interpreted as transparent
    LineBuf: PByte;          // Buffer for 1 scanline. Can be Byte, Word, TColorRGB or TColorRGBA

    // SetupRead will allocate the needed buffers, and read the colormap if needed.
    procedure SetupRead(nPalette, nRowBits: Integer; Stream: TStream;
      ReadPalette: Boolean); virtual;
    function  ColorToTrans(const InColor: TFPColor): TFPColor;
    procedure ReadScanLine(Row: Integer; Stream: TStream); virtual;
    procedure WriteScanLine(Row: Integer; Img: TFPCustomImage); virtual;
    function BmpRGBAToFPColor(Const RGBA: TColorRGBA): TFPcolor; virtual;
    // required by TFPCustomImageReader
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    procedure InternalReadHead(Stream: TStream; Img: TFPCustomImage);
    procedure InternalReadBody(Stream: TStream; Img: TFPCustomImage);
    function  InternalCheck(Stream: TStream) : boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property TransparentColor: TFPColor read FTransparentColor write FTransparentColor;
    property UseLeftBottomAsTransparent: Boolean read FUseLeftBottomAsTransparent
                                              write FUseLeftBottomAsTransparent;
  end;

  { TLazReaderPartIcon }
  { This is a FPImage writer for a single icon from an icon file }
  TLazReaderPartIcon = class (TLazReaderBMP)
  protected
    // required by TFPCustomImageReader
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function  InternalCheck(Stream: TStream) : boolean; override;
    function BmpRGBAToFPColor(Const RGBA: TColorRGBA): TFPcolor; override;
  end;

  { TLazReaderIcon }
  { This is a FPImage writer for icon images. }
  TLazReaderIcon = class (TLazReaderPartIcon)
  private
    FIcon: TObject; { Actually TIcon, but this would give rise to a circular reference }
    FnIcons: Integer;
    FnStartPos: TStreamSeekType;
    procedure SetIcon(const AValue: TObject);
  protected
    // required by TFPCustomImageReader
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function  InternalCheck(Stream: TStream) : boolean; override;
  public
    property Icon: TObject read FIcon write SetIcon;
  end;

  TLazReaderCursor = class (TLazReaderIcon)
  protected
    function  InternalCheck(Stream: TStream) : boolean; override;
  end;

function ReadCompleteStreamToString(Str: TStream; StartSize: integer): string;
procedure ReadCompleteStreamToStream(SrcStream, DestStream: TStream;
                                     StartSize: integer);
                                     
function dbgs(const FPColor: TFPColor): string; overload;

implementation

uses Graphics;

var
  IsSpaceChar, IsNumberChar, IsHexNumberChar: array[char] of Boolean;

function ReadCompleteStreamToString(Str: TStream; StartSize: integer): string;
var
  NewLength: Integer;
  ReadLen: Integer;
begin
  if (Str is TMemoryStream) or (Str is TFileStream) or (Str is TStringStream)
  then begin
    // read as one block
    SetLength(Result,Str.Size-Str.Position);
    if Result<>'' then
      Str.Read(Result[1],length(Result));
  end else begin
    // read exponential
    if StartSize=0 then StartSize:=1024;
    SetLength(Result,StartSize);
    NewLength:=0;
    repeat
      ReadLen:=Str.Read(Result[NewLength+1],length(Result)-NewLength);
      inc(NewLength,ReadLen);
      if NewLength<length(Result) then break;
      SetLength(Result,length(Result)*2);
    until false;
    SetLength(Result,NewLength);
  end;
end;

procedure ReadCompleteStreamToStream(SrcStream, DestStream: TStream;
                                     StartSize: integer);
var
  NewLength: Integer;
  ReadLen: Integer;
  Buffer: string;
begin
  if (SrcStream is TMemoryStream) or (SrcStream is TFileStream)
  or (SrcStream is TStringStream)
  then begin
    // read as one block
    if DestStream is TMemoryStream then
      TMemoryStream(DestStream).SetSize(DestStream.Size
                                        +(SrcStream.Size-SrcStream.Position));
    DestStream.CopyFrom(SrcStream,SrcStream.Size-SrcStream.Position);
  end else begin
    // read exponential
    if StartSize<=0 then StartSize:=1024;
    SetLength(Buffer,StartSize);
    NewLength:=0;
    repeat
      ReadLen:=SrcStream.Read(Buffer[NewLength+1],length(Buffer)-NewLength);
      inc(NewLength,ReadLen);
      if NewLength<length(Buffer) then break;
      SetLength(Buffer,length(Buffer)*2);
    until false;
    if NewLength>0 then
      DestStream.Write(Buffer[1],NewLength);
  end;
end;

function dbgs(const FPColor: TFPColor): string;
begin
  Result:='r='+hexStr(FPColor.Red,4)+',g='+hexStr(FPColor.green,4)
        +',b='+hexStr(FPColor.blue,4)+',a='+hexStr(FPColor.alpha,4);
end;

procedure ReadRawImageBits_1_2_4_BIO(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  OneByte: Byte;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  OneByte:=P^;
  Bits:=Word(cardinal(OneByte shr (Shift+Position.Bit)) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure ReadRawImageBits_1_2_4_BNIO(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  OneByte: Byte;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  OneByte:=P^;
  Bits:=Word(cardinal(OneByte shr (Shift+7-Position.Bit)) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure ReadRawImageBits_8(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  OneByte: Byte;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  OneByte:=P^;
  Bits:=Word(cardinal(OneByte shr Shift) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure ReadRawImageBits_16(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  TwoBytes: Word;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  TwoBytes:=PWord(P)^;
  Bits:=Word(cardinal(TwoBytes shr Shift) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure ReadRawImageBits_ReversedBytes_16(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  TwoBytes: Word;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  TwoBytes:=PWord(P)^;
  TwoBytes:=(TwoBytes shr 8) or ((TwoBytes and $ff) shl 8); // switch byte order
  Bits:=Word(cardinal(TwoBytes shr Shift) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure ReadRawImageBits_24(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  FourBytes: Cardinal;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  {$ifdef Endian_Little}
  FourBytes:=DWord(PWord(P)^) or (DWord((P+2)^) shl 16);
  {$else}
  FourBytes:=(DWord(PWord(P)^) shl 8) or DWord(P^);
  {$endif}
  Bits:=Word(cardinal(FourBytes shr Shift) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure ReadRawImageBits_ReversedBytes_24(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  FourBytes: Cardinal;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  {$ifdef Endian_Little}
  FourBytes:=(DWord(PWord(P)^) shl 8) or DWord(P^);
  {$else}
  FourBytes:=DWord(PWord(P)^) or (DWord((P+2)^) shl 16);
  {$endif}

  Bits:=Word(cardinal(FourBytes shr Shift) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure ReadRawImageBits_32(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  FourBytes: Cardinal;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  FourBytes:=PDWord(P)^;
  Bits:=Word(cardinal(FourBytes shr Shift) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure ReadRawImageBits_ReversedBytes_32(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  FourBytes: Cardinal;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);

  FourBytes:=PDWord(P)^;

  // switch byte order
  FourBytes:=(FourBytes shr 24) or ((FourBytes shr 8) and $FF00)
             or ((FourBytes and $ff00) shl 8) or ((FourBytes and $ff) shl 24);

  Bits:=Word(cardinal(FourBytes shr Shift) and PrecMask);

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure WriteRawImageBits_1_2_4_BIO(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  OneByte: Byte;
  ShiftLeft: Integer;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(Cardinal(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

  OneByte:=P^;
  ShiftLeft:=Shift+Position.Bit;
  PrecMask:=not (PrecMask shl ShiftLeft);
  OneByte:=OneByte and PrecMask; // clear old
  OneByte:=OneByte or (Bits shl ShiftLeft); // set new
  P^:=OneByte;
end;

procedure WriteRawImageBits_1_2_4_BNIO(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  OneByte: Byte;
  ShiftLeft: Integer;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(Cardinal(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

  OneByte:=P^;
  ShiftLeft:=Shift+7-Position.Bit;
  PrecMask:=not (PrecMask shl ShiftLeft);
  OneByte:=OneByte and PrecMask; // clear old
  OneByte:=OneByte or (Bits shl ShiftLeft); // set new
  P^:=OneByte;
end;

procedure WriteRawImageBits_8(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  OneByte: Byte;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(Cardinal(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

  OneByte:=P^;
  PrecMask:=not (PrecMask shl Shift);
  OneByte:=OneByte and PrecMask; // clear old
  OneByte:=OneByte or (Bits shl Shift); // set new
  P^:=OneByte;
end;

procedure WriteRawImageBits_16(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  TwoBytes: Word;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(Cardinal(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

  TwoBytes:=PWord(P)^;
  PrecMask:=not (PrecMask shl Shift);
  TwoBytes:=TwoBytes and PrecMask; // clear old
  TwoBytes:=TwoBytes or (Bits shl Shift); // set new
  PWord(P)^:=TwoBytes;
end;

procedure WriteRawImageBits_ReversedBytes_16(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  TwoBytes: Word;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(Cardinal(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

  TwoBytes:=PWord(P)^;
  TwoBytes:=(TwoBytes shr 8) or ((TwoBytes and $ff) shl 8); // switch byte order
  PrecMask:=not (PrecMask shl Shift);
  TwoBytes:=TwoBytes and PrecMask; // clear old
  TwoBytes:=TwoBytes or (Bits shl Shift); // set new
  TwoBytes:=(TwoBytes shr 8) or ((TwoBytes and $ff) shl 8); // switch byte order
  PWord(P)^:=TwoBytes;
end;

procedure WriteRawImageBits_24(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  FourBytes: Cardinal;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(Cardinal(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

{$ifdef Endian_Little}
  FourBytes:=DWord(PWord(P)^) or (DWord((P+2)^) shl 16);
{$else}
  FourBytes:=(DWord(PWord(P)^) shl 8) or DWord(P^);
{$endif}
  
  PrecMask:=not (PrecMask shl Shift);
  FourBytes:=FourBytes and PrecMask; // clear old
  FourBytes:=FourBytes or cardinal(Bits shl Shift); // set new

{$ifdef Endian_little}
  PWord(P)^ := Word(FourBytes);
  (P+2)^ := Byte(FourBytes shr 16);
{$else}  
  PWord(P)^ := Word(FourBytes shr 8);
  P^ := Byte(FourBytes);
{$endif}
end;

procedure WriteRawImageBits_ReversedBytes_24(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  FourBytes: Cardinal;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(Cardinal(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

{$ifdef Endian_Little}
  FourBytes:=(DWord(PWord(P)^) shl 8) or DWord(P^);
{$else}
  FourBytes:=DWord(PWord(P)^) or (DWord((P+2)^) shl 16);
{$endif}
  
  PrecMask:=not (PrecMask shl Shift);
  FourBytes:=FourBytes and PrecMask; // clear old
  FourBytes:=FourBytes or cardinal(Bits shl Shift); // set new

{$ifdef Endian_little}
  PWord(P)^ := Word(FourBytes shr 8);
  P^ := Byte(FourBytes);
{$else}  
  PWord(P)^ := Word(FourBytes);
  (P+2)^ := Byte(FourBytes shr 16);
{$endif}
end;

procedure WriteRawImageBits_32(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  FourBytes: Cardinal;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(Cardinal(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

  FourBytes:=PDWord(P)^;
  PrecMask:=not (PrecMask shl Shift);
  FourBytes:=FourBytes and PrecMask; // clear old
  FourBytes:=FourBytes or cardinal(Bits shl Shift); // set new
  PDWord(P)^:=FourBytes;
end;

procedure WriteRawImageBits_ReversedBytes_32(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  FourBytes: Cardinal;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(Cardinal(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);

  FourBytes:=PDWord(P)^;

  // switch byte order
  FourBytes:=(FourBytes shr 24) or ((FourBytes shr 8) and $FF00)
             or ((FourBytes and $ff00) shl 8) or ((FourBytes and $ff) shl 24);

  PrecMask:=not (PrecMask shl Shift);
  FourBytes:=FourBytes and PrecMask; // clear old
  FourBytes:=FourBytes or cardinal(Bits shl Shift); // set new

  // switch byte order
  FourBytes:=(FourBytes shr 24) or ((FourBytes shr 8) and $FF00)
             or ((FourBytes and $ff00) shl 8) or ((FourBytes and $ff) shl 24);
  PDWord(P)^:=FourBytes;
end;

procedure ReadRawImageBits_NULL(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal;
  var Bits: word);
begin
  Bits:=0;

  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure WriteRawImageBits_NULL(TheData: PByte;
  const Position: TRawImagePosition;
  Prec, Shift: cardinal; Bits: word);
begin
end;

{ TLazIntfImage }

procedure TLazIntfImage.SetDataDescription(
  const NewDescription: TRawImageDescription);
begin
  if CompareMem(@FDataDescription,@NewDescription,SizeOf(TRawImageDescription))
  then
    exit;
  CheckDescription(NewDescription,true);
  BeginUpdate;
  try
    FreeAllData;
    fDataDescription:=NewDescription;
    ChooseGetSetColorFunctions;
    SetSize(0,0);
    fCreateAllDataNeeded:=false;
  finally
    EndUpdate;
  end;
end;

procedure TLazIntfImage.ChooseRawBitsProc(BitsPerPixel: cardinal;
  ByteOrder: TRawImageByteOrder; BitOrder: TRawImageBitOrder;
  var ProcReadRawImageBits: TOnReadRawImageBits;
  var ProcWriteRawImageBits: TOnWriteRawImageBits);
begin
  case BitsPerPixel of

  1,2,4:
  begin
    if BitOrder = riboBitsInOrder then
    begin
      ProcReadRawImageBits  := @ReadRawImageBits_1_2_4_BIO;
      ProcWriteRawImageBits := @WriteRawImageBits_1_2_4_BIO;
    end else begin
      ProcReadRawImageBits  := @ReadRawImageBits_1_2_4_BNIO;
      ProcWriteRawImageBits := @WriteRawImageBits_1_2_4_BNIO;
    end;
  end;

  8:
  begin
    ProcReadRawImageBits  := @ReadRawImageBits_8;
    ProcWriteRawImageBits := @WriteRawImageBits_8;
  end;

  16:
  begin
    if DefaultByteOrder=ByteOrder then begin
      ProcReadRawImageBits  := @ReadRawImageBits_16;
      ProcWriteRawImageBits := @WriteRawImageBits_16;
    end else begin
      ProcReadRawImageBits  := @ReadRawImageBits_ReversedBytes_16;
      ProcWriteRawImageBits := @WriteRawImageBits_ReversedBytes_16;
    end;
  end;

  24:
  begin
    if DefaultByteOrder=ByteOrder then begin
      ProcReadRawImageBits  := @ReadRawImageBits_24;
      ProcWriteRawImageBits := @WriteRawImageBits_24;
    end else begin
      ProcReadRawImageBits  := @ReadRawImageBits_ReversedBytes_24;
      ProcWriteRawImageBits := @WriteRawImageBits_ReversedBytes_24;
    end;
  end;

  32:
  begin
    if DefaultByteOrder=ByteOrder then begin
      ProcReadRawImageBits  := @ReadRawImageBits_32;
      ProcWriteRawImageBits := @WriteRawImageBits_32;
    end else begin
      ProcReadRawImageBits  := @ReadRawImageBits_ReversedBytes_32;
      ProcWriteRawImageBits := @WriteRawImageBits_ReversedBytes_32;
    end;
  end;

  else
    DebugLn('WARNING: TLazIntfImage.ChooseRawBitsProc Unsupported BitsPerPixel=',dbgs(BitsPerPixel));
    ProcReadRawImageBits  := @ReadRawImageBits_NULL;
    ProcWriteRawImageBits := @WriteRawImageBits_NULL;
  end;
end;

procedure TLazIntfImage.ChooseGetSetColorFunctions;

  procedure ChooseRGBAFunctions;
  begin
    //DebugLn('ChooseRGBAFunctions ',RawImageDescriptionAsString(@FDataDescription));
    ChooseRawBitsProc(FDataDescription.BitsPerPixel,
                      FDataDescription.ByteOrder,
                      FDataDescription.BitOrder,
                      FReadRawImageBits, FWriteRawImageBits);

    if FDataDescription.AlphaPrec>0 then
    begin
      if FDataDescription.AlphaSeparate then
      begin
        if (FMaskData<>nil) then
        begin
          OnGetInternalColor:=@GetColor_NoPalette_RGBA_Alpha_Sep_Mask;
          OnSetInternalColor:=@SetColor_NoPalette_RGBA_Alpha_Sep_Mask;
          with FDataDescription do begin
            if (BitsPerPixel=32) and (Depth=24) and (BitOrder=riboBitsInOrder)
            and (ByteOrder=DefaultByteOrder) and (LineOrder=riloTopToBottom)
            and (LineEnd=rileDWordBoundary)
            and (RedPrec=8) and (RedShift=16)
            and (GreenPrec=8) and (GreenShift=8)
            and (BluePrec=8) and (BlueShift=0)
            then begin
              if ByteOrder=DefaultByteOrder then
                OnSetInternalColor:=@SetColor_BPP32_R8G8B8_A1_BIO_TTB
              else
                OnSetInternalColor:=@SetColor_BPP32_R8G8B8_A1_BIO_TTB_RBO;
            end;
          end;
        end else begin
          OnGetInternalColor:=@GetColor_NoPalette_RGBA_Alpha_Sep_NoMask;
          OnSetInternalColor:=@SetColor_NoPalette_RGBA_NoAlpha;
        end;
        ChooseRawBitsProc(FDataDescription.AlphaBitsPerPixel,
                          FDataDescription.AlphaByteOrder,
                          FDataDescription.AlphaBitOrder,
                          FAlphaReadRawImageBits, FAlphaWriteRawImageBits);
      end else begin
        OnGetInternalColor:=@GetColor_NoPalette_RGBA_Alpha_NoSep;
        OnSetInternalColor:=@SetColor_NoPalette_RGBA_Alpha_NoSep;
        ChooseRawBitsProc(FDataDescription.BitsPerPixel,
          FDataDescription.AlphaByteOrder, FDataDescription.AlphaBitOrder,
          FAlphaReadRawImageBits, FAlphaWriteRawImageBits);
      end;
    end else begin
      OnGetInternalColor:=@GetColor_NoPalette_RGBA_NoAlpha;
      OnSetInternalColor:=@SetColor_NoPalette_RGBA_NoAlpha;
    end;
  end;

begin
  // Default: use the generic functions, that can handle all kinds of RawImages
  OnGetInternalColor:=@GenericGetColor;
  OnSetInternalColor:=@GenericSetColor;
  
  if FUpdateCount>0 then begin
    FGetSetColorFunctionsUpdateNeeded:=true;
    exit;
  end;
  FGetSetColorFunctionsUpdateNeeded:=false;

  if not FDataDescription.HasPalette then
  begin
    case FDataDescription.Format of

    ricfRGBA:
      ChooseRGBAFunctions;

    ricfGray:
      begin
        ChooseRawBitsProc(FDataDescription.BitsPerPixel,
                          FDataDescription.ByteOrder,
                          FDataDescription.BitOrder,
                          FReadRawImageBits, FWriteRawImageBits);
        OnGetInternalColor:=@GetColor_NoPalette_Gray;
        OnSetInternalColor:=@SetColor_NoPalette_Gray;
      end;

    end;
  end else begin
    // palette
    // ToDo
    DebugLn('WARNING: TLazIntfImage.ChooseGetSetColorFunctions Palette is unsupported');
  end;
end;

procedure TLazIntfImage.GenericGetColor(x, y: integer; var Value: TFPColor);
var
  Position: TRawImagePosition;
  MaskPosition: TRawImagePosition;
begin
  GetXYDataPostion(x,y,Position);
  if not FDataDescription.HasPalette then begin
    case FDataDescription.Format of
    ricfRGBA:
      begin
        ReadRawImageBits(FPixelData,Position,FDataDescription.BitsPerPixel,
                     FDataDescription.RedPrec,FDataDescription.RedShift,
                     FDataDescription.BitOrder,Value.Red);
        ReadRawImageBits(FPixelData,Position,FDataDescription.BitsPerPixel,
                     FDataDescription.GreenPrec,FDataDescription.GreenShift,
                     FDataDescription.BitOrder,Value.Green);
        ReadRawImageBits(FPixelData,Position,FDataDescription.BitsPerPixel,
                     FDataDescription.BluePrec,FDataDescription.BlueShift,
                     FDataDescription.BitOrder,Value.Blue);
        if FDataDescription.AlphaPrec>0 then begin
          if FDataDescription.AlphaSeparate then begin
            if (FMaskData<>nil) then begin
              GetXYMaskPostion(x,y,MaskPosition);
              ReadRawImageBits(FMaskData,MaskPosition,
                           FDataDescription.AlphaBitsPerPixel,
                           FDataDescription.AlphaPrec,
                           FDataDescription.AlphaShift,
                           FDataDescription.AlphaBitOrder,Value.Alpha);
            end else begin
              // no alpha mask -> set opaque
              Value.Alpha:=high(Value.Alpha);
            end;
          end else
            ReadRawImageBits(FPixelData,Position,FDataDescription.BitsPerPixel,
                         FDataDescription.AlphaPrec,FDataDescription.AlphaShift,
                         FDataDescription.AlphaBitOrder,Value.Alpha)
        end else begin
          // no alpha -> set opaque
          Value.Alpha:=high(Value.Alpha);
        end;
      end;

    ricfGray:
      begin
        ReadRawImageBits(FPixelData,Position,FDataDescription.BitsPerPixel,
                     FDataDescription.RedPrec,FDataDescription.RedShift,
                     FDataDescription.BitOrder,Value.Red);
        Value.Green:=Value.Red;
        Value.Blue:=Value.Red;
      end;

    else
      Value.Red:=0;
      Value.Green:=0;
      Value.Blue:=0;
      Value.Alpha:=0;
    end;
  end else begin
    // ToDo: read index, then palette
    Value.Red:=0;
    Value.Green:=0;
    Value.Blue:=0;
    Value.Alpha:=0;
  end;
end;

procedure TLazIntfImage.GenericSetColor(x, y: integer; const Value: TFPColor);
var
  Position: TRawImagePosition;
  MaskPosition: TRawImagePosition;
begin
  GetXYDataPostion(x,y,Position);
  if not FDataDescription.HasPalette then begin
    case FDataDescription.Format of
    ricfRGBA:
      begin
        WriteRawImageBits(FPixelData,Position,FDataDescription.BitsPerPixel,
                      FDataDescription.RedPrec,FDataDescription.RedShift,
                      FDataDescription.BitOrder,Value.Red);
        WriteRawImageBits(FPixelData,Position,FDataDescription.BitsPerPixel,
                      FDataDescription.GreenPrec,FDataDescription.GreenShift,
                      FDataDescription.BitOrder,Value.Green);
        WriteRawImageBits(FPixelData,Position,FDataDescription.BitsPerPixel,
                      FDataDescription.BluePrec,FDataDescription.BlueShift,
                      FDataDescription.BitOrder,Value.Blue);
        if FDataDescription.AlphaPrec>0 then begin
          if FDataDescription.AlphaSeparate then begin
            if (FMaskData<>nil) then begin
              GetXYMaskPostion(x,y,MaskPosition);
              WriteRawImageBits(FMaskData,MaskPosition,
                            FDataDescription.AlphaBitsPerPixel,
                            FDataDescription.AlphaPrec,
                            FDataDescription.AlphaShift,
                            FDataDescription.AlphaBitOrder,Value.Alpha);
            end else begin
              // no alpha mask
            end;
          end else
            WriteRawImageBits(FPixelData,Position,FDataDescription.BitsPerPixel,
                          FDataDescription.AlphaPrec,
                          FDataDescription.AlphaShift,
                          FDataDescription.AlphaBitOrder,Value.Alpha)
        end else begin
          // no alpha
        end;
      end;

    ricfGray:
      begin
        WriteRawImageBits(FPixelData,Position,FDataDescription.BitsPerPixel,
                      FDataDescription.RedPrec,FDataDescription.RedShift,
                      FDataDescription.BitOrder,Value.Red);
      end;

    else
    end;
  end else begin
    // ToDo: Palette
  end;
end;

procedure TLazIntfImage.GetColor_NoPalette_RGBA_Alpha_Sep_Mask(x, y: integer;
  var Value: TFPColor);
var
  Position: TRawImagePosition;
  MaskPosition: TRawImagePosition;
begin
  GetXYDataPostion(x,y,Position);
  FReadRawImageBits(FPixelData,Position,
               FDataDescription.RedPrec,FDataDescription.RedShift,
               Value.Red);
  FReadRawImageBits(FPixelData,Position,
               FDataDescription.GreenPrec,FDataDescription.GreenShift,
               Value.Green);
  FReadRawImageBits(FPixelData,Position,
               FDataDescription.BluePrec,FDataDescription.BlueShift,
               Value.Blue);

  GetXYMaskPostion(x,y,MaskPosition);
  FAlphaReadRawImageBits(FMaskData,MaskPosition,
               FDataDescription.AlphaPrec,
               FDataDescription.AlphaShift,
               Value.Alpha);
end;

procedure TLazIntfImage.GetColor_NoPalette_RGBA_Alpha_Sep_NoMask(x, y: integer; var Value: TFPColor);
var
  Position: TRawImagePosition;
begin
  GetXYDataPostion(x,y,Position);
  FReadRawImageBits(FPixelData,Position,
               FDataDescription.RedPrec,FDataDescription.RedShift,
               Value.Red);
  FReadRawImageBits(FPixelData,Position,
               FDataDescription.GreenPrec,FDataDescription.GreenShift,
               Value.Green);
  FReadRawImageBits(FPixelData,Position,
               FDataDescription.BluePrec,FDataDescription.BlueShift,
               Value.Blue);

  // no alpha mask -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_NoPalette_RGBA_Alpha_NoSep(x, y: integer; var Value: TFPColor);
var
  Position: TRawImagePosition;
begin
  GetXYDataPostion(x,y,Position);
  FReadRawImageBits(FPixelData,Position,
               FDataDescription.RedPrec,FDataDescription.RedShift,
               Value.Red);
  FReadRawImageBits(FPixelData,Position,
               FDataDescription.GreenPrec,FDataDescription.GreenShift,
               Value.Green);
  FReadRawImageBits(FPixelData,Position,
               FDataDescription.BluePrec,FDataDescription.BlueShift,
               Value.Blue);

  FAlphaReadRawImageBits(FPixelData,Position,
               FDataDescription.AlphaPrec,FDataDescription.AlphaShift,
               Value.Alpha)
end;

procedure TLazIntfImage.GetColor_NoPalette_RGBA_NoAlpha(x, y: integer; var Value: TFPColor);
var
  Position: TRawImagePosition;
begin
  GetXYDataPostion(x,y,Position);
  FReadRawImageBits(FPixelData,Position,
               FDataDescription.RedPrec,FDataDescription.RedShift,
               Value.Red);
  FReadRawImageBits(FPixelData,Position,
               FDataDescription.GreenPrec,FDataDescription.GreenShift,
               Value.Green);
  FReadRawImageBits(FPixelData,Position,
               FDataDescription.BluePrec,FDataDescription.BlueShift,
               Value.Blue);

  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_NoPalette_Gray(x, y: integer; var Value: TFPColor);
var
  Position: TRawImagePosition;
begin
  GetXYDataPostion(x,y,Position);
  FReadRawImageBits(FPixelData,Position,
               FDataDescription.RedPrec,FDataDescription.RedShift,
               Value.Red);
  Value.Green:=Value.Red;
  Value.Blue:=Value.Red;
end;

procedure TLazIntfImage.GetColor_NULL(x, y: integer; var Value: TFPColor);
var
  Position: TRawImagePosition;
begin
  GetXYDataPostion(x,y,Position);
  Value.Red:=0;
  Value.Green:=0;
  Value.Blue:=0;
  Value.Alpha:=0;
end;

procedure TLazIntfImage.SetColor_NoPalette_RGBA_Alpha_Sep_Mask(x, y: integer;
  const Value: TFPColor);
var
  Position: TRawImagePosition;
  MaskPosition: TRawImagePosition;
begin
  GetXYDataPostion(x,y,Position);
  FWriteRawImageBits(FPixelData,Position,
                FDataDescription.RedPrec,FDataDescription.RedShift,
                Value.Red);
  FWriteRawImageBits(FPixelData,Position,
                FDataDescription.GreenPrec,FDataDescription.GreenShift,
                Value.Green);
  FWriteRawImageBits(FPixelData,Position,
                FDataDescription.BluePrec,FDataDescription.BlueShift,
                Value.Blue);

  GetXYMaskPostion(x,y,MaskPosition);
  FAlphaWriteRawImageBits(FMaskData,MaskPosition,
                FDataDescription.AlphaPrec,
                FDataDescription.AlphaShift,
                Value.Alpha);
end;

procedure TLazIntfImage.SetColor_NoPalette_RGBA_Alpha_NoSep(x, y: integer;
  const Value: TFPColor);
var
  Position: TRawImagePosition;
begin
  GetXYDataPostion(x,y,Position);
  FWriteRawImageBits(FPixelData,Position,
                FDataDescription.RedPrec,FDataDescription.RedShift,
                Value.Red);
  FWriteRawImageBits(FPixelData,Position,
                FDataDescription.GreenPrec,FDataDescription.GreenShift,
                Value.Green);
  FWriteRawImageBits(FPixelData,Position,
                FDataDescription.BluePrec,FDataDescription.BlueShift,
                Value.Blue);

  FAlphaWriteRawImageBits(FPixelData,Position,
                FDataDescription.AlphaPrec,
                FDataDescription.AlphaShift,
                Value.Alpha)
end;

procedure TLazIntfImage.SetColor_NoPalette_RGBA_NoAlpha(x, y: integer; const Value: TFPColor);
var
  Position: TRawImagePosition;
begin
  GetXYDataPostion(x,y,Position);
  FWriteRawImageBits(FPixelData,Position,
                FDataDescription.RedPrec,FDataDescription.RedShift,
                Value.Red);
  FWriteRawImageBits(FPixelData,Position,
                FDataDescription.GreenPrec,FDataDescription.GreenShift,
                Value.Green);
  FWriteRawImageBits(FPixelData,Position,
                FDataDescription.BluePrec,FDataDescription.BlueShift,
                Value.Blue);

  // no alpha -> ignore
end;

procedure TLazIntfImage.SetColor_NoPalette_Gray(x, y: integer;
  const Value: TFPColor);
var
  Position: TRawImagePosition;
begin
  GetXYDataPostion(x,y,Position);
  FWriteRawImageBits(FPixelData,Position,
                FDataDescription.RedPrec,FDataDescription.RedShift,
                Value.Red);
end;

procedure TLazIntfImage.SetColor_NULL(x, y: integer; const Value: TFPColor);
begin
  // NULL, not implemented
end;

procedure TLazIntfImage.SetColor_BPP32_R8G8B8_A1_BIO_TTB(x, y: integer;
  const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
// BitOrder=riboBitsInOrder ByteOrder=DefaultByteOrder
// LineOrder=riloTopToBottom
// BitsPerPixel=32 LineEnd=rileDWordBoundary
// RedPrec=8 RedShift=16 GreenPrec=8 GreenShift=8 BluePrec=8 BlueShift=0
// AlphaSeparate=true
var
  Position: PCardinal;
  Pixel: Cardinal;
  AlphaPosition: Integer;
  AlphaBitPosition: Integer;
  AlphaByte: Byte;
begin
  Position:=PCardinal(FPixelData+FLineStarts[y].Byte+(x shl 2));
  Pixel:=((Value.Red shr 8) shl 16)
         +((Value.Green shr 8) shl 8)
         +(Value.Blue shr 8);
  Position^:=Pixel;

  AlphaPosition:=FMaskLineStarts[y].Byte+(cardinal(x) shr 3);
  AlphaBitPosition:=(x and 7);
  AlphaByte:=FMaskData[AlphaPosition];
  if Value.Alpha>=$8000 then
    AlphaByte:=AlphaByte or (1 shl AlphaBitPosition)
  else
    AlphaByte:=AlphaByte and not (1 shl AlphaBitPosition);
  FMaskData[AlphaPosition]:=AlphaByte;
end;

procedure TLazIntfImage.SetColor_BPP32_R8G8B8_A1_BIO_TTB_RBO(x, y: integer;
  const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
// BitOrder=riboBitsInOrder ByteOrder=not DefaultByteOrder
// LineOrder=riloTopToBottom
// BitsPerPixel=32 LineEnd=rileDWordBoundary
// RedPrec=8 RedShift=16 GreenPrec=8 GreenShift=8 BluePrec=8 BlueShift=0
// AlphaSeparate=true
var
  Position: PCardinal;
  Pixel: Cardinal;
  AlphaPosition: Integer;
  AlphaBitPosition: Integer;
  AlphaByte: Byte;
begin
  Position:=PCardinal(FPixelData+FLineStarts[y].Byte+(x shl 2));
  Pixel:=(Value.Red shr 8)
         +((Value.Green shr 8) shl 8)
         +((Value.Blue shr 8) shl 16);
  Position^:=Pixel;

  AlphaPosition:=FMaskLineStarts[y].Byte+(cardinal(x) shr 3);
  AlphaBitPosition:=(x and 7);
  AlphaByte:=FMaskData[AlphaPosition];
  if Value.Alpha>=$8000 then
    AlphaByte:=AlphaByte or (1 shl AlphaBitPosition)
  else
    AlphaByte:=AlphaByte and not (1 shl AlphaBitPosition);
  FMaskData[AlphaPosition]:=AlphaByte;
end;

procedure TLazIntfImage.SetAutoCreateMask(const AValue: boolean);
begin
  if FAutoCreateMask=AValue then exit;
  FAutoCreateMask:=AValue;
  if AutoCreateMask then
    CreateMaskData
  else
    FreeMaskData;
end;

function TLazIntfImage.GetTColors(x, y: integer): TGraphicsColor;
begin
  Result:=FPColorToTColor(Colors[x,y]);
end;

procedure TLazIntfImage.SetTColors(x, y: integer; const AValue: TGraphicsColor);
begin
  Colors[x,y]:=TColorToFPColor(AValue);
end;

procedure TLazIntfImage.SetUsePalette(Value: boolean);
begin
  inherited SetUsePalette(False);  // Can't handle palettes at the moment
end;

procedure TLazIntfImage.SetInternalColor(x, y: integer; const Value: TFPColor);
begin
  {if (x=0) and (y=0) then begin
    // a common bug in the readers is that Alpha is reversed
    DebugLn('TLazIntfImage.SetInternalColor ',x,',',y,' ',Value.Red,',',Value.Green,',',Value.Blue,',',Value.Alpha);
    if Value.Alpha<>alphaOpaque then
      RaiseGDBException('');
  end;}
  OnSetInternalColor(x,y,Value);
  {if y=Height-1 then
    DebugLn(['TLazIntfImage.SetInternalColor x=',x,' y=',y,' ',dbgs(Value),' ',dbgs(GetInternalColor(x,y))]);}
end;

function TLazIntfImage.GetInternalColor(x, y: integer): TFPColor;
begin
  OnGetInternalColor(x,y,Result);
end;

procedure TLazIntfImage.SetInternalPixel(x, y: integer; Value: integer);
begin

end;

function TLazIntfImage.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TLazIntfImage.FreeAllData;
begin
  FreePixelData;
  FreeMaskData;
end;

procedure TLazIntfImage.FreePixelData;
begin
  ReallocMem(FPixelData,0);
  FPixelDataSize:=0;
  ReallocMem(FLineStarts,0);
end;

procedure TLazIntfImage.FreeMaskData;
begin
  ReallocMem(FMaskData,0);
  FMaskDataSize:=0;
  ReallocMem(FMaskLineStarts,0);
end;

procedure TLazIntfImage.CreateAllData;
begin
  if FUpdateCount>0 then begin
    fCreateAllDataNeeded:=true;
    exit;
  end;
  fCreateAllDataNeeded:=false;
  CreatePixelData;
  if AutoCreateMask then
    CreateMaskData
  else
    FreeMaskData;
end;

procedure TLazIntfImage.CreatePixelData;
begin
  FreePixelData;
  CreateDataAndLineStarts(FPixelData,FPixelDataSize,
                          FLineStarts,FDataDescription.BitsPerPixel,
                          FDataDescription.LineEnd);
end;

procedure TLazIntfImage.CreateMaskData;
var
  MaskDataExisted: boolean;
begin
  MaskDataExisted:=FMaskData<>nil;
  FreeMaskData;
  if (FDataDescription.AlphaBitsPerPixel>0)
  and FDataDescription.AlphaSeparate then begin
    CreateDataAndLineStarts(FMaskData,FMaskDataSize,FMaskLineStarts,
                            FDataDescription.AlphaBitsPerPixel,
                            FDataDescription.AlphaLineEnd);
  end;
  if MaskDataExisted xor (FMaskData<>nil) then
    ChooseGetSetColorFunctions;
end;

function TLazIntfImage.HasTransparency: boolean;
var
  RawImage: TRawImage;
begin
  Result:=true;
  GetRawImage(RawImage);
  if not RawImageMaskIsEmpty(@RawImage,true) then exit;
  Result:=false;
end;

procedure TLazIntfImage.CreateDataAndLineStarts(var Data: Pointer;
  var DataSize: PtrUInt; var TheLineStarts: PRawImagePosition;
  TheBitsPerPixel: cardinal; TheLineEnd: TRawImageLineEnd);
begin
  CreateRawImageLineStarts(Width,Height,TheBitsPerPixel,TheLineEnd,
                           TheLineStarts);
  CreateRawImageData(Width,Height,TheBitsPerPixel,TheLineEnd,Data,DataSize);
end;

constructor TLazIntfImage.Create(AWidth, AHeight: integer);
begin
  FAutoCreateMask:=true;
  OnGetInternalColor:=@GetColor_NULL;
  OnSetInternalColor:=@SetColor_NULL;
  inherited Create(AWidth, AHeight);
end;

destructor TLazIntfImage.Destroy;
begin
  FreeAllData;
  inherited Destroy;
end;

procedure TLazIntfImage.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TLazIntfImage.EndUpdate;
begin
  dec(FUpdateCount);
  if (FUpdateCount=0) then begin
    if fCreateAllDataNeeded then
      CreateAllData;
    if FGetSetColorFunctionsUpdateNeeded then
      ChooseGetSetColorFunctions;
  end;
end;

procedure TLazIntfImage.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  if (AWidth<0) or (AHeight<0) then
    Raise FPImageException.Create('Invalid Size');
  inherited SetSize(AWidth, AHeight);
  FDataDescription.Width:=Width;
  FDataDescription.Height:=Height;
  CreateAllData;
end;

function TLazIntfImage.CheckDescription(
  const ADescription: TRawImageDescription; ExceptionOnError: boolean
    ): boolean;

  procedure DoError(const Msg: string);
  begin
    if ExceptionOnError then Raise FPImageException.Create(Msg);
    DebugLn('TLazIntfImage.CheckDescription: ',Msg);
  end;

begin
  Result:=false;
  // check format
  if (not (ADescription.Format
      in [low(TRawImageColorFormat)..high(TRawImageColorFormat)]))
  then begin
    DoError('Invalid Raw Image Description Format'); exit;
  end;

  Result:=true;
end;

procedure TLazIntfImage.GetXYDataPostion(x, y: integer;
  var Position: TRawImagePosition);
var
  BitOffset: cardinal;
begin
  if FDataDescription.LineOrder=riloBottomToTop then
    y:=Height-y;
  Position:=FLineStarts[y];
  BitOffset:=FDataDescription.BitsPerPixel*cardinal(x)+Position.Bit;
  Position.Bit:=(BitOffset and 7);
  inc(Position.Byte,BitOffset shr 3);
end;

procedure TLazIntfImage.GetXYMaskPostion(x, y: integer;
  var Position: TRawImagePosition);
var
  BitOffset: cardinal;
begin
  if FDataDescription.LineOrder=riloBottomToTop then
    y:=Height-y;
  Position:=FMaskLineStarts[y];
  BitOffset:=FDataDescription.AlphaBitsPerPixel*cardinal(x)+Position.Bit;
  Position.Bit:=(BitOffset and 7);
  inc(Position.Byte,BitOffset shr 3);
end;

function TLazIntfImage.GetDataLineStart(y: integer): Pointer;
begin
  if FDataDescription.LineOrder=riloBottomToTop then
    y:=Height-y;
  Result:=FPixelData+FLineStarts[y].Byte;
end;

procedure TLazIntfImage.LoadFromDevice(DC: HDC);
var
  ARect: TRect;
  ARawImage: TRawImage;
  DeviceSize: TPoint;
begin
  GetDeviceSize(DC,DeviceSize);
  ARect:=Rect(0,0,DeviceSize.X,DeviceSize.Y);
  if not GetRawImageFromDevice(DC,ARect,ARawImage) then
    raise FPImageException.Create('Failed to get raw image from device');
  SetRawImage(ARawImage);
end;

procedure TLazIntfImage.LoadFromBitmap(Bitmap, MaskBitmap: HBitmap;
  AWidth: integer; AHeight: integer);
var
  ARect: TRect;
  ARawImage: TRawImage;
  NewDataDescription: TRawImageDescription;
begin
  if not GetBitmapRawImageDescription(Bitmap,@NewDataDescription) then
    raise FPImageException.Create('Failed to get raw image description from bitmap');
  ARect:=Rect(0,0,NewDataDescription.Width,NewDataDescription.Height);
  if (AWidth >= 0) and (dword(AWidth) < NewDataDescription.Width) then
    ARect.Right := AWidth;
  if (AHeight >= 0) and (dword(AHeight) > NewDataDescription.Height) then
    ARect.Bottom := AHeight;
  if not GetRawImageFromBitmap(Bitmap,MaskBitmap,ARect,ARawImage) then
    raise FPImageException.Create('Failed to get raw image from bitmap');
  SetRawImage(ARawImage);
end;

procedure TLazIntfImage.CreateBitmap(var Bitmap, MaskBitmap: HBitmap;
  AlwaysCreateMask: boolean);
var
  ARawImage: TRawImage;
begin
  GetRawImage(ARawImage);
  if not CreateBitmapFromRawImage(ARawImage,Bitmap,MaskBitmap,AlwaysCreateMask)
  then
    raise FPImageException.Create('Failed to create bitmaps');
end;

procedure TLazIntfImage.SetRawImage(const RawImage: TRawImage);
var
  OldRawImage: TRawImage;
begin
  GetRawImage(OldRawImage);
  if CompareMem(@OldRawImage,@RawImage,SizeOf(TRawImage)) then exit;
  BeginUpdate;
  try
    FreeAllData;
    fDataDescription:=RawImage.Description;
    FPixelData:=RawImage.Data;
    FPixelDataSize:=RawImage.DataSize;
    FMaskData:=RawImage.Mask;
    FMaskDataSize:=RawImage.MaskSize;
    SetSize(FDataDescription.Width,FDataDescription.Height);
    fCreateAllDataNeeded:=false;
    CreateRawImageLineStarts(Width,Height,FDataDescription.BitsPerPixel,
                     FDataDescription.LineEnd,FLineStarts);
    if (FMaskData<>nil) then
      CreateRawImageLineStarts(Width,Height,FDataDescription.AlphaBitsPerPixel,
                       FDataDescription.AlphaLineEnd,FMaskLineStarts);
    ChooseGetSetColorFunctions;
  finally
    EndUpdate;
  end;
end;

procedure TLazIntfImage.GetRawImage(out RawImage: TRawImage);
begin
  FillChar(RawImage,SizeOf(TRawImage),0);
  RawImage.Description:=FDataDescription;
  RawImage.Data:=FPixelData;
  RawImage.DataSize:=FPixelDataSize;
  RawImage.Mask:=FMaskData;
  RawImage.MaskSize:=FMaskDataSize;
end;

procedure TLazIntfImage.GetDescriptionFromDevice(DC: HDC);
var
  NewDataDescription: TRawImageDescription;
begin
  if DC=0 then begin
    DC:=GetDC(0);
    try
      if not GetDeviceRawImageDescription(DC,@NewDataDescription) then
        raise FPImageException.Create('Failed to get raw image description from device');
    finally
      ReleaseDC(0,DC);
    end;
  end else begin
    if not GetDeviceRawImageDescription(DC,@NewDataDescription) then
      raise FPImageException.Create('Failed to get raw image description from device');
  end;
  DataDescription:=NewDataDescription;
end;

procedure TLazIntfImage.GetDescriptionFromBitmap(Bitmap: HBitmap);
var
  NewDataDescription: TRawImageDescription;
begin
  if not GetBitmapRawImageDescription(Bitmap,@NewDataDescription) then
    raise FPImageException.Create('Failed to get raw image description from bitmap');
  DataDescription:=NewDataDescription;
end;

procedure TLazIntfImage.Set_BPP24_B8G8R8_A1_BIO_TTB(NewWidth, NewHeight: integer
  );
{ pf24bit:

 Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
 BitOrder=riboBitsInOrder ByteOrder=DefaultByteOrder
 LineOrder=riloTopToBottom
 BitsPerPixel=24 LineEnd=rileDWordBoundary
 RedPrec=8 RedShift=16 GreenPrec=8 GreenShift=8 BluePrec=8 BlueShift=0
 AlphaSeparate=false }
var
  ADesc: TRawImageDescription;
begin
  // setup an artificial ScanLineImage with format RGB 24 bit, 24bit depth format
  FillChar(ADesc,SizeOf(ADesc),0);
  with ADesc do begin
    Format:=ricfRGBA;
    Depth:=24; // used bits per pixel
    Width:=0;
    Height:=0;
    BitOrder:=riboBitsInOrder;
    ByteOrder:=DefaultByteOrder;
    LineOrder:=riloTopToBottom;
    BitsPerPixel:=24; // bits per pixel. can be greater than Depth.
    LineEnd:=rileDWordBoundary;
    RedPrec:=8; // red precision. bits for red
    RedShift:=16;
    GreenPrec:=8;
    GreenShift:=8; // bitshift. Direction: from least to most signifikant
    BluePrec:=8;
    BlueShift:=0;
    AlphaPrec:=0;
    AlphaSeparate:=false;
  end;

  DataDescription:=ADesc;
  SetSize(NewWidth,NewHeight);
end;

procedure TLazIntfImage.Set_BPP32_B8G8R8_A1_BIO_TTB(NewWidth, NewHeight: integer
  );
{ pf32bit:

 Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
 BitOrder=riboBitsInOrder ByteOrder=DefaultByteOrder
 LineOrder=riloTopToBottom
 BitsPerPixel=32 LineEnd=rileDWordBoundary
 RedPrec=8 RedShift=16 GreenPrec=8 GreenShift=8 BluePrec=8 BlueShift=0
 AlphaSeparate=false }
var
  ADesc: TRawImageDescription;
begin
  // setup an artificial ScanLineImage with format RGB 24 bit, 32bit depth format
  FillChar(ADesc,SizeOf(ADesc),0);
  with ADesc do begin
    Format:=ricfRGBA;
    Depth:=24; // used bits per pixel
    Width:=0;
    Height:=0;
    BitOrder:=riboBitsInOrder;
    ByteOrder:=DefaultByteOrder;
    LineOrder:=riloTopToBottom;
    BitsPerPixel:=32; // bits per pixel. can be greater than Depth.
    LineEnd:=rileDWordBoundary;
    RedPrec:=8; // red precision. bits for red
    RedShift:=16;
    GreenPrec:=8;
    GreenShift:=8; // bitshift. Direction: from least to most signifikant
    BluePrec:=8;
    BlueShift:=0;
    AlphaPrec:=0;
    AlphaSeparate:=false;
  end;

  DataDescription:=ADesc;
  SetSize(NewWidth,NewHeight);
end;

procedure TLazIntfImage.FillPixels(const Color: TFPColor);
var
  ColorChar: char;
  ColorWord: Word;
  Cnt: Integer;
  i: Integer;
  ColorDWord: Cardinal;
  y: Integer;
  x: Integer;
begin
  if (Width=0) or (Height=0) or (FPixelData=nil) then exit;

  case FDataDescription.BitsPerPixel of

  8:
    begin
      SetInternalColor(0,0,Color);
      ColorChar:=Char(FPixelData[0]);
      FillChar(FPixelData^,FPixelDataSize,ColorChar);
    end;

  16:
    begin
      SetInternalColor(0,0,Color);
      ColorWord:=PWord(FPixelData)[0];
      Cnt:=FPixelDataSize div 2;
      for i:=0 to Cnt-1 do
        PWord(FPixelData)[i]:=ColorWord;
    end;

  32:
    begin
      SetInternalColor(0,0,Color);
      ColorDWord:=PDWord(FPixelData)[0];
      Cnt:=FPixelDataSize div 4;
      for i:=0 to Cnt-1 do
        PDWord(FPixelData)[i]:=ColorDWord;
    end;

  else
    for y:=0 to Height-1 do
      for x:=0 to Width-1 do
        SetInternalColor(x,y,Color);
  end;

  // ToDo: mask
end;

procedure TLazIntfImage.CopyPixels(Src: TFPCustomImage);
var
  y: Integer;
  x: Integer;
  SrcImg: TLazIntfImage;
begin
  if (Src.Width<>Width) or (Src.Height<>Height) then
    SetSize(Src.Width,Src.Height);
  if Src is TLazIntfImage then begin
    SrcImg:=TLazIntfImage(Src);
    if CompareMem(@FDataDescription,@SrcImg.FDataDescription,
      SizeOf(FDataDescription))
    then begin
      // same description -> copy
      if FPixelData<>nil then
        System.Move(SrcImg.FPixelData^,FPixelData^,FPixelDataSize);
      if FMaskData<>nil then
        System.Move(SrcImg.FMaskData^,FMaskData^,FMaskDataSize);
      exit;
    end;
  end;
    
  // copy pixels
  for y:=0 to Height-1 do
    for x:=0 to Width-1 do
      Colors[x,y]:=Src.Colors[x,y];
end;

{ TLazReaderXPM }

type
  TXPMPixelToColorEntry = record
    Color: TFPColor;
  end;
  PXPMPixelToColorEntry = ^TXPMPixelToColorEntry;

procedure TLazReaderXPM.ClearPixelToColorTree;
var
  Entry: PXPMPixelToColorEntry;
  ArrNode: TArrayNode;
begin
  if FPixelToColorTree<>nil then begin
    ArrNode:=FPixelToColorTree.Root;
    while ArrNode<>nil do begin
      Entry:=PXPMPixelToColorEntry(ArrNode.Data);
      if Entry<>nil then begin
        //DebugLn('TLazReaderXPM.ClearPixelToColorTree A ',DbgS(ArrNode),' ',DbgS(Entry));
        Dispose(Entry);
      end;
      ArrNode:=ArrNode.FindNext;
    end;
    FPixelToColorTree.Free;
    FPixelToColorTree:=nil;
  end;
end;

procedure TLazReaderXPM.InternalRead(Str: TStream; Img: TFPCustomImage);
type
  TSrcLine = record
    StartPos: integer;
    EndPos: integer;
  end;

var
  SrcPos: integer;
  Src: String;
  SrcLen: Integer;
  CurLineNumber, LastLineStart: integer;

  procedure RaiseXPMReadError(const Msg: string; ReadPos: integer);
  var
    CurColumn: Integer;
  begin
    CurColumn:=ReadPos-LastLineStart+1;
    raise Exception.Create(Msg
                           +' in xpm stream at line '+IntToStr(CurLineNumber)
                           +' column '+IntToStr(CurColumn));
  end;

  // read next string constant "" and skip comments
  function ReadNextLine(var Line: TSrcLine;
    ExceptionOnNotFound: Boolean): boolean;
  begin
    while SrcPos<=SrcLen do begin
      case Src[SrcPos] of

      #10,#13:
        begin
          // count linenumbers for nicer error output
          inc(SrcPos);
          inc(CurLineNumber);
          if (SrcPos<=SrcLen) and (Src[SrcPos] in [#10,#13])
          and (Src[SrcPos]<>Src[SrcPos-1]) then
            inc(SrcPos);
          LastLineStart:=SrcPos;
        end;

      '/':
        begin
          if (SrcPos<SrcLen) and (Src[SrcPos+1]='*') then begin
            // this is a C comment
            // -> skip comment
            inc(SrcPos,2);
            while (SrcPos<SrcLen) do begin
              if (Src[SrcPos]='*') and (Src[SrcPos+1]='/') then begin
                // comment end found
                inc(SrcPos,2);
                break;
              end;
              inc(SrcPos);
            end;
          end else
            RaiseXPMReadError('syntax error',SrcPos);
        end;

      '"':
        begin
          // start of a string constant
          inc(SrcPos);
          Line.StartPos:=SrcPos;
          while (SrcPos<SrcLen) do begin
            if (Src[SrcPos]='"') and (Src[SrcPos-1]<>'\') then begin
              // string end found
              Line.EndPos:=SrcPos;
              //DebugLn('  ',copy(Src,Line.StartPos-1,Line.EndPos-Line.StartPos+2));
              inc(SrcPos);
              Result:=true;
              exit;
            end;
            inc(SrcPos);
          end;
        end;

      else
        inc(SrcPos);
      end;
    end;
    Result:=false;
    if ExceptionOnNotFound then
      Raise Exception.Create('Unexpected end of xpm stream');
  end;

  function ReadNumber(var ReadPos: integer;
    ExceptionOnNotFound: Boolean): integer;
  begin
    // skip spaces
    while IsSpaceChar[Src[ReadPos]] do inc(ReadPos);
    // read number
    Result:=0;
    if IsNumberChar[Src[ReadPos]] then begin
      repeat
        Result:=Result*10+ord(Src[ReadPos])-Ord('0');
        inc(ReadPos);
      until not IsNumberChar[Src[ReadPos]];
    end else if ExceptionOnNotFound then
      RaiseXPMReadError('number expected',ReadPos);
  end;

  procedure ReadHeader;
  var
    FirstLine: TSrcLine;
  begin
    ReadNextLine(FirstLine,true);
    FWidth:=ReadNumber(FirstLine.StartPos,true);
    FHeight:=ReadNumber(FirstLine.StartPos,true);
    FColorCount:=ReadNumber(FirstLine.StartPos,true);
    FCharsPerPixel:=ReadNumber(FirstLine.StartPos,true);
    fXHot:=ReadNumber(FirstLine.StartPos,false);
    fYHot:=ReadNumber(FirstLine.StartPos,fXHot<>0);
    //DebugLn('ReadHeader A Width=',FWidth,' Height=',FHeight,' ColorCount=',FColorCount,' CharsPerPixel=',FCharsPerPixel);
    // ToDo: parse XPMExt tag
  end;

  function HexToColor(HexStart, HexEnd: integer): TFPColor;

    procedure ReadHexNumber(var StartPos: integer; Len: integer;
      var Number: word);
    var
      c: Char;
      i: Integer;
    begin
      Number:=0;
      for i:=1 to 4 do begin
        Number:=Number shl 4;
        if i<=Len then begin
          c:=Src[StartPos];
          case c of
          '0'..'9': inc(Number,ord(c)-ord('0'));
          'A'..'F': inc(Number,ord(c)-ord('A')+10);
          'a'..'f': inc(Number,ord(c)-ord('a')+10);
          end;
          inc(StartPos);
        end;
      end;
      // fill missing bits
      case Len of
      1: Number:=Number or (Number shr 4) or (Number shr 8) or (Number shr 12);
      2: Number:=Number or (Number shr 8);
      3: Number:=Number or (Number shr 12);
      end;
    end;

  var
    HexLen: Integer;
    SampleLen: Integer;
    SampleStart: Integer;
  begin
    HexLen:=HexEnd-HexStart;
    case HexLen of
    3: SampleLen:=1;
    6: SampleLen:=2;
    9: SampleLen:=3;
    12:SampleLen:=4;
    else
      RaiseXPMReadError('hexnumber expected',HexStart);
    end;
    SampleStart:=HexStart;
    ReadHexNumber(SampleStart,SampleLen,Result.Red);
    ReadHexNumber(SampleStart,SampleLen,Result.Green);
    ReadHexNumber(SampleStart,SampleLen,Result.Blue);
    Result.Alpha:=alphaOpaque;
  end;

  function TextToColor(TextStart, TextEnd: integer): TFPColor;
  var
    s: String;
  begin
    s := lowercase(copy(Src,TextStart,TextEnd-TextStart));
    if s = 'transparent' then
      Result := FPImage.colTransparent
    else if s = 'none' then
      Result := FPImage.colTransparent
    else if s = 'black' then
      result := FPImage.colBlack
    else if s = 'blue' then
      Result := FPImage.colBlue
    else if s = 'green' then
      Result := FPImage.colGreen
    else if s = 'cyan' then
      Result := FPImage.colCyan
    else if s = 'red' then
      Result := FPImage.colRed
    else if s = 'magenta' then
      Result := FPImage.colMagenta
    else if s = 'yellow' then
      Result := FPImage.colYellow
    else if s = 'white' then
      Result := FPImage.colWhite
    else if s = 'gray' then
      Result := FPImage.colGray
    else if s = 'lightgray' then
      Result := FPImage.colLtGray
    else if (s = 'darkgray') or (s='grey') then
      Result := FPImage.colDKGray
    else if s = 'darkblue' then
      Result := FPImage.colDkBlue
    else if s = 'darkgreen' then
      Result := FPImage.colDkGreen
    else if s = 'darkcyan' then
      Result := FPImage.colDkCyan
    else if s = 'darkred' then
      Result := FPImage.colDkRed
    else if s = 'darkmagenta' then
      Result := FPImage.colDkMagenta
    else if s = 'darkyellow' then
      Result := FPImage.colDkYellow
    else if s = 'maroon' then
      Result := FPImage.colMaroon
    else if s = 'lightgreen' then
      Result := FPImage.colLtGreen
    else if s = 'olive' then
      Result := FPImage.colOlive
    else if s = 'navy' then
      Result := FPImage.colNavy
    else if s = 'purple' then
      Result := FPImage.colPurple
    else if s = 'teal' then
      Result := FPImage.colTeal
    else if s = 'silver' then
      Result := FPImage.colSilver
    else if s = 'lime' then
      Result := FPImage.colLime
    else if s = 'fuchsia' then
      Result := FPImage.colFuchsia
    else if s = 'aqua' then
      Result := FPImage.colAqua
    else
      Result := FPImage.colTransparent;
  end;

  procedure AddColor(PixelStart: integer; const AColor: TFPColor;
    IntArray: PInteger);
  var
    NewEntry: PXPMPixelToColorEntry;
    i: Integer;
  begin
    {DebugLn('TLazReaderXPM.InternalRead.AddColor A "',DbgStr(copy(Src,PixelStart,FCharsPerPixel)),'"=',
      DbgS(AColor.Red),',',
      DbgS(AColor.Green),',',
      DbgS(AColor.Blue),',',
      DbgS(AColor.Alpha));}
    New(NewEntry);
    NewEntry^.Color:=AColor;
    // add entry to Array Tree
    if FPixelToColorTree=nil then
      FPixelToColorTree:=TArrayNodesTree.Create;
    for i:=0 to FCharsPerPixel-1 do
      IntArray[i]:=ord(Src[PixelStart+i]);
    FPixelToColorTree.SetNode(IntArray,FCharsPerPixel,NewEntry);
    //if FPixelToColorTree.FindData(IntArray,FCharsPerPixel)<>NewEntry then RaiseGDBException('');
  end;

  procedure ReadPalette(IntArray: PInteger);
  var
    i: Integer;
    Line: TSrcLine;
    ReadPos: Integer;
    ColorStart: Integer;
    ColorEnd: Integer;
    NewColor: TFPColor;
    PixelStart: Integer;
  begin
    for i:=1 to FColorCount do begin
      ReadNextLine(Line,true);
      ReadPos:=Line.StartPos;
      // read pixel string
      PixelStart:=ReadPos;
      inc(ReadPos,FCharsPerPixel);
      // skip spaces
      while IsSpaceChar[Src[ReadPos]] do inc(ReadPos);
      // read 'c' (sometimes the 'c' is an 's')
      if not (Src[ReadPos] in ['c','s']) then
        RaiseXPMReadError('"c" expected',ReadPos);
      inc(ReadPos);
      // skip spaces
      while IsSpaceChar[Src[ReadPos]] do inc(ReadPos);
      // read color string
      ColorStart:=ReadPos;
      if Src[ReadPos]='#' then begin
        inc(ColorStart);
        // read as hexnumber
        repeat
          inc(ReadPos);
        until not (IsHexNumberChar[Src[ReadPos]]);
        ColorEnd:=ReadPos;
        NewColor:=HexToColor(ColorStart,ColorEnd);
      end else begin
        // read as text
        repeat
          inc(ReadPos);
        until not (Src[ReadPos] in ['A'..'Z','a'..'z']);
        ColorEnd:=ReadPos;
        NewColor:=TextToColor(ColorStart,ColorEnd);
      end;
      AddColor(PixelStart,NewColor,IntArray);
    end;
  end;

  procedure ReadPixels(IntArray: PInteger);
  var
    y: Integer;
    Line: TSrcLine;
    ReadPos: Integer;
    x: Integer;
    i: Integer;
    CurColor: TFPColor;
    ProgressCount: Integer;
    ContinueReading: Boolean;
    CurEntry: PXPMPixelToColorEntry;
  begin
    Img.SetSize(FWidth,fHeight);
    ProgressCount:=10000;
    for y:=0 to fHeight-1 do begin
      ReadNextLine(Line,true);
      ReadPos:=Line.StartPos;
      if Line.EndPos-Line.StartPos<FCharsPerPixel*FWidth then
        RaiseXPMReadError('line too short',ReadPos);
      for x:=0 to FWidth-1 do begin
        //DebugLn('ReadPixels x=',dbgs(x),' y=',dbgs(y),' color="',DbgStr(copy(Src,ReadPos,FCharsPerPixel)),'"');
        for i:=0 to FCharsPerPixel-1 do begin
          IntArray[i]:=ord(Src[ReadPos]);
          inc(ReadPos);
        end;
        CurEntry:=PXPMPixelToColorEntry(
                           FPixelToColorTree.FindData(IntArray,FCharsPerPixel));
        if CurEntry<>nil then
          CurColor:=CurEntry^.Color
        else
          RaiseXPMReadError('invalid color',ReadPos-FCharsPerPixel);
        {if CurEntry2<>CurEntry then begin
          DebugLn('x=',x,' y=',y,' Pixel=',Entry^.Pixel,
            ' RefPixel=',CurEntry^.Pixel,
            ' Color=',
            DbgS(CurColor.Red),',',
            DbgS(CurColor.Green),',',
            DbgS(CurColor.Blue),',',
            DbgS(CurColor.Alpha));
          DebugLn('Entry2: Pixel=',CurEntry2^.Pixel,
            ' RefPixel=',CurEntry2^.Pixel,
            ' Color=',
            DbgS(CurEntry2^.Color.Red),',',
            DbgS(CurEntry2^.Color.Green),',',
            DbgS(CurEntry2^.Color.Blue),',',
            DbgS(CurEntry2^.Color.Alpha));
        end;}

        {DebugLn('x=',x,' y=',y,' Pixel=',Entry^.Pixel,
          ' RefPixel=',PXPMPixelToColorEntry(Node.Data)^.Pixel,
          ' Color=',
          DbgS(CurColor.Red),',',
          DbgS(CurColor.Green),',',
          DbgS(CurColor.Blue),',',
          DbgS(CurColor.Alpha));}
        Img.Colors[x,y]:=CurColor;
      end;
      if ProgressCount>0 then begin
        dec(ProgressCount,FWidth);
      end else begin
        if Assigned(Img.OnProgress) then begin
          ContinueReading:=true;
          Img.OnProgress(Self,FPImage.psRunning,Byte((y*100) div FHeight),
            true,Rect(0,0,FWidth,y),'reading XPM pixels',ContinueReading);
          if not ContinueReading then exit;
        end;
        ProgressCount:=10000;
      end;
    end;
  end;

var
  IntArray: PInteger;
begin
  ClearPixelToColorTree;
  Src:=ReadCompleteStreamToString(Str,1024);
  SrcLen:=length(Src);
  SrcPos:=1;
  CurLineNumber:=1;
  LastLineStart:=1;
  ReadHeader;
  GetMem(IntArray,SizeOf(Integer)*(FCharsPerPixel+1));
  try
    ReadPalette(IntArray);
    //FPixelToColorTree.ConsistencyCheck;
    ReadPixels(IntArray);
  finally
    FreeMem(IntArray);
  end;
end;

function TLazReaderXPM.InternalCheck(Str: TStream): boolean;
var s : string[9];
    l : integer;
begin
  try
    l := str.Read (s[1],9);
    s[0] := char(l);
    if l <> 9 then
      result := False
    else
      result := (s = '/* XPM */');
  except
    result := false;
  end;
end;

constructor TLazReaderXPM.Create;
begin
  inherited Create;
end;

destructor TLazReaderXPM.Destroy;
begin
  ClearPixelToColorTree;
  inherited Destroy;
end;

{ TLazAVLPalette }

type
  TLazAVLPaletteEntry = record
    Palette: TLazAVLPalette;
    Index: integer;
  end;
  PLazAVLPaletteEntry = ^TLazAVLPaletteEntry;

function CompareLazAVLPaletteEntries(Entry1, Entry2: PLazAVLPaletteEntry): integer;
begin
  Result := Entry1^.Palette.CompareEntries(Entry1^.Index, Entry2^.Index);
end;

function ComparePFPColorAndLazAVLPalEntry(PColor: PFPColor; Entry: PLazAVLPaletteEntry): integer;
begin
  Result := Entry^.Palette.CompareColorWithEntries(PColor^, Entry^.Index);
end;

procedure TLazAVLPalette.SetCount(NewCount: integer);
var
  NewAVLPalEntry: PLazAVLPaletteEntry;
  AVLNode: TAvgLvlTreeNode;
  CurAVLPalEntry: PLazAVLPaletteEntry;
  Index: Integer;
begin
  if FCount=NewCount then exit;
  // remove unused colors from 'color to index' tree
  if FAVLPalette<>nil then begin
    for Index:=FCount-1 downto NewCount do begin
      AVLNode:=FAVLNodes[Index];
      CurAVLPalEntry:=PLazAVLPaletteEntry(AVLNode.Data);
      FAVLPalette.Delete(AVLNode);
      FAVLNodes[Index]:=nil;
      Dispose(CurAVLPalEntry);
    end;
  end;
  inherited SetCount(NewCount);
  // create tree if not already done
  if (FAVLPalette=nil) and (FCount>0) then
    FAVLPalette:=TAvgLvlTree.Create(TListSortCompare(@CompareLazAVLPaletteEntries));
  if FAVLPalette=nil then exit;
  // add new colors to 'color to index' tree and 'index to node' array
  while FAVLPalette.Count<FCount do begin
    Index:=FAVLPalette.Count;
    New(NewAVLPalEntry);
    NewAVLPalEntry^.Palette:=Self;
    NewAVLPalEntry^.Index:=Index;
    FAVLNodes[Index]:=FAVLPalette.Add(NewAVLPalEntry);
  end;
end;

procedure TLazAVLPalette.SetColor(Index: integer; const NewColor: TFPColor);
var
  Node: TAvgLvlTreeNode;
  Entry: PLazAVLPaletteEntry;
begin
  if Index=FCount then
    Add(NewColor)
  else begin
    CheckIndex(Index);
    if FData^[Index]=NewColor then exit;
    // remove node from tree
    Node:=FAVLNodes[Index];
    Entry:=PLazAVLPaletteEntry(Node.Data);
    FAVLPalette.Delete(Node);
    // change color
    FData^[index] := NewColor;
    // add node
    FAVLNodes[Index]:=FAVLPalette.Add(Entry);
  end;
end;

destructor TLazAVLPalette.Destroy;
begin
  SetCount(0);
  FAVLPalette.Free;
  FAVLPalette:=nil;
  if FCapacity>0 then
    FreeMem(FAVLNodes);
  inherited Destroy;
end;

function TLazAVLPalette.IndexOf(const AColor: TFPColor): integer;
var
  Node: TAvgLvlTreeNode;
begin
  if FAVLPalette<>nil then
    Node:=FAVLPalette.FindKey(@AColor,TListSortCompare(@ComparePFPColorAndLazAVLPalEntry))
  else
    Node:=nil;
  if Node<>nil then
    Result:=PLazAVLPaletteEntry(Node.Data)^.Index
  else
    Result:=Add(AColor);
end;

function TLazAVLPalette.Add(const NewColor: TFPColor): integer;
begin
  Result:=FCount;
  if FCount=FCapacity then EnlargeData;
  SetCount(FCount+1);
  SetColor(Result,NewColor);
end;

function TLazAVLPalette.CompareEntries(Index1, Index2: integer): integer;
begin
  Result:=CompareColors(FData^[Index1],FData^[Index2]);
end;

function TLazAVLPalette.CompareColorWithEntries(const AColor: TFPColor;
  Index: integer): integer;
begin
  Result:=CompareColors(AColor,FData^[Index]);
end;

procedure TLazAVLPalette.EnlargeData;
var
  NewCapacity: Integer;
begin
  if FCapacity<16 then
    NewCapacity:=32
  else if FCapacity<64 then
    NewCapacity:=128
  else
    NewCapacity:=FCapacity*2;
  ReallocMem(FData,SizeOf(TFPColor)*NewCapacity);
  ReallocMem(FAVLNodes,SizeOf(Pointer)*NewCapacity);
  FCapacity:=NewCapacity;
end;

procedure TLazAVLPalette.CheckConsistency;
var
  Node: TAvgLvlTreeNode;
  Entry: PLazAVLPaletteEntry;
  i: Integer;
begin
  if FAVLPalette<>nil then begin
    if FAVLPalette.ConsistencyCheck<>0 then
      RaiseGDBException('TLazAVLPalette.ConsistencyCheck');
    if FAVLPalette.Count<>FCount then
      RaiseGDBException('TLazAVLPalette.ConsistencyCheck');
  end;
  if FAVLNodes<>nil then begin
    for i:=0 to FCapacity-1 do begin
      Node:=FAVLNodes[i];
      if i>=FCount then begin
        continue;
      end;
      if Node=nil then
        RaiseGDBException('TLazAVLPalette.ConsistencyCheck');
      Entry:=PLazAVLPaletteEntry(Node.Data);
      if Entry=nil then
        RaiseGDBException('TLazAVLPalette.ConsistencyCheck');
      if Entry^.Index<>i then
        RaiseGDBException('TLazAVLPalette.ConsistencyCheck');
      if Entry^.Palette<>Self then
        RaiseGDBException('TLazAVLPalette.ConsistencyCheck');
    end;
  end;
end;

{ TLazWriterXPM }

const
  DefXPMPalChars = '.,-*abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
                  +'0123456789@#;:=+%$()[]';

procedure TLazWriterXPM.SetNibblesPerSample(const AValue: word);
begin
  if FNibblesPerSample=AValue then exit;
  FNibblesPerSample:=AValue;
  if FNibblesPerSample>4 then FNibblesPerSample:=4;
  FRightShiftSample:=(4-FNibblesPerSample)*4;
end;

procedure TLazWriterXPM.InternalWrite(Str: TStream; Img: TFPCustomImage);
var
  Palette: TLazAVLPalette;
  PixelStrings: ^AnsiString;
  ColorStrings: ^AnsiString;
  CharsPerPixel: Integer;
  LineEnd: string;

  function GetColor(x,y: integer): TFPColor;
  begin
    Result:=Img.Colors[x,y];
    if (Result.Alpha>=(alphaOpaque shr 1)) then
      Result.Alpha:=alphaOpaque
    else
      Result:=colTransparent;
    Result.Red:=Result.Red shr FRightShiftSample;
    Result.Green:=Result.Green shr FRightShiftSample;
    Result.Blue:=Result.Blue shr FRightShiftSample;
  end;

  function SampleToHex(Sample: word): string;
  begin
    Result:=HexStr(Sample,FNibblesPerSample);
  end;

  procedure BuildPalette;
  var
    x: Integer;
    y: Integer;
    PixelStringsSize: Integer;
    i: Integer;
    Rest: Integer;
    c: char;
    CharPos: Integer;
    ColorStringsSize: Integer;
    Color: TFPColor;
  begin
    // create Palette
    Palette:=TLazAVLPalette.Create(0);
    for y:=0 to Img.Height-1 do
      for x:=0 to Img.Width-1 do
        Palette.IndexOf(GetColor(x,y));
    // calclulate CharsPerPixel
    CharsPerPixel:=0;
    i:=Palette.Count;
    while i>0 do begin
      i:=i div length(DefXPMPalChars);
      inc(CharsPerPixel);
    end;
    // create pixel strings
    PixelStringsSize:=SizeOf(Pointer)*Palette.Count;
    ReAllocMem(PixelStrings,PixelStringsSize);
    FillChar(PixelStrings^,PixelStringsSize,0);
    for i:=0 to Palette.Count-1 do begin
      SetLength(PixelStrings[i],CharsPerPixel);
      Rest:=i;
      for CharPos:=CharsPerPixel downto 1 do begin
        c:=DefXPMPalChars[(Rest mod length(DefXPMPalChars))+1];
        PixelStrings[i][CharPos]:=c;
        Rest:=Rest div length(DefXPMPalChars);
      end;
    end;
    // create color strings
    ColorStringsSize:=SizeOf(Pointer)*Palette.Count;
    ReAllocMem(ColorStrings,ColorStringsSize);
    FillChar(ColorStrings^,ColorStringsSize,0);
    for i:=0 to Palette.Count-1 do begin
      Color:=Palette[i];
      if Color.Alpha=0 then begin
        ColorStrings[i]:='None';
      end else begin
        ColorStrings[i]:='#'+SampleToHex(Color.Red)+SampleToHex(Color.Green)
                            +SampleToHex(Color.Blue);
      end;
    end;
  end;

  procedure WriteString(const s: string);
  begin
    Str.Write(s[1],length(s));
  end;

  procedure WriteHeader;
  var
    s: String;
  begin
    s:='/* XPM */'+LineEnd;
    s:=s+'static char *graphic[] = {'+LineEnd;
    s:=s+'"'+IntToStr(Img.Width)+' '+IntToStr(Img.Height)
        +' '+IntToStr(Palette.Count)+' '+IntToStr(CharsPerPixel)+'"';
    if Palette.Count>0 then s:=s+',';
    s:=s+LineEnd;
    WriteString(s);
  end;

  procedure WritePalette;
  var
    s: string;
    SrcPos: Integer;

    procedure WriteToSrc(const AddString: string);
    var
      i: Integer;
    begin
      for i:=1 to length(AddString) do begin
        s[SrcPos]:=AddString[i];
        inc(SrcPos);
      end;
    end;

  var
    PaletteLineLen: Integer;
    i: Integer;
    SrcLen: Integer;
  begin
    // calculate needed memory
    PaletteLineLen:=length('"')+CharsPerPixel+length(' c ')+length('",'+LineEnd);
    SrcLen:=0;
    for i:=0 to Palette.Count-1 do begin
      inc(SrcLen,PaletteLineLen);
      inc(SrcLen,length(ColorStrings[i]));
    end;
    // build palette source
    SetLength(s,SrcLen);
    SrcPos:=1;
    for i:=0 to Palette.Count-1 do begin
      WriteToSrc('"');
      WriteToSrc(PixelStrings[i]);
      WriteToSrc(' c ');
      WriteToSrc(ColorStrings[i]);
      WriteToSrc('",');
      WriteToSrc(LineEnd);
    end;
    if SrcPos<>length(s)+1 then
      RaiseGDBException('TLazWriterXPM.InternalWrite consistency ERROR SrcPos<>length(s)');
    WriteString(s);
  end;

  procedure WritePixels;
  var
    s: string;
    SrcPos: Integer;

    procedure WriteToSrc(const AddString: string);
    var
      i: Integer;
    begin
      for i:=1 to length(AddString) do begin
        s[SrcPos]:=AddString[i];
        inc(SrcPos);
      end;
    end;

  var
    y: Integer;
    x: Integer;
    i: Integer;
    SrcLenPerLine: Integer;
    SrcLen: Integer;
  begin
    // calculate needed memory
    SrcLenPerLine:=length('"')+CharsPerPixel*Img.Width+length('",')+length(LineEnd);
    SrcLen:=Img.Height*SrcLenPerLine;
    // build palette source
    SetLength(s,SrcLen);
    SrcPos:=1;
    for y:=0 to Img.Height-1 do begin
      WriteToSrc('"');
      for x:=0 to Img.Width-1 do begin
        i:=Palette.IndexOf(GetColor(x,y));
        WriteToSrc(PixelStrings[i]);
      end;
      if y<Img.Height-1 then
        WriteToSrc('",'+LineEnd)
      else
        WriteToSrc('"}'+LineEnd);
    end;
    if SrcPos<>length(s)+1 then
      RaiseGDBException('TLazWriterXPM.InternalWrite consistency ERROR SrcPos<>length(s)');
    WriteString(s);
  end;

var
  i: Integer;
begin
  Palette:=nil;
  PixelStrings:=nil;
  ColorStrings:=nil;
  LineEnd:=#10;
  try
    BuildPalette;
    WriteHeader;
    WritePalette;
    WritePixels;
  finally
    if PixelStrings<>nil then begin
      for i:=0 to Palette.Count-1 do begin
        PixelStrings[i]:='';
        ColorStrings[i]:='';
      end;
      ReAllocMem(PixelStrings,0);
      ReAllocMem(ColorStrings,0);
    end;
    Palette.Free;
  end;
end;

constructor TLazWriterXPM.Create;
begin
  inherited Create;
  FNibblesPerSample:=2;
  FRightShiftSample:=8;
end;

{ TArrayNode }

constructor TArrayNode.Create;
begin
  //DebugLn('TArrayNode.Create ',Capacity,' Self=',DbgS(Self));
end;

destructor TArrayNode.Destroy;
begin
  DeleteChilds;
  UnbindFromParent;
  inherited Destroy;
end;

procedure TArrayNode.DeleteChilds;
var
  i: Integer;
begin
  if Childs<>nil then begin
    for i:=0 to Capacity-1 do
      Childs[i].Free;
    FreeMem(Childs);
    Childs:=nil;
    Capacity:=0;
  end;
end;

procedure TArrayNode.UnbindFromParent;
begin
  if Parent<>nil then begin
    Parent.Childs[Value-Parent.StartValue]:=nil;
    Parent:=nil;
  end;
end;

procedure TArrayNode.CreateChildNode(ChildValue: integer);
var
  NewNode: TArrayNode;
  Index: Integer;
begin
  NewNode:=TArrayNode.Create;
  NewNode.Value:=ChildValue;
  NewNode.Parent:=Self;
  Index:=ChildValue-StartValue;
  Childs[Index]:=NewNode;
end;

function TArrayNode.GetChildNode(ChildValue: integer; CreateIfNotExists: boolean
  ): TArrayNode;
var
  Index: Integer;
begin
  Result:=nil;
  Index:=ChildValue-StartValue;
  if (Index<0) or (Index>=Capacity) then begin
    // out of range
    if not CreateIfNotExists then exit;
    Expand(ChildValue);
    Index:=ChildValue-StartValue;
  end;
  Result:=Childs[Index];
  if (Result=nil) and CreateIfNotExists then begin
    CreateChildNode(ChildValue);
    Result:=Childs[Index];
  end;
end;

procedure TArrayNode.Expand(ValueToInclude: integer);
var
  Index: Integer;
  NewChilds: PArrayNode;
  NewSize: Integer;
  i: Integer;
  NewStartValue: Integer;
  NewCapacity: Integer;
  OldSize: Integer;
begin
  //DebugLn('TArrayNode.Expand A ',ValueToInclude,' Capacity=',Capacity,' StartValue=',StartValue);
  if Childs=nil then begin
    NewStartValue:=ValueToInclude;
    NewCapacity:=4;
  end else begin
    Index:=ValueToInclude-StartValue;
    if (Index>=0) and (Index<Capacity) then exit;
    NewStartValue:=StartValue;
    NewCapacity:=Capacity;
    if NewStartValue>ValueToInclude then begin
      inc(NewCapacity,NewStartValue-ValueToInclude);
      NewStartValue:=ValueToInclude;
    end else begin
      Index:=ValueToInclude-NewStartValue;
      if Index>=NewCapacity then
        NewCapacity:=Index+1;
    end;
    // make NewCapacity a power of 2
    for i:=1 to 30 do begin
      if (1 shl i)>=NewCapacity then begin
        NewCapacity:=1 shl i;
        break;
      end;
    end;
  end;
  NewSize:=SizeOf(Pointer)*NewCapacity;
  GetMem(NewChilds,NewSize);
  FillChar(NewChilds^,NewSize,0);
  if Childs<>nil then begin
    OldSize:=SizeOf(Pointer)*Capacity;
    System.Move(Childs^,NewChilds[StartValue-NewStartValue],OldSize);
    FreeMem(Childs);
  end;
  Childs:=NewChilds;
  StartValue:=NewStartValue;
  Capacity:=NewCapacity;
end;

function TArrayNode.FindPrevSibling: TArrayNode;
var
  i: Integer;
begin
  Result:=nil;
  if Parent=nil then exit;
  i:=Value-Parent.StartValue-1;
  while (i>=0) do begin
    if Parent.Childs[i]<>nil then begin
      Result:=Parent.Childs[i];
      exit;
    end;
    dec(i);
  end;
end;

function TArrayNode.FindNextSibling: TArrayNode;
var
  i: Integer;
begin
  Result:=nil;
  if Parent=nil then exit;
  i:=Value-Parent.StartValue+1;
  while (i<Parent.Capacity) do begin
    if Parent.Childs[i]<>nil then begin
      Result:=Parent.Childs[i];
      exit;
    end;
    inc(i);
  end;
end;

function TArrayNode.FindNext: TArrayNode;
var
  SiblingNode: TArrayNode;
begin
  Result:=FindFirstChild;
  if Result<>nil then exit;
  SiblingNode:=Self;
  while SiblingNode<>nil do begin
    Result:=SiblingNode.FindNextSibling;
    if Result<>nil then exit;
    SiblingNode:=SiblingNode.Parent;
  end;
end;

function TArrayNode.FindPrev: TArrayNode;
begin
  Result:=FindPrevSibling;
  if Result=nil then begin
    Result:=Parent;
    exit;
  end;
  Result:=Result.FindLastSubChild;
end;

function TArrayNode.FindFirstChild: TArrayNode;
var
  i: Integer;
begin
  Result:=nil;
  if Capacity=0 then exit;
  i:=0;
  while i<Capacity do begin
    if Childs[i]<>nil then begin
      Result:=Childs[i];
      exit;
    end;
    inc(i);
  end;
end;

function TArrayNode.FindLastChild: TArrayNode;
var
  i: Integer;
begin
  Result:=nil;
  if Capacity=0 then exit;
  i:=Capacity-1;
  while i>=0 do begin
    if Childs[i]<>nil then begin
      Result:=Childs[i];
      exit;
    end;
    dec(i);
  end;
end;

function TArrayNode.FindLastSubChild: TArrayNode;
var
  ANode: TArrayNode;
begin
  ANode:=Self;
  while ANode<>nil do begin
    Result:=ANode;
    ANode:=ANode.FindLastChild;
  end;
end;

function TArrayNode.FindFirstSibling: TArrayNode;
begin
  if Parent=nil then
    Result:=nil
  else
    Result:=Parent.FindFirstChild;
end;

function TArrayNode.FindLastSibling: TArrayNode;
begin
  if Parent=nil then
    Result:=nil
  else
    Result:=Parent.FindLastChild;
end;

procedure TArrayNode.ConsistencyCheck;

  procedure R(const Msg: string);
  begin
    RaiseGDBException(Msg);
  end;

var
  i: Integer;
  ChildNode: TArrayNode;
begin
  if Childs<>nil then begin
    if Capacity<=0 then R('Capacity too small');
    for i:=0 to Capacity-1 do begin
      ChildNode:=Childs[i];
      if ChildNode<>nil then begin
        if ChildNode.Value<>i+StartValue then
          R('Value wrong');
        if ChildNode.Parent<>Self then
          R('Parent wrong');
        ChildNode.ConsistencyCheck;
      end;
    end;
  end else begin
    if Capacity<>0 then R('Capacity wrong');
  end;
end;

{ TArrayNodesTree }

function TArrayNodesTree.FindNode(Path: PInteger; Count: integer
  ): TArrayNode;
var
  i: Integer;
begin
  Result:=Root;
  i:=0;
  while (Result<>nil) and (i<Count) do begin
    Result:=Result.GetChildNode(Path[i],false);
    inc(i);
  end;
end;

function TArrayNodesTree.FindData(Path: PInteger; Count: integer): Pointer;
var
  ANode: TArrayNode;
begin
  ANode:=FindNode(Path,Count);
  if ANode<>nil then
    Result:=ANode.Data
  else
    Result:=nil;
end;

function TArrayNodesTree.SetNode(Path: PInteger; Count: integer;
  Data: Pointer): TArrayNode;
var
  i: Integer;
begin
  if Root=nil then
    Root:=TArrayNode.Create;
  Result:=Root;
  for i:=0 to Count-1 do begin
    //DebugLn('TArrayNodesTree.SetNode A ',DbgS(Result));
    Result:=Result.GetChildNode(Path[i],true);
  end;
  Result.Data:=Data;
end;

procedure TArrayNodesTree.Delete(Node: TArrayNode);
begin
  if Node=nil then exit;
  if Node=Root then Root:=nil;
  Node.Free;
end;

procedure TArrayNodesTree.Clear;
begin
  Delete(Root);
end;

constructor TArrayNodesTree.Create;
begin

end;

destructor TArrayNodesTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TArrayNodesTree.ConsistencyCheck;
begin
  if Root<>nil then
    Root.ConsistencyCheck;
end;

{ TLazReaderBMP }

function TLazReaderBMP.BmpRGBAToFPColor(Const RGBA: TColorRGBA): TFPcolor;
begin
  Result.Red   := (RGBA.R shl 8) or RGBA.R;
  Result.Green := (RGBA.G shl 8) or RGBA.G;
  Result.Blue  := (RGBA.B shl 8) or RGBA.B;
    // Specification for bitmap files has these bits always zero - unused
  Result.Alpha := AlphaOpaque;
end;

Function BmpRGBToFPColor(Const RGB: TColorRGB) : TFPColor;
begin
  Result.Red   := (RGB.R shl 8) + RGB.R;
  Result.Green := (RGB.G shl 8) + RGB.G;
  Result.Blue  := (RGB.B shl 8) + RGB.B;
  Result.Alpha := AlphaOpaque;
end;

Function Bmp16BitToFPColor(Const RGB: Word): TFPColor;
var
  V1, V2: Cardinal;
begin
{
  // 5 bit for red  -> 16 bit for TFPColor
  Result.Red:=(RGB shr 11) and $1f;
  Result.Red:=(Result.Red shl 11) or MissingBits[5,Result.Red shr 2];
  // 6 bit for green -> 16 bit for TFPColor
  Result.Green:=(RGB shr 5) and $3f;
  Result.Green:=(Result.Green shl 10) or MissingBits[6,Result.Green shr 3];
  // 5 bit for blue -> 16 bit for TFPColor
  Result.Blue:=RGB and $1f;
  Result.Blue  := (Result.Blue shl 11) or MissingBits[5, Result.Blue shr 2];
}
  // 5 bit for red  -> 16 bit for TFPColor
  V1 := RGB and $F800;             // 15..11
  V2 := V1;
  V1 := V1 shr 5;                  // 10..6
  V2 := V2 or V1;
  V1 := V1 shr 5;                  // 5..1
  V2 := V2 or V1;
  V1 := V1 shr 5;                  // 0
  Result.Red := Word(V2 or V1);
  // 6 bit for green -> 16 bit for TFPColor
  V1 := (RGB shl 5) and $FC00;     // 15..10
  V2 := V1;
  V1 := V1 shr 6;                  // 9..4
  V2 := V2 or V1;
  V1 := V1 shr 6;                  // 4..0
  Result.Green := Word(V2 or V1);
  // 5 bit for blue -> 16 bit for TFPColor
  V1 := (RGB shl 11) and $F800;    // 15..11
  V2 := V1;
  V1 := V1 shr 5;
  V2 := V2 or V1;                  // 10..6
  V1 := V1 shr 5;
  V2 := V2 or V1;                  // 5..1
  V1 := V1 shr 5;
  Result.Blue := Word(V2 or V1);   // 0
  // opaque, no mask
  Result.Alpha := AlphaOpaque;
end;

function Bmp15BitToFPColor(const RGB: Word): TFPColor;
var
  V1, V2: Cardinal;
begin
  // 5 bit for red  -> 16 bit for TFPColor
  V1 := (RGB shl 1) and $F800;     // 15..11
  V2 := V1;
  V1 := V1 shr 5;                  // 10..6
  V2 := V2 or V1;
  V1 := V1 shr 5;                  // 5..1
  V2 := V2 or V1;
  V1 := V1 shr 5;                  // 0
  Result.Red := Word(V2 or V1);
  // 5 bit for red  -> 16 bit for TFPColor
  V1 := (RGB shl 6) and $F800;     // 15..11
  V2 := V1;
  V1 := V1 shr 5;                  // 10..6
  V2 := V2 or V1;
  V1 := V1 shr 5;                  // 5..1
  V2 := V2 or V1;
  V1 := V1 shr 5;                  // 0
  Result.Green := Word(V2 or V1);
  // 5 bit for blue -> 16 bit for TFPColor
  V1 := (RGB shl 11) and $F800;    // 15..11
  V2 := V1;
  V1 := V1 shr 5;
  V2 := V2 or V1;                  // 10..6
  V1 := V1 shr 5;
  V2 := V2 or V1;                  // 5..1
  V1 := V1 shr 5;
  Result.Blue := Word(V2 or V1);   // 0
  // opaque, no mask
  Result.Alpha:=alphaOpaque;
end;

procedure TLazReaderBMP.FreeBufs;
begin
  If (LineBuf<>Nil) then
    begin
    FreeMem(LineBuf);
    LineBuf:=Nil;
    end;
  If (FPalette<>Nil) then
    begin
    FreeMem(FPalette);
    FPalette:=Nil;
    end;
end;

function TLazReaderBMP.ColorToTrans(const InColor: TFPColor): TFPColor;
begin
  //DebugLn('TLazReaderBMP.ColorToTrans InColor=',dbgs(InColor),' FTransparentColor=',dbgs(FTransparentColor));
  Result := InColor;
  if (InColor = FTransparentColor) then
    Result.alpha := alphaTransparent;
end;

procedure TLazReaderBMP.SetupRead(nPalette, nRowBits: Integer; Stream: TStream;
  ReadPalette: Boolean);
var
  ColInfo: ARRAY OF TColorRGBA;
  i: Integer;
  FPcolor: TFPcolor;
begin
  if nPalette > 0
  then begin
    GetMem(FPalette, nPalette*SizeOf(TFPColor));
    SetLength(ColInfo, nPalette);
    if ReadPalette then begin
      if  (BFI.biClrUsed > 0)
      and (BFI.biClrUsed <= Cardinal(nPalette)) // prevent buffer overflow
      then Stream.Read(ColInfo[0], BFI.biClrUsed * SizeOf(ColInfo[0]))
      else Stream.Read(ColInfo[0], nPalette * SizeOf(ColInfo[0]));
      for i := 0 to nPalette-1 do
      begin
        FPcolor := BmpRGBAToFPColor(ColInfo[i]);
        FPcolor.alpha := alphaOpaque; { No transparency info in palette }
        FPalette[i] := FPcolor;
      end;
    end;
  end
  else begin
    { Skip palette }
    if BFI.biClrUsed > 0
    then Stream.Position := Stream.Position
                          + TStreamSeekType(BFI.biClrUsed*SizeOf(TColorRGBA));
  end;
  ReadSize:=((nRowBits + 31) div 32) shl 2;
  GetMem(LineBuf,ReadSize);
end;

procedure TLazReaderBMP.ReadScanLine(Row: Integer; Stream: TStream);
{$IFDEF FPC_BIG_ENDIAN}
var
  n: Integer;
{$ENDIF}
begin
  {
    Add here support for compressed lines. The 'readsize' is the same in the end.
  }
  Stream.Read(LineBuf[0],ReadSize);
  {$IFDEF FPC_BIG_ENDIAN}
  // MWE: don't know if linebuf is used externally
  // if it is only used internally, then the conversion can better
  // be done in writescanline for Bmp15ToFPColor and Bmp16ToFPColor
  
  if (FBitsPerPixel = 15)
  or (FBitsPerPixel = 16)
  then begin
    for n := 0 to (ReadSize div 2) - 1 do
      PWord(LineBuf)[n] := LEtoN(PWord(LineBuf)[n]);
  end;

  {$ENDIF}
  
end;

procedure TLazReaderBMP.WriteScanLine(Row: Integer; Img: TFPCustomImage);
Var
  Column : Integer;
begin
  case FBitsPerPixel of
   1 :
     for Column:=0 to Img.Width-1 do
       if ((LineBuf[Column div 8] shr (7-(Column and 7)) ) and 1) <> 0 then
         img.colors[Column,Row]:=ColorToTrans(FPalette[1])
       else
         img.colors[Column,Row]:=ColorToTrans(FPalette[0]);
   4 :
      for Column:=0 to img.Width-1 do
        img.colors[Column,Row]:=ColorToTrans(FPalette[(LineBuf[Column div 2] shr (((not Column) and 1)*4)) and $0f]);
   8 :
      for Column:=0 to img.Width-1 do
        img.colors[Column,Row]:=ColorToTrans(FPalette[LineBuf[Column]]);
   15:
      for Column := 0 to img.Width - 1 do
        Img.colors[Column,Row]:=ColorToTrans(Bmp15BitToFPColor(PWord(LineBuf)[Column]));
   16 :
      for Column:=0 to img.Width-1 do
        img.colors[Column,Row]:=ColorToTrans(Bmp16BitToFPColor(PWord(LineBuf)[Column]));
   24 :
      for Column:=0 to img.Width-1 do
        img.colors[Column,Row]:=ColorToTrans(BmpRGBToFPColor(PColorRGB(LineBuf)[Column]));
   32 :  // BmpRGBA already does transparency
      for Column:=0 to img.Width-1 do
        img.colors[Column,Row]:=BmpRGBAToFPColor(PColorRGBA(LineBuf)[Column]);
  end;
end;

procedure TLazReaderBMP.InternalRead(Stream: TStream; Img: TFPCustomImage);
begin
  InternalReadHead(Stream, Img);
  InternalReadBody(Stream, Img);
end;

procedure TLazReaderBMP.InternalReadHead(Stream: TStream; Img: TFPCustomImage);
begin
  Stream.Read(BFI,SizeOf(BFI));
  {$IFDEF FPC_BIG_ENDIAN}
  BFI.biSize          := LEtoN(BFI.biSize         );
  BFI.biWidth         := LEtoN(BFI.biWidth        );
  BFI.biHeight        := LEtoN(BFI.biHeight       );
  BFI.biPlanes        := LEtoN(BFI.biPlanes       );
  BFI.biBitCount      := LEtoN(BFI.biBitCount     );
  BFI.biCompression   := LEtoN(BFI.biCompression  );
  BFI.biSizeImage     := LEtoN(BFI.biSizeImage    );
  BFI.biXPelsPerMeter := LEtoN(BFI.biXPelsPerMeter);
  BFI.biYPelsPerMeter := LEtoN(BFI.biYPelsPerMeter);
  BFI.biClrUsed       := LEtoN(BFI.biClrUsed      );
  BFI.biClrImportant  := LEtoN(BFI.biClrImportant );
  {$ENDIF}
end;

procedure TLazReaderBMP.InternalReadBody(Stream: TStream; Img: TFPCustomImage);
type
  TPixelMasks = packed record
    R, G, B: LongWord;
  end;
const
  SWrongCombination = 'Bitmap with wrong combination of bit count (%d) and compression (%d)';
Var
  PixelMasks: TPixelMasks;
  Row : Integer;
  firstLine: boolean;

  procedure SaveTransparentColor;
  begin
    //DebugLn('SaveTransparentColor ',dbgs(UseLeftBottomAsTransparent),' ',dbgs(FBitsPerPixel));
    if UseLeftBottomAsTransparent then begin
      // define transparent color: 1-8 use palette, 15-24 use fixed color
      case FBitsPerPixel of
       1 : FPalette[(LineBuf[0] shr 7) and 1] := fpimage.colTransparent;
       4 : FPalette[(LineBuf[0] shr 4) and $f] := fpimage.colTransparent;
       8 : FPalette[LineBuf[0]] := fpimage.colTransparent;
       15: FTransparentColor := Bmp15BitToFPColor(PWord(LineBuf)[0]);
       16: FTransparentColor := Bmp16BitToFPColor(PWord(LineBuf)[0]);
       24: FTransparentColor := BmpRGBToFPColor(PColorRGB(LineBuf)[0]);
       32: ; // BmpRGBA already does transparency
      end;
    end;
  end;
  
begin
  { This will move past any junk after the BFI header }
  Stream.Position:=Stream.Position+TStreamSeekType(BFI.biSize-SizeOf(BFI));
  Img.Width := BFI.biWidth;
  Img.Height := BFI.biHeight;
  FBitsPerPixel := BFI.biBitCount;
  Case BFI.biBitCount of
    1: begin  { Monochrome }
      if BFI.biCompression <> BI_RGB then
        raise FPImageException.CreateFmt(SWrongCombination, [BFI.biBitCount, BFI.biCompression]);
      SetupRead(2,Img.Width,Stream,true);
    end;
    4: begin
      case BFI.biCompression of
        BI_RGB:
      SetupRead(16,Img.Width*4,Stream,true);
        BI_RLE4:
          raise FPImageException.Create('4 bit RLE Bitmaps not supported');
      else
        raise FPImageException.CreateFmt(SWrongCombination, [BFI.biBitCount, BFI.biCompression]);
      end;
    end;
    8: begin
      case BFI.biCompression of
        BI_RGB:
      SetupRead(256,Img.Width*8,Stream,true);
        BI_RLE8:
          raise FPImageException.Create('8 bit RLE Bitmaps not supported');
      else
        raise FPImageException.CreateFmt(SWrongCombination, [BFI.biBitCount, BFI.biCompression]);
      end;
    end;
    16: begin
      case BFI.biCompression of
        BI_RGB:                                          // 5-5-5
          FBitsPerPixel := 15;
        BI_BITFIELDS: begin                              // 5-5-5 or 5-6-5
          Stream.Read(PixelMasks, SizeOf(PixelMasks));
          {$IFDEF FPC_BIG_ENDIAN}
            PixelMasks.R := LEtoN(PixelMasks.R);
            PixelMasks.G := LEtoN(PixelMasks.G);
            PixelMasks.B := LEtoN(PixelMasks.B);
          {$ENDIF}
          if (PixelMasks.R = $7C00) and     // 5 red
             (PixelMasks.G = $03E0) and     // 5 green
             (PixelMasks.B = $001F) then    // 5 blue
            FBitsPerPixel := 15
          else
          if (PixelMasks.R = $F800) and     // 5 red
             (PixelMasks.G = $07E0) and     // 6 green
             (PixelMasks.B = $001F) then    // 5 blue
            FBitsPerPixel := 16
          else
            raise FPImageException.Create('Bitmap with non-standard pixel masks not supported');
        end;
      else
        raise FPImageException.CreateFmt(SWrongCombination, [BFI.biBitCount, BFI.biCompression]);
      end;
      SetupRead(0, Img.Width * 16, Stream, True);
    end;
    24: begin
      case BFI.biCompression of
        BI_RGB: ;
        BI_BITFIELDS: begin  // actually not a valid value
          Stream.Read(PixelMasks, SizeOf(PixelMasks));
          {$IFDEF FPC_BIG_ENDIAN}
            PixelMasks.R := LEtoN(PixelMasks.R);
            PixelMasks.G := LEtoN(PixelMasks.G);
            PixelMasks.B := LEtoN(PixelMasks.B);
          {$ENDIF}
          if (PixelMasks.R <> $FF0000) or     // 8 red
             (PixelMasks.G <> $00FF00) or     // 8 green
             (PixelMasks.B <> $0000FF) then   // 8 blue
            raise FPImageException.Create('Bitmap with non-standard pixel masks not supported');
        end;
      else
        raise FPImageException.CreateFmt(SWrongCombination, [BFI.biBitCount, BFI.biCompression]);
      end;
      SetupRead(0, Img.Width * 24, Stream, True);
    end;
    32: begin
      case BFI.biCompression of
        BI_RGB: ;
        BI_BITFIELDS: begin
          Stream.Read(PixelMasks, SizeOf(PixelMasks));
          {$IFDEF FPC_BIG_ENDIAN}
            PixelMasks.R := LEtoN(PixelMasks.R);
            PixelMasks.G := LEtoN(PixelMasks.G);
            PixelMasks.B := LEtoN(PixelMasks.B);
          {$ENDIF}
          if (PixelMasks.R <> $00FF0000) or     // 8 red
             (PixelMasks.G <> $0000FF00) or     // 8 green
             (PixelMasks.B <> $000000FF) then   // 8 blue
            raise FPImageException.Create('Bitmap with non-standard pixel masks not supported');
        end;
      else
        raise FPImageException.CreateFmt(SWrongCombination, [BFI.biBitCount, BFI.biCompression]);
      end;
      SetupRead(0, Img.Width * 32, Stream, True);
    end;
  else
    raise FPImageException.CreateFmt('Wrong bitmap bit count: %d', [BFI.biBitCount]);
  end;
  Try
    firstLine := true;
    for Row := Img.Height - 1 downto 0 do begin
      ReadScanLine(Row,Stream); // Scanline in LineBuf with Size ReadSize.
      if firstLine then
      begin
        SaveTransparentColor;
        firstLine := false;
      end;
      WriteScanLine(Row,Img);
      end;
  finally
    FreeBufs;
  end;
end;

function TLazReaderBMP.InternalCheck(Stream: TStream): boolean;
var
  BFH:TBitMapFileHeader;
begin
  stream.Read(BFH,SizeOf(BFH));
  With BFH do
    Result:=(LEtoN(bfType)=BMmagic); // Just check magic number
end;

constructor TLazReaderBMP.Create;
begin
  inherited Create;
  FTransparentColor:=colTransparent;
end;

destructor TLazReaderBMP.Destroy;
begin
  FreeBufs;
  inherited Destroy;
end;

{ TLazIntfImageMask }

procedure TLazIntfImageMask.SetInternalColor(x, y: integer;
  const Value: TFPColor);
var
  MaskValue: TFPColor;
begin
  MaskValue:=FImage.GetInternalColor(x,y);
  MaskValue.Alpha:=$FFFF-Value.Red;
  FImage.SetInternalColor(x, y, MaskValue);
end;

function TLazIntfImageMask.GetInternalColor(x, y: integer): TFPColor;
var
  MaskValue: TFPColor;
begin
  MaskValue:=FImage.GetInternalColor(x,y);
  Result:=FPImage.colBlack;
  Result.Red:=MaskValue.Alpha;
  Result.Green:=MaskValue.Alpha;
  Result.Blue:=MaskValue.Alpha;
end;

procedure TLazIntfImageMask.SetInternalPixel(x, y: integer; Value: integer);
begin
  if UsePalette then
    SetInternalColor(x,y,Palette.Color[Value])
end;

function TLazIntfImageMask.GetInternalPixel(x, y: integer): integer;
begin
  if UsePalette then
    Result := Palette.IndexOf(GetInternalColor(x,y))
  else
    Result:=0;
end;

constructor TLazIntfImageMask.CreateWithImage(TheImage: TLazIntfImage);
begin
  FImage:=TheImage;
  inherited Create(FImage.Width,FImage.Height);
end;

{ TLazReaderPartIcon }

procedure TLazReaderPartIcon.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  Row, Column: Integer;
  NewColor: TFPColor;
begin
  InternalReadHead(Stream, Img);

  BFI.biHeight := BFI.biHeight div 2; { Height field is doubled, to (sort of) accomodate mask }
  InternalReadBody(Stream, Img); { Now read standard bitmap }

  { Mask immediately follows unless bitmap was 32 bit - monchrome bitmap with no header }
  // MWE: is the height then stil devided by 2 ?
  if BFI.biBitCount < 32 then
  begin
    ReadSize:=((Img.Width + 31) div 32) shl 2;
    SetupRead(2,Img.Width,Stream,False);
    try
      for Row:=Img.Height-1 downto 0 do
      begin
        ReadScanLine(Row,Stream); // Scanline in LineBuf with Size ReadSize.
        {
          ---------------- delete this comment, or apply ------------------------
          Paul Ishenin: My suggestion is to skip this color setting at all
          and replace it with direct writing of LineBuf into FMaskData. This will
          significantly speed up mask setting.
          
          e.g. I test this code and it works fine:

          if (Img is TLazIntfImage) and (TLazIntfImage(Img).FMaskData <> nil) then
            for i := 0 to ReadSize - 1 do
              TLazIntfImage(Img).FMaskData[(Row * ReadSize) + i] := LineBuf[i];

          ---------------- now it works so: --------------------------------------       
          For cursors: we should not change main bitmap colors, but we should
          set mask. If we get 1 in LineBuf bit, then we need set 1 into Mask.
          If alpha part of color is alphaOpaque ($FFFF) then we set 1 into Mask
          else if alpha is alphaTransparent ($0000) then we set 0 into Mask
          so it is just "bit by bit copying" from LineBuf into FMaskData
          if we need speed up this copying then read my comment before
        }
        
        for Column:=0 to Img.Width-1 do
          if ((LineBuf[Column div 8] shr (7-(Column and 7)) ) and 1) <> 0 then
          begin
            // I dont want change something in Icon loading code, so I add conditions
            // ClassType = TLazReaderCursor when need
            if ClassType = TLazReaderCursor then
            begin
              NewColor := img.colors[Column,Row];
              NewColor.alpha := alphaOpaque;
            end else
              NewColor := colTransparent;
            img.colors[Column,Row] := NewColor;
          end else
          if ClassType = TLazReaderCursor then
          begin
            NewColor := img.colors[Column,Row];
            NewColor.alpha := alphaTransparent;
            img.colors[Column,Row] := NewColor;
          end;
      end;
    finally
      FreeBufs;
    end;
  end;
end;

function TLazReaderPartIcon.InternalCheck(Stream: TStream): boolean;
//var bfh: Array[0..21] of byte;
begin
  //Stream.Read(bfh,22); // dummy read of ico file header
  Result:=True; { Assumes stream in the correct place }
end;

function TLazReaderPartIcon.BmpRGBAToFPColor(const RGBA: TColorRGBA): TFPcolor;
begin
  Result:=inherited BmpRGBAToFPColor(RGBA);
  Result.alpha := (RGBA.A shl 8) or RGBA.A; { For icon files (only) upper byte is used for transparency }
end;

{ TLazReaderIcon }

type
  TIconHeader = packed record
    idReserved: Word; {0}
    idType: Word;     {1 - Icon, 2 - Cursor}
    idCount: Word;    {number of icons in file}
  end;

  TIconDirEntry = packed record
    bWidth: Byte;          {ie: 16 or 32}
    bHeight: Byte;         {ie: 16 or 32}
    bColorCount: Byte;     {number of entires in pallette table below}
    bReserved: Byte;       { not used  = 0}
    wXHotSpot: Word;       { used for Cursor otherwise = 0}
    wYHotSpot: Word;       { used for Cursor otherwise = 0}
    dwBytesInRes: Longint;  {total number bytes in images including pallette data
                             XOR, AND     and bitmap info header}
    dwImageOffset: Longint;  {pos of image as offset from the beginning of file}
  end;

  PIconDirEntry = ^TIconDirEntry;
  
procedure TLazReaderIcon.SetIcon(const AValue: TObject);
begin
  if AValue is TIcon then
    FIcon:=AValue;
end;

procedure TLazReaderIcon.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  CurrentDirEntry, BestDirEntry, IconDir: PIconDirEntry;
  i: Integer;
  Bitmap: TBitmap;
begin
  GetMem(IconDir, FnIcons*Sizeof(TIconDirEntry));
  try
    Stream.Read(IconDir^, FnIcons*Sizeof(TIconDirEntry));
    BestDirEntry := IconDir;
    CurrentDirEntry := IconDir+1;
    IconDir^.dwBytesInRes := LEtoN(IconDir^.dwBytesInRes);
    IconDir^.dwImageOffset := LEtoN(IconDir^.dwImageOffset);
    { First locate largest and/or most colourful icon as the default image }
    for i := 2 to FnIcons do begin
      CurrentDirEntry^.dwBytesInRes := LEtoN(CurrentDirEntry^.dwBytesInRes);
      CurrentDirEntry^.dwImageOffset := LEtoN(CurrentDirEntry^.dwImageOffset);
      if ((CurrentDirEntry^.bWidth > BestDirEntry^.bWidth)
           and (CurrentDirEntry^.bHeight > BestDirEntry^.bHeight))
         or ((CurrentDirEntry^.bWidth = BestDirEntry^.bWidth)
             and (CurrentDirEntry^.bHeight = BestDirEntry^.bHeight)
             and (CurrentDirEntry^.dwBytesInRes > BestDirEntry^.dwBytesInRes)) then
        BestDirEntry := CurrentDirEntry;
      Inc(CurrentDirEntry);
    end;
    if Assigned(Icon) then
    begin
      CurrentDirEntry := IconDir;
      if Icon is TCursorImage then
        TCursorImage(Icon).HotSpot := Point(IconDir^.wXHotSpot, IconDir^.wYHotSpot);
      for i := 1 to FnIcons do
      begin
        Stream.Position := FnStartPos + CurrentDirEntry^.dwImageOffset;
        if CurrentDirEntry = BestDirEntry then
         inherited InternalRead(Stream, Img)
        else
        begin
          Bitmap := TBitmap.Create;
          try
            Bitmap.ReadStreamWithFPImage(Stream, False, 0, TLazReaderPartIcon);
          except
            Bitmap.Free;
            raise;
          end;
          TIcon(Icon).AddBitmap(Bitmap);
        end;
        Inc(CurrentDirEntry);
      end;
    end else
    begin
      Stream.Position := FnStartPos + BestDirEntry^.dwImageOffset;
      inherited InternalRead(Stream, Img);
      { Finally skip remaining icons }
      Stream.Position := FnStartPos + CurrentDirEntry^.dwImageOffset + CurrentDirEntry^.dwBytesInRes;
    end;
  finally
    FreeMem(IconDir);
  end;
end;

function TLazReaderIcon.InternalCheck(Stream: TStream): boolean;
var
  IconHeader: TIconHeader;
begin
  FnStartPos := Stream.Position;
  Stream.Read(IconHeader,SizeOf(IconHeader));
  With IconHeader do
    Result := (idReserved=0) and (LEtoN(idType)=1);
  FnIcons := LEtoN(IconHeader.idCount);
end;

{ TLazReaderCursor }
function TLazReaderCursor.InternalCheck(Stream: TStream): boolean;
var
  IconHeader: TIconHeader;
begin
  FnStartPos := Stream.Position;
  Stream.Read(IconHeader,SizeOf(IconHeader));
  With IconHeader do
    Result := (idReserved=0) and (LEtoN(idType)=2);
  FnIcons := LEtoN(IconHeader.idCount);
end;

//------------------------------------------------------------------------------
procedure InternalInit;
var
  c: Char;
begin
  for c:=Low(char) to High(char) do begin
    IsSpaceChar[c]:=c in [' ',#9,#10,#13];
    IsNumberChar[c]:=c in ['0'..'9'];
    IsHexNumberChar[c]:=c in ['0'..'9','A'..'F','a'..'f'];
  end;
end;

initialization
  InternalInit;

end.
