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
  LCLProc, GraphType, LCLIntf, FPReadPNG, IcnsTypes;

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

  TLazIntfImageGetPixelProc = procedure(x, y: integer; out Color: TFPColor) of object;
  TLazIntfImageSetPixelProc = procedure(x, y: integer; const Color: TFPColor) of object;

  TOnReadRawImageBits = procedure(TheData: PByte;
    const Position: TRawImagePosition;
    Prec, Shift: cardinal; var Bits: word);

  TOnWriteRawImageBits = procedure(TheData: PByte;
    const Position: TRawImagePosition;
    Prec, Shift: cardinal; Bits: word);


  { TLazIntfImage }

  TLazIntfImage = class(TFPCustomImage)
  private
    FRawImage: TRawImage;
    FLineStarts: PRawImageLineStarts;
    FMaskLineStarts: PRawImageLineStarts;
    FMaskSet: Boolean; // Set when atleast one maskpixel is set
    FUpdateCount: integer;
    fCreateAllDataNeeded: boolean;
    FGetSetColorFunctionsUpdateNeeded: boolean;
    FReadRawImageBits: TOnReadRawImageBits;
    FWriteRawImageBits: TOnWriteRawImageBits;
    FMaskReadRawImageBits: TOnReadRawImageBits;
    FMaskWriteRawImageBits: TOnWriteRawImageBits;
    FDataOwner: Boolean;
    {$ifdef VER2_0}
    // workaround for IE200310221 on 2.0.4
    function GetDataDescription: TRawImageDescription;
    function GetMaskData: PByte;
    function GetPixelData: PByte;
    {$endif}
    function GetMasked(x, y: integer): Boolean;
    function GetTColors(x, y: integer): TGraphicsColor;

    procedure InternalSetSize(AWidth, AHeight: integer);

    procedure SetMasked(x, y: integer; const AValue: Boolean);
    procedure SetTColors(x, y: integer; const AValue: TGraphicsColor);
  protected
    FGetInternalColorProc: TLazIntfImageGetPixelProc;
    FSetInternalColorProc: TLazIntfImageSetPixelProc;
    procedure SetUsePalette (Value: boolean); override;
    procedure SetInternalColor(x, y: integer; const Value: TFPColor); override;
    function  GetInternalColor(x, y: integer): TFPColor; override;
    procedure SetInternalPixel (x,y:integer; Value:integer); override;
    function  GetInternalPixel (x,y:integer) : integer; override;
    procedure FreeData; virtual;
    procedure SetDataDescription(const ADescription: TRawImageDescription); virtual;
    procedure ChooseGetSetColorFunctions; virtual;
    procedure ChooseRawBitsProc(BitsPerPixel: cardinal;
                                ByteOrder: TRawImageByteOrder;
                                BitOrder: TRawImageBitOrder;
                                out ProcReadRawImageBits: TOnReadRawImageBits;
                                out ProcWriteRawImageBits: TOnWriteRawImageBits);
    // get color functions
    procedure GetColor_Generic(x, y: integer; out Value: TFPColor);
    procedure GetColor_RGBA_NoPalette(x, y: integer; out Value: TFPColor);
    procedure GetColor_RGB_NoPalette(x, y: integer; out Value: TFPColor);
    procedure GetColor_Gray_NoPalette(x, y: integer; out Value: TFPColor);
    procedure GetColor_NULL(x, y: integer; out Value: TFPColor);
    // 32 bpp alpha
    procedure GetColor_BPP32_A8R8G8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_A8B8G8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_A8G8R8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_A8G8B8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_A8R8B8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_A8B8R8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_B8G8R8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_R8G8B8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_G8B8R8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_G8R8B8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_B8R8G8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_R8B8G8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
    // 32 bpp no alpha
    procedure GetColor_BPP32_X8R8G8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_X8B8G8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_X8G8R8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_X8G8B8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_X8R8B8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_X8B8R8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_B8G8R8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_R8G8B8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_G8B8R8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_G8R8B8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_B8R8G8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_R8B8G8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
    // 24 bpp
    procedure GetColor_BPP24_B8G8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP24_R8G8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP24_G8B8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP24_G8R8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP24_B8R8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP24_R8B8G8_BIO_TTB(x, y: integer; out Value: TFPColor);

    procedure GetMask_Generic(x, y: integer; out AValue: Boolean);

    // set color functions
    procedure SetColor_Generic(x, y: integer; const Value: TFPColor);
    procedure SetColor_RGBA_NoPalette(x, y: integer; const Value: TFPColor);
    procedure SetColor_RGB_NoPalette(x, y: integer; const Value: TFPColor);
    procedure SetColor_Gray_NoPalette(x, y: integer; const Value: TFPColor);
    procedure SetColor_NULL(x, y: integer; const Value: TFPColor);
    // 32 bpp alpha
    procedure SetColor_BPP32_A8R8G8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_A8B8G8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_A8G8R8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_A8G8B8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_A8R8B8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_A8B8R8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_B8G8R8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_R8G8B8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_G8B8R8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_G8R8B8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_B8R8G8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_R8B8G8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
    // 32 bpp no alpha
    procedure SetColor_BPP32_X8R8G8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_X8B8G8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_X8G8R8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_X8G8B8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_X8R8B8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_X8B8R8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_B8G8R8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_R8G8B8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_G8B8R8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_G8R8B8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_B8R8G8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_R8B8G8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
    // 24 bpp
    procedure SetColor_BPP24_B8G8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP24_R8G8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP24_G8B8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP24_G8R8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP24_B8R8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP24_R8B8G8_BIO_TTB(x, y: integer; const Value: TFPColor);

    procedure SetMask_Generic(x, y: integer; const AValue: Boolean);
  public
    constructor Create(AWidth, AHeight: integer); override;
    constructor Create(ARawImage: TRawImage; ADataOwner: Boolean);
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SetSize(AWidth, AHeight: integer); override;
    function CheckDescription(const ADescription: TRawImageDescription;
                              ExceptionOnError: boolean): boolean; virtual;
    procedure LoadFromDevice(DC: HDC); virtual;
    procedure LoadFromBitmap(ABitmap, AMaskBitmap: HBitmap; AWidth: integer = -1; AHeight: integer = -1); virtual;
    procedure CreateBitmaps(out ABitmap, AMask: HBitmap; ASkipMask: boolean = False); virtual;
    procedure SetRawImage(const ARawImage: TRawImage; ADataOwner: Boolean = True); virtual;
    procedure GetRawImage(out ARawImage: TRawImage; ATransferOwnership: Boolean = False); virtual;
    procedure FillPixels(const Color: TFPColor); virtual;
    procedure CopyPixels(ASource: TFPCustomImage; XDst: Integer = 0; YDst: Integer = 0); virtual;
    procedure AlphaFromMask(AKeepAlpha: Boolean = True);
    procedure GetXYDataPostion(x, y: integer; out Position: TRawImagePosition);
    procedure GetXYMaskPostion(x, y: integer; out Position: TRawImagePosition);
    function  GetDataLineStart(y: integer): Pointer;// similar to Delphi TBitmap.ScanLine. Only works with byte aligned lines.
    procedure CreateData; virtual;
    function  HasTransparency: boolean; virtual;
    function  HasMask: boolean; virtual;
  public
    {$ifdef VER2_0}
    // workaround for IE200310221 on 2.0.4
    property PixelData: PByte read GetPixelData;
    property MaskData: PByte read GetMaskData;
    property DataDescription: TRawImageDescription read GetDataDescription
                                                   write SetDataDescription;
    {$else}
    property PixelData: PByte read FRawImage.Data;
    property MaskData: PByte read FRawImage.Mask;
    property DataDescription: TRawImageDescription read FRawImage.Description
                                                   write SetDataDescription;
    {$endif}
    property TColors[x,y: integer]: TGraphicsColor read GetTColors write SetTColors;
    property Masked[x,y:integer]: Boolean read GetMasked write SetMasked;
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
  { This descendent of TFPPalette uses an AVL tree for speed up. }

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
  
  
  { ILazImageReader }
  { Extention to TFPCustomImageReader to initialize a TRawImgeDescription based
    on the image to be read
  }
  
  ILazImageReader = interface
    ['{DD8B14DE-4E97-4816-8B40-DD6C4D8CCD1B}']
    function GetUpdateDescription: Boolean;
    procedure SetUpdateDescription(AValue: Boolean);

    property UpdateDescription: Boolean read GetUpdateDescription write SetUpdateDescription;
  end;

  { TLazReaderXPM }
  { This is a FPImage reader for xpm images. }

  TLazReaderXPM = class(TFPCustomImageReader, ILazImageReader)
  private
    FWidth: Integer;
    FHeight: Integer;
    FColorCount: Integer;
    FCharsPerPixel: Integer;
    fXHot: Integer;
    fYHot: Integer;
    FPixelToColorTree: TArrayNodesTree;
    FUpdateDescription: Boolean; // If set, update rawimagedescription
    function  GetUpdateDescription: Boolean;
    procedure SetUpdateDescription(AValue: Boolean);
  protected
    function QueryInterface(const iid: TGuid; out obj): LongInt; stdcall;
    function _AddRef: LongInt; stdcall;
    function _Release: LongInt; stdcall;
  protected
    procedure ClearPixelToColorTree;
    procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
    function  InternalCheck(Str: TStream): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property UpdateDescription: Boolean read GetUpdateDescription write SetUpdateDescription;
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
  { This is an imroved FPImage reader for bmp images. }
  //TODO: improved or not, derive from TFPReaderBMP the get RLE support
  
  TLazReaderMaskMode = (
    lrmmNone,  // no mask is generated
    lrmmAuto,  // a mask is generated based on the first pixel read (*)
    lrmmColor  // a mask is generated based on the given color (*)
  );
  // (*) Note: when reading images with an alpha channel and the alpha channel
  //           has no influence on the mask (unless the maskcolor is transparent)



  TLazReaderDIB = class (TFPCustomImageReader, ILazImageReader)
  private
    FImage: TLazIntfImage;
  
    FMaskMode: TLazReaderMaskMode;
    FMaskColor: TFPColor; // color which should be interpreted as masked
    FMaskIndex: Integer;  // for palette based images, index of the color which should be interpreted as masked

    FReadSize: Integer;      // Size (in bytes) of 1 scanline.
    FBFI: TBitMapInfoHeader; // The header as read from the stream.
    FPalette: PFPColor;      // Buffer with Palette entries.
    FBitsPerPixel: Integer;  // bits per pixel (1, 4, 8, 15, 16, 24, 32)
    FLineBuf: PByte;         // Buffer for 1 scanline. Can be Byte, Word, TColorRGB or TColorRGBA
    FIsRLE: Boolean;         // Is data RLE compressed?
    FUpdateDescription: Boolean; // If set, update rawimagedescription

    procedure FreeBufs;      // Free (and nil) buffers.
    function GetUpdateDescription: Boolean;
    procedure SetUpdateDescription(AValue: Boolean);
  protected
    function QueryInterface(const iid: TGuid; out obj): LongInt; stdcall;
    function _AddRef: LongInt; stdcall;
    function _Release: LongInt; stdcall;
  protected

    // SetupRead will allocate the needed buffers, and read the colormap if needed.
    procedure SetupRead(nPalette, nRowBits: Integer; ReadPalette, AIsRLE: Boolean); virtual;
    procedure ReadScanLine(Row: Integer); virtual;
    procedure WriteScanLine(Row: Cardinal); virtual;
    // required by TFPCustomImageReader
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    procedure InternalReadHead;
    procedure InternalReadBody;
    function  InternalCheck(Stream: TStream) : boolean; override;
    
    property ReadSize: Integer read FReadSize;
    property LineBuf: PByte read FLineBuf;
    property BFI: TBitMapInfoHeader read FBFI;
    property IsRLE: Boolean read FIsRLE;
  public
    constructor Create; override;
    destructor Destroy; override;
    property MaskColor: TFPColor read FMaskColor write FMaskColor;
    property MaskMode: TLazReaderMaskMode read FMaskMode write FMaskMode;
    property UpdateDescription: Boolean read GetUpdateDescription write SetUpdateDescription;
  end;
  
  { TLazReaderBMP }

  TLazReaderBMP = class(TLazReaderDIB)
  protected
    function InternalCheck(Stream: TStream) : boolean; override;
  end;

  { TLazReaderIconDIB }
  { This is a FPImage reader for a single DIB from an icon file }
  TLazReaderIconDIB = class (TLazReaderDIB)
  protected
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
  end;

  { TLazReaderPNG }

  TLazReaderPNG = class(TFPReaderPNG, ILazImageReader)
  private
    FAlphaPalette: Boolean;
    FUpdateDescription: Boolean;
    function  GetUpdateDescription: Boolean;
    procedure SetUpdateDescription(AValue: Boolean);
  protected
    function QueryInterface(const iid: TGuid; out obj): LongInt; stdcall;
    function _AddRef: LongInt; stdcall;
    function _Release: LongInt; stdcall;
  protected
    procedure DoDecompress; override;
    procedure HandleAlpha; override;
    procedure InternalRead(Str:TStream; Img:TFPCustomImage); override;
  public
    property UpdateDescription: Boolean read GetUpdateDescription write SetUpdateDescription;
  end;

  { TLazReaderIcnsPart }

  TLazReaderIcnsPart = class(TFPCustomImageReader, ILazImageReader)
  private
    FUpdateDescription: Boolean;
    FPalette: TFPPalette;
    FImage: TLazIntfImage;
    FData: PByte;
    FCalcSize: Integer;
    FDataSize: Integer;
    FIconType: TicnsIconType;
    FIconInfo: TicnsIconInfo;
  protected
    function  InternalCheck(Str:TStream): boolean; override;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function QueryInterface(const iid: TGuid; out obj): LongInt; stdcall;
    function _AddRef: LongInt; stdcall;
    function _Release: LongInt; stdcall;
    function GetUpdateDescription: Boolean;
    procedure SetUpdateDescription(AValue: Boolean);
    procedure SetupRead(AWidth, AHeight, ADepth: Integer; IsMask: Boolean);
    function Create256ColorPalette: TFPPalette;
    procedure DoReadRaw;
    procedure DoReadRLE;
    procedure DoReadJpeg2000;
    procedure DoReadMask;
  public
    constructor Create; override;
    destructor Destroy; override;
    property UpdateDescription: Boolean read GetUpdateDescription write SetUpdateDescription;
    property IconType: TicnsIconType read FIconType;
    property DataSize: Integer read FDataSize;
  end;

// extra Rawimage utility functions

function QueryDescription(AFlags: TRawImageQueryFlags; AWidth: Integer = -1; AHeight: integer = -1): TRawImageDescription;
function GetDescriptionFromDevice(ADC: HDC; AWidth: Integer = -1; AHeight: integer = -1): TRawImageDescription;
function GetDescriptionFromBitmap(ABitmap: HBitmap; AWidth: Integer = -1; AHeight: integer = -1): TRawImageDescription;
function AddAlphaToDescription(var ADesc: TRawImageDescription; APrec: Byte): Boolean;


procedure DefaultReaderDescription(AWidth, AHeight: Integer; ADepth: Byte; out ADesc: TRawImageDescription);


function ReadCompleteStreamToString(Str: TStream; StartSize: integer): string;
procedure ReadCompleteStreamToStream(SrcStream, DestStream: TStream;
                                     StartSize: integer);
                                     
function dbgs(const FPColor: TFPColor): string; overload;

implementation

uses
  Graphics;
  
type
  PFPColorBytes = ^TFPColorBytes;
  TFPColorBytes = record
    {$ifdef ENDIAN_LITTLE}
    Rl, Rh, Gl, Gh, Bl, Bh, Al, Ah: Byte;
    {$else}
    Rh, Rl, Gh, Gl, Bh, Bl, Ah, Al: Byte;
    {$endif}
  end;
  
  PFourBytes = ^TFourBytes;
  TFourBytes = record
    B0, B1, B2, B3: Byte;
  end;
  

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

function QueryDescription(AFlags: TRawImageQueryFlags; AWidth: Integer; AHeight: integer): TRawImageDescription;
begin
  if not (riqfUpdate in AFlags) then Result.Init;

  if not RawImage_QueryDescription(AFlags, Result) then Exit;
  if AWidth <> -1 then Result.Width := AWidth;
  if AHeight <> -1 then Result.Height := AHeight;
end;

function GetDescriptionFromDevice(ADC: HDC; AWidth, AHeight: integer): TRawImageDescription;
begin
  Result.Init;
  if not RawImage_DescriptionFromDevice(ADC, Result) then Exit;
  if AWidth <> -1 then Result.Width := AWidth;
  if AHeight <> -1 then Result.Height := AHeight;
end;

function GetDescriptionFromBitmap(ABitmap: HBitmap; AWidth: Integer = -1; AHeight: integer = -1): TRawImageDescription;
begin
  Result.Init;
  if not RawImage_DescriptionFromBitmap(ABitmap, Result) then Exit;
  if AWidth <> -1 then Result.Width := AWidth;
  if AHeight <> -1 then Result.Height := AHeight;
end;

function AddAlphaToDescription(var ADesc: TRawImageDescription; APrec: Byte): Boolean;
  function CreateBitMask(AShift, APrec: Byte): Cardinal; inline;
  begin
    Result := ($FFFFFFFF shr (32 - APrec)) shl AShift;
  end;
var
  Mask: Cardinal;
begin
  if ADesc.AlphaPrec >= APrec then Exit(False);
  if ADesc.BitsPerPixel <> 32 then Exit(False);
  if ADesc.Depth <> 24 then Exit(False);

  Mask := CreateBitMask(ADesc.RedShift, ADesc.RedPrec)
       or CreateBitMask(ADesc.GreenShift, ADesc.GreenPrec)
       or CreateBitMask(ADesc.BlueShift, ADesc.BluePrec);

  if (Mask and $FF = 0)
  then begin
    ADesc.AlphaShift := 0;
    Result := True;
  end
  else
    if (Mask and $FF000000 = 0)
    then begin
      ADesc.AlphaShift := 24;
      Result := True;
    end;
  if Result
  then begin
    ADesc.AlphaPrec := APrec;
    ADesc.Depth := 32;
  end;
end;

procedure CheckAlphaDescription(AImage: TFPCustomImage);
var
  Desc: TRawImageDescription;
begin
  if not (AImage is TLazIntfImage) then Exit;

  Desc := TLazIntfImage(AImage).DataDescription;
  if Desc.AlphaPrec >= 8 then Exit;

  if not AddAlphaToDescription(Desc, 8)
  then begin
    Desc.Init_BPP32_B8G8R8A8_BIO_TTB(Desc.Width, Desc.Height);
    // copy mask description
    with TLazIntfImage(AImage).DataDescription do
    begin
      Desc.MaskBitsPerPixel := MaskBitsPerPixel;
      Desc.MaskShift := MaskShift;
      Desc.MaskLineEnd := MaskLineEnd;
      Desc.MaskBitOrder := MaskBitOrder;
    end;
  end;

  TLazIntfImage(AImage).DataDescription := Desc;
end;

procedure DefaultReaderDescription(AWidth, AHeight: Integer; ADepth: Byte; out ADesc: TRawImageDescription);
begin
  // Default description, assume 24bit for palettebased
  // Maybe when RawImage palette is supported, other descriptions need to be adjusted.
  
  ADesc.Init_BPP24_B8G8R8_M1_BIO_TTB(AWidth, AHeight);

  case ADepth of
    1: begin
      ADesc.Depth := 1;
      ADesc.BitsPerPixel := 1;
      ADesc.Format := ricfGray;
      ADesc.LineEnd := rileWordBoundary;
      ADesc.RedPrec := 1;
      ADesc.RedShift := 0;
      ADesc.GreenPrec := 1;
      ADesc.GreenShift := 0;
      ADesc.BluePrec := 1;
      ADesc.BlueShift := 0;
    end;
    2..4: begin
//      ADesc.Depth := 4;
//      ADesc.BitsPerPixel := 4;
    end;
    5..8: begin
//      ADesc.Depth := 8;
//      ADesc.BitsPerPixel := 8;
    end;
    9..15: begin
      ADesc.Depth := 15;
      ADesc.BitsPerPixel := 16;
      ADesc.RedPrec := 5;
      ADesc.RedShift := 10;
      ADesc.GreenPrec := 5;
      ADesc.GreenShift := 5;
      ADesc.BluePrec := 5;
      ADesc.BlueShift := 0;
    end;
    16: begin
      ADesc.Depth := 16;
      ADesc.BitsPerPixel := 16;
      ADesc.RedPrec := 5;
      ADesc.RedShift := 10;
      ADesc.GreenPrec := 6;
      ADesc.GreenShift := 5;
      ADesc.BluePrec := 5;
      ADesc.BlueShift := 0;
    end;
    17..24: begin
      // already default
    end;
  else
    ADesc.Depth := 32;
    ADesc.BitsPerPixel := 32;
    ADesc.AlphaPrec := 8;
    ADesc.AlphaShift := 24;
  end;
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
  FourBytes:=DWord(PWord(P)^) or (DWord(P[2]) shl 16);
{$else}
  FourBytes:=(DWord(PWord(P)^) shl 8) or DWord(P^);
{$endif}
  
  PrecMask:=not (PrecMask shl Shift);
  FourBytes:=FourBytes and PrecMask; // clear old
  FourBytes:=FourBytes or cardinal(Bits shl Shift); // set new

{$ifdef Endian_little}
  PWord(P)^ := Word(FourBytes);
  P[2] := Byte(FourBytes shr 16);
{$else}  
  PWord(P)^ := Word(FourBytes shr 8);
  P[2] := Byte(FourBytes);
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

procedure TLazIntfImage.SetDataDescription(const ADescription: TRawImageDescription);
begin
  if CompareMem(@FRawImage.Description, @ADescription, SizeOf(TRawImageDescription))
  then Exit;

  CheckDescription(ADescription, True);
  BeginUpdate;
  try
    FreeData;
    FRawImage.Description := ADescription;
    ChooseGetSetColorFunctions;
    InternalSetSize(ADescription.Width, ADescription.Height);
    CreateData;
  finally
    EndUpdate;
  end;
end;

procedure TLazIntfImage.ChooseRawBitsProc(BitsPerPixel: cardinal;
  ByteOrder: TRawImageByteOrder; BitOrder: TRawImageBitOrder;
  out ProcReadRawImageBits: TOnReadRawImageBits;
  out ProcWriteRawImageBits: TOnWriteRawImageBits);
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
var
  Desc: TRawImageDescription absolute FRawImage.Description;

  function ChooseRGBA_32Bpp: Boolean;
  var
    Positions: Byte;
  begin
    Result := False;
    if Desc.Depth <> 32 then Exit;
    if Desc.BitsPerPixel <> 32 then Exit;
    if Desc.LineOrder <> riloTopToBottom then Exit;
    if Desc.AlphaPrec <> 8 then Exit;
    if Desc.RedPrec <> 8 then Exit;
    if Desc.GreenPrec <> 8 then Exit;
    if Desc.BluePrec <> 8 then Exit;
    if Desc.AlphaShift and 7 <> 0 then Exit;
    if Desc.RedShift and 7 <> 0 then Exit;
    if Desc.GreenShift and 7 <> 0 then Exit;
    if Desc.BlueShift and 7 <> 0 then Exit;
    
    Positions := ((Desc.AlphaShift shr 3) and 3) shl 6
              or ((Desc.RedShift shr 3) and 3) shl 4
              or ((Desc.GreenShift shr 3) and 3) shl 2
              or ((Desc.BlueShift shr 3) and 3);

    if Desc.ByteOrder = riboMSBFirst
    then Positions := not Positions; // reverse positions
    
    // the locations of A,R,G,B are now coded in 2 bits each: AARRBBGG
    // the 2-bit value (0..3) represents the location of the channel,
    // counting from left
    case Positions of
      {AARRGGBB}
      %00011011: begin
        FGetInternalColorProc := @GetColor_BPP32_A8R8G8B8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_A8R8G8B8_BIO_TTB;
      end;
      {AARRGGBB}
      %00111001: begin
        FGetInternalColorProc := @GetColor_BPP32_A8B8G8R8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_A8B8G8R8_BIO_TTB;
      end;
      {AARRGGBB}
      %00100111: begin
        FGetInternalColorProc := @GetColor_BPP32_A8G8R8B8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_A8G8R8B8_BIO_TTB;
      end;
      {AARRGGBB}
      %00110110: begin
        FGetInternalColorProc := @GetColor_BPP32_A8G8B8R8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_A8G8B8R8_BIO_TTB;
      end;
      {AARRGGBB}
      %00011110: begin
        FGetInternalColorProc := @GetColor_BPP32_A8R8B8G8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_A8R8B8G8_BIO_TTB;
      end;
      {AARRGGBB}
      %00101101: begin
        FGetInternalColorProc := @GetColor_BPP32_A8B8R8G8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_A8B8R8G8_BIO_TTB;
      end;
      {AARRGGBB}
      %11100100: begin
        FGetInternalColorProc := @GetColor_BPP32_B8G8R8A8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_B8G8R8A8_BIO_TTB;
      end;
      {AARRGGBB}
      %11000110: begin
        FGetInternalColorProc := @GetColor_BPP32_R8G8B8A8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_R8G8B8A8_BIO_TTB;
      end;
      {AARRGGBB}
      %11100001: begin
        FGetInternalColorProc := @GetColor_BPP32_G8B8R8A8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_G8B8R8A8_BIO_TTB;
      end;
      {AARRGGBB}
      %11010010: begin
        FGetInternalColorProc := @GetColor_BPP32_G8R8B8A8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_G8R8B8A8_BIO_TTB;
      end;
      {AARRGGBB}
      %11011000: begin
        FGetInternalColorProc := @GetColor_BPP32_B8R8G8A8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_B8R8G8A8_BIO_TTB;
      end;
      {AARRGGBB}
      %11001001: begin
        FGetInternalColorProc := @GetColor_BPP32_R8B8G8A8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_R8B8G8A8_BIO_TTB;
      end;
    else
      Exit;
    end;
    Result := True;
  end;

  function ChooseRGB_32Bpp: Boolean;
  var
    Positions: Byte;
  begin
    Result := False;
    if Desc.Depth <> 24 then Exit;
    if Desc.BitsPerPixel <> 32 then Exit;
    if Desc.LineOrder <> riloTopToBottom then Exit;
    if Desc.RedPrec <> 8 then Exit;
    if Desc.GreenPrec <> 8 then Exit;
    if Desc.BluePrec <> 8 then Exit;
    if Desc.RedShift and 7 <> 0 then Exit;
    if Desc.GreenShift and 7 <> 0 then Exit;
    if Desc.BlueShift and 7 <> 0 then Exit;

    Positions := ((Desc.RedShift shr 3) and 3) shl 4
              or ((Desc.GreenShift shr 3) and 3) shl 2
              or ((Desc.BlueShift shr 3) and 3);

    if Desc.ByteOrder = riboMSBFirst
    then Positions := not Positions and %00111111; // reverse positions

    // the locations of R,G,B are now coded in 2 bits each: xxRRBBGG
    // the 2-bit value (0..3) represents the location of the channel,
    // counting from left
    case Positions of
      {xxRRGGBB}
      %00011011: begin
        FGetInternalColorProc := @GetColor_BPP32_X8R8G8B8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_X8R8G8B8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00111001: begin
        FGetInternalColorProc := @GetColor_BPP32_X8B8G8R8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_X8B8G8R8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00100111: begin
        FGetInternalColorProc := @GetColor_BPP32_X8G8R8B8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_X8G8R8B8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00110110: begin
        FGetInternalColorProc := @GetColor_BPP32_X8G8B8R8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_X8G8B8R8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00011110: begin
        FGetInternalColorProc := @GetColor_BPP32_X8R8B8G8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_X8R8B8G8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00101101: begin
        FGetInternalColorProc := @GetColor_BPP32_X8B8R8G8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_X8B8R8G8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00100100: begin
        FGetInternalColorProc := @GetColor_BPP32_B8G8R8X8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_B8G8R8X8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00000110: begin
        FGetInternalColorProc := @GetColor_BPP32_R8G8B8X8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_R8G8B8X8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00100001: begin
        FGetInternalColorProc := @GetColor_BPP32_G8B8R8X8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_G8B8R8X8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00010010: begin
        FGetInternalColorProc := @GetColor_BPP32_G8R8B8X8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_G8R8B8X8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00011000: begin
        FGetInternalColorProc := @GetColor_BPP32_B8R8G8X8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_B8R8G8X8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00001001: begin
        FGetInternalColorProc := @GetColor_BPP32_R8B8G8X8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP32_R8B8G8X8_BIO_TTB;
      end;
    else
      Exit;
    end;
    Result := True;
  end;

  function ChooseRGB_24Bpp: Boolean;
  var
    Positions: Byte;
  begin
    Result := False;
    if Desc.Depth <> 24 then Exit;
    if Desc.BitsPerPixel <> 24 then Exit;
    if Desc.LineOrder <> riloTopToBottom then Exit;
    if Desc.RedPrec <> 8 then Exit;
    if Desc.GreenPrec <> 8 then Exit;
    if Desc.BluePrec <> 8 then Exit;
    if Desc.RedShift and 7 <> 0 then Exit;
    if Desc.GreenShift and 7 <> 0 then Exit;
    if Desc.BlueShift and 7 <> 0 then Exit;

    if Desc.ByteOrder = riboMSBFirst
    then
      Positions := (2-((Desc.RedShift   shr 3) and 3)) shl 4
                or (2-((Desc.GreenShift shr 3) and 3)) shl 2
                or (2-((Desc.BlueShift  shr 3) and 3))
    else
      Positions := ((Desc.RedShift   shr 3) and 3) shl 4
                or ((Desc.GreenShift shr 3) and 3) shl 2
                or ((Desc.BlueShift  shr 3) and 3);


    // the locations of R,G,B are now coded in 2 bits each: xxRRBBGG
    // the 2-bit value (0..3) represents the location of the channel,
    // counting from left
    case Positions of
      {xxRRGGBB}
      %00100100: begin
        FGetInternalColorProc := @GetColor_BPP24_B8G8R8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP24_B8G8R8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00000110: begin
        FGetInternalColorProc := @GetColor_BPP24_R8G8B8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP24_R8G8B8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00100001: begin
        FGetInternalColorProc := @GetColor_BPP24_G8B8R8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP24_G8B8R8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00010010: begin
        FGetInternalColorProc := @GetColor_BPP24_G8R8B8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP24_G8R8B8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00011000: begin
        FGetInternalColorProc := @GetColor_BPP24_B8R8G8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP24_B8R8G8_BIO_TTB;
      end;
      {xxRRGGBB}
      %00001001: begin
        FGetInternalColorProc := @GetColor_BPP24_R8B8G8_BIO_TTB;
        FSetInternalColorProc := @SetColor_BPP24_R8B8G8_BIO_TTB;
      end;
    else
      Exit;
    end;
    Result := True;
  end;
  
  procedure ChooseRGBAFunctions;
  begin
    ChooseRawBitsProc(Desc.BitsPerPixel, Desc.ByteOrder, Desc.BitOrder,
                      FReadRawImageBits, FWriteRawImageBits);

    if Desc.AlphaPrec > 0
    then begin
      FGetInternalColorProc := @GetColor_RGBA_NoPalette;
      FSetInternalColorProc := @SetColor_RGBA_NoPalette;
    end
    else begin
      FGetInternalColorProc := @GetColor_RGB_NoPalette;
      FSetInternalColorProc := @SetColor_RGB_NoPalette;
    end;
  end;

begin
  // Default: use the generic functions, that can handle all kinds of RawImages
  FGetInternalColorProc := @GetColor_Generic;
  FSetInternalColorProc := @SetColor_Generic;
  
  if FUpdateCount > 0
  then begin
    FGetSetColorFunctionsUpdateNeeded := true;
    Exit;
  end;
  FGetSetColorFunctionsUpdateNeeded := false;

  if Desc.MaskBitsPerPixel > 0
  then begin
    ChooseRawBitsProc(Desc.MaskBitsPerPixel,
                      Desc.ByteOrder,
                      Desc.MaskBitOrder,
                      FMaskReadRawImageBits, FMaskWriteRawImageBits);
  end;

  if Desc.PaletteColorCount = 0
  then begin
    case Desc.Format of
      ricfRGBA: begin
        if not (ChooseRGBA_32Bpp or ChooseRGB_32Bpp or ChooseRGB_24Bpp)
        then ChooseRGBAFunctions;
      end;
      ricfGray: begin
        ChooseRawBitsProc(Desc.BitsPerPixel,
                          Desc.ByteOrder,
                          Desc.BitOrder,
                          FReadRawImageBits, FWriteRawImageBits);
        FGetInternalColorProc := @GetColor_Gray_NoPalette;
        FSetInternalColorProc := @SetColor_Gray_NoPalette;
      end;
    end;
  end
  else begin
    // palette
    // ToDo
    DebugLn('WARNING: TLazIntfImage.ChooseGetSetColorFunctions Palette is unsupported');
  end;
end;

procedure TLazIntfImage.GetColor_Generic(x, y: integer; out Value: TFPColor);
var
  Position: TRawImagePosition;
begin
  GetXYDataPostion(x,y,Position);

  if FRawImage.Description.PaletteColorCount = 0
  then begin
    FRawimage.ReadChannels(Position, Value.Red, Value.Green, Value.Blue, Value.Alpha);
  end
  else begin
    // ToDo: read index, then palette
    Value.Red:=0;
    Value.Green:=0;
    Value.Blue:=0;
    Value.Alpha:=0;
  end;
end;

procedure TLazIntfImage.GetMask_Generic(x, y: integer; out AValue: Boolean);
var
  Position: TRawImagePosition;
begin
  if FRawImage.Description.MaskBitsPerPixel = 0
  then begin
    Avalue := False;
  end
  else begin
    GetXYMaskPostion(x,y,Position);
    FRawimage.ReadMask(Position, AValue);
  end;
end;

procedure TLazIntfImage.SetColor_Generic(x, y: integer; const Value: TFPColor);
var
  Position: TRawImagePosition;
begin
  GetXYDataPostion(x,y,Position);

  if FRawImage.Description.PaletteColorCount = 0
  then begin
    FRawImage.WriteChannels(Position, Value.Red, Value.Green, Value.Blue, Value.Alpha);
  end
  else begin
    // ToDo: Palette
  end;
end;

procedure TLazIntfImage.SetMask_Generic(x, y: integer; const AValue: Boolean);
var
  Desc: TRawImageDescription absolute FRawImage.Description;
  Position: TRawImagePosition;
begin
  if Desc.MaskBitsPerPixel = 0 then Exit;
  
  GetXYMaskPostion(x,y,Position);
  FRawImage.WriteMask(Position, AValue);
end;


procedure TLazIntfImage.GetColor_RGBA_NoPalette(x, y: integer; out Value: TFPColor);
var
  Desc: TRawImageDescription absolute FRawImage.Description;
  Position: TRawImagePosition;
begin
  GetXYDataPostion(x,y,Position);
  FReadRawImageBits(FRawImage.Data, Position, Desc.RedPrec, Desc.RedShift, Value.Red);
  FReadRawImageBits(FRawImage.Data, Position, Desc.GreenPrec, Desc.GreenShift, Value.Green);
  FReadRawImageBits(FRawImage.Data, Position, Desc.BluePrec, Desc.BlueShift, Value.Blue);
  FReadRawImageBits(FRawImage.Data, Position, Desc.AlphaPrec,Desc.AlphaShift, Value.Alpha);
end;

procedure TLazIntfImage.GetColor_RGB_NoPalette(x, y: integer; out Value: TFPColor);
var
  Desc: TRawImageDescription absolute FRawImage.Description;
  Position: TRawImagePosition;
begin
  GetXYDataPostion(x,y,Position);
  FReadRawImageBits(FRawImage.Data, Position, Desc.RedPrec, Desc.RedShift, Value.Red);
  FReadRawImageBits(FRawImage.Data, Position, Desc.GreenPrec, Desc.GreenShift, Value.Green);
  FReadRawImageBits(FRawImage.Data, Position, Desc.BluePrec, Desc.BlueShift, Value.Blue);
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_Gray_NoPalette(x, y: integer; out Value: TFPColor);
var
  Desc: TRawImageDescription absolute FRawImage.Description;
  Position: TRawImagePosition;
begin
  GetXYDataPostion(x,y,Position);
  FReadRawImageBits(FRawImage.Data, Position, Desc.RedPrec, Desc.RedShift, Value.Red);
  Value.Green := Value.Red;
  Value.Blue := Value.Red;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_A8B8G8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Ah := B0;
    VBytes.Al := B0;
    VBytes.Bh := B1;
    VBytes.Bl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
    VBytes.Gh := B3;
    VBytes.Gl := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_A8B8R8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Ah := B0;
    VBytes.Al := B0;
    VBytes.Bh := B1;
    VBytes.Bl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
    VBytes.Gh := B3;
    VBytes.Gl := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_A8G8B8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Ah := B0;
    VBytes.Al := B0;
    VBytes.Gh := B1;
    VBytes.Gl := B1;
    VBytes.Bh := B2;
    VBytes.Bl := B2;
    VBytes.Rh := B3;
    VBytes.Rl := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_A8G8R8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Ah := B0;
    VBytes.Al := B0;
    VBytes.Gh := B1;
    VBytes.Gl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
    VBytes.Bh := B3;
    VBytes.Bl := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_A8R8B8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Ah := B0;
    VBytes.Al := B0;
    VBytes.Rh := B1;
    VBytes.Rl := B1;
    VBytes.Bh := B2;
    VBytes.Bl := B2;
    VBytes.Gh := B3;
    VBytes.Gl := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_A8R8G8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Ah := B0;
    VBytes.Al := B0;
    VBytes.Rh := B1;
    VBytes.Rl := B1;
    VBytes.Gh := B2;
    VBytes.Gl := B2;
    VBytes.Bh := B3;
    VBytes.Bl := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_B8G8R8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Bh := B0;
    VBytes.Bl := B0;
    VBytes.Gh := B1;
    VBytes.Gl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
    VBytes.Ah := B3;
    VBytes.Al := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_B8R8G8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Bh := B0;
    VBytes.Bl := B0;
    VBytes.Rh := B1;
    VBytes.Rl := B1;
    VBytes.Gh := B2;
    VBytes.Gl := B2;
    VBytes.Ah := B3;
    VBytes.Al := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_G8B8R8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Gh := B0;
    VBytes.Gl := B0;
    VBytes.Bh := B1;
    VBytes.Bl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
    VBytes.Ah := B3;
    VBytes.Al := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_G8R8B8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Gh := B0;
    VBytes.Gl := B0;
    VBytes.Rh := B1;
    VBytes.Rl := B1;
    VBytes.Bh := B2;
    VBytes.Bl := B2;
    VBytes.Ah := B3;
    VBytes.Al := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_R8B8G8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Rh := B0;
    VBytes.Rl := B0;
    VBytes.Bh := B1;
    VBytes.Bl := B1;
    VBytes.Gh := B2;
    VBytes.Gl := B2;
    VBytes.Ah := B3;
    VBytes.Al := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_R8G8B8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Rh := B0;
    VBytes.Rl := B0;
    VBytes.Gh := B1;
    VBytes.Gl := B1;
    VBytes.Bh := B2;
    VBytes.Bl := B2;
    VBytes.Ah := B3;
    VBytes.Al := B3;
  end;
end;

procedure TLazIntfImage.GetColor_BPP32_X8B8G8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Bh := B1;
    VBytes.Bl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
    VBytes.Gh := B3;
    VBytes.Gl := B3;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_X8B8R8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Bh := B1;
    VBytes.Bl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
    VBytes.Gh := B3;
    VBytes.Gl := B3;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_X8G8B8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Gh := B1;
    VBytes.Gl := B1;
    VBytes.Bh := B2;
    VBytes.Bl := B2;
    VBytes.Rh := B3;
    VBytes.Rl := B3;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_X8G8R8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Gh := B1;
    VBytes.Gl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
    VBytes.Bh := B3;
    VBytes.Bl := B3;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_X8R8B8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Rh := B1;
    VBytes.Rl := B1;
    VBytes.Bh := B2;
    VBytes.Bl := B2;
    VBytes.Gh := B3;
    VBytes.Gl := B3;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_X8R8G8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Rh := B1;
    VBytes.Rl := B1;
    VBytes.Gh := B2;
    VBytes.Gl := B2;
    VBytes.Bh := B3;
    VBytes.Bl := B3;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_B8G8R8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Bh := B0;
    VBytes.Bl := B0;
    VBytes.Gh := B1;
    VBytes.Gl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_B8R8G8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Bh := B0;
    VBytes.Bl := B0;
    VBytes.Rh := B1;
    VBytes.Rl := B1;
    VBytes.Gh := B2;
    VBytes.Gl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_G8B8R8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Gh := B0;
    VBytes.Gl := B0;
    VBytes.Bh := B1;
    VBytes.Bl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_G8R8B8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Gh := B0;
    VBytes.Gl := B0;
    VBytes.Rh := B1;
    VBytes.Rl := B1;
    VBytes.Bh := B2;
    VBytes.Bl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_R8B8G8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Rh := B0;
    VBytes.Rl := B0;
    VBytes.Bh := B1;
    VBytes.Bl := B1;
    VBytes.Gh := B2;
    VBytes.Gl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP32_R8G8B8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    VBytes.Rh := B0;
    VBytes.Rl := B0;
    VBytes.Gh := B1;
    VBytes.Gl := B1;
    VBytes.Bh := B2;
    VBytes.Bl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP24_B8G8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=24
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    VBytes.Bh := B0;
    VBytes.Bl := B0;
    VBytes.Gh := B1;
    VBytes.Gl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP24_B8R8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=24
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    VBytes.Bh := B0;
    VBytes.Bl := B0;
    VBytes.Rh := B1;
    VBytes.Rl := B1;
    VBytes.Gh := B2;
    VBytes.Gl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP24_G8B8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=24
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    VBytes.Gh := B0;
    VBytes.Gl := B0;
    VBytes.Bh := B1;
    VBytes.Bl := B1;
    VBytes.Rh := B2;
    VBytes.Rl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP24_G8R8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=24
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    VBytes.Gh := B0;
    VBytes.Gl := B0;
    VBytes.Rh := B1;
    VBytes.Rl := B1;
    VBytes.Bh := B2;
    VBytes.Bl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP24_R8B8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=24
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    VBytes.Rh := B0;
    VBytes.Rl := B0;
    VBytes.Bh := B1;
    VBytes.Bl := B1;
    VBytes.Gh := B2;
    VBytes.Gl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_BPP24_R8G8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=24
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    VBytes.Rh := B0;
    VBytes.Rl := B0;
    VBytes.Gh := B1;
    VBytes.Gl := B1;
    VBytes.Bh := B2;
    VBytes.Bl := B2;
  end;
  // no alpha -> set opaque
  Value.Alpha:=high(Value.Alpha);
end;

procedure TLazIntfImage.GetColor_NULL(x, y: integer; out Value: TFPColor);
//var
//  Position: TRawImagePosition;
begin
//  GetXYDataPostion(x,y,Position);
  Value.Red:=0;
  Value.Green:=0;
  Value.Blue:=0;
  Value.Alpha:=0;
end;

procedure TLazIntfImage.SetColor_RGBA_NoPalette(x, y: integer; const Value: TFPColor);
var
  Desc: TRawImageDescription absolute FRawImage.Description;
  Position: TRawImagePosition;
begin
  GetXYDataPostion(x,y,Position);
  FWriteRawImageBits(FRawImage.Data, Position, Desc.RedPrec,Desc.RedShift, Value.Red);
  FWriteRawImageBits(FRawImage.Data, Position, Desc.GreenPrec, Desc.GreenShift, Value.Green);
  FWriteRawImageBits(FRawImage.Data, Position, Desc.BluePrec, Desc.BlueShift, Value.Blue);
  FWriteRawImageBits(FRawImage.Data, Position, Desc.AlphaPrec, Desc.AlphaShift, Value.Alpha)
end;

procedure TLazIntfImage.SetColor_RGB_NoPalette(x, y: integer; const Value: TFPColor);
var
  Desc: TRawImageDescription absolute FRawImage.Description;
  Position: TRawImagePosition;
begin
  GetXYDataPostion(x,y,Position);
  FWriteRawImageBits(FRawImage.Data, Position, Desc.RedPrec, Desc.RedShift, Value.Red);
  FWriteRawImageBits(FRawImage.Data, Position, Desc.GreenPrec, Desc.GreenShift, Value.Green);
  FWriteRawImageBits(FRawImage.Data, Position, Desc.BluePrec, Desc.BlueShift, Value.Blue);
  // no alpha -> ignore
end;

procedure TLazIntfImage.SetColor_Gray_NoPalette(x, y: integer; const Value: TFPColor);
var
  Desc: TRawImageDescription absolute FRawImage.Description;
  Position: TRawImagePosition;
begin
  GetXYDataPostion(x,y,Position);
  FWriteRawImageBits(FRawImage.Data, Position, Desc.RedPrec, Desc.RedShift, Value.Red);
end;

procedure TLazIntfImage.SetColor_NULL(x, y: integer; const Value: TFPColor);
begin
  // NULL, not implemented
end;

procedure TLazIntfImage.SetColor_BPP32_A8R8G8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Ah;
    B1 := VBytes.Rh;
    B2 := VBytes.Gh;
    B3 := VBytes.Bh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_A8B8G8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Ah;
    B1 := VBytes.Bh;
    B2 := VBytes.Gh;
    B3 := VBytes.Rh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_A8B8R8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Ah;
    B1 := VBytes.Bh;
    B2 := VBytes.Rh;
    B3 := VBytes.Gh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_A8G8B8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Ah;
    B1 := VBytes.Gh;
    B2 := VBytes.Bh;
    B3 := VBytes.Rh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_A8G8R8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Ah;
    B1 := VBytes.Gh;
    B2 := VBytes.Rh;
    B3 := VBytes.Bh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_A8R8B8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Ah;
    B1 := VBytes.Rh;
    B2 := VBytes.Bh;
    B3 := VBytes.Gh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_B8G8R8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Bh;
    B1 := VBytes.Gh;
    B2 := VBytes.Rh;
    B3 := VBytes.Ah;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_B8R8G8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Bh;
    B1 := VBytes.Rh;
    B2 := VBytes.Gh;
    B3 := VBytes.Ah;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_G8B8R8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Gh;
    B1 := VBytes.Bh;
    B2 := VBytes.Rh;
    B3 := VBytes.Ah;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_G8R8B8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Gh;
    B1 := VBytes.Rh;
    B2 := VBytes.Bh;
    B3 := VBytes.Ah;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_R8B8G8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Rh;
    B1 := VBytes.Bh;
    B2 := VBytes.Gh;
    B3 := VBytes.Ah;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_R8G8B8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Rh;
    B1 := VBytes.Gh;
    B2 := VBytes.Bh;
    B3 := VBytes.Ah;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_X8R8G8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B1 := VBytes.Rh;
    B2 := VBytes.Gh;
    B3 := VBytes.Bh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_X8B8G8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B1 := VBytes.Bh;
    B2 := VBytes.Gh;
    B3 := VBytes.Rh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_X8B8R8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B1 := VBytes.Bh;
    B2 := VBytes.Rh;
    B3 := VBytes.Gh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_X8G8B8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B1 := VBytes.Gh;
    B2 := VBytes.Bh;
    B3 := VBytes.Rh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_X8G8R8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B1 := VBytes.Gh;
    B2 := VBytes.Rh;
    B3 := VBytes.Bh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_X8R8B8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B1 := VBytes.Rh;
    B2 := VBytes.Bh;
    B3 := VBytes.Gh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_B8G8R8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Bh;
    B1 := VBytes.Gh;
    B2 := VBytes.Rh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_B8R8G8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Bh;
    B1 := VBytes.Rh;
    B2 := VBytes.Gh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_G8B8R8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Gh;
    B1 := VBytes.Bh;
    B2 := VBytes.Rh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_G8R8B8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Gh;
    B1 := VBytes.Rh;
    B2 := VBytes.Bh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_R8B8G8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Rh;
    B1 := VBytes.Bh;
    B2 := VBytes.Gh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP32_R8G8B8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x shl 2))^ do
  begin
    B0 := VBytes.Rh;
    B1 := VBytes.Gh;
    B2 := VBytes.Bh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP24_B8G8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    B0 := VBytes.Bh;
    B1 := VBytes.Gh;
    B2 := VBytes.Rh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP24_B8R8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    B0 := VBytes.Bh;
    B1 := VBytes.Rh;
    B2 := VBytes.Gh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP24_G8B8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    B0 := VBytes.Gh;
    B1 := VBytes.Bh;
    B2 := VBytes.Rh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP24_G8R8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    B0 := VBytes.Gh;
    B1 := VBytes.Rh;
    B2 := VBytes.Bh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP24_R8B8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    B0 := VBytes.Rh;
    B1 := VBytes.Bh;
    B2 := VBytes.Gh;
  end;
end;

procedure TLazIntfImage.SetColor_BPP24_R8G8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
// Format=ricfRGBA HasPalette=false Depth=32 PaletteColorCount=0
// BitOrder=riboBitsInOrder LineOrder=riloTopToBottom
// BitsPerPixel=32
var
  VBytes: TFPColorBytes absolute Value;
begin
  with PFourBytes(FRawImage.Data+FLineStarts^.Positions[y].Byte+(x * 3))^ do
  begin
    B0 := VBytes.Rh;
    B1 := VBytes.Gh;
    B2 := VBytes.Bh;
  end;
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
  FSetInternalColorProc(x,y,Value);
  {if y=Height-1 then
    DebugLn(['TLazIntfImage.SetInternalColor x=',x,' y=',y,' ',dbgs(Value),' ',dbgs(GetInternalColor(x,y))]);}
end;

function TLazIntfImage.GetInternalColor(x, y: integer): TFPColor;
begin
  FGetInternalColorProc(x,y,Result);
end;

procedure TLazIntfImage.SetInternalPixel(x, y: integer; Value: integer);
begin

end;

procedure TLazIntfImage.SetMasked(x, y: integer; const AValue: Boolean);
begin
//  CheckIndex(x,y);
  SetMask_Generic(x, y, AValue);
  FMaskSet := FMaskSet or AValue;
end;

function TLazIntfImage.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

function TLazIntfImage.GetMasked(x, y: integer): Boolean;
begin
//  CheckIndex (x,y);
  GetMask_Generic(x, y, Result);
end;

{$ifdef VER2_0}
function TLazIntfImage.GetDataDescription: TRawImageDescription;
begin
  Result := FRawImage.Description;
end;

function TLazIntfImage.GetMaskData: PByte;
begin
  Result := FRawImage.Mask;
end;

function TLazIntfImage.GetPixelData: PByte;
begin
  Result := FRawImage.Data;
end;
{$endif}

procedure TLazIntfImage.FreeData;
begin
  if FDataOwner
  then ReallocMem(FRawImage.Data, 0)
  else FRawImage.Data := nil;
  FRawImage.DataSize := 0;

  if FLineStarts <> nil then Dispose(FLineStarts);
  FLineStarts := nil;
  
  if FDataOwner
  then ReallocMem(FRawImage.Mask, 0)
  else FRawImage.Mask := nil;
  FRawImage.MaskSize := 0;

  if FMaskLineStarts <> nil then Dispose(FMaskLineStarts);
  FMaskLineStarts := nil;
  FMaskSet := False;
end;

procedure TLazIntfImage.CreateData;
var
  Desc: TRawImageDescription absolute FRawImage.Description;
begin
  if FUpdateCount > 0
  then begin
    FCreateAllDataNeeded := True;
    Exit;
  end;
  FCreateAllDataNeeded := False;

  FreeData;

  New(FLineStarts);
  FLineStarts^.Init(Width, Height, Desc.BitsPerPixel, Desc.LineEnd, Desc.LineOrder);
  New(FMaskLineStarts);
  FMaskLineStarts^.Init(Width, Height, Desc.MaskBitsPerPixel, Desc.MaskLineEnd, Desc.LineOrder);

  FRawImage.CreateData(False);
end;

function TLazIntfImage.HasTransparency: boolean;
begin
  Result := FMaskSet or FRawImage.IsTransparent(True);
end;

function TLazIntfImage.HasMask: boolean;
begin
  Result := FMaskSet;
end;

constructor TLazIntfImage.Create(AWidth, AHeight: integer);
begin
  FDataOwner := True;
  FGetInternalColorProc := @GetColor_NULL;
  FSetInternalColorProc := @SetColor_NULL;
  inherited Create(AWidth, AHeight);
end;


constructor TLazIntfImage.Create(ARawImage: TRawImage; ADataOwner: Boolean);
var
  Desc: TRawImageDescription absolute FRawImage.Description;
begin
  BeginUpdate;
  FRawimage := ARawImage;
  Create(Desc.Width, Desc.Height);
  FDataOwner := ADataOwner;
  FCreateAllDataNeeded := False;
  EndUpdate;
  New(FLineStarts);
  FLineStarts^.Init(Width, Height, Desc.BitsPerPixel, Desc.LineEnd, Desc.LineOrder);
  New(FMaskLineStarts);
  FMaskLineStarts^.Init(Width, Height, Desc.MaskBitsPerPixel, Desc.MaskLineEnd, Desc.LineOrder);
  ChooseGetSetColorFunctions;
end;

destructor TLazIntfImage.Destroy;
begin
  FreeData;
  inherited Destroy;
end;

procedure TLazIntfImage.AlphaFromMask(AKeepAlpha: Boolean);
var
  x, y, xStop, yStop: Integer;
  Color: TFPColor;
begin
  if FRawImage.Mask = nil then Exit;
  if FRawImage.MaskSize = 0 then Exit;

  xStop := Width - 1;
  yStop := Height - 1;

  if AKeepAlpha
  then begin
    for y:=0 to yStop do
      for x:=0 to xStop do
      begin
        if not Masked[x,y] then Continue;
        Color := Colors[x,y];
        Color.alpha := Low(Color.alpha);
        Colors[x,y] := Color;
      end;
  end
  else begin
    for y:=0 to yStop do
      for x:=0 to xStop do
      begin
        Color := Colors[x,y];
        if Masked[x,y]
        then Color.alpha := Low(Color.alpha)
        else Color.alpha := High(Color.alpha);
        Colors[x,y] := Color;
      end;
  end;
end;

procedure TLazIntfImage.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TLazIntfImage.EndUpdate;
begin
  if FUpdateCount = 0 then Exit;
  Dec(FUpdateCount);
  if FUpdateCount > 0 then Exit;

  if FCreateAllDataNeeded then
    CreateData;
  if FGetSetColorFunctionsUpdateNeeded then
    ChooseGetSetColorFunctions;
end;

procedure TLazIntfImage.InternalSetSize(AWidth, AHeight: integer);
  procedure Error;
  begin
    raise FPImageException.Create('Invalid Size');
  end;
begin
  if (AWidth = Width) and (AHeight = Height) then exit;
  if (AWidth<0) or (AHeight<0) then Error;
  inherited SetSize(AWidth, AHeight);

  FRawImage.Description.Width := Width;
  FRawImage.Description.Height := Height;
end;

procedure TLazIntfImage.SetSize(AWidth, AHeight: integer);
begin
  InternalSetSize(AWidth, AHeight);
  CreateData;
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

procedure TLazIntfImage.GetXYDataPostion(x, y: integer; out Position: TRawImagePosition);
begin
  Position := FLineStarts^.GetPosition(x, y);
end;

procedure TLazIntfImage.GetXYMaskPostion(x, y: integer; out Position: TRawImagePosition);
begin
  Position := FMaskLineStarts^.GetPosition(x, y);
end;

function TLazIntfImage.GetDataLineStart(y: integer): Pointer;
begin
  if FRawimage.Description.LineOrder = riloBottomToTop then
    y:=Height-y;
  Result := FRawImage.Data+FLineStarts^.Positions[y].Byte;
end;

procedure TLazIntfImage.LoadFromDevice(DC: HDC);
var
  R: TRect;
  RawImage: TRawImage;
  DeviceSize: TPoint;
begin
  GetDeviceSize(DC, DeviceSize);
  R := Rect(0,0,DeviceSize.X,DeviceSize.Y);
  if not RawImage_FromDevice(RawImage, DC, R) then
    raise FPImageException.Create('Failed to get raw image from device');
  SetRawImage(RawImage);
end;

procedure TLazIntfImage.LoadFromBitmap(ABitmap, AMaskBitmap: HBitmap;
  AWidth: integer; AHeight: integer);
var
  R: TRect;
  RawImage: TRawImage;
  Desc: TRawImageDescription;
begin
  if not RawImage_DescriptionFromBitmap(ABitmap, Desc) then
    raise FPImageException.Create('Failed to get raw image description from bitmap');
    
  if AWidth < 0 then AWidth := Desc.Width;
  if AHeight < 0 then AHeight := Desc.Height;
  R := Rect(0, 0, AWidth, AHeight);
  if not RawImage_FromBitmap(RawImage, ABitmap, AMaskBitmap, @R) then
    raise FPImageException.Create('Failed to get raw image from bitmap');

  SetRawImage(RawImage);
end;

procedure TLazIntfImage.CreateBitmaps(out ABitmap, AMask: HBitmap; ASkipMask: boolean);
begin
  if not RawImage_CreateBitmaps(FRawImage, ABitmap, AMask, ASkipMask)
  then raise FPImageException.Create('Failed to create handles');
end;

procedure TLazIntfImage.SetRawImage(const ARawImage: TRawImage; ADataOwner: Boolean);
var
  Desc: TRawImageDescription absolute FRawImage.Description;
begin
  if FRawImage.IsEqual(ARawImage) then Exit;

  BeginUpdate;
  try
    FreeData;
    FRawImage := ARawImage;
    FDataOwner := ADataOwner;
    SetSize(Desc.Width, Desc.Height);
    FCreateAllDataNeeded := False;
    New(FLineStarts);
    FLineStarts^.Init(Width, Height, Desc.BitsPerPixel, Desc.LineEnd, Desc.LineOrder);
    New(FMaskLineStarts);
    FMaskLineStarts^.Init(Width, Height, Desc.MaskBitsPerPixel, Desc.MaskLineEnd, Desc.LineOrder);
    ChooseGetSetColorFunctions;
  finally
    EndUpdate;
  end;
end;

procedure TLazIntfImage.GetRawImage(out ARawImage: TRawImage; ATransferOwnership: Boolean);
begin
  ARawImage := FRawImage;
  if ATransferOwnership
  then FDataOwner := False;
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
  if (Width=0) or (Height=0) or (FRawImage.Data=nil) then exit;

  case FRawImage.Description.BitsPerPixel of

  8:
    begin
      SetInternalColor(0,0,Color);
      ColorChar:=Char(FRawImage.Data[0]);
      FillChar(FRawImage.Data^,FRawImage.DataSize,ColorChar);
    end;

  16:
    begin
      SetInternalColor(0,0,Color);
      ColorWord:=PWord(FRawImage.Data)[0];
      Cnt:=FRawImage.DataSize div 2;
      for i:=0 to Cnt-1 do
        PWord(FRawImage.Data)[i]:=ColorWord;
    end;

  32:
    begin
      SetInternalColor(0,0,Color);
      ColorDWord:=PDWord(FRawImage.Data)[0];
      Cnt:=FRawImage.DataSize div 4;
      for i:=0 to Cnt-1 do
        PDWord(FRawImage.Data)[i]:=ColorDWord;
    end;

  else
    for y:=0 to Height-1 do
      for x:=0 to Width-1 do
        SetInternalColor(x,y,Color);
  end;

  // ToDo: mask
end;

procedure TLazIntfImage.CopyPixels(ASource: TFPCustomImage; XDst: Integer = 0; YDst: Integer = 0);
var
  SrcImg: TLazIntfImage absolute ASource;
  x, y, xStop, yStop: Integer;
begin
{
  if (Src.Width<>Width) or (Src.Height<>Height) then
    SetSize(Src.Width,Src.Height);
}
  if (ASource is TLazIntfImage) and
     FRawImage.Description.IsEqual(SrcImg.FRawImage.Description) and (XDst =  0) and (YDst = 0) then
  begin
    // same description -> copy
    if FRawImage.Data <> nil then
      System.Move(SrcImg.FRawImage.Data^,FRawImage.Data^,FRawImage.DataSize);
    if FRawImage.Mask <> nil then
      System.Move(SrcImg.FRawImage.Mask^,FRawImage.Mask^,FRawImage.MaskSize);
    Exit;
  end;

  // copy pixels
  xStop := ASource.Width;
  if Width - XDst < xStop
  then xStop := Width - XDst;
  yStop := ASource.Height;
  if Height - YDst < yStop
  then yStop := Height - YDst;
  Dec(xStop);
  Dec(yStop);
  for y:=0 to yStop do
    for x:=0 to xStop do
      Colors[x+XDst,y+YDst] := ASource.Colors[x,y];

  if ASource is TLazIntfImage then
    for y:=0 to yStop do
      for x:=0 to xStop do
        Masked[x+XDst,y+YDst] := SrcImg.Masked[x,y];

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
  HasAlpha: Boolean;

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
      end
      else begin
        // read as text
        repeat
          inc(ReadPos);
        until not (Src[ReadPos] in ['A'..'Z','a'..'z']);
        ColorEnd:=ReadPos;
        NewColor:=TextToColor(ColorStart,ColorEnd);
      end;
      AddColor(PixelStart,NewColor,IntArray);

      HasAlpha := HasAlpha or (NewColor.alpha <> alphaOpaque);
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
  IntArray: array of Integer;
  Desc: TRawImageDescription;
begin
  ClearPixelToColorTree;
  Src:=ReadCompleteStreamToString(Str,1024);
  SrcLen:=length(Src);
  SrcPos:=1;
  CurLineNumber:=1;
  LastLineStart:=1;
  ReadHeader;
  
  SetLength(IntArray, FCharsPerPixel+1);

  HasAlpha := False;
  ReadPalette(@IntArray[0]);

  if FUpdateDescription and (theImage is TLazIntfImage)
  then begin
    if HasAlpha
    then DefaultReaderDescription(FWidth, FHeight, 32, Desc)
    else DefaultReaderDescription(FWidth, FHeight, 24, Desc);
//  MWE: keep mask ?
//    if FMaskMode = lrmmNone
//    then Desc.MaskBitsPerPixel := 0;
    TLazIntfImage(theImage).DataDescription := Desc;
  end
  else begin
    if HasAlpha
    then CheckAlphaDescription(TheImage);
  end;
  //FPixelToColorTree.ConsistencyCheck;
  ReadPixels(@IntArray[0]);
end;

function TLazReaderXPM.QueryInterface(const iid: TGuid; out obj): longint; stdcall;
begin
  if GetInterface(iid, obj)
  then Result := S_OK
  else Result := E_NOINTERFACE;
end;

procedure TLazReaderXPM.SetUpdateDescription(AValue: Boolean);
begin
  FUpdateDescription := AValue;
end;

function TLazReaderXPM._AddRef: LongInt; stdcall;
begin
  Result := -1;
end;

function TLazReaderXPM._Release: LongInt; stdcall;
begin
  Result := -1;
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

function TLazReaderXPM.GetUpdateDescription: Boolean;
begin
  Result := FUpdateDescription;
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

{ TLazReaderDIB_ }

function TLazReaderBMP.InternalCheck(Stream: TStream): boolean;
var
  BFH: TBitMapFileHeader;
begin
  stream.Read(BFH, SizeOf(BFH));
  with BFH do
    Result := (LEtoN(bfType) = BMmagic); // Just check magic number
end;

{ TLazReaderDIB }

function BmpRGBAToFPColor(const RGBA: TColorRGBA): TFPcolor;
begin
  Result.Red   := (RGBA.R shl 8) or RGBA.R;
  Result.Green := (RGBA.G shl 8) or RGBA.G;
  Result.Blue  := (RGBA.B shl 8) or RGBA.B;
  Result.Alpha := (RGBA.A shl 8) or RGBA.A;
end;

function BmpRGBToFPColor(const RGBA: TColorRGBA): TFPcolor;
begin
  Result.Red   := (RGBA.R shl 8) or RGBA.R;
  Result.Green := (RGBA.G shl 8) or RGBA.G;
  Result.Blue  := (RGBA.B shl 8) or RGBA.B;
  Result.Alpha := AlphaOpaque;
end;

function BmpRGBToFPColor(const RGB: TColorRGB) : TFPColor;
begin
  Result.Red   := (RGB.R shl 8) + RGB.R;
  Result.Green := (RGB.G shl 8) + RGB.G;
  Result.Blue  := (RGB.B shl 8) + RGB.B;
  Result.Alpha := AlphaOpaque;
end;

function Bmp16BitToFPColor(Const RGB: Word): TFPColor;
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

procedure TLazReaderDIB.FreeBufs;
begin
  FreeMem(FLineBuf);
  FLineBuf := nil;
  FreeMem(FPalette);
  FPalette := nil;
end;

function TLazReaderDIB.GetUpdateDescription: Boolean;
begin
  Result := FUpdateDescription;
end;

procedure TLazReaderDIB.SetupRead(nPalette, nRowBits: Integer; ReadPalette, AIsRLE: Boolean);
var
  ColInfo: array of TColorRGBA;
  i: Integer;
begin
  FIsRLE := AIsRLE;
  if nPalette > 0
  then begin
    GetMem(FPalette, nPalette*SizeOf(TFPColor));
    SetLength(ColInfo, nPalette);
    if ReadPalette then begin
      if  (BFI.biClrUsed > 0)
      and (BFI.biClrUsed <= Cardinal(nPalette)) // prevent buffer overflow
      then TheStream.Read(ColInfo[0], BFI.biClrUsed * SizeOf(ColInfo[0]))
      else TheStream.Read(ColInfo[0], nPalette * SizeOf(ColInfo[0]));
      for i := 0 to nPalette-1 do
        FPalette[i] := BmpRGBToFPColor(ColInfo[i]);
    end;
  end
  else begin
    { Skip palette }
    if BFI.biClrUsed > 0
    then TheStream.Position := TheStream.Position
                          + TStreamSeekType(BFI.biClrUsed*SizeOf(TColorRGBA));
  end;
  FReadSize := ((nRowBits + 31) div 32) shl 2;
  GetMem(FLineBuf, FReadSize);
end;

procedure TLazReaderDIB.ReadScanLine(Row: Integer);
//{$IFDEF FPC_BIG_ENDIAN}
//var
//  n: Integer;
//{$ENDIF}
var
  d: array[0..1] of Byte;
  Offset: Integer;
begin
  // Add here support for compressed lines. The 'readsize' is the same in the end.

  // MWE: Note: when doing so, keep in mind that the bufer is expected to be in Little Endian.
  // for better performance, the conversion is done when writeing the buffer.

  if IsRLE then
  begin
    Offset := 0;
    while True do
    begin
      TheStream.Read(d[0], 2);
      if d[0] > 0 then
      begin
        while d[0] > 0 do
        begin
          LineBuf[Offset] := d[1];
          Inc(Offset);
          Dec(d[0]);
        end;
      end else
        case d[1] of
          0, 1: break;       // End of scanline or end of bitmap
          2: raise FPImageException.Create('RLE code #2 is not supported');
        else
           begin
             TheStream.Read(LineBuf[Offset], d[1]);
             Inc(Offset, d[1]);
             if Odd(d[1]) then
               TheStream.Read(d[1], 1);      // Jump to even file position
           end;
        end;
    end;
  end else
    TheStream.Read(LineBuf[0], ReadSize);


(*
  {$ifdef FPC_BIG_ENDIAN}

  if (FBitsPerPixel = 15)
  or (FBitsPerPixel = 16)
  then begin
    for n := 0 to (ReadSize div 2) - 1 do
      PWord(LineBuf)[n] := LEtoN(PWord(LineBuf)[n]);
  end;

  {$ENDIF}
*)
end;

procedure TLazReaderDIB.SetUpdateDescription(AValue: Boolean);
begin
  FUpdateDescription := AValue;
end;

procedure TLazReaderDIB.WriteScanLine(Row: Cardinal);
// using cardinals generates compacter code
var
  Column: Cardinal;
  Color: TFPColor;
  Index: Byte;
begin
  if FMaskMode = lrmmNone
  then begin
    case FBitsPerPixel of
     1 :
       for Column := 0 to TheImage.Width - 1 do
         TheImage.colors[Column,Row] := FPalette[Ord(LineBuf[Column div 8] and ($80 shr (Column and 7)) <> 0)];
     4 :
       for Column := 0 to TheImage.Width - 1 do
         TheImage.colors[Column,Row] := FPalette[(LineBuf[Column div 2] shr (((not Column) and 1)*4)) and $0f];
     8 :
       for Column := 0 to TheImage.Width - 1 do
         TheImage.colors[Column,Row] := FPalette[LineBuf[Column]];
     15:
       for Column := 0 to TheImage.Width - 1 do
         TheImage.colors[Column,Row] := Bmp15BitToFPColor({$ifdef FPC_BIG_ENDIAN}LeToN{$endif}(PWord(LineBuf)[Column]));
     16:
       for Column := 0 to TheImage.Width - 1 do
         TheImage.colors[Column,Row] := Bmp16BitToFPColor({$ifdef FPC_BIG_ENDIAN}LeToN{$endif}(PWord(LineBuf)[Column]));
     24:
       for Column := 0 to TheImage.Width - 1 do
         TheImage.colors[Column,Row] := BmpRGBToFPColor(PColorRGB(LineBuf)[Column]);
     32:
       for Column := 0 to TheImage.Width - 1 do
         TheImage.colors[Column,Row] := BmpRGBAToFPColor(PColorRGBA(LineBuf)[Column]);
    end;
  end
  else begin
    case FBitsPerPixel of
     1 :
       for Column := 0 to TheImage.Width - 1 do
       begin
         Index := Ord(LineBuf[Column div 8] and ($80 shr (Column and 7)) <> 0);
         FImage.colors[Column,Row] := FPalette[Index];
         FImage.Masked[Column,Row] := Index = FMaskIndex;
       end;
     4 :
       for Column := 0 to TheImage.Width - 1 do
       begin
         Index := (LineBuf[Column div 2] shr (((not Column) and 1)*4)) and $0f;
         FImage.colors[Column,Row] := FPalette[Index];
         FImage.Masked[Column,Row] := Index = FMaskIndex;
       end;
     8 :
       for Column := 0 to TheImage.Width - 1 do
       begin
         Index := LineBuf[Column];
         FImage.colors[Column,Row] := FPalette[Index];
         FImage.Masked[Column,Row] := Index = FMaskIndex;
       end;
     15:
       for Column := 0 to TheImage.Width - 1 do
       begin
         Color := Bmp15BitToFPColor({$ifdef FPC_BIG_ENDIAN}LeToN{$endif}(PWord(LineBuf)[Column]));
         FImage.colors[Column,Row] := Color;
         FImage.Masked[Column,Row] := Color = FMaskColor;
       end;
     16:
       for Column := 0 to TheImage.Width - 1 do
       begin
         Color := Bmp16BitToFPColor({$ifdef FPC_BIG_ENDIAN}LeToN{$endif}(PWord(LineBuf)[Column]));
         FImage.colors[Column,Row] := Color;
         FImage.Masked[Column,Row] := Color = FMaskColor;
       end;
     24:
       for Column := 0 to TheImage.Width - 1 do
       begin
         Color := BmpRGBToFPColor(PColorRGB(LineBuf)[Column]);
         FImage.colors[Column,Row] := Color;
         FImage.Masked[Column,Row] := Color = FMaskColor;
       end;
     32:
       for Column := 0 to TheImage.Width - 1 do
       begin
         Color := BmpRGBAToFPColor(PColorRGBA(LineBuf)[Column]);
         FImage.colors[Column,Row] := Color;
         FImage.Masked[Column,Row] := Color = FMaskColor;
       end;
    end;
  end;
end;

function TLazReaderDIB._AddRef: LongInt; stdcall;
begin
  Result := -1;
end;

function TLazReaderDIB._Release: LongInt; stdcall;
begin
  Result := -1;
end;

procedure TLazReaderDIB.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  Desc: TRawImageDescription;
begin
  FImage := TheImage as TLazIntfImage;
  InternalReadHead;
  
  if FUpdateDescription
  then begin
    DefaultReaderDescription(FBFI.biWidth, FBFI.biHeight, FBFI.biBitCount, Desc);
//    if FMaskMode = lrmmNone
//    then Desc.MaskBitsPerPixel := 0;
    FImage.DataDescription := Desc;
  end;
  
  InternalReadBody;
end;

procedure TLazReaderDIB.InternalReadHead;
begin
  TheStream.Read(FBFI,SizeOf(FBFI));
  {$IFDEF FPC_BIG_ENDIAN}
  FBFI.biSize          := LEtoN(FBFI.biSize         );
  FBFI.biWidth         := LEtoN(FBFI.biWidth        );
  FBFI.biHeight        := LEtoN(FBFI.biHeight       );
  FBFI.biPlanes        := LEtoN(FBFI.biPlanes       );
  FBFI.biBitCount      := LEtoN(FBFI.biBitCount     );
  FBFI.biCompression   := LEtoN(FBFI.biCompression  );
  FBFI.biSizeImage     := LEtoN(FBFI.biSizeImage    );
  FBFI.biXPelsPerMeter := LEtoN(FBFI.biXPelsPerMeter);
  FBFI.biYPelsPerMeter := LEtoN(FBFI.biYPelsPerMeter);
  FBFI.biClrUsed       := LEtoN(FBFI.biClrUsed      );
  FBFI.biClrImportant  := LEtoN(FBFI.biClrImportant );
  {$ENDIF}
end;

function TLazReaderDIB.QueryInterface(const iid: TGuid; out obj): LongInt; stdcall;
begin
  if GetInterface(iid, obj)
  then Result := S_OK
  else Result := E_NOINTERFACE;
end;

procedure TLazReaderDIB.InternalReadBody;
type
  TPixelMasks = packed record
    R, G, B: LongWord;
  end;

const
  SWrongCombination = 'Bitmap with wrong combination of bit count (%d) and compression (%d)';
  SUnsupportedPixelMask = 'Bitmap with non-standard pixel masks not supported';

  procedure SaveTransparentColor;
  begin
    //DebugLn('SaveTransparentColor ',dbgs(UseLeftBottomAsTransparent),' ',dbgs(FBitsPerPixel));
    if FMaskMode <> lrmmAuto then Exit;

    // define transparent color: 1-8 use palette, 15-24 use fixed color
    FMaskIndex := -1;
    case FBitsPerPixel of
     1 : FMaskIndex := (LineBuf[0] shr 7) and 1;
     4 : FMaskIndex := (LineBuf[0] shr 4) and $f;
     8 : FMaskIndex := LineBuf[0];
     15: FMaskColor := Bmp15BitToFPColor({$ifdef FPC_BIG_ENDIAN}LeToN{$endif}(PWord(LineBuf)[0]));
     16: FMaskColor := Bmp16BitToFPColor({$ifdef FPC_BIG_ENDIAN}LeToN{$endif}(PWord(LineBuf)[0]));
     24: FMaskColor := BmpRGBToFPColor(PColorRGB(LineBuf)[0]);
     32: FMaskCOlor := BmpRGBAToFPColor(PColorRGBA(LineBuf)[0]);
    end;
    if FMaskIndex <> -1
    then FMaskColor := FPalette[FMaskIndex];
  end;
  
var
  PixelMasks: TPixelMasks;
  Row : Cardinal;
begin

  { This will move past any junk after the BFI header }
  TheStream.Position := TheStream.Position + TStreamSeekType(BFI.biSize-SizeOf(BFI));
  TheImage.SetSize(BFI.biWidth, Abs(BFI.biHeight));
  // Note for Abs - height can be negative if bitmap data is not stored upside-down
  FBitsPerPixel := BFI.biBitCount;

  case BFI.biBitCount of
    1: begin  { Monochrome }
      if BFI.biCompression <> BI_RGB then
        raise FPImageException.CreateFmt(SWrongCombination, [BFI.biBitCount, BFI.biCompression]);
      SetupRead(2, TheImage.Width, True, False);
    end;
    4: begin
      case BFI.biCompression of
        BI_RGB: SetupRead(16, TheImage.Width * 4, True, False);
        BI_RLE4: SetupRead(16, TheImage.Width * 4, True, True);
      else
        raise FPImageException.CreateFmt(SWrongCombination, [BFI.biBitCount, BFI.biCompression]);
      end;
    end;
    8: begin
      case BFI.biCompression of
        BI_RGB: SetupRead(256, TheImage.Width * 8, True, False);
        BI_RLE8: SetupRead(256, TheImage.Width * 8, True, True);
      else
        raise FPImageException.CreateFmt(SWrongCombination, [BFI.biBitCount, BFI.biCompression]);
      end;
    end;
    16: begin
      case BFI.biCompression of
        BI_RGB:                                          // 5-5-5
          FBitsPerPixel := 15;
        BI_BITFIELDS: begin                              // 5-5-5 or 5-6-5
          THeStream.Read(PixelMasks, SizeOf(PixelMasks));
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
            raise FPImageException.Create(SUnsupportedPixelMask);
        end;
      else
        raise FPImageException.CreateFmt(SWrongCombination, [BFI.biBitCount, BFI.biCompression]);
      end;
      SetupRead(0, TheImage.Width * 16, True, False);
    end;
    24: begin
      case BFI.biCompression of
        BI_RGB: ;
        BI_BITFIELDS: begin  // actually not a valid value
          TheStream.Read(PixelMasks, SizeOf(PixelMasks));
          {$IFDEF FPC_BIG_ENDIAN}
            PixelMasks.R := LEtoN(PixelMasks.R);
            PixelMasks.G := LEtoN(PixelMasks.G);
            PixelMasks.B := LEtoN(PixelMasks.B);
          {$ENDIF}
          if (PixelMasks.R <> $FF0000) or     // 8 red
             (PixelMasks.G <> $00FF00) or     // 8 green
             (PixelMasks.B <> $0000FF) then   // 8 blue
            raise FPImageException.Create(SUnsupportedPixelMask);
        end;
      else
        raise FPImageException.CreateFmt(SWrongCombination, [BFI.biBitCount, BFI.biCompression]);
      end;
      SetupRead(0, TheImage.Width * 24, True, False);
    end;
    32: begin
      case BFI.biCompression of
        BI_RGB: ;
        BI_BITFIELDS: begin
          TheStream.Read(PixelMasks, SizeOf(PixelMasks));
          {$IFDEF FPC_BIG_ENDIAN}
            PixelMasks.R := LEtoN(PixelMasks.R);
            PixelMasks.G := LEtoN(PixelMasks.G);
            PixelMasks.B := LEtoN(PixelMasks.B);
          {$ENDIF}
          if (PixelMasks.R <> $00FF0000) or     // 8 red
             (PixelMasks.G <> $0000FF00) or     // 8 green
             (PixelMasks.B <> $000000FF) then   // 8 blue
            raise FPImageException.Create(SUnsupportedPixelMask);
        end;
      else
        raise FPImageException.CreateFmt(SWrongCombination, [BFI.biBitCount, BFI.biCompression]);
      end;
      // force alpha description
      
      CheckAlphaDescription(TheImage);
      SetupRead(0, TheImage.Width * 32, True, False);
    end;
  else
    raise FPImageException.CreateFmt('Wrong bitmap bit count: %d', [BFI.biBitCount]);
  end;

  if (TheImage.Height = 0)
  or (TheImage.Width = 0)
  then begin
    FreeBufs;
    Exit;
  end;

  try
    Row := TheImage.Height - 1;
    ReadScanLine(Row);
    SaveTransparentColor;
    
    if BFI.biHeight > 0 then
      WriteScanLine(Row) // upside-down
    else
      WriteScanLine(TheImage.Height - 1 - Row);

    while Row > 0 do
    begin
      Dec(Row);
      ReadScanLine(Row); // Scanline in LineBuf with Size ReadSize.

      if BFI.biHeight > 0 then
        WriteScanLine(Row) // upside-down
      else
        WriteScanLine(TheImage.Height - 1 - Row);
    end;
  finally
    FreeBufs;
  end;
end;

function TLazReaderDIB.InternalCheck(Stream: TStream): boolean;
begin
  Result := True;
end;

constructor TLazReaderDIB.Create;
begin
  inherited Create;
  FMaskColor := colTransparent;
end;

destructor TLazReaderDIB.Destroy;
begin
  FreeBufs;
  inherited Destroy;
end;

{ TLazIntfImageMask }

procedure TLazIntfImageMask.SetInternalColor(x, y: integer; const Value: TFPColor);
begin
  FImage.Masked[x, y] := Value.red < $8000;
end;

function TLazIntfImageMask.GetInternalColor(x, y: integer): TFPColor;
begin
  if FImage.Masked[x, y]
  then Result := FPImage.colWhite
  else Result := FPImage.colBlack;
end;

procedure TLazIntfImageMask.SetInternalPixel(x, y: integer; Value: integer);
begin
  FImage.Masked[x, y] := Value <> 0;
end;

function TLazIntfImageMask.GetInternalPixel(x, y: integer): integer;
begin
  Result := Ord(FImage.Masked[x, y]);
end;

constructor TLazIntfImageMask.CreateWithImage(TheImage: TLazIntfImage);
begin
  FImage:=TheImage;
  inherited Create(FImage.Width,FImage.Height);
end;

{ TLazReaderIconDIB }

procedure TLazReaderIconDIB.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  Desc: TRawImageDescription;
  Row, Column: Integer;
  NewColor: TFPColor;
  BufPtr: PByte;
  MaskBit: Byte;
begin
  FImage := TheImage as TLazIntfImage;
  InternalReadHead;
  
  // Height field is doubled, to (sort of) accomodate mask
  // MWE: it shoud be safer to verify the division agains the dirinfo.height
  //      anyway I haven't encountered an icon in the wild which doesn't have a mask
  FBFI.biHeight := FBFI.biHeight div 2;
  if FUpdateDescription
  then begin
    DefaultReaderDescription(FBFI.biWidth, FBFI.biHeight, FBFI.biBitCount, Desc);
//    if FMaskMode = lrmmNone
//    then Desc.MaskBitsPerPixel := 0;
    FImage.DataDescription := Desc;
  end
  else Desc := FImage.DataDescription;
  InternalReadBody; { Now read standard bitmap }

  // Mask immediately follows unless bitmap was 32 bit - monchrome bitmap with no header
  // MWE: Correction, it seems that even 32bit icons can have a mask following
  // if BFI.biBitCount >= 32 then Exit;

  FReadSize := ((Desc.Width + 31) div 32) shl 2;
  SetupRead(2, Desc.Width, False, False);
  try
    for Row := Desc.Height - 1 downto 0 do
    begin
      ReadScanLine(Row); // Scanline in LineBuf with Size ReadSize.
      BufPtr := LineBuf;
      MaskBit := $80;
      for Column:=0 to Desc.Width - 1 do
      begin
        if BufPtr^ and MaskBit = 0
        then begin
          // opaque
          FImage.Masked[Column, Row] := False;
        end
        else begin
          // transparent
          FImage.Masked[Column, Row] := True;
          // add alpha when source wasn't 32bit
          if (Desc.AlphaPrec <> 0) and (Desc.Depth < 32)
          then begin
            NewColor := FImage.Colors[Column, Row];
            NewColor.Alpha := alphaTransparent;
            FImage.Colors[Column, Row] := NewColor;
          end;
        end;
        if MaskBit = 1
        then begin
          MaskBit := $80;
          Inc(BufPtr);
        end
        else begin
          MaskBit := MaskBit shr 1;
        end;
      end;
    end;
  finally
    FreeBufs;
  end;
end;


{ TLazReaderPNG }

procedure TLazReaderPNG.DoDecompress;
var
  Desc: TRawImageDescription;
  IsAlpha, IsGray: Boolean;
begin
  if FUpdateDescription and (theImage is TLazIntfImage)
  then begin
    // init some default

    IsGray := Header.ColorType and 3 = 0;

    // Paul: if we have a mask in the description then we need to set it manually
    // by Masked[x, y] := Color.Alpha = AlphaTransparent, but to do that we must
    // read format ourself. fpReaders set alpha instead - they dont have Masked[].
    // So if we want true description with mask we must teach our SetInternalColor
    // method to handle Alpha if mask needed (or do it any other way). In other words
    // this is now unimplemented and we'll get randomly masked image.
    // As a temporary solution I'm enable alpha description if transparent color
    // is present. This is indicated by UseTransparent property.
    // When we will handle Mask in SetInternalColor please remove UseTransparent
    // from the IsAlpha assignment.
    
    IsAlpha := (Header.ColorType and 4 <> 0) or FAlphaPalette or UseTransparent;
    
    if not IsAlpha and UseTransparent
    then Desc.Init_BPP32_B8G8R8A8_M1_BIO_TTB(Header.Width, Header.height)
    else Desc.Init_BPP32_B8G8R8A8_BIO_TTB(Header.Width, Header.height);

    if IsGray
    then Desc.Format := ricfGray;
    if not IsAlpha
    then Desc.AlphaPrec := 0;

    // check palette
    if (Header.ColorType and 1 <> 0)
    then begin
      // todo: palette
    end
    else begin
      // no palette, adjust description
      if IsGray
      then Desc.Depth := Header.BitDepth
      else if IsAlpha
      then Desc.Depth := 4 * Header.BitDepth
      else Desc.Depth := 3 * Header.BitDepth;

      case Header.BitDepth of
        1,2,4: begin
          // only gray
          Desc.BitsPerPixel := Header.BitDepth;
          Desc.RedPrec := Header.BitDepth;
          Desc.RedShift := 0;
        end;
        8: begin
          // no change
        end;
        16: begin
          Desc.BitsPerPixel := Desc.Depth;
          Desc.RedPrec := 16;
          Desc.RedShift := Desc.RedShift * 2;
          Desc.GreenPrec := 16;
          Desc.GreenShift := Desc.GreenShift * 2;
          Desc.BluePrec := 16;
          Desc.BlueShift := Desc.BlueShift * 2;
          Desc.AlphaPrec := Desc.AlphaPrec * 2; // might be zero
          Desc.AlphaShift := Desc.AlphaShift * 2;
        end;
      end;
    end;

    TLazIntfImage(theImage).DataDescription := Desc;
  end;

  inherited DoDecompress;
end;

function TLazReaderPNG.GetUpdateDescription: Boolean;
begin
  Result := FUpdateDescription;
end;

procedure TLazReaderPNG.HandleAlpha;
begin
  inherited HandleAlpha;
  FAlphaPalette := Header.ColorType = 3;
end;

procedure TLazReaderPNG.InternalRead(Str: TStream; Img: TFPCustomImage);
begin
  FAlphaPalette := False;
  inherited InternalRead(Str, Img);
end;

function TLazReaderPNG.QueryInterface(const iid: TGuid; out obj): LongInt; stdcall;
begin
  if GetInterface(iid, obj)
  then Result := S_OK
  else Result := E_NOINTERFACE;
end;

procedure TLazReaderPNG.SetUpdateDescription(AValue: Boolean);
begin
  FUpdateDescription := AValue;
end;

function TLazReaderPNG._AddRef: LongInt; stdcall;
begin
  Result := -1;
end;

function TLazReaderPNG._Release: LongInt; stdcall;
begin
  Result := -1;
end;

{ TLazReaderIcnsPart }

function TLazReaderIcnsPart.InternalCheck(Str: TStream): boolean;
begin
  // todo: write check code
  Result := True;
end;

procedure TLazReaderIcnsPart.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  Desc: TRawImageDescription;
  Element: TIconFamilyElement;
  IsMask: Boolean;
begin
  FImage := TheImage as TLazIntfImage;

  Stream.Read(Element, SizeOf(Element));
  Element.elementSize := BEtoN(Element.elementSize);
  FIconType := GetIcnsIconType(Element.elementType);
  FIconInfo := icnsIconTypeInfo[FIconType];
  IsMask := FIconType in icnsMaskTypes;

  if UpdateDescription
  then begin
    if IsMask then
    begin
      if FIconInfo.Depth = 1 then
        DefaultReaderDescription(FIconInfo.Width, FIconInfo.Height, FIconInfo.Depth, Desc)
      else
        DefaultReaderDescription(FIconInfo.Width, FIconInfo.Height, 32, Desc);
    end
    else
      DefaultReaderDescription(FIconInfo.Width, FIconInfo.Height, FIconInfo.Depth, Desc);
    if (Desc.BitsPerPixel = 32) then
      Desc.MaskBitsPerPixel := 0;
    FImage.DataDescription := Desc;
  end
  else Desc := FImage.DataDescription;

  SetupRead(FIconInfo.Width, FIconInfo.Height, FIconInfo.Depth, IsMask);

  FDataSize := Element.elementSize - SizeOf(Element);

  GetMem(FData, FDataSize);
  try
    Stream.Read(FData^, FDataSize);
    if FIconType in icnsWithAlpha then
      DoReadJpeg2000
    else
    if IsMask then
      DoReadMask
    else
    if FIconType in icnsRGB then
      DoReadRLE
    else
      DoReadRaw;
  finally
    FreeMem(FData);
    FData := nil;
  end;
end;

function TLazReaderIcnsPart.QueryInterface(const iid: TGuid; out obj): LongInt; stdcall;
begin
  if GetInterface(iid, obj)
  then Result := S_OK
  else Result := E_NOINTERFACE;
end;

function TLazReaderIcnsPart._AddRef: LongInt; stdcall;
begin
  Result := -1;
end;

function TLazReaderIcnsPart._Release: LongInt; stdcall;
begin
  Result := -1;
end;

function TLazReaderIcnsPart.GetUpdateDescription: Boolean;
begin
  Result := FUpdateDescription;
end;

procedure TLazReaderIcnsPart.SetUpdateDescription(AValue: Boolean);
begin
  FUpdateDescription := AValue;
end;

procedure TLazReaderIcnsPart.SetupRead(AWidth, AHeight, ADepth: Integer; IsMask: Boolean);
begin
  if FData <> nil then
    FreeMem(FData);
  FreeAndNil(FPalette);
  if not IsMask then
    case ADepth of
      4: FPalette := CreateVGAPalette;
      8: FPalette := Create256ColorPalette;
    end;

  FCalcSize := ((AWidth * AHeight * ADepth) shr 3);
  TheImage.SetSize(AWidth, AHeight);
end;

procedure TLazReaderIcnsPart.DoReadRaw;
var
  Row, Column: Integer;
  shift: byte;
  b: PByte;
begin
  // only 4 and 8 are stored as raw image format
  case FIconInfo.Depth of
    4 :
      begin
        b := FData;
        shift := 4;
        for Row := 0 to FIconInfo.Height - 1 do
          for Column := 0 to FIconInfo.Width - 1 do
          begin
            FImage.colors[Column, Row] := FPalette[(b^ shr shift) mod 16];
            if shift = 0 then
            begin
              shift := 4;
              inc(b);
            end
            else
              shift := 0;
          end;
      end;
    8 :
      begin
        b := FData;
        for Row := 0 to FIconInfo.Height - 1 do
          for Column := 0 to FIconInfo.Width - 1 do
          begin
            FImage.colors[Column, Row] := FPalette[b^];
            inc(b);
          end;
      end;
  end;
end;

procedure TLazReaderIcnsPart.DoReadRLE;
var
  ADecompData: PDWord;
  ARGBAData: PRGBAQuad;
  Component, Shift: Byte;
  PixelCount, j, l: Integer;
  RepeatValue: DWord;
  SourcePtr: PByte;
  DestPtr: PDWord;
begin
  // only 24 bit RGB is RLE encoded the same way as TIFF or TGA RLE
  // data is encoded channel by channel:
  // high bit = 0 => length = low 0..6 bits + 1; read length times next value
  // high bit = 1 => length = value - 125      ; read one value and repeat length times

  ADecompData := AllocMem(FCalcSize);
  DestPtr := ADecompData;

  if FIconType = iitThumbnail32BitData
  then SourcePtr := @FData[4]
  else SourcePtr := FData;

  PixelCount := FIconInfo.Height * FIconInfo.Width;

  for Component := 0 to 2 do
  begin
    DestPtr := ADecompData;
    Shift := (2 - Component) * 8;
    while DestPtr - ADecompData < PixelCount do
    begin
      l := SourcePtr^;
      inc(SourcePtr);
      if (l and $80) = 0 then // high bit = 0
      begin
        for j := 0 to l do
        begin
          DestPtr^ := DestPtr^ or (DWord(SourcePtr^) shl Shift);
          inc(SourcePtr);
          inc(DestPtr);
        end;
      end
      else
      begin                   // high bit = 1
        l := l - 126;
        RepeatValue := DWord(SourcePtr^) shl Shift;
        inc(SourcePtr);
        for j := 0 to l do
        begin
          DestPtr^ := DestPtr^ or RepeatValue;
          inc(DestPtr);
        end;
      end;
    end;
  end;

  ARGBAData := PRGBAQuad(ADecompData);
  for l := 0 to FIconInfo.Height - 1 do
    for j := 0 to FIconInfo.Width - 1 do
    begin
      FImage.Colors[j, l] :=
        FPColor(ARGBAData^.Red shl 8 or ARGBAData^.Red,
                ARGBAData^.Green shl 8 or ARGBAData^.Green,
                ARGBAData^.Blue shl 8 or ARGBAData^.Blue,
                alphaOpaque);
      inc(ARGBAData);
    end;
  FreeMem(ADecompData);
end;

procedure TLazReaderIcnsPart.DoReadJpeg2000;
begin
  // TODO: according to some research in the web we need to read jpeg 2000 data
end;

procedure TLazReaderIcnsPart.DoReadMask;
var
  Row, Column: Integer;
  shift: byte;
  b: PByte;
begin
  case FIconInfo.Depth of
    1:
      begin
        // actually here is stored 2 1-bit images, but we will get only first
        shift := 7;
        b := FData;
        for Row := 0 to FIconInfo.Height - 1 do
        begin
          for Column := 0 to FIconInfo.Width - 1 do
          begin
            FImage.colors[Column, Row] := FPColor(0, 0, 0);
            FImage.Masked[Column, Row] := (b^ shr shift) mod 2 = 0;
            if shift = 0 then
            begin
              shift := 7;
              inc(b);
            end
            else
              dec(shift);
          end;
        end;
      end;
    8:
      begin
        b := FData;
        for Row := 0 to FIconInfo.Height - 1 do
          for Column := 0 to FIconInfo.Width - 1 do
          begin
            FImage.colors[Column, Row] := FPColor(0, 0, 0, (b^ shl 8) or b^);
            inc(b);
          end;
      end;
  end;
end;

function TLazReaderIcnsPart.Create256ColorPalette: TFPPalette;
const
  CHANNELVAL: array[0..15] of Word = (
    $FFFF, $CCCC, $9999, $6666, $3333, $0000,
    $EEEE, $DDDD, $BBBB, $AAAA, $8888,
    $7777, $5555, $4444, $2222, $1111
  );

var
  rIdx, gIdx, bIdx: byte;
  PalIdx: Byte;
begin
  Result := TFPPalette.Create(256);
  PalIdx := 0;
  for rIdx := 0 to 5 do
  begin
    for gIdx := 0 to 5 do
    begin
      for bIdx := 0 to 5 do
      begin
        Result[PalIdx] := FPColor(CHANNELVAL[rIdx], CHANNELVAL[gIdx], CHANNELVAL[bIdx]);
        Inc(PalIdx);
      end;
    end;
  end;
  for rIdx := 6 to 15 do
  begin
    Result[PalIdx] := FPColor(CHANNELVAL[rIdx], 0, 0);
    Inc(PalIdx);
  end;
  for gIdx := 6 to 15 do
  begin
    Result[PalIdx] := FPColor(0, CHANNELVAL[gIdx], 0);
    Inc(PalIdx);
  end;
  for bIdx := 6 to 15 do
  begin
    Result[PalIdx] := FPColor(0, 0, CHANNELVAL[bIdx]);
    Inc(PalIdx);
  end;
  for rIdx := 6 to 15 do
  begin
    Result[PalIdx] := FPColor(CHANNELVAL[rIdx], CHANNELVAL[rIdx], CHANNELVAL[rIdx]);
    Inc(PalIdx);
  end;
  Result[PalIdx] := FPColor(0, 0, 0);
end;

constructor TLazReaderIcnsPart.Create;
begin
  inherited Create;
  FData := nil;
  FPalette := nil;
  FCalcSize := 0;
  FIconType := iitNone;
end;

destructor TLazReaderIcnsPart.Destroy;
begin
  FPalette.Free;
  FreeMem(FData);
  inherited Destroy;
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
