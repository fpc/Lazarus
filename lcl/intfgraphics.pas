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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  Classes, SysUtils, fpImage, AvgLvlTree, LCLLinux, LCLType, LCLProc, Graphics,
  GraphType;
  
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

  TLazIntfImage = class(TFPCustomImage)
  private
    FAutoCreateMask: boolean;
    FDataDescription: TRawImageDescription;
    FPixelData: PByte;
    FPixelDataSize: cardinal;
    FMaskData: PByte;
    FMaskDataSize: cardinal;
    FLineStarts: PRawImagePosition;
    FMaskLineStarts: PRawImagePosition;
    FUpdateCount: integer;
    fCreateAllDataNeeded: boolean;
    procedure SetAutoCreateMask(const AValue: boolean);
  protected
    procedure SetInternalColor(x, y: integer; const Value: TFPColor); override;
    function GetInternalColor(x, y: integer): TFPColor; override;
    procedure FreeAllData; virtual;
    procedure FreePixelData; virtual;
    procedure FreeMaskData; virtual;
    procedure CreateAllData; virtual;
    procedure CreatePixelData; virtual;
    procedure CreateMaskData; virtual;
    procedure CreateDataAndLineStarts(var Data: Pointer; var DataSize: cardinal;
                                      var TheLineStarts: PRawImagePosition;
                                      TheBitsPerPixel: cardinal;
                                      TheLineEnd: TRawImageLineEnd); virtual;
    procedure SetDataDescription(const NewDescription: TRawImageDescription); virtual;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SetSize(AWidth, AHeight: integer); override;
    function CheckDescription(const ADescription: TRawImageDescription;
                              ExceptionOnError: boolean): boolean; virtual;
    procedure GetDescriptionFromDevice(DC: HDC); virtual;
    procedure LoadFromDevice(DC: HDC); virtual;
    procedure LoadFromBitmap(Bitmap, MaskBitmap: HBitmap); virtual;
    procedure CreateBitmap(var Bitmap, MaskBitmap: HBitmap); virtual;
    procedure SetRawImage(const RawImage: TRawImage); virtual;
    procedure GetRawImage(var RawImage: TRawImage); virtual;
    procedure FillPixels(const Color: TFPColor); virtual;
    procedure GetXYDataPostion(x, y: integer; var Position: TRawImagePosition);
    procedure GetXYMaskPostion(x, y: integer; var Position: TRawImagePosition);
  public
    property PixelData: PByte read FPixelData;
    property MaskData: PByte read FMaskData;
    property DataDescription: TRawImageDescription read FDataDescription
                                                   write SetDataDescription;
    property AutoCreateMask: boolean read FAutoCreateMask write SetAutoCreateMask;
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
    FPixelToColorTree: TAvgLvlTree;
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
  

procedure CreateRawImageData(Width, Height, BitsPerPixel: cardinal;
                             LineEnd: TRawImageLineEnd;
                             var Data: Pointer; var DataSize: cardinal);
procedure CreateRawImageLineStarts(Width, Height, BitsPerPixel: cardinal;
                                   LineEnd: TRawImageLineEnd;
                                   var LineStarts: PRawImagePosition);
function GetBitsPerLine(Width, BitsPerPixel: cardinal;
                        LineEnd: TRawImageLineEnd): cardinal;
function ReadCompleteStreamToString(Str: TStream; StartSize: integer): string;
procedure ReadRawImageBits(TheData: PByte; const Position: TRawImagePosition;
                       BitsPerPixel, Prec, Shift: cardinal; var Bits: word);
procedure WriteRawImageBits(TheData: PByte; const Position: TRawImagePosition;
                       BitsPerPixel, Prec, Shift: cardinal; Bits: word);

implementation

var
  IsSpaceChar, IsNumberChar, IsHexNumberChar: array[char] of Boolean;
  MissingBits: array[0..15] of array[0..7] of word;

procedure CreateRawImageData(Width, Height, BitsPerPixel: cardinal;
  LineEnd: TRawImageLineEnd; var Data: Pointer; var DataSize: cardinal);
var
  PixelCount: cardinal;
  BitsPerLine: cardinal;
  DataBits: Int64;
begin
  // get current size
  PixelCount:=Width*Height;
  if PixelCount=0 then exit;

  // calculate BitsPerLine
  BitsPerLine:=GetBitsPerLine(Width,BitsPerPixel,LineEnd);

  // create pixels
  DataBits:=int64(BitsPerLine)*Height;
  DataSize:=cardinal((DataBits+7) shr 3);
  ReAllocMem(Data,DataSize);
  FillChar(Data^,DataSize,0);
end;

procedure CreateRawImageLineStarts(Width, Height, BitsPerPixel: cardinal;
  LineEnd: TRawImageLineEnd; var LineStarts: PRawImagePosition);
var
  PixelCount: cardinal;
  BitsPerLine: cardinal;
  CurLine: Integer;
  BytesPerLine: Integer;
  ExtraBitsPerLine: Integer;
  CurBitOffset: Cardinal;
begin
  // get current size
  PixelCount:=Width*Height;
  if PixelCount=0 then exit;

  // calculate BitsPerLine, BytesPerLine and ExtraBitsPerLine
  BitsPerLine:=GetBitsPerLine(Width,BitsPerPixel,LineEnd);
  BytesPerLine:=BitsPerLine shr 3;
  ExtraBitsPerLine:=BitsPerLine and 7;

  // create line start array
  ReAllocMem(LineStarts,Height*SizeOf(TRawImagePosition));
  LineStarts[0].Byte:=0;
  LineStarts[0].Bit:=0;
  for CurLine:=1 to Height-1 do begin
    CurBitOffset:=LineStarts[CurLine-1].Bit+ExtraBitsPerLine;
    LineStarts[CurLine].Byte:=LineStarts[CurLine-1].Byte+BytesPerLine
                                 +(CurBitOffset shr 3);
    LineStarts[CurLine].Bit:=CurBitOffset and 7;
  end;
end;

function GetBitsPerLine(Width, BitsPerPixel: cardinal;
                        LineEnd: TRawImageLineEnd): cardinal;
var
  BitsPerLine: Cardinal;
begin
  BitsPerLine:=Width*BitsPerPixel;
  case LineEnd of
  rileTight: ;
  rileByteBoundary:  BitsPerLine:=(BitsPerLine+7) and not 7;
  rileWordBoundary:  BitsPerLine:=(BitsPerLine+15) and not 15;
  rileDWordBoundary: BitsPerLine:=(BitsPerLine+31) and not 31;
  rileQWordBoundary: BitsPerLine:=(BitsPerLine+63) and not 63;
  end;
  Result:=BitsPerLine;
end;

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

procedure ReadRawImageBits(TheData: PByte;
  const Position: TRawImagePosition;
  BitsPerPixel, Prec, Shift: cardinal; var Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  OneByte: Byte;
  TwoBytes: Word;
  FourBytes: Cardinal;
begin
  PrecMask:=(Cardinal(1) shl Prec)-1;
  P:=@(TheData[Position.Byte]);
  case BitsPerPixel of
  1,2,4:
      begin
        OneByte:=P^;
        Bits:=Word((OneByte shr (Shift+Position.Bit)) and PrecMask);
      end;
  8:  begin
        OneByte:=P^;
        Bits:=Word((OneByte shr Shift) and PrecMask);
      end;
  16: begin
        TwoBytes:=PWord(P)^;
        Bits:=Word((TwoBytes shr Shift) and PrecMask);
      end;
  32: begin
        FourBytes:=PDWord(P)^;
        Bits:=Word((FourBytes shr Shift) and PrecMask);
      end;
  else
    Bits:=0;
  end;
  if Prec<16 then begin
    // add missing bits
    Bits:=(Bits shl (16-Prec));
    Bits:=Bits or MissingBits[Prec,Bits shr 13];
  end;
end;

procedure WriteRawImageBits(TheData: PByte;
  const Position: TRawImagePosition;
  BitsPerPixel, Prec, Shift: cardinal; Bits: word);
var
  P: PByte;
  PrecMask: Cardinal;
  OneByte: Byte;
  TwoBytes: Word;
  FourBytes: Cardinal;
  ShiftLeft: Integer;
begin
  P:=@(TheData[Position.Byte]);
  PrecMask:=(Cardinal(1) shl Prec)-1;
  Bits:=Bits shr (16-Prec);
  {writeln('TLazIntfImage.WriteDataBits WRITE Position=',Position.Byte,'/',Position.Bit,
    ' Shift=',Shift,' Prec=',Prec,' BitsPerPixel=',BitsPerPixel,
    ' PrecMask=',HexStr(Cardinal(PrecMask),4),
    ' Bits=',HexStr(Cardinal(Bits),4),
    '');}
  case BitsPerPixel of
  1,2,4:
      begin
        OneByte:=P^;
        ShiftLeft:=Shift+Position.Bit;
        PrecMask:=not (PrecMask shl ShiftLeft);
        OneByte:=OneByte and PrecMask; // clear old
        OneByte:=OneByte or (Bits shl ShiftLeft); // set new
        P^:=OneByte;
        //writeln('TLazIntfImage.WriteDataBits 1,2,4 Result=',HexStr(Cardinal(OneByte),2));
      end;
  8:  begin
        OneByte:=P^;
        PrecMask:=not (PrecMask shl Shift);
        OneByte:=OneByte and PrecMask; // clear old
        OneByte:=OneByte or (Bits shl Shift); // set new
        P^:=OneByte;
        //writeln('TLazIntfImage.WriteDataBits 8 Result=',HexStr(Cardinal(OneByte),2));
      end;
  16: begin
        TwoBytes:=PWord(P)^;
        PrecMask:=not (PrecMask shl Shift);
        TwoBytes:=TwoBytes and PrecMask; // clear old
        TwoBytes:=TwoBytes or (Bits shl Shift); // set new
        PWord(P)^:=TwoBytes;
        //writeln('TLazIntfImage.WriteDataBits 16 Result=',HexStr(Cardinal(TwoBytes),4));
      end;
  32: begin
        FourBytes:=PDWord(P)^;
        PrecMask:=not (PrecMask shl Shift);
        FourBytes:=FourBytes and PrecMask; // clear old
        FourBytes:=FourBytes or (Bits shl Shift); // set new
        PDWord(P)^:=FourBytes;
        //writeln('TLazIntfImage.WriteDataBits 32 Result=',HexStr(Cardinal(FourBytes),8));
      end;
  end;
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
    SetSize(0,0);
    fCreateAllDataNeeded:=false;
  finally
    EndUpdate;
  end;
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

procedure TLazIntfImage.SetInternalColor(x, y: integer; const Value: TFPColor);
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
                      Value.Red);
        WriteRawImageBits(FPixelData,Position,FDataDescription.BitsPerPixel,
                      FDataDescription.GreenPrec,FDataDescription.GreenShift,
                      Value.Green);
        WriteRawImageBits(FPixelData,Position,FDataDescription.BitsPerPixel,
                      FDataDescription.BluePrec,FDataDescription.BlueShift,
                      Value.Blue);
        if FDataDescription.AlphaPrec>0 then begin
          if FDataDescription.AlphaSeparate then begin
            if (FMaskData<>nil) then begin
              GetXYMaskPostion(x,y,MaskPosition);
              WriteRawImageBits(FMaskData,MaskPosition,
                            FDataDescription.AlphaBitsPerPixel,
                            FDataDescription.AlphaPrec,
                            FDataDescription.AlphaShift,
                            Value.Alpha);
            end else begin
              // no alpha mask
            end;
          end else
            WriteRawImageBits(FPixelData,Position,FDataDescription.BitsPerPixel,
                          FDataDescription.AlphaPrec,
                          FDataDescription.AlphaShift,
                          Value.Alpha)
        end else begin
          // no alpha
        end;
      end;

    ricfGray:
      begin
        WriteRawImageBits(FPixelData,Position,FDataDescription.BitsPerPixel,
                      FDataDescription.RedPrec,FDataDescription.RedShift,
                      Value.Red);
      end;

    else
    end;
  end else begin
    // ToDo:
  end;
end;

function TLazIntfImage.GetInternalColor(x, y: integer): TFPColor;
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
                     Result.Red);
        ReadRawImageBits(FPixelData,Position,FDataDescription.BitsPerPixel,
                     FDataDescription.GreenPrec,FDataDescription.GreenShift,
                     Result.Green);
        ReadRawImageBits(FPixelData,Position,FDataDescription.BitsPerPixel,
                     FDataDescription.BluePrec,FDataDescription.BlueShift,
                     Result.Blue);
        if FDataDescription.AlphaPrec>0 then begin
          if FDataDescription.AlphaSeparate then begin
            if (FMaskData<>nil) then begin
              GetXYMaskPostion(x,y,MaskPosition);
              ReadRawImageBits(FMaskData,MaskPosition,
                           FDataDescription.AlphaBitsPerPixel,
                           FDataDescription.AlphaPrec,
                           FDataDescription.AlphaShift,
                           Result.Alpha);
            end else begin
              // no alpha mask -> set opaque
              Result.Alpha:=high(Result.Alpha);
            end;
          end else
            ReadRawImageBits(FPixelData,Position,FDataDescription.BitsPerPixel,
                         FDataDescription.AlphaPrec,FDataDescription.AlphaShift,
                         Result.Alpha)
        end else begin
          // no alpha -> set opaque
          Result.Alpha:=high(Result.Alpha);
        end;
      end;

    ricfGray:
      begin
        ReadRawImageBits(FPixelData,Position,FDataDescription.BitsPerPixel,
                     FDataDescription.RedPrec,FDataDescription.RedShift,
                     Result.Red);
        Result.Green:=Result.Red;
        Result.Blue:=Result.Blue;
      end;
      
    else
      Result.Red:=0;
      Result.Green:=0;
      Result.Blue:=0;
      Result.Alpha:=0;
    end;
  end else begin
    // ToDo: read index, then palette
    Result.Red:=0;
    Result.Green:=0;
    Result.Blue:=0;
    Result.Alpha:=0;
  end;
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
begin
  FreeMaskData;
  if (FDataDescription.AlphaBitsPerPixel>0)
  and FDataDescription.AlphaSeparate then begin
    CreateDataAndLineStarts(FMaskData,FMaskDataSize,FMaskLineStarts,
                            FDataDescription.AlphaBitsPerPixel,
                            FDataDescription.AlphaLineEnd);
  end;
end;

procedure TLazIntfImage.CreateDataAndLineStarts(var Data: Pointer;
  var DataSize: cardinal; var TheLineStarts: PRawImagePosition;
  TheBitsPerPixel: cardinal; TheLineEnd: TRawImageLineEnd);
begin
  CreateRawImageLineStarts(Width,Height,TheBitsPerPixel,TheLineEnd,
                           TheLineStarts);
  CreateRawImageData(Width,Height,TheBitsPerPixel,TheLineEnd,Data,DataSize);
end;

constructor TLazIntfImage.Create(AWidth, AHeight: integer);
begin
  FAutoCreateMask:=true;
  inherited Create(AWidth, AHeight);
end;

destructor TLazIntfImage.Destroy;
begin
  inherited Destroy;
end;

procedure TLazIntfImage.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TLazIntfImage.EndUpdate;
begin
  dec(FUpdateCount);
  if (FUpdateCount=0) and fCreateAllDataNeeded then begin
    CreateAllData;
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
    writeln('TLazIntfImage.CheckDescription: ',Msg);
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
  Position.Bit:=BitOffset and 7;
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
  Position.Bit:=BitOffset and 7;
  inc(Position.Byte,BitOffset shr 3);
end;

procedure TLazIntfImage.LoadFromDevice(DC: HDC);
var
  ARect: TRect;
  ARawImage: TRawImage;
  DeviceSize: TPoint;
begin
  GetDeviceSize(DC,DeviceSize);
  DeviceSize.X:=20;
  DeviceSize.Y:=30;
  ARect:=Rect(0,0,DeviceSize.X,DeviceSize.Y);
  if not GetRawImageFromDevice(DC,ARect,ARawImage) then
    raise FPImageException.Create('Failed to get raw image from device');
  SetRawImage(ARawImage);
end;

procedure TLazIntfImage.LoadFromBitmap(Bitmap, MaskBitmap: HBitmap);
var
  ARect: TRect;
  ARawImage: TRawImage;
  NewDataDescription: TRawImageDescription;
begin
  if not GetBitmapRawImageDescription(Bitmap,@NewDataDescription) then
    raise FPImageException.Create('Failed to get raw image description from bitmap');
  ARect:=Rect(0,0,NewDataDescription.Width,NewDataDescription.Height);
  if not GetRawImageFromBitmap(Bitmap,MaskBitmap,ARect,ARawImage) then
    raise FPImageException.Create('Failed to get raw image from bitmap');
  SetRawImage(ARawImage);
end;

procedure TLazIntfImage.CreateBitmap(var Bitmap, MaskBitmap: HBitmap);
var
  ARawImage: TRawImage;
begin
  GetRawImage(ARawImage);
  if not CreateBitmapFromRawImage(ARawImage,Bitmap,MaskBitmap) then
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
  finally
    EndUpdate;
  end;
end;

procedure TLazIntfImage.GetRawImage(var RawImage: TRawImage);
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
end;

{ TLazReaderXPM }

type
  TXPMPixelToColorEntry = record
    Pixel: string;
    Color: TFPColor;
  end;
  PXPMPixelToColorEntry = ^TXPMPixelToColorEntry;
  
function CompareXPMPixelToColorEntries(
  Entry1, Entry2: PXPMPixelToColorEntry): integer;
begin
  Result:=CompareStr(Entry1^.Pixel,Entry2^.Pixel);
end;

procedure TLazReaderXPM.ClearPixelToColorTree;
var
  Node: TAvgLvlTreeNode;
  Entry: PXPMPixelToColorEntry;
begin
  if FPixelToColorTree=nil then exit;
  Node:=FPixelToColorTree.FindLowest;
  while Node<>nil do begin
    Entry:=PXPMPixelToColorEntry(Node.Data);
    Dispose(Entry);
    Node:=FPixelToColorTree.FindSuccessor(Node);
  end;
  FPixelToColorTree.Free;
  FPixelToColorTree:=nil;
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
              //writeln('  "',copy(Src,Line.StartPos,SrcPos-Line.StartPos),'"');
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
    //writeln('ReadHeader A Width=',FWidth,' Height=',FHeight,' ColorCount=',FColorCount,' CharsPerPixel=',FCharsPerPixel);
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
    s := copy(Src,TextStart,TextEnd-TextStart);
    if s = 'transparent' then
      Result := FPImage.clTransparent
    else if s = 'none' then
      Result := FPImage.clTransparent
    else if s = 'black' then
      result := FPImage.clBlack
    else if s = 'blue' then
      Result := FPImage.clBlue
    else if s = 'green' then
      Result := FPImage.clGreen
    else if s = 'cyan' then
      Result := FPImage.clCyan
    else if s = 'red' then
      Result := FPImage.clRed
    else if s = 'magenta' then
      Result := FPImage.clMagenta
    else if s = 'yellow' then
      Result := FPImage.clYellow
    else if s = 'white' then
      Result := FPImage.clWhite
    else if s = 'gray' then
      Result := FPImage.clGray
    else if s = 'ltgray' then
      Result := FPImage.clLtGray
    else if s = 'dkblue' then
      Result := FPImage.clDkBlue
    else if s = 'dkgreen' then
      Result := FPImage.clDkGreen
    else if s = 'dkcyan' then
      Result := FPImage.clDkCyan
    else if s = 'dkred' then
      Result := FPImage.clDkRed
    else if s = 'dkmagenta' then
      Result := FPImage.clDkMagenta
    else if s = 'dkyellow' then
      Result := FPImage.clDkYellow
    else if s = 'maroon' then
      Result := FPImage.clMaroon
    else if s = 'ltgreen' then
      Result := FPImage.clLtGreen
    else if s = 'olive' then
      Result := FPImage.clOlive
    else if s = 'navy' then
      Result := FPImage.clNavy
    else if s = 'purple' then
      Result := FPImage.clPurple
    else if s = 'teal' then
      Result := FPImage.clTeal
    else if s = 'silver' then
      Result := FPImage.clSilver
    else if s = 'lime' then
      Result := FPImage.clLime
    else if s = 'fuchsia' then
      Result := FPImage.clFuchsia
    else if s = 'aqua' then
      Result := FPImage.clAqua
    else
      Result := FPImage.clTransparent;
  end;
  
  procedure AddColor(const PixelString: string; const AColor: TFPColor);
  var
    NewEntry: PXPMPixelToColorEntry;
  begin
    {writeln('TLazReaderXPM.InternalRead.AddColor A "',PixelString,'"=',
      HexStr(Cardinal(AColor.Red),4),',',
      HexStr(Cardinal(AColor.Green),4),',',
      HexStr(Cardinal(AColor.Blue),4),',',
      HexStr(Cardinal(AColor.Alpha),4));}
    New(NewEntry);
    NewEntry^.Pixel:=PixelString;
    NewEntry^.Color:=AColor;
    if FPixelToColorTree=nil then
      FPixelToColorTree:=TAvgLvlTree.Create(@CompareXPMPixelToColorEntries);
    FPixelToColorTree.Add(NewEntry);
  end;

  procedure ReadPalette;
  var
    i: Integer;
    Line: TSrcLine;
    PixelString: String;
    ReadPos: Integer;
    ColorStart: Integer;
    ColorEnd: Integer;
    NewColor: TFPColor;
  begin
    for i:=1 to FColorCount do begin
      ReadNextLine(Line,true);
      ReadPos:=Line.StartPos;
      // read pixel string
      PixelString:=copy(Src,ReadPos,FCharsPerPixel);
      inc(ReadPos,FCharsPerPixel);
      // skip spaces
      while IsSpaceChar[Src[ReadPos]] do inc(ReadPos);
      // read 'c'
      if Src[ReadPos]<>'c' then RaiseXPMReadError('"c" expected',ReadPos);
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
      AddColor(PixelString,NewColor);
    end;
  end;
  
  procedure ReadPixels;
  var
    Entry: PXPMPixelToColorEntry;
    y: Integer;
    Line: TSrcLine;
    ReadPos: Integer;
    Node: TAvgLvlTreeNode;
    x: Integer;
    i: Integer;
    CurColor: TFPColor;
    ProgressCount: Integer;
    ContinueReading: Boolean;
  begin
    New(Entry);
    SetLength(Entry^.Pixel,FCharsPerPixel);
    Img.SetSize(FWidth,fHeight);
    try
      ProgressCount:=10000;
      for y:=0 to fHeight-1 do begin
        ReadNextLine(Line,true);
        ReadPos:=Line.StartPos;
        if Line.EndPos-Line.StartPos<FCharsPerPixel*FWidth then
          RaiseXPMReadError('line too short',ReadPos);
        for x:=0 to FWidth-1 do begin
          for i:=1 to FCharsPerPixel do begin
            Entry^.Pixel[i]:=Src[ReadPos];
            inc(ReadPos);
          end;
          Node:=FPixelToColorTree.Find(Entry);
          if Node=nil then
            RaiseXPMReadError('pixel not found',ReadPos-FCharsPerPixel);
          CurColor:=PXPMPixelToColorEntry(Node.Data)^.Color;
          {writeln('x=',x,' y=',y,' Pixel=',Entry^.Pixel,
            ' RefPixel=',PXPMPixelToColorEntry(Node.Data)^.Pixel,
            ' Color=',
            HexStr(Cardinal(CurColor.Red),4),',',
            HexStr(Cardinal(CurColor.Green),4),',',
            HexStr(Cardinal(CurColor.Blue),4),',',
            HexStr(Cardinal(CurColor.Alpha),4));}
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
    finally
      Dispose(Entry);
    end;
  end;

begin
  ClearPixelToColorTree;
  Src:=ReadCompleteStreamToString(Str,1024);
  SrcLen:=length(Src);
  SrcPos:=1;
  CurLineNumber:=1;
  LastLineStart:=1;
  ReadHeader;
  ReadPalette;
  ReadPixels;
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
  
function CompareLazAVLPaletteEntries(
  Entry1, Entry2: PLazAVLPaletteEntry): integer;
begin
  Result:=Entry1^.Palette.CompareEntries(Entry1^.Index,Entry2^.Index);
end;

function ComparePFPColorAndLazAVLPalEntry(PColor: PFPColor;
  Entry: PLazAVLPaletteEntry): integer;
begin
  Result:=Entry^.Palette.CompareColorWithEntries(PColor^,Entry^.Index);
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
    FAVLPalette:=TAvgLvlTree.Create(@CompareLazAVLPaletteEntries);
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
  inherited Destroy;
end;

function TLazAVLPalette.IndexOf(const AColor: TFPColor): integer;
var
  Node: TAvgLvlTreeNode;
begin
  if FAVLPalette<>nil then
    Node:=FAVLPalette.FindKey(@AColor,@ComparePFPColorAndLazAVLPalEntry)
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
      Result:=clTransparent;
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

//------------------------------------------------------------------------------
procedure InternalInit;
var
  c: Char;
  Prec: Integer;
  HighValue: word;
  Bits: word;
  CurShift: Integer;
begin
  for c:=Low(char) to High(char) do begin
    IsSpaceChar[c]:=c in [' ',#9,#10,#13];
    IsNumberChar[c]:=c in ['0'..'9'];
    IsHexNumberChar[c]:=c in ['0'..'9','A'..'F','a'..'f'];
  end;
  for Prec:=0 to 15 do begin
    For HighValue:=0 to 7 do begin
      // Value represents the three highest bits
      // For example:
      //   Prec=5 and the read value is %10110
      //   => Value=%101
      if Prec=0 then begin
        MissingBits[Prec,HighValue]:=0;
        continue;
      end;
      // copy the value till all missing bits are set
      // For example:
      //   Prec=5, HighValue=%110
      // => MissingBits[5,6]:=%0000011011011011
      Bits:=HighValue;
      if Prec<3 then
        // for Precision 1 and 2 the high bits are less
        Bits:=Bits shr (3-Prec);
      MissingBits[Prec,HighValue]:=0;
      CurShift:=16-Prec;
      while CurShift>0 do begin
        MissingBits[Prec,HighValue]:=
          MissingBits[Prec,HighValue] or (Bits shl CurShift);
        dec(CurShift,Prec);
      end;
    end;
  end;
end;

initialization
  InternalInit;

end.

