{  $Id$  }
{
 /***************************************************************************
                                graphtype.pp
                                ------------
                    Graphic related platform independent types
                    and utility functions.
                    Initial Revision  : Sat Feb 02 0:02:58 2002

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
unit GraphType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LCLProc;

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

type
  TGraphicsColor = -$7FFFFFFF-1..$7FFFFFFF;
  TGraphicsFillStyle = (fsSurface, fsBorder);
  TGraphicsBevelCut = (bvNone, bvLowered, bvRaised, bvSpace);

//------------------------------------------------------------------------------
// raw image data
type
  { Colorformat: Higher values means higher intensity.
    For example: Red=0 means no red, Alpha=0 means transparent }
  TRawImageColorFormat = (
    ricfRGBA,   // one pixel contains red, green, blue and alpha
                // If AlphaPrec=0 then there is no alpha.
                // Same for RedPrec, GreenPrec and BluePrec.
    ricfGray    // R=G=B. The Red stores the Gray. AlphaPrec can be >0.
    );

  TRawImageByteOrder = (
    riboLSBFirst, // least significant byte first
    riboMSBFirst  // most significant byte first
    );
    
  TRawImageBitOrder = (
    riboBitsInOrder, // Bit 0 is pixel 0
    riboReversedBits // Bit 0 is pixel 7 (Bit 1 is pixel 6, ...)
    );

  TRawImageLineEnd = (
    rileTight,         // no gap at end of lines
    rileByteBoundary,  // each line starts at byte boundary. For example:
                       // If BitsPerPixel=3 and Width=1, each line has a gap
                       // of 5 unused bits at the end.
    rileWordBoundary,  // each line starts at word (16bit) boundary
    rileDWordBoundary, // each line starts at double word (32bit) boundary
    rileQWordBoundary  // each line starts at quad word (64bit) boundary
    );

  TRawImageLineOrder = (
    riloTopToBottom, // The line 0 is the top line
    riloBottomToTop  // The line 0 is the bottom line
    );

  TRawImageDescription = record
    Format: TRawImageColorFormat;
    HasPalette: boolean; // if true, each pixel is an index in the palette
    Depth: cardinal; // used bits per pixel
    Width: cardinal;
    Height: cardinal;
    PaletteColorCount: integer;
    BitOrder: TRawImageBitOrder;
    ByteOrder: TRawImageByteOrder;
    LineOrder: TRawImageLineOrder;
    ColorCount: cardinal; // entries in color palette. Ignore when no palette.
    BitsPerPixel: cardinal; // bits per pixel. can be greater than Depth.
    LineEnd: TRawImageLineEnd;
    RedPrec: cardinal; // red precision. bits for red
    RedShift: cardinal;
    GreenPrec: cardinal;
    GreenShift: cardinal; // bitshift. Direction: from least to most signifikant
    BluePrec: cardinal;
    BlueShift: cardinal;
    AlphaPrec: cardinal;
    AlphaShift: cardinal;
    AlphaSeparate: boolean; // the alpha is stored as separate Mask
    // The next values are only valid, if there is a separate alpha mask
    AlphaBitsPerPixel: cardinal; // bits per alpha mask pixel.
    AlphaLineEnd: TRawImageLineEnd;
    AlphaBitOrder: TRawImageBitOrder;
    AlphaByteOrder: TRawImageByteOrder;
    // ToDo: add attributes for palette
  end;
  PRawImageDescription = ^TRawImageDescription;
  
  // Note: not all devices/images have all parts at any time. But if a part can
  // be applied to the device/image, the 'Description' describes its structure.
  TRawImage = record
    Description: TRawImageDescription;
    Data: PByte;
    DataSize: cardinal;
    Mask: PByte;
    MaskSize: cardinal;
    Palette: PByte;
    PaletteSize: cardinal;
  end;
  PRawImage = ^TRawImage;

  TRawImagePosition = record
    Byte: cardinal;
    Bit: cardinal;
  end;
  PRawImagePosition = ^TRawImagePosition;

const
  RawImageColorFormatNames: array[TRawImageColorFormat] of string = (
    'ricfRGBA',
    'ricfGray'
    );

  RawImageByteOrderNames: array[TRawImageByteOrder] of string = (
    'riboLSBFirst',
    'riboMSBFirst'
    );

  RawImageBitOrderNames: array[TRawImageBitOrder] of string = (
    'riboBitsInOrder',
    'riboReversedBits'
    );

  RawImageLineEndNames: array[TRawImageLineEnd] of string = (
    'rileTight',
    'rileByteBoundary',
    'rileWordBoundary',
    'rileDWordBoundary',
    'rileQWordBoundary'
    );

  RawImageLineOrderNames: array[TRawImageLineOrder] of string = (
    'riloTopToBottom',
    'riloBottomToTop'
    );
    
  DefaultByteOrder = {$IFDEF Endian_Little}riboLSBFirst{$ELSE}riboMSBFirst{$ENDIF};


function RawImageMaskIsEmpty(RawImage: PRawImage; TestPixels: boolean): boolean;
function RawImageDescriptionAsString(Desc: PRawImageDescription): string;
procedure FreeRawImageData(RawImage: PRawImage);
procedure ReleaseRawImageData(RawImage: PRawImage);

procedure CreateRawImageData(Width, Height, BitsPerPixel: cardinal;
                             LineEnd: TRawImageLineEnd;
                             var Data: Pointer; var DataSize: cardinal);
procedure CreateRawImageLineStarts(Width, Height, BitsPerPixel: cardinal;
                                   LineEnd: TRawImageLineEnd;
                                   var LineStarts: PRawImagePosition);
procedure CreateRawImageDescFromMask(SrcRawImageDesc,
  DestRawImageDesc: PRawImageDescription);
procedure GetRawImageXYPosition(RawImageDesc: PRawImageDescription;
                                LineStarts: PRawImagePosition; x, y: cardinal;
                                var Position: TRawImagePosition);
procedure ExtractRawImageRect(SrcRawImage: PRawImage; const SrcRect: TRect;
                              DestRawImage: PRawImage);
procedure ExtractRawImageDataRect(SrcRawImageDesc: PRawImageDescription;
  const SrcRect: TRect; SrcData: Pointer;
  DestRawImageDesc: PRawImageDescription;
  var DestData: Pointer; var DestDataSize: cardinal);
function GetBitsPerLine(Width, BitsPerPixel: cardinal;
                        LineEnd: TRawImageLineEnd): cardinal;
procedure ReadRawImageBits(TheData: PByte; const Position: TRawImagePosition;
                       BitsPerPixel, Prec, Shift: cardinal;
                       BitOrder: TRawImageBitOrder; var Bits: word);
procedure WriteRawImageBits(TheData: PByte; const Position: TRawImagePosition;
                       BitsPerPixel, Prec, Shift: cardinal;
                       BitOrder: TRawImageBitOrder; Bits: word);
var
  MissingBits: array[0..15] of array[0..7] of word;

implementation

uses Math;


{------------------------------------------------------------------------------
  Function: IntersectRect
  Params:  var DestRect: TRect; const SrcRect1, SrcRect2: TRect
  Returns: Boolean

  Intersects SrcRect1 and SrcRect2 into DestRect.
  Intersecting means that DestRect will be the overlapping area of SrcRect1 and
  SrcRect2. If SrcRect1 and SrcRect2 do not overlapp the Result is false, else
  true.
 ------------------------------------------------------------------------------}
function IntersectRect(var DestRect: TRect;
  const SrcRect1, SrcRect2: TRect): Boolean;
begin
  Result := False;

  // test if rectangles intersects
  Result:=(SrcRect2.Left < SrcRect1.Right)
      and (SrcRect2.Right > SrcRect1.Left)
      and (SrcRect2.Top < SrcRect1.Bottom)
      and (SrcRect2.Bottom > SrcRect1.Top);

  if Result then begin
    DestRect.Left:=Max(SrcRect1.Left,SrcRect2.Left);
    DestRect.Top:=Max(SrcRect1.Top,SrcRect2.Top);
    DestRect.Right:=Min(SrcRect1.Right,SrcRect2.Right);
    DestRect.Bottom:=Min(SrcRect1.Bottom,SrcRect2.Bottom);
  end else begin
    FillChar(DestRect,SizeOf(DestRect),0);
  end;
end;

function RawImageMaskIsEmpty(RawImage: PRawImage; TestPixels: boolean): boolean;
var
  Width: cardinal;
  Height: cardinal;
  BitsPerLine: cardinal;
  UsedBitsPerLine: cardinal;
  UnusedBitsAtEnd: cardinal;
  p: PByte;
  y: cardinal;
  x: cardinal;
  UnusedByteMask: Byte; // Alpha Bits should be all set. The Byte at line end
                        // can contain some unused bits. This mask OR byte at
                        // line end makes the unsused bits all true.
  UsedBytesPerLine: cardinal;
begin
  Result:=true;
  //DebugLn('RawImageMaskIsEmpty Quicktest: empty ',dbgs(RawImage^.Description.Width),'x',dbgs(RawImage^.Description.Height));

  // quick test
  if (RawImage^.Mask=nil) or (RawImage^.MaskSize=0)
  or (RawImage^.Description.Width=0) or (RawImage^.Description.Height=0)
  or (RawImage^.Description.AlphaPrec=0) then begin
    {$IFDEF VerboseRawImage}
    DebugLn('RawImageMaskIsEmpty Quicktest: empty');
    {$ENDIF}
    exit;
  end;
  Result:=false;

  // slow test
  if TestPixels then begin
    Width:=RawImage^.Description.Width;
    Height:=RawImage^.Description.Height;
    if RawImage^.Description.AlphaSeparate then begin
      BitsPerLine:=GetBitsPerLine(Width,RawImage^.Description.AlphaBitsPerPixel,
                                  RawImage^.Description.AlphaLineEnd);
      UsedBitsPerLine:=Width*RawImage^.Description.AlphaBitsPerPixel;
      if RawImage^.MaskSize<((Height*BitsPerLine+7) shr 3) then
        raise Exception('RawImageMaskIsEmpty Invalid MaskSize');
      if (BitsPerLine and 7)=0 then begin
        // byte boundary
        UsedBytesPerLine:=UsedBitsPerLine shr 3;
        UnusedBitsAtEnd:=(8-(UsedBitsPerLine and 7)) and 7;
        if RawImage^.Description.AlphaBitOrder=riboBitsInOrder then
          UnusedByteMask:=(($ff00 shr UnusedBitsAtEnd) and $ff)
        else
          UnusedByteMask:=(1 shl UnusedBitsAtEnd) - 1;
        p:=RawImage^.Mask;
        for y:=0 to Height-1 do begin
          // check fully used bytes in line
          for x:=0 to UsedBytesPerLine-1 do begin
            if p^<>$ff then begin
              // not all bits set -> transparent pixels found -> Mask needed
              {$IFDEF VerboseRawImage}
              DebugLn('RawImageMaskIsEmpty FullByte y=',dbgs(y),' x=',dbgs(x),' Byte=',DbgS(p^));
              {$ENDIF}
              exit;
            end;
            inc(p);
          end;
          // check partly used bytes at end of line
          if UnusedBitsAtEnd>0 then begin
            if (p^ or UnusedByteMask)<>$ff then begin
              // not all bits set -> transparent pixels found -> Mask needed
              {$IFDEF VerboseRawImage}
              DebugLn('RawImageMaskIsEmpty EdgeByte y=',dbgs(y),' x=',dbgs(x),
                ' Byte=',HexStr(Cardinal(p^),2),
                ' UnusedByteMask=',HexStr(Cardinal(UnusedByteMask),2),
                ' OR='+dbgs(p^ or UnusedByteMask),
                ' UnusedBitsAtEnd='+dbgs(UnusedBitsAtEnd),
                ' UsedBitsPerLine='+dbgs(UsedBitsPerLine),
                ' Width='+dbgs(Width),
                ' RawImage^.Description.AlphaBitsPerPixel='+dbgs(RawImage^.Description.AlphaBitsPerPixel));
              {$ENDIF}
              exit;
            end;
            inc(p);
          end;
        end;
      end else begin
        // ToDo: AlphaSeparate and rileTight
        {$IFDEF VerboseRawImage}
        DebugLn('RawImageMaskIsEmpty TODO AlphaSeparate and rileTight');
        {$ENDIF}
        exit;
      end;
    end else begin
      {$IFDEF VerboseRawImage}
      DebugLn('RawImageMaskIsEmpty TODO');
      {$ENDIF}
      exit;
    end;
    // no pixel is transparent
    Result:=true;
  end;
  {$IFDEF VerboseRawImage}
  DebugLn('RawImageMaskIsEmpty Empty=',dbgs(Result));
  {$ENDIF}
end;

function RawImageDescriptionAsString(Desc: PRawImageDescription): string;

  function BoolStr(b: boolean): string;
  begin
    if b then
      Result:='true'
    else
      Result:='false';
  end;

begin
  Result:='';
  with Desc^ do begin
    Result:=
       ' Format='+RawImageColorFormatNames[Format]
      +' HasPalette='+BoolStr(HasPalette)
      +' Depth='+IntToStr(Depth)
      +' Width='+IntToStr(Width)
      +' Height='+IntToStr(Height)
      +' PaletteColorCount='+IntToStr(PaletteColorCount)
      +' BitOrder='+RawImageBitOrderNames[BitOrder]
      +' ByteOrder='+RawImageByteOrderNames[ByteOrder]
      +' LineOrder='+RawImageLineOrderNames[LineOrder]
      +' ColorCount='+IntToStr(ColorCount)
      +' BitsPerPixel='+IntToStr(BitsPerPixel)
      +' LineEnd='+RawImageLineEndNames[LineEnd]
      +' RedPrec='+IntToStr(RedPrec)
      +' RedShift='+IntToStr(RedShift)
      +' GreenPrec='+IntToStr(GreenPrec)
      +' GreenShift='+IntToStr(GreenShift)
      +' BluePrec='+IntToStr(BluePrec)
      +' BlueShift='+IntToStr(BlueShift)
      +' AlphaSeparate='+BoolStr(AlphaSeparate)
      +' AlphaPrec='+IntToStr(AlphaPrec)
      +' AlphaShift='+IntToStr(AlphaShift)
      +' AlphaBitsPerPixel='+IntToStr(AlphaBitsPerPixel)
      +' AlphaLineEnd='+RawImageLineEndNames[AlphaLineEnd]
      +' AlphaBitOrder='+RawImageBitOrderNames[AlphaBitOrder]
      +' AlphaByteOrder='+RawImageByteOrderNames[AlphaByteOrder]
      +'';
  end;
end;

procedure FreeRawImageData(RawImage: PRawImage);
begin
  ReAllocMem(RawImage^.Data,0);
  RawImage^.DataSize:=0;
  ReAllocMem(RawImage^.Mask,0);
  RawImage^.MaskSize:=0;
  ReAllocMem(RawImage^.Palette,0);
  RawImage^.PaletteSize:=0;
end;

procedure ReleaseRawImageData(RawImage: PRawImage);
begin
  RawImage^.Data:=nil;
  RawImage^.DataSize:=0;
  RawImage^.Mask:=nil;
  RawImage^.MaskSize:=0;
  RawImage^.Palette:=nil;
  RawImage^.PaletteSize:=0;
end;

{-------------------------------------------------------------------------------
  Beware: Data is used in ReallocMem

-------------------------------------------------------------------------------}
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

procedure CreateRawImageDescFromMask(SrcRawImageDesc,
  DestRawImageDesc: PRawImageDescription);
begin
  if (not SrcRawImageDesc^.AlphaSeparate) then
    RaiseGDBException('CreateRawImageFromMask Alpha not separate');
  DestRawImageDesc^:=SrcRawImageDesc^;
  // set values
  with DestRawImageDesc^ do begin
    Format:=ricfGray;
    HasPalette:=false;
    Depth:=AlphaBitsPerPixel; // used bits per pixel
    PaletteColorCount:=0;
    ColorCount:=0; // entries in color palette. Ignore when no palette.
    BitsPerPixel:=AlphaBitsPerPixel; // bits per pixel. can be greater than Depth.
    LineEnd:=AlphaLineEnd;
    RedPrec:=AlphaPrec; // gray precision. bits for gray
    RedShift:=AlphaShift;
    AlphaPrec:=0;
    AlphaShift:=0;
    AlphaSeparate:=false; // the alpha is stored as separate Mask
    // The next values are only valid, if there is a separate alpha mask
    AlphaBitsPerPixel:=0; // bits per alpha mask pixel.
    // ToDo: add attributes for palette
  end;
end;

procedure GetRawImageXYPosition(RawImageDesc: PRawImageDescription;
  LineStarts: PRawImagePosition; x, y: cardinal;
  var Position: TRawImagePosition);
var
  BitOffset: cardinal;
begin
  if RawImageDesc^.LineOrder=riloBottomToTop then
    y:=RawImageDesc^.Height-y;
  Position:=LineStarts[y];
  BitOffset:=RawImageDesc^.BitsPerPixel*cardinal(x)+Position.Bit;
  Position.Bit:=(BitOffset and 7);
  inc(Position.Byte,BitOffset shr 3);
end;

procedure ExtractRawImageRect(SrcRawImage: PRawImage; const SrcRect: TRect;
  DestRawImage: PRawImage);
var
  SrcMaskDesc, DestMaskDesc: TRawImageDescription;
begin
  //DebugLn'ExtractRawImageRect SrcRawImage=',RawImageDescriptionAsString(@SrcRawImage^.Description),
  //  ' SrcRect=',SrcRect.Left,',',SrcRect.Top,',',SrcRect.Right,',',SrcRect.Bottom);

  // copy description
  DestRawImage^:=SrcRawImage^;
  ReleaseRawImageData(DestRawImage);
  // extract rectangle from Data
  ExtractRawImageDataRect(@SrcRawImage^.Description,SrcRect,SrcRawImage^.Data,
    @DestRawImage^.Description,DestRawImage^.Data,DestRawImage^.DataSize);
  // extract rectangle from separate Alpha
  //DebugLn'ExtractRawImageDataRect data=',DbgS(DestRawImage^.Data),' Size=',DestRawImage^.DataSize);

  if SrcRawImage^.Description.AlphaSeparate
  and (SrcRawImage^.Mask<>nil) then begin
    CreateRawImageDescFromMask(@SrcRawImage^.Description,@SrcMaskDesc);
    //DebugLn'ExtractRawImageRect Mask SrcRawImage=',RawImageDescriptionAsString(@SrcMaskDesc));
    ExtractRawImageDataRect(@SrcMaskDesc,SrcRect,SrcRawImage^.Mask,
      @DestMaskDesc,DestRawImage^.Mask,DestRawImage^.MaskSize);
  end;
end;

{-------------------------------------------------------------------------------
  Beware: DestData is used in ReallocMem

-------------------------------------------------------------------------------}
procedure ExtractRawImageDataRect(SrcRawImageDesc: PRawImageDescription;
  const SrcRect: TRect; SrcData: Pointer;
  DestRawImageDesc: PRawImageDescription;
  var DestData: Pointer; var DestDataSize: cardinal);
var
  SrcWidth: cardinal;
  SrcHeight: cardinal;
  MaxRect, SourceRect: TRect;
  y: Integer;
  TotalWidth: cardinal;
  TotalHeight: cardinal;
  BitsPerPixel: cardinal;
  LineEnd: TRawImageLineEnd;
  SrcLineStarts, DestLineStarts: PRawImagePosition;
  SrcLineStartPosition, SrcLineEndPosition: TRawImagePosition;
  DestLineStartPosition: TRawImagePosition;
  w: Word;
  Shift: LongWord;
  SrcPos: PByte;
  DestPos: PByte;
  ByteCount: LongWord;
  x: Integer;
begin
  // init
  DestRawImageDesc^:=SrcRawImageDesc^;

  // intersect SrcRect
  TotalWidth:=SrcRawImageDesc^.Width;
  TotalHeight:=SrcRawImageDesc^.Height;
  BitsPerPixel:=SrcRawImageDesc^.BitsPerPixel;
  LineEnd:=SrcRawImageDesc^.LineEnd;
  MaxRect:=Bounds(0,0,TotalWidth,TotalHeight);
  IntersectRect(SourceRect,MaxRect,SrcRect);
  if (SourceRect.Right<=SourceRect.Left)
  or (SourceRect.Bottom<=SourceRect.Top) then exit;
  SrcWidth:=SourceRect.Right-SourceRect.Left;
  SrcHeight:=SourceRect.Bottom-SourceRect.Top;

  // allocate Data
  DestRawImageDesc^.Width:=SrcWidth;
  DestRawImageDesc^.Height:=SrcHeight;
  //DebugLn'ExtractRawImageDataRect Src=',SrcWidth,',',SrcHeight,' DestData=',DbgS(DestData));
  CreateRawImageData(SrcWidth,SrcHeight,BitsPerPixel,LineEnd,
                     DestData,DestDataSize);
  //DebugLn'ExtractRawImageDataRect data=',DbgS(DestData),' Size=',DestDataSize);
  if (SrcWidth=TotalWidth) and (TotalHeight=SrcHeight) then begin
    // copy whole source
    System.Move(SrcData^,DestData^,DestDataSize);
    exit;
  end;

  // calculate line starts for source
  SrcLineStarts:=nil;
  CreateRawImageLineStarts(TotalWidth,TotalHeight,BitsPerPixel,LineEnd,
                           SrcLineStarts);
  // calculate line starts for destination
  DestLineStarts:=nil;
  CreateRawImageLineStarts(SrcWidth,SrcHeight,BitsPerPixel,LineEnd,
                           DestLineStarts);
  // copy
  for y:=0 to SrcHeight-1 do begin
    GetRawImageXYPosition(SrcRawImageDesc,SrcLineStarts,
                          SourceRect.Left,y+SourceRect.Top,
                          SrcLineStartPosition);
    GetRawImageXYPosition(SrcRawImageDesc,SrcLineStarts,
                          SourceRect.Right,y+SourceRect.Top,
                          SrcLineEndPosition);
    GetRawImageXYPosition(DestRawImageDesc,DestLineStarts,0,y,
                          DestLineStartPosition);
    //DebugLn'ExtractRawImageDataRect A y=',y,' SrcByte=',SrcLineStartPosition.Byte,' SrcBit=',SrcLineStartPosition.Bit,
    //' DestByte=',DestLineStartPosition.Byte,' DestBit=',DestLineStartPosition.Bit);
    if (SrcLineStartPosition.Bit=0)
    and (DestLineStartPosition.Bit=0) then begin
      // copy bytes
      ByteCount:=SrcLineEndPosition.Byte-SrcLineStartPosition.Byte;
      if SrcLineEndPosition.Bit>0 then
        inc(ByteCount);
      //DebugLn'ExtractRawImageDataRect B ByteCount=',ByteCount);
      System.Move(
        Pointer(PtrUInt(SrcData)+SrcLineStartPosition.Byte)^,
        Pointer(PtrUInt(DestData)+DestLineStartPosition.Byte)^,
        ByteCount);
    end else if (DestLineStartPosition.Bit=0) then begin
      // copy and move bits
      ByteCount:=((SrcWidth*BitsPerPixel)+7) shr 3;
      Shift:=8-SrcLineStartPosition.Bit;
      SrcPos:=PByte(PtrUInt(SrcData)+SrcLineStartPosition.Byte);
      DestPos:=PByte(PtrUInt(DestData)+DestLineStartPosition.Byte);
      for x:=0 to ByteCount-1 do begin
        w:=PWord(SrcPos)^;
        w:=w shr Shift;
        DestPos^:=byte(w);
        inc(SrcPos);
        inc(DestPos);
      end;
    end else begin
      DebugLn('ToDo: ExtractRawImageRect DestLineStartPosition.Bit>0');
      break;
    end;
  end;
  // clean up
  FreeMem(SrcLineStarts);
  FreeMem(DestLineStarts);
end;

procedure CreateRawImageLineStarts(Width, Height, BitsPerPixel: cardinal;
  LineEnd: TRawImageLineEnd; var LineStarts: PRawImagePosition);
// LineStarts is recreated, so make sure it is nil or a valid mem
var
  PixelCount: cardinal;
  BitsPerLine: cardinal;
  CurLine: cardinal;
  BytesPerLine: cardinal;
  ExtraBitsPerLine: cardinal;
  CurBitOffset: cardinal;
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
  rileByteBoundary:  BitsPerLine:=(BitsPerLine+7) and not cardinal(7);
  rileWordBoundary:  BitsPerLine:=(BitsPerLine+15) and not cardinal(15);
  rileDWordBoundary: BitsPerLine:=(BitsPerLine+31) and not cardinal(31);
  rileQWordBoundary: BitsPerLine:=(BitsPerLine+63) and not cardinal(63);
  end;
  Result:=BitsPerLine;
end;

procedure ReadRawImageBits(TheData: PByte;
  const Position: TRawImagePosition;
  BitsPerPixel, Prec, Shift: cardinal; BitOrder: TRawImageBitOrder;
  var Bits: word);
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
        if BitOrder=riboBitsInOrder then
          Bits:=Word(cardinal(OneByte shr (Shift+Position.Bit)) and PrecMask)
        else
          Bits:=Word(cardinal(OneByte shr (Shift+7-Position.Bit)) and PrecMask);
      end;
  8:  begin
        OneByte:=P^;
        Bits:=Word(cardinal(OneByte shr Shift) and PrecMask);
      end;
  16: begin
        TwoBytes:=PWord(P)^;
        Bits:=Word(cardinal(TwoBytes shr Shift) and PrecMask);
      end;
  32: begin
        FourBytes:=PDWord(P)^;
        Bits:=Word(cardinal(FourBytes shr Shift) and PrecMask);
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
  BitsPerPixel, Prec, Shift: cardinal; BitOrder: TRawImageBitOrder; Bits: word);
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
  {DebugLn'WriteDataBits WRITE Position=',Position.Byte,'/',Position.Bit,
    ' Shift=',Shift,' Prec=',Prec,' BitsPerPixel=',BitsPerPixel,
    ' PrecMask=',DbgS(PrecMask),
    ' Bits=',DbgS(Bits),
    '');}
  case BitsPerPixel of
  1,2,4:
      begin
        OneByte:=P^;
        if BitOrder=riboBitsInOrder then
          ShiftLeft:=Shift+Position.Bit
        else
          ShiftLeft:=Shift+7-Position.Bit;
        PrecMask:=not (PrecMask shl ShiftLeft);
        OneByte:=OneByte and PrecMask; // clear old
        OneByte:=OneByte or (Bits shl ShiftLeft); // set new
        P^:=OneByte;
        //DebugLn'WriteDataBits 1,2,4 Result=',DbgS(OneByte));
      end;
  8:  begin
        OneByte:=P^;
        PrecMask:=not (PrecMask shl Shift);
        OneByte:=OneByte and PrecMask; // clear old
        OneByte:=OneByte or (Bits shl Shift); // set new
        P^:=OneByte;
        //DebugLn'WriteDataBits 8 Result=',DbgS(OneByte));
      end;
  16: begin
        TwoBytes:=PWord(P)^;
        PrecMask:=not (PrecMask shl Shift);
        TwoBytes:=TwoBytes and PrecMask; // clear old
        TwoBytes:=TwoBytes or (Bits shl Shift); // set new
        PWord(P)^:=TwoBytes;
        //DebugLn'WriteDataBits 16 Result=',DbgS(TwoBytes));
      end;
  32: begin
        FourBytes:=PDWord(P)^;
        PrecMask:=not (PrecMask shl Shift);
        FourBytes:=FourBytes and PrecMask; // clear old
        FourBytes:=FourBytes or cardinal(Bits shl Shift); // set new
        PDWord(P)^:=FourBytes;
        //DebugLn'WriteDataBits 32 Result=',DbgS(FourBytes));
      end;
  end;
end;

//------------------------------------------------------------------------------
procedure InternalInit;
var
  Prec: Integer;
  HighValue: word;
  Bits: word;
  CurShift: Integer;
begin
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

{
  $Log$
  Revision 1.40  2005/03/07 21:59:44  vincents
  changed hexstr(cardinal()) for pointers to dbgs() and other 64-bits fixes   from Peter Vreman

  Revision 1.39  2005/03/05 13:09:27  mattias
  added BitOrder test for RawImageMaskEmpty

  Revision 1.38  2005/03/05 12:08:49  mattias
  OI now locks during doubleclick and synedit now uses only MouseCapture instead of heuristic

  Revision 1.37  2005/02/07 15:27:11  micha
  fix calculation of unusedbytemask

  Revision 1.36  2005/01/28 20:33:59  peter
    * correctly calculate UnusedBitsAtEnd if there are no unused bits
      to prevent overflow

  Revision 1.35  2005/01/17 11:53:39  mattias
  added showing all four sides to AnchorEditor

  Revision 1.34  2004/12/11 01:28:58  mattias
  implemented bvSpace of TBevelCut

  Revision 1.33  2004/08/13 20:40:27  mattias
  fixed DebugLn for VerboseRawImage

  Revision 1.32  2004/06/28 17:03:37  mattias
  clean up

  Revision 1.31  2004/05/11 11:42:26  mattias
  replaced writeln by debugln

  Revision 1.30  2004/04/02 19:39:46  mattias
  fixed checking empty mask raw image

  Revision 1.29  2004/03/30 17:45:31  mattias
  fixed FindEndOfExpression for bogus statements

  Revision 1.28  2004/03/28 12:49:22  mattias
  implemented mask merge and extraction for raw images

  Revision 1.27  2004/02/28 00:34:35  mattias
  fixed CreateComponent for buttons, implemented basic Drag And Drop

  Revision 1.26  2004/02/21 01:01:03  mattias
  added uninstall popupmenuitem to package graph explorer

  Revision 1.25  2004/02/19 05:07:16  mattias
  CreateBitmapFromRawImage now creates mask only if needed

  Revision 1.24  2004/02/09 19:52:52  mattias
  implemented ByteOrder for TLazIntfImage and added call of to LM_SETFONT

  Revision 1.23  2003/12/06 19:20:46  mattias
  codecompletion: forward proc body position now block sensitive

  Revision 1.22  2003/11/28 11:25:49  mattias
  added BitOrder for RawImages

  Revision 1.21  2003/11/26 21:30:19  mattias
  reduced unit circles, fixed fpImage streaming

  Revision 1.20  2003/08/25 16:43:32  mattias
  moved many graphics types form graphtype.pp to graphics.pp

  Revision 1.19  2003/08/19 12:23:23  mattias
  moved types from graphtype.pp back to graphics.pp

  Revision 1.18  2003/07/07 13:19:08  mattias
  added raw image examples

  Revision 1.17  2003/07/04 22:06:49  mattias
  implemented interface graphics

  Revision 1.16  2003/07/03 18:10:55  mattias
  added fontdialog options to win32 intf from Wojciech Malinowski

  Revision 1.15  2003/07/02 10:02:51  mattias
  fixed TPaintStruct

  Revision 1.14  2003/07/01 15:37:03  mattias
  fixed exception handling

  Revision 1.13  2003/07/01 13:49:36  mattias
  clean up

  Revision 1.12  2003/07/01 09:29:51  mattias
  attaching menuitems topdown

  Revision 1.11  2003/06/30 14:58:29  mattias
  implemented multi file add to package editor

  Revision 1.10  2002/08/18 04:57:01  mattias
  fixed csDashDot

  Revision 1.9  2003/01/27 13:49:16  mattias
  reduced speedbutton invalidates, added TCanvas.Frame

  Revision 1.8  2002/12/12 17:47:45  mattias
  new constants for compatibility

  Revision 1.7  2002/09/27 20:52:21  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>

  Here is the run down of what it includes -

   -Vasily Volchenko's Updated Russian Localizations

   -improvements to GTK Styles/SysColors
   -initial GTK Palette code - (untested, and for now useless)

   -Hint Windows and Modal dialogs now try to stay transient to
    the main program form, aka they stay on top of the main form
    and usually minimize/maximize with it.

   -fixes to Form BorderStyle code(tool windows needed a border)

   -fixes DrawFrameControl DFCS_BUTTONPUSH to match Win32 better
    when flat

   -fixes DrawFrameControl DFCS_BUTTONCHECK to match Win32 better
    and to match GTK theme better. It works most of the time now,
    but some themes, noteably Default, don't work.

   -fixes bug in Bitmap code which broke compiling in NoGDKPixbuf
    mode.

   -misc other cleanups/ fixes in gtk interface

   -speedbutton's should now draw correctly when flat in Win32

   -I have included an experimental new CheckBox(disabled by
    default) which has initial support for cbGrayed(Tri-State),
    and WordWrap, and misc other improvements. It is not done, it
    is mostly a quick hack to test DrawFrameControl
    DFCS_BUTTONCHECK, however it offers many improvements which
    can be seen in cbsCheck/cbsCrissCross (aka non-themed) state.

   -fixes Message Dialogs to more accurately determine
    button Spacing/Size, and Label Spacing/Size based on current
    System font.
   -fixes MessageDlgPos, & ShowMessagePos in Dialogs
   -adds InputQuery & InputBox to Dialogs

   -re-arranges & somewhat re-designs Control Tabbing, it now
    partially works - wrapping around doesn't work, and
    subcontrols(Panels & Children, etc) don't work. TabOrder now
    works to an extent. I am not sure what is wrong with my code,
    based on my other tests at least wrapping and TabOrder SHOULD
    work properly, but.. Anyone want to try and fix?

   -SynEdit(Code Editor) now changes mouse cursor to match
    position(aka over scrollbar/gutter vs over text edit)

   -adds a TRegion property to Graphics.pp, and Canvas. Once I
    figure out how to handle complex regions(aka polygons) data
    properly I will add Region functions to the canvas itself
    (SetClipRect, intersectClipRect etc.)

   -BitBtn now has a Stored flag on Glyph so it doesn't store to
    lfm/lrs if Glyph is Empty, or if Glyph is not bkCustom(aka
    bkOk, bkCancel, etc.) This should fix most crashes with older
    GDKPixbuf libs.

  Revision 1.6  2002/09/03 08:07:19  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.5  2002/08/06 09:32:48  lazarus
  MG: moved TColor definition to graphtype.pp and registered TColor names

  Revision 1.4  2002/06/04 15:17:21  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.3  2002/05/10 06:05:50  lazarus
  MG: changed license to LGPL

  Revision 1.2  2002/03/08 16:16:55  lazarus
  MG: fixed parser of end blocks in initialization section added label sections

  Revision 1.1  2002/02/03 00:24:00  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.21  2002/01/02 15:24:58  lazarus
  MG: added TCanvas.Polygon and TCanvas.Polyline

  Revision 1.20  2002/01/02 12:10:01  lazarus
  MG: fixed typo

  Revision 1.19  2001/12/28 11:41:50  lazarus
  MG: added TCanvas.Ellipse, TCanvas.Pie

  Revision 1.18  2001/12/21 18:16:59  lazarus
  Added TImage class
  Shane

  Revision 1.17  2001/11/12 22:12:57  lazarus
  MG: fixed parser: multiple brackets, nil, string[]

  Revision 1.16  2001/11/09 19:14:23  lazarus
  HintWindow changes
  Shane

  Revision 1.15  2001/10/25 19:02:18  lazarus
  MG: fixed parsing constants with OR, AND, XOR, MOD, DIV, SHL, SHR

  Revision 1.14  2001/10/24 00:35:55  lazarus
  MG: fixes for fpc 1.1: range check errors

  Revision 1.13  2001/09/30 08:34:49  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.12  2001/08/05 10:14:50  lazarus
  MG: removed double props in OI, small bugfixes

  Revision 1.11  2001/06/26 00:08:35  lazarus
  MG: added code for form icons from Rene E. Beszon

  Revision 1.10  2001/06/04 09:32:17  lazarus
  MG: fixed bugs and cleaned up messages

  Revision 1.9  2001/03/21 00:20:29  lazarus
  MG: fixed memory leaks

  Revision 1.7  2001/03/19 14:00:50  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.6  2001/03/05 14:20:04  lazarus
  added streaming to tgraphic, added tpicture

  Revision 1.5  2001/02/04 19:23:26  lazarus
  Goto dialog added
  Shane

  Revision 1.4  2001/02/04 18:24:41  lazarus
  Code cleanup
  Shane

  Revision 1.3  2001/01/31 21:16:45  lazarus
  Changed to TCOmboBox focusing.
  Shane

  Revision 1.2  2000/08/10 18:56:23  lazarus
  Added some winapi calls.
  Most don't have code yet.
  SetTextCharacterExtra
  CharLowerBuff
  IsCharAlphaNumeric
  Shane

  Revision 1.1  2000/07/13 10:28:23  michael
  + Initial import

  Revision 1.46  2000/05/08 15:56:58  lazarus
  MWE:
    + Added support for mwedit92 in Makefiles
    * Fixed bug # and #5 (Fillrect)
    * Fixed labelsize in ApiWizz
    + Added a call to the resize event in WMWindowPosChanged

  Revision 1.45  2000/03/30 18:07:53  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.44  2000/03/21 23:47:33  lazarus
  MWE:
    + Added TBitmap.MaskHandle & TGraphic.Draw & TBitmap.Draw

  Revision 1.43  2000/03/16 23:58:46  lazarus
  MWE:
    Added TPixmap for XPM support

  Revision 1.42  2000/03/15 20:15:31  lazarus
  MOdified TBitmap but couldn't get it to work
  Shane

  Revision 1.41  2000/03/10 13:13:37  lazarus
  *** empty log message ***

  Revision 1.40  2000/03/09 23:44:03  lazarus
  MWE:
    * Fixed colorcache
    * Fixed black window in new editor
    ~ Did some cosmetic stuff

  From Peter Dyson <peter@skel.demon.co.uk>:
    + Added Rect api support functions
    + Added the start of ScrollWindowEx

  Revision 1.39  2000/03/08 23:57:38  lazarus
  MWE:
    Added SetSysColors
    Fixed TEdit text bug (thanks to hans-joachim ott <hjott@compuserve.com>)
    Finished GetKeyState
    Added changes from Peter Dyson <peter@skel.demon.co.uk>
    - a new GetSysColor
    - some improvements on ExTextOut

  Revision 1.38  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.37  2000/01/26 19:16:24  lazarus
  Implemented TPen.Style properly for GTK. Done SelectObject for pen objects.
  Misc bug fixes.
  Corrected GDK declaration for gdk_gc_set_slashes.

  Revision 1.36  2000/01/17 20:36:25  lazarus
  Fixed Makefile again.
  Made implementation of TScreen and screen info saner.
  Began to implemented DeleteObject in GTKWinAPI.
  Fixed a bug in GDI allocation which in turn fixed A LOT of other bugs :-)

  Revision 1.35  1999/12/14 22:05:37  lazarus
  More changes for TToolbar
  Shane

  Revision 1.34  1999/12/02 19:00:59  lazarus
  MWE:
    Added (GDI)Pen
    Changed (GDI)Brush
    Changed (GDI)Font (color)
    Changed Canvas to use/create pen/brush/font
    Hacked mwedit to allow setting the number of chars (till it get a WM/LM_SIZE event)
    The editor shows a line !

  Revision 1.33  1999/11/29 00:46:47  lazarus
  MWE:
    Added TBrush as gdiobject
    commented out some more mwedit MWE_FPC ifdefs

  Revision 1.32  1999/11/25 23:45:08  lazarus
  MWE:
    Added font as GDIobject
    Added some API testcode to testform
    Commented out some more IFDEFs in mwCustomEdit

  Revision 1.31  1999/11/19 01:09:43  lazarus
  MWE:
    implemented TCanvas.CopyRect
    Added StretchBlt
    Enabled creation of TCustomControl.Canvas
    Added a temp hack in TWinControl.Repaint to get a LM_PAINT

  Revision 1.30  1999/11/18 00:13:08  lazarus
  MWE:
    Partly Implemented SelectObject
    Added  ExTextOut
    Added  GetTextExtentPoint
    Added  TCanvas.TextExtent/TextWidth/TextHeight
    Added  TSize and HPEN

  Revision 1.29  1999/11/17 01:16:39  lazarus
  MWE:
    Added some more API stuff
    Added an initial TBitmapCanvas
    Added some DC stuff
    Changed and commented out, original gtk linedraw/rectangle code. This
      is now called through the winapi wrapper.

  Revision 1.28  1999/11/09 17:19:54  lazarus
  added the property PITCH to TFONT.
  Shane

  Revision 1.26  1999/11/05 17:48:17  lazarus
  Added a mwedit1 component to lazarus (MAIN.PP)
  It crashes on create.
  Shane

  Revision 1.25  1999/11/01 01:28:29  lazarus
  MWE: Implemented HandleNeeded/CreateHandle/CreateWND
       Now controls are created on demand. A call to CreateComponent shouldn't
       be needed. It is now part of CreateWnd

  Revision 1.24  1999/10/28 17:17:42  lazarus
  Removed references to FCOmponent.
  Shane

  Revision 1.23  1999/10/25 17:38:52  lazarus
  More stuff added for compatability.  Most stuff added was put in the windows.pp file.  CONST scroll bar messages and such.  2 functions were also added to that unit that needs to be completed.
  Shane

  Revision 1.22  1999/10/22 21:01:51  lazarus

        Removed calls to InterfaceObjects except for controls.pp. Commented
        out any gtk depend lines of code.     MAH

  Revision 1.21  1999/10/19 21:16:23  lazarus
  TColor added to graphics.pp

  Revision 1.20  1999/10/18 07:32:42  lazarus
  Added definitions for Load methods in the TBitmap class. The
  methods have not been implemented yet. They need to be implemented.   CAW

  Revision 1.19  1999/09/26 16:58:01  lazarus
  MWE: Added TBitMap.Mask method

  Revision 1.18  1999/08/26 23:36:02  peter
    + paintbox
    + generic keydefinitions and gtk conversion
    * gtk state -> shiftstate conversion

  Revision 1.17  1999/08/25 18:53:02  lazarus
  Added Canvas.pixel property which allows
  the user to get/set the pixel color.  This will be used in the editor
  to create the illusion of the cursor by XORing the pixel with black.

  Shane

  Revision 1.16  1999/08/20 15:44:37  lazarus
  TImageList changes added from Marc Weustink

  Revision 1.15  1999/08/17 16:46:25  lazarus
  Slight modification to Editor.pp
  Shane

  Revision 1.14  1999/08/16 20:48:03  lazarus
  Added a changed event for TFOnt and code to get the average size of the font.  Doesn't seem to work very well yet.
  The "average size" code is found in gtkobject.inc.

  Revision 1.13  1999/08/16 15:48:49  lazarus
  Changes by file:
       Control: TCOntrol-Function GetRect added
                         ClientRect property added
                TImageList - Added Count
                TWinControl- Function Focused added.
      Graphics: TCanvas - CopyRect added - nothing finished on it though
                          Draw added - nothing finiushed on it though
                clbtnhighlight and clbtnshadow added.  Actual color values not right.
               IMGLIST.PP and IMGLIST.INC files added.

   A few other minor changes for compatability added.

    Shane

  Revision 1.12  1999/08/13 19:55:47  lazarus
  TCanvas.MoveTo added for compatability.

  Revision 1.11  1999/08/13 19:51:07  lazarus
  Minor changes for compatability made.

  Revision 1.10  1999/08/11 20:41:33  lazarus

  Minor changes and additions made.  Lazarus may not compile due to these changes

  Revision 1.9  1999/08/02 01:13:33  lazarus
  Added new colors and corrected BTNFACE
  Need the TSCrollbar class to go further with the editor.
  Mouse doesn't seem to be working correctly yet when I click on the editor window

  Revision 1.8  1999/08/01 21:46:26  lazarus
  Modified the GETWIDTH and GETHEIGHT of TFOnt so you can use it to calculate the length in Pixels of a string.  This is now used in the editor.

  Shane

  Revision 1.7  1999/07/31 06:39:26  lazarus

       Modified the IntCNSendMessage3 to include a data variable. It isn't used
       yet but will help in merging the Message2 and Message3 features.

       Adjusted TColor routines to match Delphi color format

       Added a TGdkColorToTColor routine in gtkproc.inc

       Finished the TColorDialog added to comDialog example.        MAH

 }
