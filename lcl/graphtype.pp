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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  TGraphicsFillStyle = (
    fsSurface, // fill till the color (it fills all execpt this color)
    fsBorder   // fill this color (it fills only conneted pixels of this color)
    );
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
    DataSize: PtrUInt;
    Mask: PByte;
    MaskSize: PtrUInt;
    Palette: PByte;
    PaletteSize: PtrUInt;
  end;
  PRawImage = ^TRawImage;

  TRawImagePosition = record
    Byte: PtrUInt;
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
                             var Data: Pointer; var DataSize: PtrUInt);
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
                             var DestData: Pointer; var DestDataSize: PtrUInt);
function GetBytesPerLine(Width, BitsPerPixel: cardinal;
                         LineEnd: TRawImageLineEnd): PtrUInt;
function GetBitsPerLine(Width, BitsPerPixel: cardinal;
                        LineEnd: TRawImageLineEnd): PtrUInt;
procedure ReadRawImageBits(TheData: PByte; const Position: TRawImagePosition;
                       BitsPerPixel, Prec, Shift: cardinal;
                       BitOrder: TRawImageBitOrder; var Bits: word);
procedure WriteRawImageBits(TheData: PByte; const Position: TRawImagePosition;
                       BitsPerPixel, Prec, Shift: cardinal;
                       BitOrder: TRawImageBitOrder; Bits: word);
procedure ReAlignRawImageLines(var Data: Pointer; var Size: PtrUInt;
  Width, Height, BitsPerPixel: cardinal;
  var OldLineEnd: TRawImageLineEnd; NewLineEnd: TRawImageLineEnd);

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
      if RawImage^.MaskSize<PtrUInt((Height*BitsPerLine+7) shr 3) then
        raise Exception.Create('RawImageMaskIsEmpty Invalid MaskSize');
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
      +' BytesPerLine='+IntToStr(GetBytesPerLine(Width,BitsPerPixel,LineEnd))
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
      +' AlphaBytesPerLine='+IntToStr(GetBytesPerLine(Width,AlphaBitsPerPixel,AlphaLineEnd))
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
  LineEnd: TRawImageLineEnd; var Data: Pointer; var DataSize: PtrUInt);
var
  PixelCount: PtrUInt;
  BitsPerLine: PtrUInt;
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
  var DestData: Pointer; var DestDataSize: PtrUInt);
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
  ByteCount: PtrUInt;
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

function GetBytesPerLine(Width, BitsPerPixel: cardinal;
  LineEnd: TRawImageLineEnd): PtrUInt;
begin
  Result:=(GetBitsPerLine(Width,BitsPerPixel,LineEnd)+7) shr 3;
end;

function GetBitsPerLine(Width, BitsPerPixel: cardinal;
                        LineEnd: TRawImageLineEnd): PtrUInt;
var
  BitsPerLine: PtrUInt;
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

procedure ReAlignRawImageLines(var Data: Pointer; var Size: PtrUInt;
  Width, Height, BitsPerPixel: cardinal;
  var OldLineEnd: TRawImageLineEnd; NewLineEnd: TRawImageLineEnd);
var
  OldBytesPerLine: PtrUInt;
  OldSize: PtrUInt;
  NewBytesPerLine: PtrUInt;
  NewSize: PtrUInt;
  y: Integer;
  OldPos: Pointer;
  NewPos: Pointer;
begin
  if OldLineEnd=NewLineEnd then exit;
  if (Width=0) or (Height=0) then exit;
  OldBytesPerLine:=GetBytesPerLine(Width,BitsPerPixel,OldLineEnd);
  OldSize:=OldBytesPerLine*PtrUInt(Height);
  if OldSize<>Size then
    RaiseGDBException('ReAlignRawImageLines OldSize<>Size');
  NewBytesPerLine:=GetBytesPerLine(Width,BitsPerPixel,NewLineEnd);
  NewSize:=NewBytesPerLine*PtrUInt(Height);
  //DebugLn(['ReAlignRawImageLines OldBytesPerLine=',OldBytesPerLine,' NewBytesPerLine=',NewBytesPerLine]);
  
  // enlarge before
  if OldSize<NewSize then
    ReAllocMem(Data,NewSize);

  // move data
  OldPos:=Data;
  NewPos:=Data;
  if OldBytesPerLine>NewBytesPerLine then begin
    // compress
    for y:=0 to Height-1 do begin
      System.Move(OldPos^,NewPos^,NewBytesPerLine);
      inc(OldPos,OldBytesPerLine);
      inc(NewPos,NewBytesPerLine);
    end;
  end else begin
    // expand
    inc(OldPos,OldSize);
    inc(NewPos,NewSize);
    for y:=Height-1 downto 0 do begin
      dec(OldPos,OldBytesPerLine);
      dec(NewPos,NewBytesPerLine);
      System.Move(OldPos^,NewPos^,OldBytesPerLine);
    end;
  end;
      
  // shrink after
  if OldSize>NewSize then
    ReAllocMem(Data,NewSize);
    
  Size:=NewSize;
  OldLineEnd:=NewLineEnd;
end;

//------------------------------------------------------------------------------
procedure InternalInit;
var
  Prec: Integer;
  HighValue: word;
  Bits: word;
  CurShift, DShift: Integer;
begin
  for Prec := 0 to 15 do
  begin
    for HighValue := 0 to 7 do
    begin
      // Value represents the three highest bits
      // For example:
      //   Prec=5 and the read value is %10110
      //   => Value=%101
      // copy the value till all missing bits are set
      // For example:
      //   Prec=5, HighValue=%110
      // => MissingBits[5,6]:=%0000011011011011
      MissingBits[Prec, HighValue] := 0;
      if Prec = 0 then Continue;
      if Prec >= 3 then DShift := 3
      else DShift := Prec;
      
      Bits := HighValue;
      
      CurShift := 16 - Prec;
      while CurShift > 0 do
      begin
        if CurShift >= DShift then
          MissingBits[Prec, HighValue] :=
            MissingBits[Prec, HighValue] or (Bits shl (CurShift - DShift))
        else
          MissingBits[Prec, HighValue] :=
            MissingBits[Prec, HighValue] or (Bits shr (DShift - CurShift));
        Dec(CurShift, DShift);
      end;
    end;
  end;
end;

initialization
  InternalInit;

end.
