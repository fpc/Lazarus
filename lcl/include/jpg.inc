{%MainUnit ../graphics.pp}

{******************************************************************************
                         TJpegImage
 ******************************************************************************

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

{$IFDEF UseSimpleJpeg}
{ TJpegImage }

function TJpegImage.GetFileExtensions: string;
begin
  Result:='jpg';
end;

function TJpegImage.GetDefaultFPReader: TFPCustomImageReaderClass;
begin
  Result:=TFPReaderJpeg;
end;

function TJpegImage.GetDefaultFPWriter: TFPCustomImageWriterClass;
begin
  Result:=TFPWriterJpeg;
end;
{$ENDIF}


// included by graphics.pp

