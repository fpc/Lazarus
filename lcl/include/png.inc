// included by graphics.pp

{******************************************************************************
                         TPortableNetworkGraphic
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

{ TPortableNetworkGraphic }

{$IFNDEF DisableFPImage}
function TPortableNetworkGraphic.GetFileExtensions: string;
begin
  Result:='png';
end;

function TPortableNetworkGraphic.GetDefaultFPReader: TFPCustomImageReaderClass;
begin
  Result:=TFPReaderPNG;
end;

function TPortableNetworkGraphic.GetDefaultFPWriter: TFPCustomImageWriterClass;
begin
  Result:=TFPWriterPNG;
end;
{$ENDIF}


// included by graphics.pp

