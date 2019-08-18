{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpdumpdwarf  -  DWARF debug dump
 ---------------------------------------------------------------------------

 Utility to test and dump DWARF dubug info.

 ---------------------------------------------------------------------------

 @created(Sat Jul 1th WET 2006)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.nl>)

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
program FPDumpDwarf;

{$mode objfpc}{$H+}

uses
  Classes, Windows, SysUtils, maps, LazLogger, FpDbgPETypes, FpDbgDwarfConst, FpDbgSymbols,
  FpDbgLoader, FpDbgDwarf, FpDbgDwarfVerbosePrinter;

var
  n, idx: Integer;
  Dwarf: TFpDwarfInfo;
  AbbrevDecoder: TDwarfAbbrevDecoder;
  StatementDecoder: TDwarfStatementDecoder;
  FrameDecoder: TVerboseDwarfCallframeDecoder;
  Loader: TDbgImageLoader;
  ImageLoaderList: TDbgImageLoaderList;

begin
  if ParamCount < 1
  then begin
    WriteLN('Usage: FPDumpDwarf <filename>');
    Exit;
  end;

  DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_VERBOSE')^.Enabled := True;
  
  Loader := TDbgImageLoader.Create(ParamStr(1));
  ImageLoaderList := TDbgImageLoaderList.Create(True);
  Loader.AddToLoaderList(ImageLoaderList);

  Dwarf := TFpDwarfInfo.Create(ImageLoaderList);
  n := Dwarf.LoadCompilationUnits;
  for idx := 0 to n - 1 do
  begin
    AbbrevDecoder := TDwarfAbbrevDecoder.Create(Dwarf.CompilationUnits[idx]);
    AbbrevDecoder.Decode;
    AbbrevDecoder.Free;
    StatementDecoder := TDwarfStatementDecoder.Create(Dwarf.CompilationUnits[idx]);
    StatementDecoder.Decode;
    StatementDecoder.Free;
    WriteLN;
  end;
  Dwarf.Free;

  WriteLn('Call info:');
  FrameDecoder := TVerboseDwarfCallframeDecoder.Create(Loader);
  FrameDecoder.Decode;
  FrameDecoder.Free;

  Loader.Free;
end.

