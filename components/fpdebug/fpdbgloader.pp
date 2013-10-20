{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpdbgloader.pp  -  Native Freepascal debugger - Section loader
 ---------------------------------------------------------------------------

 This unit contains helper classes for loading secions form images.

 This file contains some functionality ported from DUBY. See svn log for details

 ---------------------------------------------------------------------------

 @created(Mon Aug 1st WET 2006)
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
unit FpDbgLoader;

{$mode objfpc}{$H+}

interface

uses
  LCLType,
  FpImgReaderBase, FpImgReaderWinPE, FpImgReaderElf, FpImgReaderMacho,
  Classes, SysUtils;

type

  {$ifdef windows}
    {$define USE_WIN_FILE_MAPPING}
  {$endif}
  { TDbgImageLoader }

  TDbgImageLoader = class(TObject)
  private
    FFileLoader: TDbgFileLoader;
    FImgReader: TDbgImageReader;
    function GetSection(const AName: String): PDbgImageSection; virtual;
  protected
    FImage64Bit: Boolean  unimplemented;
    FImageBase: QWord unimplemented;
    //procedure SetImageBase(ABase: QWord);
    //procedure SetImage64Bit(AValue: Boolean);
  public
    constructor Create; virtual;
    constructor Create(AFileName: String);
    {$ifdef USE_WIN_FILE_MAPPING}
    constructor Create(AFileHandle: THandle);
    {$endif}
    destructor Destroy; override;
    function IsValid: Boolean;
    property ImageBase: QWord read FImageBase; unimplemented;
    Property Image64Bit: Boolean read FImage64Bit; unimplemented;
    property Section[const AName: String]: PDbgImageSection read GetSection;
  end;



implementation

{ TDbgImageLoader }

function TDbgImageLoader.GetSection(const AName: String): PDbgImageSection;
begin
  if FImgReader <> nil then
    Result := FImgReader.Section[AName]
  else
    Result := nil;
end;

constructor TDbgImageLoader.Create;
begin
  inherited Create;
end;

constructor TDbgImageLoader.Create(AFileName: String);
begin
  FFileLoader := TDbgFileLoader.Create(AFileName);
  FImgReader := GetImageReader(FFileLoader, True);
end;

{$ifdef USE_WIN_FILE_MAPPING}
constructor TDbgImageLoader.Create(AFileHandle: THandle);
begin
  FFileLoader := TDbgFileLoader.Create(AFileHandle);
  FImgReader := GetImageReader(FFileLoader, True);
end;
{$endif}

destructor TDbgImageLoader.Destroy;
begin
  FreeAndNil(FImgReader);
  inherited Destroy;
end;

function TDbgImageLoader.IsValid: Boolean;
begin
  Result := FImgReader <> nil;
end;

end.

