{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Abstract:
    Base class for resource type (e.g. .lfm) of units.
}
unit UnitResourceIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, System.UITypes,
  LazMemManager;

type
  { TUnitResourceFileFormat }

  TUnitResourceFileFormat = class
  public
    class function  FindResourceDirective(Source: TObject): boolean; virtual; abstract;
    class function  GetUnitResourceFilename(AUnitFilename: string; Loading: boolean): string; virtual; abstract;
    class procedure TextStreamToBinStream(ATxtStream, ABinStream: TExtMemoryStream); virtual; abstract;
    class procedure BinStreamToTextStream(ABinStream, ATxtStream: TExtMemoryStream); virtual; abstract;
    class function  GetClassNameFromStream(s: TStream; out IsInherited: Boolean): shortstring; virtual; abstract;
    class function  CreateReader(s: TStream; var DestroyDriver: boolean): TReader; virtual; abstract;
    class function  CreateWriter(s: TStream; var DestroyDriver: boolean): TWriter; virtual; abstract;
    class function  QuickCheckResourceBuffer(
      PascalBuffer, LFMBuffer: TObject; // TCodeBuffer
      out LFMType, LFMComponentName, LFMClassName: string;
      out LCLVersion: string;
      out MissingClasses: TStrings;// e.g. 'TMyFrame' or 'MyUnit.TMyFrame'
      out AmbiguousClasses: TFPList // list of TRegisteredComponent
      ): TModalResult; virtual; abstract;
    class function Priority: integer; virtual; // higher priority is tested first
    class function DefaultComponentClass: TComponentClass; virtual; abstract;
    class function FindComponentClass({%H-}aClassName: string): TComponentClass; virtual;
  end;
  TUnitResourceFileFormatClass = class of TUnitResourceFileFormat;
  TUnitResourceFileFormatArr = array of TUnitResourceFileFormatClass;

var
  LFMUnitResourceFileFormat: TUnitResourcefileFormatClass = nil;// set by IDE

procedure RegisterUnitResourcefileFormat(AResourceFileFormat: TUnitResourcefileFormatClass);
function GetUnitResourcefileFormats: TUnitResourcefileFormatArr;


implementation

var
  GUnitResourcefileFormats: TUnitResourcefileFormatArr;

procedure RegisterUnitResourcefileFormat(AResourceFileFormat: TUnitResourcefileFormatClass);
var
  i: Integer;
  Priority: Integer;
  l: Integer;
begin
  Priority:=AResourceFileFormat.Priority;
  i:=0;
  while (i<length(GUnitResourcefileFormats))
  and (GUnitResourcefileFormats[i].Priority>=Priority) do
    inc(i);
  l:=length(GUnitResourcefileFormats)-i;
  SetLength(GUnitResourcefileFormats, length(GUnitResourcefileFormats)+1);
  if l>0 then
    System.Move(GUnitResourcefileFormats[i],GUnitResourcefileFormats[i+1],
      l*SizeOf(TUnitResourcefileFormatClass));
  GUnitResourcefileFormats[high(GUnitResourcefileFormats)] := AResourceFileFormat;
end;

function GetUnitResourcefileFormats: TUnitResourcefileFormatArr;
begin
  Result := GUnitResourcefileFormats;
end;


{ TUnitResourceFileFormat }

class function TUnitResourceFileFormat.Priority: integer;
begin
  Result:=0;
end;

class function TUnitResourceFileFormat.FindComponentClass(aClassName: string): TComponentClass;
begin
  Result:=nil;
end;

end.

