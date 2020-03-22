{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author:
    Joost van der Sluis

  Abstract:
    Change the resource type (e.g. .lfm) of forms.
    Every unit can have one resource file. Default is .lfm.
    This unit allows to define other formats, like .xib.
}
unit UnitResources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLMemManager, Forms, LResources;

type

  { TUnitResourcefileFormat }

  TUnitResourcefileFormat = class
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
      out MissingClasses: TStrings// e.g. MyFrame2:TMyFrame
      ): TModalResult; virtual; abstract;
    class function Priority: integer; virtual; // higher priority is tested first
    class function DefaultComponentClass: TComponentClass; virtual;
    class function FindComponentClass({%H-}aClassName: string): TComponentClass; virtual;
  end;
  TUnitResourcefileFormatClass = class of TUnitResourcefileFormat;
  TUnitResourcefileFormatArr = array of TUnitResourcefileFormatClass;

  { TCustomLFMUnitResourceFileFormat }

  TCustomLFMUnitResourceFileFormat = class(TUnitResourcefileFormat)
  public
    class function ResourceDirectiveFilename: string; virtual;
    class function GetUnitResourceFilename(AUnitFilename: string; {%H-}Loading: boolean): string; override;
    class procedure TextStreamToBinStream(ATxtStream, ABinStream: TExtMemoryStream); override;
    class procedure BinStreamToTextStream(ABinStream, ATxtStream: TExtMemoryStream); override;
    class function GetClassNameFromStream(s: TStream; out IsInherited: Boolean): shortstring; override;
    class function CreateReader(s: TStream; var DestroyDriver: boolean): TReader; override;
    class function CreateWriter(s: TStream; var DestroyDriver: boolean): TWriter; override;
    class function DefaultComponentClass: TComponentClass; override;
    class function FindComponentClass(aClassName: string): TComponentClass; override;
  end;

var
  LFMUnitResourceFileFormat: TUnitResourcefileFormatClass = nil;// set by IDE

procedure RegisterUnitResourcefileFormat(AResourceFileFormat: TUnitResourcefileFormatClass);
function GetUnitResourcefileFormats: TUnitResourcefileFormatArr;

implementation

uses
  FormEditingIntf;

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

{ TCustomLFMUnitResourceFileFormat }

class function TCustomLFMUnitResourceFileFormat.ResourceDirectiveFilename: string;
// Note: $R uses fpcres, which supports only a few formats like dfm and lfm.
// In other words: If you want other formats you need to extend fpcres or use
// other storages like include files (e.g. like the old lrs format).
begin
  Result := '*.lfm';
end;

class function TCustomLFMUnitResourceFileFormat.GetUnitResourceFilename(
  AUnitFilename: string; Loading: boolean): string;
begin
  Result := ChangeFileExt(AUnitFilename,'.lfm');
end;

class procedure TCustomLFMUnitResourceFileFormat.TextStreamToBinStream(ATxtStream,
  ABinStream: TExtMemoryStream);
begin
  LRSObjectTextToBinary(ATxtStream,ABinStream);
end;

class procedure TCustomLFMUnitResourceFileFormat.BinStreamToTextStream(ABinStream,
  ATxtStream: TExtMemoryStream);
begin
  LRSObjectBinaryToText(ABinStream,ATxtStream);
end;

class function TCustomLFMUnitResourceFileFormat.GetClassNameFromStream(s: TStream;
  out IsInherited: Boolean): shortstring;
begin
  Result := GetClassNameFromLRSStream(s,IsInherited);
end;

class function TCustomLFMUnitResourceFileFormat.CreateReader(s: TStream;
  var DestroyDriver: boolean): TReader;
begin
  Result := CreateLRSReader(s,DestroyDriver);
end;

class function TCustomLFMUnitResourceFileFormat.CreateWriter(s: TStream;
  var DestroyDriver: boolean): TWriter;
begin
  Result := CreateLRSWriter(s, DestroyDriver);
end;

class function TCustomLFMUnitResourceFileFormat.DefaultComponentClass: TComponentClass;
begin
  Result := FormEditingHook.StandardDesignerBaseClasses[DesignerBaseClassId_TForm];
end;

class function TCustomLFMUnitResourceFileFormat.FindComponentClass(
  aClassName: string): TComponentClass;
begin
  if CompareText(aClassName,'TForm')=0 then
    Result:=FormEditingHook.StandardDesignerBaseClasses[DesignerBaseClassId_TForm]
  else if CompareText(aClassName,'TFrame')=0 then
    Result:=FormEditingHook.StandardDesignerBaseClasses[DesignerBaseClassId_TFrame]
  else if CompareText(aClassName,'TDataModule')=0 then
    Result:=FormEditingHook.StandardDesignerBaseClasses[DesignerBaseClassId_TDataModule]
  else
    Result:=nil;
end;

{ TUnitResourcefileFormat }

class function TUnitResourcefileFormat.Priority: integer;
begin
  Result:=0;
end;

class function TUnitResourcefileFormat.DefaultComponentClass: TComponentClass;
begin
  Result:=TForm;
end;

class function TUnitResourcefileFormat.FindComponentClass(aClassName: string): TComponentClass;
begin
  Result:=nil;
end;

end.

