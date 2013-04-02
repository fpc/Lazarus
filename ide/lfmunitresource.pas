{
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

 Author: Joost van der Sluis

 Abstract:
   Registers the lfm resource format of forms.
}
unit lfmUnitResource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LCLMemManager,
  Forms,
  UnitResources, LazFileCache;

type

  { TLFMUnitResourcefileFormat }

  TLFMUnitResourcefileFormat = class(TUnitResourcefileFormat)
  public
    class function FindResourceDirective(Source: TObject): boolean; override;
    class function ResourceDirectiveFilename: string; override;
    class function GetUnitResourceFilename(AUnitFilename: string; {%H-}Loading: boolean): string; override;
    class procedure TextStreamToBinStream(ATxtStream, ABinStream: TExtMemoryStream); override;
    class procedure BinStreamToTextStream(ABinStream, ATextStream: TExtMemoryStream); override;
    class function GetClassNameFromStream(s: TStream; out IsInherited: Boolean): shortstring; override;
    class function CreateReader(s: TStream; var DestroyDriver: boolean): TReader; override;
    class function QuickCheckResourceBuffer(PascalBuffer, LFMBuffer: TObject; out
      LFMType, LFMComponentName, LFMClassName: string; out LCLVersion: string;
      out MissingClasses: TStrings): TModalResult; override;
    class function CreateWriter(s: TStream; var DestroyDriver: boolean): TWriter; override;
  end;

implementation

uses
  LResources,
  CodeCache,
  CodeToolManager,
  CheckLFMDlg;

{ TLFMUnitResourcefileFormat }

class function TLFMUnitResourcefileFormat.FindResourceDirective(Source: TObject): boolean;
var
  NewCode: TCodeBuffer;
  NewX,NewY,NewTopLine: integer;
  CodeBuf: TCodeBuffer;
begin
  CodeBuf:=Source as TCodeBuffer;
  Result := CodeToolBoss.FindResourceDirective(CodeBuf,1,1,
    NewCode,NewX,NewY,NewTopLine, ResourceDirectiveFilename,false);
  if (not Result) and (ResourceDirectiveFilename<>'*.dfm') then
    Result := CodeToolBoss.FindResourceDirective(CodeBuf,1,1,
                   NewCode,NewX,NewY,NewTopLine, '*.dfm',false);
end;

class function TLFMUnitResourcefileFormat.ResourceDirectiveFilename: string;
begin
  Result := '*.lfm';
end;

class function TLFMUnitResourcefileFormat.GetUnitResourceFilename(
  AUnitFilename: string; Loading: boolean): string;
var
  DFMFilename: String;
begin
  Result := ChangeFileExt(AUnitFilename,'.lfm');
  if not FileExistsCached(Result)
  // ToDo: search in source editor
  then begin
    DFMFilename:=ChangeFileExt(AUnitFilename,'.dfm');
    if FileExistsCached(DFMFilename) then
      Result:=DFMFilename;
  end;
end;

class procedure TLFMUnitResourcefileFormat.TextStreamToBinStream(ATxtStream,
  ABinStream: TExtMemoryStream);
begin
  LRSObjectTextToBinary(ATxtStream,ABinStream);
end;

class procedure TLFMUnitResourcefileFormat.BinStreamToTextStream(ABinStream,
  ATextStream: TExtMemoryStream);
begin
  LRSObjectBinaryToText(ABinStream,ATextStream);
end;

class function TLFMUnitResourcefileFormat.GetClassNameFromStream(s: TStream;
  out IsInherited: Boolean): shortstring;
begin
  result := GetClassNameFromLRSStream(s,IsInherited);
end;

class function TLFMUnitResourcefileFormat.CreateReader(s: TStream;
  var DestroyDriver: boolean): TReader;
begin
  result := CreateLRSReader(s,DestroyDriver);
end;

class function TLFMUnitResourcefileFormat.QuickCheckResourceBuffer(PascalBuffer,
  LFMBuffer: TObject; out LFMType, LFMComponentName, LFMClassName: string; out
  LCLVersion: string; out MissingClasses: TStrings): TModalResult;
begin
  result := QuickCheckLFMBuffer(PascalBuffer as TCodeBuffer, LFMBuffer as TCodeBuffer, LFMType, LFMComponentName, LFMClassName,
    LCLVersion, MissingClasses);
end;

class function TLFMUnitResourcefileFormat.CreateWriter(s: TStream;
  var DestroyDriver: boolean): TWriter;
begin
  result := CreateLRSWriter(s, DestroyDriver);
end;

initialization
  RegisterUnitResourcefileFormat(TLFMUnitResourcefileFormat);
end.

unit lfmUnitResource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LCLMemManager,
  Forms,
  UnitResources;

type

  { TLFMUnitResourcefileFormat }

  TLFMUnitResourcefileFormat = class(TUnitResourcefileFormat)
  public
    class function FindResourceDirective(Source: TObject): boolean; override;
    class function ResourceDirectiveFilename: string; override;
    class function GetUnitResourceFilename(AUnitFilenae: string): string; override;
    class procedure TextStreamToBinStream(ATxtStream, ABinStream: TExtMemoryStream); override;
    class procedure BinStreamToTextStream(ABinStream, ATextStream: TExtMemoryStream); override;
    class function GetClassNameFromStream(s: TStream; out IsInherited: Boolean): shortstring; override;
    class function CreateReader(s: TStream; var DestroyDriver: boolean): TReader; override;
    class function QuickCheckResourceBuffer(PascalBuffer, LFMBuffer: TObject; out
      LFMType, LFMComponentName, LFMClassName: string; out LCLVersion: string;
      out MissingClasses: TStrings): TModalResult; override;
    class function CreateWriter(s: TStream; var DestroyDriver: boolean): TWriter; override;
  end;

implementation

uses
  LResources,
  CodeCache,
  CodeToolManager,
  CheckLFMDlg;

{ TLFMUnitResourcefileFormat }

class function TLFMUnitResourcefileFormat.FindResourceDirective(Source: TObject): boolean;
var
  cb: TCodeBuffer;
  nx,ny,nt: integer;
begin
  result := CodeToolBoss.FindResourceDirective(Source as TCodeBuffer,1,1,cb,nx,ny,nt, ResourceDirectiveFilename,false);
end;

class function TLFMUnitResourcefileFormat.ResourceDirectiveFilename: string;
begin
  result := '*.lfm';
end;

class function TLFMUnitResourcefileFormat.GetUnitResourceFilename(
  AUnitFilenae: string): string;
begin
  result := ChangeFileExt(AUnitFilenae,'.lfm');
end;

class procedure TLFMUnitResourcefileFormat.TextStreamToBinStream(ATxtStream,
  ABinStream: TExtMemoryStream);
begin
  LRSObjectTextToBinary(ATxtStream,ABinStream);
end;

class procedure TLFMUnitResourcefileFormat.BinStreamToTextStream(ABinStream,
  ATextStream: TExtMemoryStream);
begin
  LRSObjectBinaryToText(ABinStream,ATextStream);
end;

class function TLFMUnitResourcefileFormat.GetClassNameFromStream(s: TStream;
  out IsInherited: Boolean): shortstring;
begin
  result := GetClassNameFromLRSStream(s,IsInherited);
end;

class function TLFMUnitResourcefileFormat.CreateReader(s: TStream;
  var DestroyDriver: boolean): TReader;
begin
  result := CreateLRSReader(s,DestroyDriver);
end;

class function TLFMUnitResourcefileFormat.QuickCheckResourceBuffer(PascalBuffer,
  LFMBuffer: TObject; out LFMType, LFMComponentName, LFMClassName: string; out
  LCLVersion: string; out MissingClasses: TStrings): TModalResult;
begin
  result := QuickCheckLFMBuffer(PascalBuffer as TCodeBuffer, LFMBuffer as TCodeBuffer, LFMType, LFMComponentName, LFMClassName,
    LCLVersion, MissingClasses);
end;

class function TLFMUnitResourcefileFormat.CreateWriter(s: TStream;
  var DestroyDriver: boolean): TWriter;
begin
  result := CreateLRSWriter(s, DestroyDriver);
end;

initialization
  RegisterUnitResourcefileFormat(TLFMUnitResourcefileFormat);
end.

