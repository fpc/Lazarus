{
/***************************************************************************
                               LazDoc.pas
                               ----------

 ***************************************************************************/

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
unit LazDoc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LCLProc,
  CodeToolManager, CodeCache, FileProcs,
  Laz_DOM, Laz_XMLRead, Laz_XMLWrite,
  LazHelpIntf,
  EnvironmentOpts;

type
  { TLazFPDocFile }

  TLazFPDocFile = class
  public
    Doc: TXMLdocument;
    Filename: string;
    ChangeStep: integer;
    CodeBuffer: TCodeBuffer;
    destructor Destroy; override;
  end;
  
  { TLazDocManager }

  TLazDocManager = class
  private
    FDocs: TFPList;// list of loaded TLazFPDocFile
  public
    constructor Create;
    destructor Destroy; override;
    function FindFPDocFile(const Filename: string): TLazFPDocFile;
    function LoadFPDocFile(const Filename: string;
                           UpdateFromDisk, Revert: Boolean;
                           out ADocFile: TLazFPDocFile): Boolean;
    function GetFPDocFilenameForHelpContext(
                                       Context: TPascalHelpContextList): string;
    function GetFPDocFilenameForSource(const SrcFilename: string): string;
    procedure FreeDocs;
  end;

implementation

{ TLazFPDocFile }

destructor TLazFPDocFile.Destroy;
begin
  FreeAndNil(Doc);
  inherited Destroy;
end;

constructor TLazDocManager.Create;
begin
  FDocs:=TFPList.Create;
end;

destructor TLazDocManager.Destroy;
begin
  FreeDocs;
  FreeAndNil(FDocs);
  inherited Destroy;
end;

function TLazDocManager.FindFPDocFile(const Filename: string): TLazFPDocFile;
var
  i: Integer;
begin
  for i:=0 to FDocs.Count-1 do begin
    Result:=TLazFPDocFile(FDocs[i]);
    if CompareFilenames(Result.Filename,Filename)=0 then exit;
  end;
  Result:=nil;
end;

function TLazDocManager.LoadFPDocFile(const Filename: string; UpdateFromDisk,
  Revert: Boolean; out ADocFile: TLazFPDocFile): Boolean;
var
  MemStream: TMemoryStream;
begin
  Result:=false;
  ADocFile:=FindFPDocFile(Filename);
  if ADocFile=nil then begin
    ADocFile:=TLazFPDocFile.Create;
    ADocFile.Filename:=Filename;
    FDocs.Add(ADocFile);
  end;
  ADocFile.CodeBuffer:=CodeToolBoss.LoadFile(Filename,UpdateFromDisk,Revert);
  if ADocFile.CodeBuffer=nil then begin
    DebugLn(['TLazDocForm.LoadFPDocFile unable to load "',Filename,'"']);
    FreeAndNil(ADocFile.Doc);
    exit;
  end;
  if (ADocFile.Doc<>nil)
  and (ADocFile.ChangeStep=ADocFile.CodeBuffer.ChangeStep)
  then begin
    // no update needed
    exit(true);
  end;

  // parse XML
  ADocFile.ChangeStep:=ADocFile.CodeBuffer.ChangeStep;
  FreeAndNil(ADocFile.Doc);

  MemStream:=TMemoryStream.Create;
  try
    ADocFile.CodeBuffer.SaveToStream(MemStream);
    MemStream.Position:=0;
    ReadXMLFile(ADocFile.Doc, MemStream);
    Result:=true;
  finally
    MemStream.Free;;
  end;
end;

function TLazDocManager.GetFPDocFilenameForHelpContext(
  Context: TPascalHelpContextList): string;
var
  i: Integer;
  SrcFilename: String;
begin
  Result:='';
  if Context=nil then exit;
  for i:=0 to Context.Count-1 do begin
    if Context.Items[i].Descriptor<>pihcFilename then continue;
    SrcFilename:=Context.Items[i].Context;
    Result:=GetFPDocFilenameForSource(SrcFilename);
    exit;
  end;
end;

function TLazDocManager.GetFPDocFilenameForSource(const SrcFilename: string
  ): string;
var
  SrcDir: String;
  FPDocName: String;
begin
  SrcDir:=ExtractFilePath(SrcFilename);
  FPDocName:=lowercase(ExtractFileNameOnly(SrcFilename))+'.xml';
  // check if SrcFilename is in one of the project directories
  
  // check if SrcFilename is in one of package directories
  
  // check if SrcFilename is one of the Lazarus sources
  
  // search in the default LazDoc paths

  Result:='';
end;

procedure TLazDocManager.FreeDocs;
var
  i: Integer;
begin
  for i:=FDocs.Count-1 downto 0 do
    TObject(FDocs[i]).Free;
  FDocs.Clear;
end;

end.

