{
 /***************************************************************************
                        w32manifest.pas  -  Lazarus IDE unit
                        ---------------------------------------
              TProjectXPManifest is responsible for the inclusion of the 
                   manifest in windows executables.


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
unit W32Manifest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, LCLProc, Controls, Forms,
  CodeToolManager, CodeCache, CodeAtom, LazConf, SourceChanger, LResources;
   
type
  { TProjectXPManifest }

  TProjectXPManifest = class(TObject)
  private
    FMessages: TStrings;
    FUseManifest: boolean;
    resFilename: string;
    procedure SetUseManifest(const AValue: boolean);
    procedure SetFileNames(const MainFilename: string);
  public
    constructor Create;
    destructor Destroy; override;
    
    function CompileRCFile(const MainFilename, TargetOS: string): TModalResult;
    function CreateManifest: Boolean;
    function UpdateMainSourceFile(const AFilename: string): TModalResult;

    property Messages: TStrings read FMessages;
    property UseManifest: boolean read FUseManifest write SetUseManifest;
  end;

implementation

{-----------------------------------------------------------------------------
 TProjectXPManifest CompileRCFile
-----------------------------------------------------------------------------}
function TProjectXPManifest.CompileRCFile(const MainFilename, TargetOS: string): TModalResult;
begin
  // in future we will compile manifest from rc, but now we just add our template
  
  Result := mrOk;
  SetFileNames(MainFilename);
  if (TargetOS = 'win32') and UseManifest then
  begin
    if not CreateManifest then
      Result := mrCancel;
  end;
end;

function TProjectXPManifest.CreateManifest: Boolean;
var
  Res: TLResource;
  Stream: TStream;
begin
  // here will be better to compile res from rc, but we will only extract
  // precompiled res from lazarus resource due to error in windres.
  
  // Error description:
  // if we compile manifest by windres then we will not link our project if we
  // have other res files before manifest (I tested with version info resource)
  // But if we compile that manifest.rc with other resource compiler then we have
  // not such errors. So at this moment we decided to extract precompiled res insted
  // of have problems with rc compilation and further linking
  
  Result := False;
  Res := LazarusResources.Find('manifest');
  if (Res <> nil) and (Res.Value <> '') and (Res.ValueType = 'RES') then
  begin
    Stream := nil;
    try
      Stream := TFileStream.Create(resFileName, fmCreate);
      Stream.Write(Res.Value[1], length(Res.Value));
      Result := True;
    finally
      Stream.Free;
    end;
  end;
end;

procedure TProjectXPManifest.SetUseManifest(const AValue: boolean);
begin
  FUseManifest := AValue;
end;

{-----------------------------------------------------------------------------
 TProjectXPManifest UpdateMainSourceFile
-----------------------------------------------------------------------------}
function TProjectXPManifest.UpdateMainSourceFile(const AFilename: string): TModalResult;
var
  NewX, NewY, NewTopLine: integer;
  ManifestCodeBuf: TCodeBuffer;
  CodePos: TCodeXYPosition;
begin
  Result := mrCancel;
  ManifestCodeBuf := CodeToolBoss.LoadFile(AFilename,false,false);
  if ManifestCodeBuf <> nil then
  begin
    SetFileNames(AFilename);
    if not CodeToolBoss.FindResourceDirective(ManifestCodeBuf, 1, 1,
                               ManifestCodeBuf, NewX, NewY,
                               NewTopLine, ExtractFileName(resFileName)) then
    begin
      if UseManifest then
      begin
        if not CodeToolBoss.AddResourceDirective(ManifestCodeBuf, ExtractFileName(resFileName)) then
        begin
          Messages.Add('Could not add "{$R'+ ExtractFileName(resFileName) +'"} to main source!');
          exit;
        end;
      end;
    end else
    if not UseManifest then
    begin
      with CodeToolBoss.CurCodeTool do
      begin
        CodeToolBoss.SourceChangeCache.MainScanner := Scanner;
        CodePos.Code := ManifestCodeBuf;
        CodePos.X := NewX;
        CodePos.Y := NewY;
        CaretToCleanPos(CodePos, NewX);
        if CodeToolBoss.SourceChangeCache.Replace(gtNone, gtNone, NewX, NewX + Length(ExtractFileName(resFileName)) + 5, '') then
        begin
          if not CodeToolBoss.SourceChangeCache.Apply then
            exit;
        end else
          exit;
      end;
    end;
  end;
  Result := mrOk;
end;

{-----------------------------------------------------------------------------
 TProjectXPManifest SetFileNames
-----------------------------------------------------------------------------}
procedure TProjectXPManifest.SetFileNames(const MainFilename: string);
begin
  resFilename := ExtractFilePath(MainFilename) + 'manifest.res';
end;

constructor TProjectXPManifest.Create;
begin
  FMessages := TStringList.Create;
end;

destructor TProjectXPManifest.Destroy;
begin
  FMessages.Free;
  inherited Destroy;
end;

initialization
  {$i manifest.lrs}
end.

