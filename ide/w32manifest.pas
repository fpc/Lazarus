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
    FModified: boolean;
    FUseManifest: boolean;
    rcFilename: string;
    procedure SetUseManifest(const AValue: boolean);
    procedure SetFileNames(const MainFilename: string);
  public
    constructor Create;
    destructor Destroy; override;
    
    function CreateRCFile(const MainFilename, TargetOS: string): TModalResult;
    function CreateManifest: Boolean;
    function UpdateMainSourceFile(const AFilename: string): TModalResult;

    property Messages: TStrings read FMessages;
    property UseManifest: boolean read FUseManifest write SetUseManifest;
    property Modified: boolean read FModified write FModified;
  end;

implementation

const
  sManifest: String =
    '#define RT_MANIFEST  24'#$D#$A+
    '#define CREATEPROCESS_MANIFEST_RESOURCE_ID 1'#$D#$A+
    '#define ISOLATIONAWARE_MANIFEST_RESOURCE_ID 2'#$D#$A+
    '#define ISOLATIONAWARE_NOSTATICIMPORT_MANIFEST_RESOURCE_ID 3'#$D#$A#$D#$A+
    'CREATEPROCESS_MANIFEST_RESOURCE_ID RT_MANIFEST MOVEABLE PURE'#$D#$A+
    '{'#$D#$A+
    ' "<?xml version=""1.0"" encoding=""UTF-8"" standalone=""yes""?>"'#$D#$A+
    ' "<assembly xmlns=""urn:schemas-microsoft-com:asm.v1"" manifestVersion=""1.0"">"'#$D#$A+
    ' "<assemblyIdentity version=""1.0.0.0"" processorArchitecture=""*"" name=""CompanyName.ProductName.YourApp"" type=""win32""/>"'#$D#$A+
    ' "<description>Your application description here.</description>"'#$D#$A+
    ' "<dependency>"'#$D#$A+
    ' "<dependentAssembly>"'#$D#$A+
    ' "<assemblyIdentity type=""win32"" name=""Microsoft.Windows.Common-Controls"" version=""6.0.0.0"" processorArchitecture=""*"" publicKeyToken=""6595b64144ccf1df"" language=""*""/>"'#$D#$A+
    ' "</dependentAssembly>"'#$D#$A+
    ' "</dependency>"'#$D#$A+
    ' "<trustInfo xmlns=""urn:schemas-microsoft-com:asm.v3"">"'#$D#$A+
    ' "<security>"'#$D#$A+
    ' "<requestedPrivileges>"'#$D#$A+
    ' "<requestedExecutionLevel level=""asInvoker"" uiAccess=""false""/>"'#$D#$A+
    ' "</requestedPrivileges>"'#$D#$A+
    ' "</security>"'#$D#$A+
    ' "</trustInfo>"'#$D#$A+
    ' "</assembly>"'#$D#$A+
    '}';

{-----------------------------------------------------------------------------
 TProjectXPManifest CreateRCFile
-----------------------------------------------------------------------------}
function TProjectXPManifest.CreateRCFile(const MainFilename, TargetOS: string): TModalResult;
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
  Stream: TStream;
begin
  Result := False;
  try
    Stream := TFileStream.Create(rcFileName, fmCreate);
    Stream.Write(sManifest[1], length(sManifest));
    Result := True;
  finally
    Stream.Free;
  end;
end;

procedure TProjectXPManifest.SetUseManifest(const AValue: boolean);
begin
  if FUseManifest = AValue then exit;
  FUseManifest := AValue;
  Modified:=true;
end;

{-----------------------------------------------------------------------------
 TProjectXPManifest UpdateMainSourceFile
-----------------------------------------------------------------------------}
function TProjectXPManifest.UpdateMainSourceFile(const AFilename: string): TModalResult;
var
  NewX, NewY, NewTopLine: integer;
  ManifestCodeBuf, NewCode: TCodeBuffer;
  Filename: String;
begin
  Result := mrCancel;
  ManifestCodeBuf := CodeToolBoss.LoadFile(AFilename,false,false);
  if ManifestCodeBuf <> nil then
  begin
    SetFileNames(AFilename);
    Filename:=ExtractFileName(rcFileName);
    DebugLn(['TProjectXPManifest.UpdateMainSourceFile ',Filename]);
    if CodeToolBoss.FindResourceDirective(ManifestCodeBuf, 1, 1,
                               NewCode, NewX, NewY,
                               NewTopLine, Filename, false) then
    begin
      // there is a resource directive in the source
      if not UseManifest then begin
        if not CodeToolBoss.RemoveDirective(NewCode, NewX,NewY,true) then
        begin
          Messages.Add('Could not remove "{$R'+ Filename +'"} from main source!');
          exit;
        end;
      end;
    end else if UseManifest then
    begin
      if not CodeToolBoss.AddResourceDirective(ManifestCodeBuf,
        Filename,false,'{$IFDEF WINDOWS}{$R '+Filename+'}{$ENDIF}') then
      begin
        Messages.Add('Could not add "{$R'+ Filename +'"} to main source!');
        exit;
      end;
    end;
  end;
  DebugLn(['TProjectXPManifest.UpdateMainSourceFile END ',ManifestCodeBuf.Source]);
  Result := mrOk;
end;

{-----------------------------------------------------------------------------
 TProjectXPManifest SetFileNames
-----------------------------------------------------------------------------}
procedure TProjectXPManifest.SetFileNames(const MainFilename: string);
begin
  rcFilename := ExtractFilePath(MainFilename) + 'manifest.rc';
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

end.

