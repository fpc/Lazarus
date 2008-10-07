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
  Classes, SysUtils, FileUtil, Process, LCLProc, Controls, Forms,
  CodeToolManager, CodeCache, CodeAtom, LazConf, LResources, projectresourcesintf;
   
type
  { TProjectXPManifest }

  TProjectXPManifest = class(TObject)
  private
    FModified: boolean;
    FUseManifest: boolean;
    FOnModified: TNotifyEvent;
    procedure SetUseManifest(const AValue: boolean);
    procedure SetModified(const AValue: Boolean);
  public
    function UpdateResources(AResources: TAbstractProjectResources; const MainFilename, TargetOS: string): Boolean;

    property UseManifest: boolean read FUseManifest write SetUseManifest;
    property Modified: boolean read FModified write SetModified;

    property OnModified: TNotifyEvent read FOnModified write FOnModified;
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

procedure TProjectXPManifest.SetUseManifest(const AValue: boolean);
begin
  if FUseManifest = AValue then exit;
  FUseManifest := AValue;
  Modified := True;
end;

function TProjectXPManifest.UpdateResources(AResources: TAbstractProjectResources;
  const MainFilename, TargetOS: string): Boolean;
begin
  Result := True;
  if (TargetOS = 'win32') and UseManifest then
    AResources.AddSystemResource(sManifest);
end;

procedure TProjectXPManifest.SetModified(const AValue: Boolean);
begin
  if FModified = AValue then
    Exit;
  FModified := AValue;
  if Assigned(OnModified) then 
    OnModified(Self);
end;

end.

