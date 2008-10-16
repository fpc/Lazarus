{
 /***************************************************************************
                    projectresources.pas  -  Lazarus IDE unit
                    -----------------------------------------

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

 Abstract: Project Resources - is a list of System and Lazarus resources.
 Currently it contains:
   - Version information
   - XP manifest
   - Project icon
}
unit ProjectResources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LCLProc, LResources, FileUtil, Laz_XMLCfg,
  ProjectResourcesIntf,
  W32VersionInfo, W32Manifest, ProjectIcon, IDEProcs, DialogProcs,
  BasicCodeTools, CodeToolManager, CodeCache, CodeAtom;

type

  { TProjectResources }

  TProjectResources = class(TAbstractProjectResources)
  private
    FModified: Boolean;
    FOnModified: TNotifyEvent;
    FInModified: Boolean;

    FSystemResources: TStringList;
    FLazarusResources: TStringList;

    rcFileName: String;
    lrsFileName: String;

    FVersionInfo: TProjectVersionInfo;
    FXPManifest: TProjectXPManifest;
    FProjectIcon: TProjectIcon;
    rcCodeBuf: TCodeBuffer;
    lrsCodeBuf: TCodeBuffer;

    procedure SetFileNames(const MainFileName: String);
    procedure SetModified(const AValue: Boolean);
    procedure EmbeddedObjectModified(Sender: TObject);
    function Update: Boolean;
    function UpdateMainSourceFile(const AFileName: string): Boolean;
    function Save(const MainFileName: String): Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddSystemResource(const AResource: String); override;
    procedure AddLazarusResource(AResource: TStream; const ResourceName, ResourceType: String); override;

    procedure Clear;
    function Regenerate(const MainFileName: String; UpdateSource, PerformSave: Boolean): Boolean;
    function RenameDirectives(const OldFileName, NewFileName: String): Boolean;

    function HasSystemResources(CheckLists: Boolean): Boolean;
    function HasLazarusResources(CheckLists: Boolean): Boolean;

    procedure WriteToProjectFile(AConfig: TXMLConfig; Path: String);
    procedure ReadFromProjectFile(AConfig: TXMLConfig; Path: String);

    property VersionInfo: TProjectVersionInfo read FVersionInfo;
    property XPManifest: TProjectXPManifest read FXPManifest;
    property ProjectIcon: TProjectIcon read FProjectIcon;

    property Modified: Boolean read FModified write SetModified;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
  end;

implementation
const
  LazResourcesUnit = 'LResources';

{ TProjectResources }

procedure TProjectResources.SetFileNames(const MainFileName: String);
begin
  rcFileName := ChangeFileExt(MainFileName, '.rc');
  lrsFileName := ChangeFileExt(MainFileName, '.lrs');
end;

procedure TProjectResources.SetModified(const AValue: Boolean);
begin
  if FInModified then
    Exit;
  FInModified := True;
  if FModified <> AValue then
  begin
    FModified := AValue;
    if not FModified then
    begin
      VersionInfo.Modified := False;
      XPManifest.Modified := False;
      ProjectIcon.Modified := False;
    end;
    if Assigned(FOnModified) then
      OnModified(Self);
  end;
  FInModified := False;
end;

function TProjectResources.Update: Boolean;
begin
  // CheckMode is used only to test whether we have paticular resources
  Clear;
  // handle versioninfo
  Result := VersionInfo.UpdateResources(Self, rcFileName);
  if not Result then
    Exit;

  // handle manifest
  Result := XPManifest.UpdateResources(Self, rcFileName);
  if not Result then
    Exit;

  // handle project icon
  Result := ProjectIcon.UpdateResources(Self, rcFileName);
end;

procedure TProjectResources.EmbeddedObjectModified(Sender: TObject);
begin
  Modified :=
    VersionInfo.Modified or
    XPManifest.Modified or
    ProjectIcon.Modified;
end;

constructor TProjectResources.Create;
begin
  inherited Create;

  FInModified := False;

  rcCodeBuf := nil;
  lrsCodeBuf := nil;

  FSystemResources := TStringList.Create;
  FLazarusResources := TStringList.Create;

  FVersionInfo := TProjectVersionInfo.Create;
  FVersionInfo.OnModified := @EmbeddedObjectModified;

  FXPManifest := TProjectXPManifest.Create;
  FXPManifest.UseManifest := False;
  FXPManifest.OnModified := @EmbeddedObjectModified;

  FProjectIcon := TProjectIcon.Create;
  FProjectIcon.OnModified := @EmbeddedObjectModified;
end;

destructor TProjectResources.Destroy;
begin
  FVersionInfo.Free;
  FXPManifest.Free;
  FProjectIcon.Free;

  FSystemResources.Free;
  FLazarusResources.Free;
  FMessages.Free;

  rcCodeBuf.Free;
  lrsCodeBuf.Free;
end;

procedure TProjectResources.AddSystemResource(const AResource: String);
begin
  FSystemResources.Add(AResource);
end;

procedure TProjectResources.AddLazarusResource(AResource: TStream;
  const ResourceName, ResourceType: String);
var
  OutStream: TStringStream;
begin
  OutStream := TStringStream.Create('');
  BinaryToLazarusResourceCode(AResource, OutStream, ResourceName, ResourceType);
  FLazarusResources.Add(OutStream.DataString);
  OutStream.Free;
end;

procedure TProjectResources.Clear;
begin
  FSystemResources.Clear;
  FLazarusResources.Clear;
  FMessages.Clear;
end;

function TProjectResources.Regenerate(const MainFileName: String; UpdateSource, PerformSave: Boolean): Boolean;
begin
  Result := False;

  if (MainFileName = '') then
    Exit;

  SetFileNames(MainFileName);

  if not Update then
    Exit;

  if HasSystemResources(True) then
  begin
    if rcCodeBuf = nil then
      rcCodeBuf := CodeToolBoss.CreateFile(ExtractFileName(rcFileName));
    rcCodeBuf.Source:= FSystemResources.Text;
  end
  else
    FreeAndNil(rcCodeBuf);

  if HasLazarusResources(True) then
  begin
    if lrsCodeBuf = nil then
      lrsCodeBuf := CodeToolBoss.CreateFile(ExtractFileName(lrsFileName));
    lrsCodeBuf.Source := FLazarusResources.Text;
  end
  else
    FreeAndNil(lrsCodeBuf);

  if UpdateSource and not UpdateMainSourceFile(MainFileName) then
      Exit;

  if PerformSave and not Save(MainFileName) then
    Exit;

  Result := True;
end;

function TProjectResources.HasSystemResources(CheckLists: Boolean): Boolean;
begin
  if CheckLists then
    Result := FSystemResources.Count > 0
  else
    Result := VersionInfo.HasAnySystemResource or
              XPManifest.HasAnySystemResource or
              ProjectIcon.HasAnySystemResource;
end;

function TProjectResources.HasLazarusResources(CheckLists: Boolean): Boolean;
begin
  if CheckLists then
    Result := FLazarusResources.Count > 0
  else
    Result := VersionInfo.HasAnyLazarusResource or
              XPManifest.HasAnyLazarusResource or
              ProjectIcon.HasAnyLazarusResource;
end;

procedure TProjectResources.WriteToProjectFile(AConfig: TXMLConfig; Path: String);
begin
  // todo: further split by classes
  with AConfig do
  begin
    SetDeleteValue(Path+'General/Icon/Value', ProjectIcon.IconText, '');
    SetDeleteValue(Path+'General/UseXPManifest/Value', XPManifest.UseManifest, False);
    SetDeleteValue(Path+'VersionInfo/UseVersionInfo/Value', VersionInfo.UseVersionInfo,false);
    SetDeleteValue(Path+'VersionInfo/AutoIncrementBuild/Value', VersionInfo.AutoIncrementBuild,false);
    SetDeleteValue(Path+'VersionInfo/CurrentVersionNr/Value', VersionInfo.VersionNr,0);
    SetDeleteValue(Path+'VersionInfo/CurrentMajorRevNr/Value', VersionInfo.MajorRevNr,0);
    SetDeleteValue(Path+'VersionInfo/CurrentMinorRevNr/Value', VersionInfo.MinorRevNr,0);
    SetDeleteValue(Path+'VersionInfo/CurrentBuildNr/Value', VersionInfo.BuildNr,0);
    SetDeleteValue(Path+'VersionInfo/ProjectVersion/Value', VersionInfo.ProductVersionString,'1.0.0.0');
    SetDeleteValue(Path+'VersionInfo/Language/Value', VersionInfo.HexLang,'0409');
    SetDeleteValue(Path+'VersionInfo/CharSet/Value', VersionInfo.HexCharSet,'04E4');
    SetDeleteValue(Path+'VersionInfo/Comments/Value', VersionInfo.CommentsString,'');
    SetDeleteValue(Path+'VersionInfo/CompanyName/Value', VersionInfo.CompanyString,'');
    SetDeleteValue(Path+'VersionInfo/FileDescription/Value', VersionInfo.DescriptionString,'');
    SetDeleteValue(Path+'VersionInfo/InternalName/Value', VersionInfo.InternalNameString,'');
    SetDeleteValue(Path+'VersionInfo/LegalCopyright/Value', VersionInfo.CopyrightString,'');
    SetDeleteValue(Path+'VersionInfo/LegalTrademarks/Value', VersionInfo.TrademarksString,'');
    SetDeleteValue(Path+'VersionInfo/OriginalFilename/Value', VersionInfo.OriginalFilenameString,'');
    SetDeleteValue(Path+'VersionInfo/ProductName/Value', VersionInfo.ProdNameString,'');
  end;
end;

procedure TProjectResources.ReadFromProjectFile(AConfig: TXMLConfig; Path: String);
begin
  // todo: further split by classes
  with AConfig do
  begin
    ProjectIcon.IconText := GetValue(Path+'General/Icon/Value', '');
    XPManifest.UseManifest := GetValue(Path+'General/UseXPManifest/Value', False);
    VersionInfo.UseVersionInfo := GetValue(Path+'VersionInfo/UseVersionInfo/Value', False);
    VersionInfo.AutoIncrementBuild := GetValue(Path+'VersionInfo/AutoIncrementBuild/Value', False);
    VersionInfo.VersionNr := GetValue(Path+'VersionInfo/CurrentVersionNr/Value', 0);
    VersionInfo.MajorRevNr := GetValue(Path+'VersionInfo/CurrentMajorRevNr/Value', 0);
    VersionInfo.MinorRevNr := GetValue(Path+'VersionInfo/CurrentMinorRevNr/Value', 0);
    VersionInfo.BuildNr := GetValue(Path+'VersionInfo/CurrentBuildNr/Value', 0);
    VersionInfo.ProductVersionString := GetValue(Path+'VersionInfo/ProjectVersion/Value', '1.0.0.0');
    VersionInfo.HexLang := GetValue(Path+'VersionInfo/Language/Value', '0409');
    VersionInfo.HexCharSet := GetValue(Path+'VersionInfo/CharSet/Value', '04E4');
    VersionInfo.CommentsString := LineBreaksToSystemLineBreaks(GetValue(Path+'VersionInfo/Comments/Value', ''));
    VersionInfo.CompanyString := LineBreaksToSystemLineBreaks(GetValue(Path+'VersionInfo/CompanyName/Value', ''));
    VersionInfo.DescriptionString := LineBreaksToSystemLineBreaks(GetValue(Path+'VersionInfo/FileDescription/Value', ''));
    VersionInfo.InternalNameString := LineBreaksToSystemLineBreaks(GetValue(Path+'VersionInfo/InternalName/Value', ''));
    VersionInfo.CopyrightString := LineBreaksToSystemLineBreaks(GetValue(Path+'VersionInfo/LegalCopyright/Value', ''));
    VersionInfo.TrademarksString := LineBreaksToSystemLineBreaks(GetValue(Path+'VersionInfo/LegalTrademarks/Value', ''));
    VersionInfo.OriginalFilenameString := GetValue(Path+'VersionInfo/OriginalFilename/Value', '');
    VersionInfo.ProdNameString := LineBreaksToSystemLineBreaks(GetValue(Path+'VersionInfo/ProductName/Value', ''));
  end;
end;

function TProjectResources.UpdateMainSourceFile(const AFileName: string): Boolean;
var
  NewX, NewY, NewTopLine: integer;
  CodeBuf, NewCode: TCodeBuffer;
  Filename: String;
  NamePos, InPos: integer;
begin
  Result := True;

  CodeBuf := CodeToolBoss.LoadFile(AFilename, False, False);
  if CodeBuf <> nil then
  begin
    SetFileNames(AFileName);
    Filename := ExtractFileName(rcFileName);
    //debugln(['TProjectResources.UpdateMainSourceFile HasSystemResources=',HasSystemResources,' Filename=',Filename,' HasLazarusResources=',HasLazarusResources]);

    // update LResources uses
    if CodeToolBoss.FindUnitInAllUsesSections(CodeBuf, LazResourcesUnit, NamePos, InPos) then
    begin
      if not HasLazarusResources(False) then
      begin
        if not CodeToolBoss.RemoveUnitFromAllUsesSections(CodeBuf, LazResourcesUnit) then
        begin
          Result := False;
          Messages.Add('Could not remove "' + LazResourcesUnit + '" from main source!');
          debugln(['TProjectResources.UpdateMainSourceFile adding LResources to main source failed']);
        end;
      end;
    end
    else
    if HasLazarusResources(False) then
    begin
      if not CodeToolBoss.AddUnitToMainUsesSection(CodeBuf, LazResourcesUnit,'') then
      begin
        Result := False;
        Messages.Add('Could not add "' + LazResourcesUnit + '" to main source!');
        debugln(['TProjectResources.UpdateMainSourceFile adding LResources to main source failed']);
      end;
    end;

    // update {$R filename} directive
    if CodeToolBoss.FindResourceDirective(CodeBuf, 1, 1,
                               NewCode, NewX, NewY,
                               NewTopLine, Filename, false) then
    begin
      // there is a resource directive in the source
      if not HasSystemResources(False) then
      begin
        if not CodeToolBoss.RemoveDirective(NewCode, NewX, NewY, true) then
        begin
          Result := False;
          Messages.Add('Could not remove "{$R '+ Filename +'"} from main source!');
          debugln(['TProjectResources.UpdateMainSourceFile failed: removing resource directive']);
        end;
      end;
    end
    else
    if HasSystemResources(False) then
    begin
      if not CodeToolBoss.AddResourceDirective(CodeBuf,
        Filename,false,'{$IFDEF WINDOWS}{$R '+Filename+'}{$ENDIF}') then
      begin
        Result := False;
        Messages.Add('Could not add "{$R '+ Filename +'"} to main source!');
        debugln(['TProjectResources.UpdateMainSourceFile failed: adding resource directive']);
      end;
    end;

    // update {$I filename} directive
    Filename := ExtractFileName(lrsFileName);
    if CodeToolBoss.FindIncludeDirective(CodeBuf, 1, 1,
                               NewCode, NewX, NewY,
                               NewTopLine, Filename, false) then
    begin
      // there is a resource directive in the source
      //debugln(['TProjectResources.UpdateMainSourceFile include directive found']);
      if not HasLazarusResources(False) then
      begin
        if not CodeToolBoss.RemoveDirective(NewCode, NewX, NewY, true) then
        begin
          Result := False;
          Messages.Add('Could not remove "{$I '+ Filename +'"} from main source!');
          debugln(['TProjectResources.UpdateMainSourceFile removing include directive from main source failed']);
          Exit;
        end;
      end;
    end
    else
    if HasLazarusResources(False) then
    begin
      //debugln(['TProjectResources.UpdateMainSourceFile include directive not found']);
      if not CodeToolBoss.AddIncludeDirective(CodeBuf,
        Filename,'') then
      begin
        Result := False;
        Messages.Add('Could not add "{$I '+ Filename +'"} to main source!');
        debugln(['TProjectResources.UpdateMainSourceFile adding include directive to main source failed']);
        Exit;
      end;
    end;
  end;
end;

function TProjectResources.RenameDirectives(const OldFileName,
  NewFileName: String): Boolean;
var
  NewX, NewY, NewTopLine: integer;
  CodeBuf, NewCode: TCodeBuffer;

  oldRcFileName, oldLrsFileName,
  newRcFileName, newLrsFileName: String;
begin
  Result := True;

  CodeBuf := CodeToolBoss.LoadFile(OldFileName, False, False);
  if CodeBuf <> nil then
  begin
    SetFileNames(OldFileName);
    oldRcFilename := ExtractFileName(rcFileName);
    oldLrsFileName := ExtractFileName(lrsFileName);
    SetFileNames(NewFileName);
    newRcFilename := ExtractFileName(rcFileName);
    newLrsFileName := ExtractFileName(lrsFileName);

    // update {$R filename} directive
    if CodeToolBoss.FindResourceDirective(CodeBuf, 1, 1,
                               NewCode, NewX, NewY,
                               NewTopLine, oldRcFileName, false) then
    begin
      // there is a resource directive in the source
      if not CodeToolBoss.RemoveDirective(NewCode, NewX, NewY, true) then
      begin
        Result := False;
        Messages.Add('Could not remove "{$R '+ oldRcFileName +'"} from main source!');
        debugln(['TProjectResources.RenameDirectives failed: removing resource directive']);
      end;
      if not CodeToolBoss.AddResourceDirective(CodeBuf,
        newRcFileName, false, '{$IFDEF WINDOWS}{$R '+ newRcFileName +'}{$ENDIF}') then
      begin
        Result := False;
        Messages.Add('Could not add "{$R '+ newRcFileName +'"} to main source!');
        debugln(['TProjectResources.RenameDirectives failed: adding resource directive']);
      end;
    end;

    // update {$I filename} directive
    if CodeToolBoss.FindIncludeDirective(CodeBuf, 1, 1,
                               NewCode, NewX, NewY,
                               NewTopLine, oldLrsFileName, false) then
    begin
      // there is a resource directive in the source
      if not CodeToolBoss.RemoveDirective(NewCode, NewX, NewY, true) then
      begin
        Result := False;
        Messages.Add('Could not remove "{$I '+ oldLrsFileName +'"} from main source!');
        debugln(['TProjectResources.RenameDirectives removing include directive from main source failed']);
        Exit;
      end;
      if not CodeToolBoss.AddIncludeDirective(CodeBuf, newLrsFileName, '') then
      begin
        Result := False;
        Messages.Add('Could not add "{$I '+ newLrsFileName +'"} to main source!');
        debugln(['TProjectResources.RenameDirectives adding include directive to main source failed']);
        Exit;
      end;
    end;
  end;
end;

function TProjectResources.Save(const MainFileName: String): Boolean;
begin
  Result := False;
  if (MainFileName = '') or not FilenameIsAbsolute(MainFileName) then
    Exit;

  SetFileNames(MainFileName);

  if (rcCodeBuf <> nil) and (SaveCodeBufferToFile(rcCodeBuf, rcFileName) = mrAbort) then
    Exit;

  if (lrsCodeBuf <> nil) and (SaveCodeBufferToFile(lrsCodeBuf, lrsFileName) = mrAbort) then
    Exit;
  Result := True;
end;

end.

