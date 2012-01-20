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

 Author: Mattias Gaertner

 Abstract:
   IDE dialog showing stats about the IDE.
}
unit IDEInfoDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, LCLProc, LazHelpHTML, LazHelpIntf, DefineTemplates, EnvironmentOpts,
  AboutFrm, LazConf, IDEHelpIntf, LazarusIDEStrConsts, Project, SourceEditor,
  PackageSystem, PackageDefs;

type

  { TIDEInfoDialog }

  TIDEInfoDialog = class(TForm)
    GeneralMemo: TMemo;
    HelpMemo: TMemo;
    ModifiedMemo: TMemo;
    PageControl1: TPageControl;
    GeneralTabSheet: TTabSheet;
    ModifiedTabSheet: TTabSheet;
    HelpTabSheet: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    procedure GatherHelpDB(Prefix: string; const HelpDB: THelpDatabase; const sl: TStrings);
    procedure GatherHelpViewer(Prefix: string; const Viewer: THelpViewer; const sl: TStrings);
    // general
    procedure GatherIDEVersion(sl: TStrings);
    procedure GatherParameters(sl: TStrings);
    procedure GatherEnvironmentVars(sl: TStrings);
    procedure GatherGlobalOptions(sl: TStrings);
    // modified
    procedure GatherModifiedProject(AProject: TProject; sl: TStrings);
    procedure GatherModifiedPackages(sl: TStrings);
    // help
    procedure GatherHelpDatabases(sl: TStrings);
    procedure GatherHelpViewers(sl: TStrings);
  public
    procedure UpdateGeneralMemo;
    procedure UpdateModifiedMemo;
    procedure UpdateHelpMemo;
  end;

var
  IDEInfoDialog: TIDEInfoDialog;

function ShowIDEInfo: TModalResult;


implementation

function ShowIDEInfo: TModalResult;
var
  Dlg: TIDEInfoDialog;
begin
  Dlg:=TIDEInfoDialog.Create(nil);
  try
    Result:=Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

{$R *.lfm}

{ TIDEInfoDialog }

procedure TIDEInfoDialog.FormCreate(Sender: TObject);
begin
  Caption:=lisIDEInfoInformationAboutTheIDE;

  UpdateGeneralMemo;
  UpdateModifiedMemo;
  UpdateHelpMemo;
  PageControl1.ActivePage:=GeneralTabSheet;
end;

procedure TIDEInfoDialog.GatherHelpDB(Prefix: string;
  const HelpDB: THelpDatabase; const sl: TStrings);
var
  DBIRegExprMessage: THelpDBIRegExprMessage;
  DBISourceDirectories: THelpDBISourceDirectories;
  DBISourceDirectory: THelpDBISourceDirectory;
  DBISourceFile: THelpDBISourceFile;
  DBItem: THelpDBItem;
  i: Integer;
  HTMLHelp: THTMLHelpDatabase;
begin
  sl.Add(Prefix+DbgSName(HelpDB));
  sl.Add(Prefix+'ID='+HelpDB.ID);
  sl.Add(Prefix+'LocalizedName='+dbgstr(HelpDB.GetLocalizedName));
  if HelpDB.BasePathObject<>nil then
    sl.Add(Prefix+'BasePathObject='+DbgSName(HelpDB.BasePathObject));
  sl.Add(Prefix+'Registered='+dbgs(HelpDB.Registered));
  if HelpDB.SupportedMimeTypes<>nil then begin
    sl.Add(Prefix+'SupportedMimeTypes: '+IntToStr(HelpDB.SupportedMimeTypes.Count));
    for i:=0 to HelpDB.SupportedMimeTypes.Count-1 do
      sl.Add(Prefix+'  '+HelpDB.SupportedMimeTypes[i]);
  end;
  if HelpDB is THTMLHelpDatabase then begin
    HTMLHelp:=THTMLHelpDatabase(HelpDB);
    sl.Add(Prefix+'BaseURL='+dbgstr(HTMLHelp.BaseURL));
    if HTMLHelp.BaseURL='' then
      sl.Add(Prefix+'DefaultBaseURL='+dbgstr(HTMLHelp.DefaultBaseURL));
    sl.Add(Prefix+'KeywordPrefix='+dbgstr(HTMLHelp.KeywordPrefix));
  end;

  // registered items
  sl.Add(Prefix+'Items:');
  for i:=0 to HelpDB.RegisteredItemCount-1 do begin
    DBItem:=HelpDB.GetRegisteredItem(i);
    sl.Add(Prefix+'  '+IntToStr(i+1)+'/'+IntToStr(HelpDB.RegisteredItemCount)+': '+DbgSName(
      DBItem));
    if DBItem is THelpDBISourceFile then begin
      DBISourceFile:=THelpDBISourceFile(DBItem);
      sl.Add(Prefix+'    Filename='+DBISourceFile.Filename);
      sl.Add(Prefix+'    FullFilename='+DBISourceFile.GetFullFilename);
      sl.Add(Prefix+'    BasePath='+DBISourceFile.GetBasePath);
      if DBISourceFile is THelpDBISourceDirectory then begin
        DBISourceDirectory:=THelpDBISourceDirectory(DBISourceFile);
        sl.Add(Prefix+'    FileMask='+DBISourceDirectory.FileMask);
        sl.Add(Prefix+'    WithSubDirectories='+dbgs(DBISourceDirectory.
          WithSubDirectories));
        if DBISourceDirectory is THelpDBISourceDirectories then begin
          DBISourceDirectories:=THelpDBISourceDirectories(DBISourceDirectory);
          sl.Add(Prefix+'    BaseDirectory='+DBISourceDirectories.BaseDirectory);
        end;
      end;
    end else if DBItem is THelpDBIClass then begin
      sl.Add(Prefix+'    Class='+DbgSName(THelpDBIClass(DBItem)));
    end else if DBItem is THelpDBIRegExprMessage then begin
      DBIRegExprMessage:=THelpDBIRegExprMessage(DBItem);
      sl.Add(Prefix+'    Expression='+DBIRegExprMessage.Expression);
      sl.Add(Prefix+'    ModifierStr='+DBIRegExprMessage.ModifierStr);
    end;
  end;
  sl.Add('');
end;

procedure TIDEInfoDialog.GatherHelpViewer(Prefix: string;
  const Viewer: THelpViewer; const sl: TStrings);
var
  i: Integer;
begin
  sl.Add(Prefix+DbgSName(Viewer));
  sl.Add(Prefix+'StorageName='+Viewer.StorageName);
  sl.Add(Prefix+'ParameterHelp='+Viewer.ParameterHelp);
  sl.Add(Prefix+'LocalizedName='+dbgstr(Viewer.GetLocalizedName));
  if Viewer.SupportedMimeTypes<>nil then begin
    sl.Add(Prefix+'SupportedMimeTypes: '+IntToStr(Viewer.SupportedMimeTypes.Count));
    for i:=0 to Viewer.SupportedMimeTypes.Count-1 do
      sl.Add(Prefix+'  '+Viewer.SupportedMimeTypes[i]);
  end;
  sl.Add('');
end;

procedure TIDEInfoDialog.GatherIDEVersion(sl: TStrings);
const
  LazarusVersionStr= {$I version.inc};
begin
  sl.Add('Lazarus version: '+GetLazarusVersionString);
  sl.Add('Lazarus svn revision: '+LazarusRevisionStr);
  sl.Add('Lazarus build date: '+{$I %date%});
  sl.Add('Lazarus was compiled for '+GetCompiledTargetCPU+'-'+GetCompiledTargetOS);
  sl.Add('Lazarus was compiled with fpc '+{$I %FPCVERSION%});
  sl.Add('');
end;

procedure TIDEInfoDialog.GatherParameters(sl: TStrings);
var
  i: Integer;
begin
  sl.Add('Working directory='+GetCurrentDirUTF8);
  sl.Add('Application.ExeName='+Application.ExeName);
  sl.Add('');
  sl.add('Parameters:');
  for i:=0 to Paramcount do
    sl.Add(ParamStrUTF8(i));
  sl.Add('');
end;

procedure TIDEInfoDialog.GatherEnvironmentVars(sl: TStrings);
var
  i: Integer;
begin
  sl.Add('Environment variables:');
  for i:=0 to GetEnvironmentVariableCount-1 do
    sl.Add(GetEnvironmentStringUTF8(i));
  sl.Add('');
end;

procedure TIDEInfoDialog.GatherGlobalOptions(sl: TStrings);
begin
  sl.add('Global IDE options:');
  sl.Add('Primary config directory='+GetPrimaryConfigPath);
  sl.Add('Secondary config directory='+GetSecondaryConfigPath);
  sl.Add('LazarusDirectory='+EnvironmentOptions.LazarusDirectory);
  sl.Add('CompilerFilename='+EnvironmentOptions.CompilerFilename);
  sl.Add('Real CompilerFilename='+EnvironmentOptions.GetCompilerFilename);
  sl.Add('CompilerMessagesFilename='+EnvironmentOptions.CompilerMessagesFilename);
  sl.Add('FPC source directory='+EnvironmentOptions.FPCSourceDirectory);
  sl.Add('Real FPC source directory='+EnvironmentOptions.GetFPCSourceDirectory);
  sl.Add('Test directory='+EnvironmentOptions.GetTestBuildDirectory);
  sl.Add('');
end;

procedure TIDEInfoDialog.GatherModifiedProject(AProject: TProject; sl: TStrings);
var
  aFile: TUnitInfo;
  HeaderWritten: Boolean;
  s: String;
begin
  // summary
  if AProject.Modified then
    sl.Add('Project.Modified');
  if AProject.SessionModified then
    sl.Add('Project.SessionModified');
  if Project1.SomethingModified(true,false) then
    sl.Add('Project.SomethingModified Data');
  if Project1.SomethingModified(false,true) then
    sl.Add('Project.SomethingModified Session');
  if SourceEditorManager.SomethingModified(false) then
    sl.Add('SourceEditorManager.SomethingModified');
  if AProject.BuildModes.IsModified(false) then
    sl.Add('Project.BuildModes.IsModified data');
  if AProject.BuildModes.IsModified(true) then
    sl.Add('Project.BuildModes.IsModified session');
  sl.Add('');

  // details
  HeaderWritten:=false;
  aFile:=AProject.FirstPartOfProject;
  while aFile<>nil do begin
    if aFile.Modified or aFile.SessionModified
    or ((aFile.Source<>nil) and aFile.Source.Modified)
    then begin
      if not HeaderWritten then begin
        sl.Add('Project units:');
        s:=aFile.GetShortFilename(true);
        if aFile.Modified then
          s:=s+' Modified';
        if aFile.SessionModified then
          s:=s+' SessionModified';
        if (aFile.Source<>nil) and (aFile.Source.Modified) then
          s:=s+' Source.Modified';
        sl.Add(s);
      end;
    end;
    aFile:=aFile.NextPartOfProject;
  end;
  if HeaderWritten then
    sl.Add('');
end;

procedure TIDEInfoDialog.GatherModifiedPackages(sl: TStrings);
var
  i: Integer;
  Pkg: TLazPackage;
  HeaderWritten: Boolean;
begin
  HeaderWritten:=false;
  for i:=0 to PackageGraph.Count-1 do begin
    Pkg:=PackageGraph[i];
    if Pkg.Modified then begin
      if not HeaderWritten then begin
        HeaderWritten:=true;
        sl.Add('Packages:');
      end;
      sl.Add(Pkg.Name);
    end;
  end;
  if HeaderWritten then
    sl.Add('');
end;

procedure TIDEInfoDialog.GatherHelpDatabases(sl: TStrings);
var
  i: Integer;
begin
  sl.Add('LazarusHelp='+DbgSName(LazarusHelp));
  sl.Add('HelpDatabases='+DbgSName(HelpDatabases));
  for i:=0 to HelpDatabases.Count-1 do begin
    sl.Add('DB '+IntToStr(i+1)+'/'+IntToStr(HelpDatabases.Count));
    GatherHelpDB('  ',HelpDatabases.Items[i],sl);
  end;
end;

procedure TIDEInfoDialog.GatherHelpViewers(sl: TStrings);
var
  i: Integer;
begin
  sl.Add('HelpViewers='+DbgSName(HelpViewers));
  for i:=0 to HelpViewers.Count-1 do begin
    sl.Add('DB '+IntToStr(i+1)+'/'+IntToStr(HelpViewers.Count));
    GatherHelpViewer('  ',HelpViewers.Items[i],sl);
  end;
end;

procedure TIDEInfoDialog.UpdateGeneralMemo;
var
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    GatherIDEVersion(sl);
    GatherGlobalOptions(sl);
    GatherParameters(sl);
    GatherEnvironmentVars(sl);
    GeneralMemo.Lines.Assign(sl);
  finally
    sl.Free;
  end;
end;

procedure TIDEInfoDialog.UpdateModifiedMemo;
var
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    GatherModifiedProject(Project1,sl);
    GatherModifiedPackages(sl);
    ModifiedMemo.Lines.Assign(sl);
  finally
    sl.Free;
  end;
end;

procedure TIDEInfoDialog.UpdateHelpMemo;
var
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    GatherHelpDatabases(sl);
    GatherHelpViewers(sl);
    HelpMemo.Lines.Assign(sl);
  finally
    sl.Free;
  end;
end;

end.

