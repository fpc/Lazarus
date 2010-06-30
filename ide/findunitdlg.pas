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
    Dialog to search a missing unit.

  ToDo:
    - search in packages on disk

}
unit FindUnitDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, FileUtil, AvgLvlTree,
  // codetools
  CodeToolsStrConsts, Laz_XMLCfg, CodeCache, CodeToolManager,
  // IDEIntf
  LazIDEIntf, TextTools, IDEMsgIntf, PackageIntf,
  // IDE
  DialogProcs, PackageDefs, Project, IDEProcs, LazarusIDEStrConsts, MsgQuickFixes,
  PackageLinks, PackageSystem, BasePkgManager;

type
  TFindUnitDialog = class;

  { TQuickFixMissingUnit }

  TQuickFixMissingUnit = class
  public
    Dlg: TFindUnitDialog;
    Caption: string;
    constructor Create(aDlg: TFindUnitDialog; aCaption: string);
  end;

  { TQuickFixMissingUnitRemoveFromUses }

  TQuickFixMissingUnitRemoveFromUses = class(TQuickFixMissingUnit)
  public
    constructor Create(aDlg: TFindUnitDialog);
  end;

  { TQuickFixMissingUnitAddRequirement }

  TQuickFixMissingUnitAddRequirement = class(TQuickFixMissingUnit)
  public
    PackageName: string;
    constructor Create(aDlg: TFindUnitDialog; aPackageName: string);
  end;

  { TFindUnitDialog }

  TFindUnitDialog = class(TForm)
    OkButton: TButton;
    CancelButton: TButton;
    BtnPanel: TPanel;
    InfoGroupBox: TGroupBox;
    ProgressBar1: TProgressBar;
    QuickFixRadioGroup: TRadioGroup;
    Splitter1: TSplitter;
    InfoTreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  private
    FCode: TCodeBuffer;
    FMainOwner: TObject;
    FMainOwnerName: string;
    FMissingUnitName: string;
    FSearchPackages: TStrings;
    FSearchPackagesIndex: integer;
    fQuickFixes: TFPList;// list of TQuickFixMissingUnit
    fLastUpdateProgressBar: TDateTime;
    procedure InitSearchPackages;
    procedure OnIteratePkgLinks(APackage: TLazPackageID);
    procedure AddQuickFix(Item: TQuickFixMissingUnit);
    procedure AddRequirement(Item: TQuickFixMissingUnitAddRequirement);
    procedure RemoveFromUsesSection(Item: TQuickFixMissingUnitRemoveFromUses);
    function MainOwnerHasRequirement(PackageName: string): boolean;
    procedure UpdateProgressBar;
    function CheckPackageOnDisk(PkgFilename: string): boolean;
    function FindQuickFixAddRequirement(PkgName: string): TQuickFixMissingUnitAddRequirement;
  public
    procedure InitWithMsg(Msg: TIDEMessageLine; Line: string; aCode: TCodeBuffer;
                          aMissingUnitName: string);
    property Code: TCodeBuffer read FCode;
    property MissingUnitName: string read FMissingUnitName;
    property MainOwner: TObject read FMainOwner;
    property MainOwnerName: string read FMainOwnerName;
  end;

  { TQuickFixUnitNotFound_Search }

  TQuickFixUnitNotFound_Search = class(TIDEMsgQuickFixItem)
  public
    constructor Create;
    function IsApplicable(Line: TIDEMessageLine): boolean; override;
    procedure Execute(const Msg: TIDEMessageLine; Step: TIMQuickFixStep); override;
  end;

  { TQuickFixIncludeNotFound_Search }

  TQuickFixIncludeNotFound_Search = class(TIDEMsgQuickFixItem)
  public
    constructor Create;
    function IsCodetoolsErrorIncludeFileNotFound(Msg: string;
                                              out IncludeFile: string): boolean;
    function IsApplicable(Line: TIDEMessageLine): boolean; override;
    procedure Execute(const Msg: TIDEMessageLine; Step: TIMQuickFixStep); override;
  end;

procedure InitFindUnitQuickFixItems;

implementation

{$R *.lfm}

procedure InitFindUnitQuickFixItems;
begin
  RegisterIDEMsgQuickFix(TQuickFixUnitNotFound_Search.Create);
end;

{ TQuickFixUnitNotFound_Search }

constructor TQuickFixUnitNotFound_Search.Create;
begin
  Name:='Search unit: Error: Can''t find unit Name';
  Caption:=lisSearchUnit;
  Steps:=[imqfoMenuItem];
end;

function TQuickFixUnitNotFound_Search.IsApplicable(Line: TIDEMessageLine
  ): boolean;
const
  SearchStr = ') Fatal: Can''t find unit ';
var
  Msg: String;
  p: integer;
begin
  Result:=false;
  if (Line.Parts=nil) then exit;
  Msg:=Line.Msg;
  p:=System.Pos(SearchStr,Msg);
  if p<1 then exit;
  Result:=true;
end;

procedure TQuickFixUnitNotFound_Search.Execute(const Msg: TIDEMessageLine;
  Step: TIMQuickFixStep);
var
  AnUnitName: String;
  Dlg: TFindUnitDialog;
  CodeBuf: TCodeBuffer;
  Filename: string;
  Caret: TPoint;
begin
  if Step=imqfoMenuItem then begin
    DebugLn(['TQuickFixUnitNotFound_Search.Execute ']);
    // get source position
    if not GetMsgLineFilename(Msg,CodeBuf,false) then exit;
    Msg.GetSourcePosition(Filename,Caret.Y,Caret.X);
    if not LazarusIDE.BeginCodeTools then begin
      DebugLn(['TQuickFixUnitNotFound_Search.Execute failed because IDE busy']);
      exit;
    end;

    // get unitname
    if not REMatches(Msg.Msg,'Fatal: Can''t find unit ([a-z_0-9]+) ','I') then begin
      DebugLn('TQuickFixUnitNotFound_Search invalid message ',Msg.Msg);
      exit;
    end;
    AnUnitName:=REVar(1);
    DebugLn(['TQuickFixUnitNotFound_Search.Execute Unit=',AnUnitName]);

    if (AnUnitName='') or (not IsValidIdent(AnUnitName)) then begin
      DebugLn(['TQuickFixUnitNotFound_Search.Execute not an identifier "',dbgstr(AnUnitName),'"']);
      exit;
    end;

    // show dialog
    Dlg:=TFindUnitDialog.Create(nil);
    try
      Dlg.InitWithMsg(Msg,Msg.Msg,CodeBuf,AnUnitName);
      Dlg.ShowModal;
    finally
      Dlg.Free;
    end;
  end;
end;

{ TFindUnitDialog }

procedure TFindUnitDialog.FormCreate(Sender: TObject);
begin
  fQuickFixes:=TFPList.Create;

  Caption:=lisFindMissingUnit;
  CancelButton.Caption:=dlgCancel;
  OkButton.Caption:=dlgMouseOptBtnOk;
  OkButton.Enabled:=false;
  InfoGroupBox.Caption:=lisInformation;
  QuickFixRadioGroup.Caption:=lisQuickFixes;

  Application.AddOnIdleHandler(@OnIdle);
end;

procedure TFindUnitDialog.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  Application.RemoveOnIdleHandler(@OnIdle);
  for i:=0 to fQuickFixes.Count-1 do TObject(fQuickFixes[i]).Free;
  FreeAndNil(fQuickFixes);
  FreeAndNil(FSearchPackages);
end;

procedure TFindUnitDialog.OkButtonClick(Sender: TObject);
var
  i: LongInt;
  Item: TQuickFixMissingUnit;
begin
  i:=QuickFixRadioGroup.ItemIndex;
  if i<0 then begin
    OkButton.Enabled:=false;
    exit;
  end;
  Item:=TQuickFixMissingUnit(fQuickFixes[i]);
  if Item is TQuickFixMissingUnitRemoveFromUses then
    RemoveFromUsesSection(TQuickFixMissingUnitRemoveFromUses(Item))
  else if Item is TQuickFixMissingUnitAddRequirement then
    AddRequirement(TQuickFixMissingUnitAddRequirement(Item));
end;

procedure TFindUnitDialog.OnIdle(Sender: TObject; var Done: Boolean);
var
  Filename: string;
  i: Integer;
  APackage: TLazPackage;
  Found: Boolean;
  t: TDateTime;
begin
  t:=Now;
  while (FSearchPackages<>nil) and (FSearchPackagesIndex<FSearchPackages.Count)
  do begin
    Filename:=FSearchPackages[FSearchPackagesIndex];
    Found:=false;

    // search in open packages
    for i:=0 to PackageGraph.Count-1 do begin
      APackage:=PackageGraph.Packages[i];
      if APackage.Filename=Filename then begin
        Found:=true;
        if APackage.FindUnit(MissingUnitName)<>nil then begin
          if MainOwnerHasRequirement(APackage.Name) then begin
            // already in requirements
          end else if FindQuickFixAddRequirement(APackage.Name)=nil then begin
            // not yet in requirements -> add a quick fix
            AddQuickFix(TQuickFixMissingUnitAddRequirement.Create(Self,APackage.Name));
          end;
        end;
        break;
      end;
    end;

    // search in package on disk
    if not Found then begin
      if CheckPackageOnDisk(Filename) then Found:=true;
    end;

    inc(FSearchPackagesIndex);
    if FSearchPackagesIndex>=FSearchPackages.Count then begin
      AddQuickFix(TQuickFixMissingUnitRemoveFromUses.Create(Self));
    end;

    UpdateProgressBar;
    Done:=false;
    if Now-t>0.5/86400 then
      exit;
    // process another package
  end;
  Done:=true;
end;

procedure TFindUnitDialog.InitSearchPackages;
var
  i: Integer;
  APackage: TLazPackage;
  Filename: String;
begin
  if FSearchPackages=nil then
    FSearchPackages:=TStringList.Create;
  FSearchPackages.Clear;
  FSearchPackagesIndex:=0;
  if MainOwner=nil then exit;

  // add open packages
  for i:=0 to PackageGraph.Count-1 do begin
    APackage:=PackageGraph.Packages[i];
    Filename:=APackage.GetResolvedFilename(true);
    //DebugLn(['TFindUnitDialog.InitSearchPackages ',APackage.Name,' ',Filename]);
    if (Filename='') or (not FileExistsCached(Filename)) then continue;
    FSearchPackages.Add(APackage.Filename);
  end;
  //DebugLn(['TFindUnitDialog.InitSearchPackages ',FSearchPackages.Text]);

  // add user package links
  PkgLinks.IteratePackages(false,@OnIteratePkgLinks,[ploUser,ploGlobal]);

  if FSearchPackages.Count>0 then begin
    ProgressBar1.Max:=FSearchPackages.Count;
    fLastUpdateProgressBar:=Now;
    ProgressBar1.Visible:=true;
  end;
end;

procedure TFindUnitDialog.OnIteratePkgLinks(APackage: TLazPackageID);
var
  Link: TPackageLink;
begin
  if APackage is TPackageLink then begin
    Link:=TPackageLink(APackage);
    FSearchPackages.Add(TrimFilename(Link.GetEffectiveFilename));
  end;
end;

procedure TFindUnitDialog.AddQuickFix(Item: TQuickFixMissingUnit);
begin
  fQuickFixes.Add(Item);
  QuickFixRadioGroup.Items.Add(Item.Caption);
  if QuickFixRadioGroup.ItemIndex<0 then
    QuickFixRadioGroup.ItemIndex:=0;
  OkButton.Enabled:=true;
end;

procedure TFindUnitDialog.AddRequirement(
  Item: TQuickFixMissingUnitAddRequirement);
var
  AProject: TProject;
  APackage: TLazPackage;
  NewDependency: TPkgDependency;
begin
  if MainOwner is TProject then begin
    AProject:=TProject(MainOwner);
    //debugln(['TFindUnitDialog.AddRequirement project: ',Item.PackageName]);
    NewDependency:=TPkgDependency.Create;
    NewDependency.PackageName:=Item.PackageName;
    if PkgBoss.AddProjectDependency(AProject,NewDependency)=mrOk then
      ModalResult:=mrOK;
  end else if MainOwner is TLazPackage then begin
    APackage:=TLazPackage(MainOwner);
    if PkgBoss.AddPackageDependency(APackage,Item.PackageName)=mrOk then
      ModalResult:=mrOK;
  end;
end;

procedure TFindUnitDialog.RemoveFromUsesSection(
  Item: TQuickFixMissingUnitRemoveFromUses);
begin
  if not CodeToolBoss.RemoveUnitFromAllUsesSections(Code,MissingUnitName) then
  begin

  end;
  ModalResult:=mrOk;
end;

function TFindUnitDialog.MainOwnerHasRequirement(PackageName: string): boolean;
var
  AProject: TProject;
  APackage: TLazPackage;
begin
  Result:=false;
  if MainOwner=nil then exit;
  if MainOwner is TProject then begin
    AProject:=TProject(MainOwner);
    Result:=PackageGraph.FindDependencyRecursively(
                             AProject.FirstRequiredDependency,PackageName)<>nil;
  end else if MainOwner is TLazPackage then begin
    APackage:=TLazPackage(MainOwner);
    Result:=PackageGraph.FindDependencyRecursively(
                             APackage.FirstRequiredDependency,PackageName)<>nil;
  end;
end;

procedure TFindUnitDialog.UpdateProgressBar;
begin
  if (FSearchPackages=nil) or (FSearchPackagesIndex>=FSearchPackages.Count) then
  begin
    ProgressBar1.Visible:=false;
  end;
  if Now-fLastUpdateProgressBar>1/86400 then begin
    ProgressBar1.Position:=FSearchPackagesIndex;
    fLastUpdateProgressBar:=Now;
  end;
end;

function TFindUnitDialog.CheckPackageOnDisk(PkgFilename: string): boolean;
var
  r: TModalResult;
  XMLConfig: TXMLConfig;
  XMLCode: TCodeBuffer;
  Path: String;
  FileCount: LongInt;
  i: Integer;
  SubPath: String;
  FileType: TPkgFileType;
  AUnitName: String;
  Filename: String;
  PkgName: String;
begin
  Result:=false;
  PkgName:=ExtractFileNameOnly(PkgFilename);
  if FindQuickFixAddRequirement(PkgName)<>nil then exit;
  if not FileExistsCached(PkgFilename) then exit;
  //DebugLn(['TFindUnitDialog.CheckPackageOnDisk ',PkgFilename]);

  XMLConfig:=nil;
  XMLCode:=nil;
  try
    //DebugLn(['TFindUnitDialog.CheckPackageOnDisk loading: ',PkgFilename]);
    XMLConfig:=TXMLConfig.Create(nil);
    r:=LoadXMLConfigFromCodeBuffer(PkgFilename,XMLConfig,
                         XMLCode,[lbfUpdateFromDisk,lbfRevert,lbfQuiet],false);
    if r<>mrOk then begin
      //DebugLn(['TFindUnitDialog.CheckPackageOnDisk failed loading: ',PkgFilename]);
      exit;
    end;
    Path:='Package/Files/';
    FileCount:=XMLConfig.GetValue(Path+'Count',0);
    //DebugLn(['TFindUnitDialog.CheckPackageOnDisk FileCount=',FileCount,' ',PkgName]);
    for i:=1 to FileCount do begin
      SubPath:=Path+'Item'+IntToStr(i)+'/';
      FileType:=PkgFileTypeIdentToType(XMLConfig.GetValue(SubPath+'Type/Value',''));
      if not (FileType in PkgFileRealUnitTypes) then continue;
      Filename:=XMLConfig.GetValue(SubPath+'Filename/Value','');
      AUnitName:=ExtractFileNameOnly(Filename);
      //DebugLn(['TFindUnitDialog.CheckPackageOnDisk ',UnitName]);
      if SysUtils.CompareText(AUnitName,MissingUnitName)=0 then begin
        Result:=true;
        AddQuickFix(TQuickFixMissingUnitAddRequirement.Create(Self,PkgName));
        exit;
      end;
    end;
  finally
    XMLConfig.Free;
  end;
end;

function TFindUnitDialog.FindQuickFixAddRequirement(PkgName: string
  ): TQuickFixMissingUnitAddRequirement;
var
  i: Integer;
begin
  Result:=nil;
  if fQuickFixes=nil then exit;
  for i:=0 to fQuickFixes.Count-1 do begin
    if TObject(fQuickFixes[i]) is TQuickFixMissingUnitAddRequirement then begin
      Result:=TQuickFixMissingUnitAddRequirement(fQuickFixes[i]);
      if SysUtils.CompareText(Result.PackageName,PkgName)=0 then exit;
    end;
  end;
  Result:=nil;
end;

procedure TFindUnitDialog.InitWithMsg(Msg: TIDEMessageLine; Line: string;
  aCode: TCodeBuffer; aMissingUnitName: string);

  procedure AddPaths(ParentTVNode: TTreeNode; PathTitle, BaseDir, Paths: string;
    Expanded: boolean);
  var
    p: Integer;
    s: String;
    PathsNode: TTreeNode;
  begin
    PathsNode:=InfoTreeView.Items.AddChild(ParentTVNode,PathTitle);
    Paths:=CreateAbsoluteSearchPath(Paths,BaseDir);
    p:=1;
    repeat
      s:=GetNextDirectoryInSearchPath(Paths,p);
      if s<>'' then
        InfoTreeView.Items.AddChild(PathsNode,dbgstr(s));
    until p>length(Paths);
    PathsNode.Expanded:=Expanded;
    if (ParentTVNode<>nil) and Expanded then
      ParentTVNode.Expanded:=true;
  end;

var
  UnitPath: String;
  Directory: String;
  DirNode: TTreeNode;
  Owners: TFPList;
  i: Integer;
  OwnerNode: TTreeNode;
  AProject: TProject;
  APackage: TLazPackage;
begin
  FCode:=aCode;
  FMissingUnitName:=aMissingUnitName;
  FMainOwner:=nil;
  FMainOwnerName:='';

  InfoTreeView.BeginUpdate;
  InfoTreeView.Items.Clear;

  Owners:=nil;
  try
    InfoTreeView.Items.Add(nil,'Message: '+dbgstr(Line));
    InfoTreeView.Items.Add(nil,'File: '+dbgstr(aCode.Filename));
    InfoTreeView.Items.Add(nil,'Missingg unit: '+dbgstr(aMissingUnitName));
    Directory:=ExtractFilePath(aCode.Filename);
    DirNode:=InfoTreeView.Items.Add(nil,'Directory: '+dbgstr(Directory));

    // unit path of directory
    UnitPath:=CodeToolBoss.GetUnitPathForDirectory(Directory);
    AddPaths(DirNode,'IDE unit search path:',Directory,UnitPath,false);

    //
    Owners:=PackageEditingInterface.GetOwnersOfUnit(aCode.Filename);
    if Owners<>nil then begin
      for i:=0 to Owners.Count-1 do begin
        if TObject(Owners[i]) is TProject then begin
          AProject:=TProject(Owners[i]);
          if FMainOwner=nil then begin
            FMainOwner:=AProject;
            FMainOwnerName:='project';
          end;
          OwnerNode:=InfoTreeView.Items.Add(nil,'Owner: Project');
          AddPaths(OwnerNode,'Unit search paths',AProject.ProjectDirectory,
                   AProject.CompilerOptions.GetUnitPath(true),true);

        end
        else if TObject(Owners[i]) is TLazPackage then begin
          APackage:=TLazPackage(Owners[i]);
          if FMainOwner=nil then begin
            FMainOwner:=APackage;
            FMainOwnerName:=APackage.Name;
          end;
          OwnerNode:=InfoTreeView.Items.Add(nil,'Owner: Package '+APackage.IDAsString);
          AddPaths(OwnerNode,'Unit search paths',APackage.Directory,
                   APackage.CompilerOptions.GetUnitPath(true),true);
        end;
      end;
    end;
  finally
    Owners.Free;
  end;

  InfoTreeView.EndUpdate;

  InitSearchPackages;
end;

{ TQuickFixMissingUnit }

constructor TQuickFixMissingUnit.Create(aDlg: TFindUnitDialog; aCaption: string);
begin
  Dlg:=aDlg;;
  Caption:=aCaption;
end;

{ TQuickFixMissingUnitAddRequirement }

constructor TQuickFixMissingUnitAddRequirement.Create(aDlg: TFindUnitDialog;
  aPackageName: string);
begin
  PackageName:=aPackageName;
  Caption:='Add package '+PackageName+' as requirement to '+aDlg.MainOwnerName;
end;

{ TQuickFixMissingUnitRemoveFromUses }

constructor TQuickFixMissingUnitRemoveFromUses.Create(aDlg: TFindUnitDialog);
begin
  Caption:='Remove unit from uses clause';
end;

{ TQuickFixIncludeNotFound_Search }

constructor TQuickFixIncludeNotFound_Search.Create;
begin
  Name:='Search include file: Error: include file not found';
  Caption:=lisSearchUnit;
  Steps:=[imqfoMenuItem];
end;

function TQuickFixIncludeNotFound_Search.IsCodetoolsErrorIncludeFileNotFound(
  Msg: string; out IncludeFile: string): boolean;
var
  SearchStr: String;
  p: integer;
  StartPos: LongInt;
begin
  IncludeFile:='';
  // check for codetools 'include file not found'
  SearchStr:=ctsIncludeFileNotFound;
  p:=System.Pos('%',SearchStr);
  if p>0 then SearchStr:=copy(SearchStr,1,p-1);
  SearchStr:=SearchStr+': '+SearchStr; // e.g.: ': include file not found "'
  p:=System.Pos(SearchStr,Msg);
  if p<1 then exit(false);
  Result:=true;
  inc(p,length(SearchStr));
  StartPos:=p;
  while (p<=length(Msg)) and (Msg[p]<>'"') do inc(p);
  IncludeFile:=copy(Msg,StartPos,p-StartPos);
end;

function TQuickFixIncludeNotFound_Search.IsApplicable(Line: TIDEMessageLine
  ): boolean;
var
  Filename: string;
begin
  Result:=IsCodetoolsErrorIncludeFileNotFound(Line.Msg,Filename)
          and (Filename<>'');
end;

procedure TQuickFixIncludeNotFound_Search.Execute(const Msg: TIDEMessageLine;
  Step: TIMQuickFixStep);
var
  CodeBuf: TCodeBuffer;
  Filename, IncludeFilename: string;
  Caret: TPoint;
  Dlg: TFindUnitDialog;
begin
  if Step=imqfoMenuItem then begin
    DebugLn(['TQuickFixIncludeNotFound_Search.Execute ']);
    // get source position
    if not GetMsgLineFilename(Msg,CodeBuf,false) then exit;
    Msg.GetSourcePosition(Filename,Caret.Y,Caret.X);
    if not LazarusIDE.BeginCodeTools then begin
      DebugLn(['TQuickFixIncludeNotFound_Search.Execute failed because IDE busy']);
      exit;
    end;

    // get include file name
    if not IsCodetoolsErrorIncludeFileNotFound(Msg.Msg,IncludeFilename) then
    begin
      DebugLn('TQuickFixIncludeNotFound_Search invalid message ',Msg.Msg);
      exit;
    end;
    DebugLn(['TQuickFixIncludeNotFound_Search.Execute include file=',IncludeFilename]);

    // show dialog
    Dlg:=TFindUnitDialog.Create(nil);
    try
      Dlg.InitWithMsg(Msg,Msg.Msg,CodeBuf,IncludeFilename);
      Dlg.ShowModal;
    finally
      Dlg.Free;
    end;
  end;
end;

end.

