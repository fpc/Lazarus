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
  Classes, SysUtils, LCLProc, Forms, Controls, Dialogs, LResources, ExtCtrls,
  StdCtrls, ComCtrls, FileUtil, AvgLvlTree,
  // codetools
  CodeCache, CodeToolManager,
  // IDEIntf
  LazIDEIntf, TextTools, IDEMsgIntf, PackageIntf,
  // IDE
  PackageDefs, Project, IDEProcs, LazarusIDEStrConsts, MsgQuickFixes,
  PackageSystem, BasePkgManager;

type

  { TQuickFixMissingUnit }

  TQuickFixMissingUnit = class
  public
    Caption: string;
    constructor Create(aCaption: string);
  end;

  { TQuickFixMissingUnitAddRequirement }

  TQuickFixMissingUnitAddRequirement = class(TQuickFixMissingUnit)
  public
    PackageName: string;
    constructor Create(aPackageName: string);
  end;

  { TFindUnitDialog }

  TFindUnitDialog = class(TForm)
    OkButton: TButton;
    CancelButton: TButton;
    BtnPanel: TPanel;
    InfoGroupBox: TGroupBox;
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
    FMissingUnitName: string;
    FSearchPackages: TStrings;
    FSearchPackagesIndex: integer;
    fQuickFixes: TFPList;// list of TQuickFixMissingUnit
    procedure InitSearchPackages;
    procedure AddQuickFix(Item: TQuickFixMissingUnit);
    procedure AddRequirement(Item: TQuickFixMissingUnitAddRequirement);
    function MainOwnerHasRequirement(PackageName: string): boolean;
  public
    procedure InitWithMsg(Msg: TIDEMessageLine; Line: string; aCode: TCodeBuffer;
                          aMissingUnitName: string);
    property Code: TCodeBuffer read FCode;
    property MissingUnitName: string read FMissingUnitName;
    property MainOwner: TObject read FMainOwner;
  end;

  { TQuickFixUnitNotFound_Search }

  TQuickFixUnitNotFound_Search = class(TIDEMsgQuickFixItem)
  public
    constructor Create;
    function IsApplicable(Line: TIDEMessageLine): boolean; override;
    procedure Execute(const Msg: TIDEMessageLine; Step: TIMQuickFixStep); override;
  end;

procedure InitFindUnitQuickFixItems;

implementation

procedure InitFindUnitQuickFixItems;
begin
  RegisterIDEMsgQuickFix(TQuickFixUnitNotFound_Search.Create);
end;

{ TQuickFixUnitNotFound_Search }

constructor TQuickFixUnitNotFound_Search.Create;
begin
  Name:='Search unit: Error: Can''t find unit Name';
  Caption:='Search unit';
  Steps:=[imqfoMenuItem];
end;

function TQuickFixUnitNotFound_Search.IsApplicable(Line: TIDEMessageLine
  ): boolean;
const
  SearchStr = ') Fatal: Can''t find unit ';
var
  Msg: String;
  p: integer;
  Code: TCodeBuffer;
  Filename: string;
  Caret: TPoint;
begin
  Result:=false;
  if (Line.Parts=nil) then exit;
  Msg:=Line.Msg;
  p:=System.Pos(SearchStr,Msg);
  if p<1 then exit;
  inc(p,length(SearchStr));
  Line.GetSourcePosition(Filename,Caret.Y,Caret.X);
  if (Filename='') or (Caret.X<1) or (Caret.Y<1) then exit;
  if not FilenameIsAbsolute(Filename) then
    Filename:=Line.Directory+Filename;
  Code:=CodeToolBoss.LoadFile(Filename,true,false);
  if Code=nil then exit;
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
    // (FPC reports position right after the unknown identifier
    //  for example right after FilenameIsAbsolute)
    if not GetMsgLineFilename(Msg,CodeBuf) then exit;
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
  if Item is TQuickFixMissingUnitAddRequirement then
    AddRequirement(TQuickFixMissingUnitAddRequirement(Item));
end;

procedure TFindUnitDialog.OnIdle(Sender: TObject; var Done: Boolean);
var
  Filename: string;
  i: Integer;
  APackage: TLazPackage;
begin
  if (FSearchPackages<>nil) and (FSearchPackagesIndex<FSearchPackages.Count)
  then begin
    Filename:=FSearchPackages[FSearchPackagesIndex];
    // search in open packages
    for i:=0 to PackageGraph.Count-1 do begin
      APackage:=PackageGraph.Packages[i];
      if APackage.Filename=Filename then begin
        if APackage.FindUnit(MissingUnitName)<>nil then begin
          if MainOwnerHasRequirement(APackage.Name) then begin
            // already in requirements
          end else begin
            // not yet in requirements -> add a quick fix
            AddQuickFix(TQuickFixMissingUnitAddRequirement.Create(APackage.Name));
          end;
        end;
      end;
    end;
    // search in package on disk

    inc(FSearchPackagesIndex);
  end;
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
  if FMainOwner=nil then exit;
  for i:=0 to PackageGraph.Count-1 do begin
    APackage:=PackageGraph.Packages[i];
    Filename:=APackage.GetResolvedFilename(true);
    //DebugLn(['TFindUnitDialog.InitSearchPackages ',APackage.Name,' ',Filename]);
    if (Filename='') or (not FileExistsCached(Filename)) then continue;
    FSearchPackages.Add(APackage.Filename);
  end;
  //DebugLn(['TFindUnitDialog.InitSearchPackages ',FSearchPackages.Text]);

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
begin
  if FMainOwner is TProject then begin
    AProject:=TProject(FMainOwner);
    if PkgBoss.AddProjectDependencies(AProject,Item.PackageName)=mrOk then
      ModalResult:=mrOK;
  end else if FMainOwner is TLazPackage then begin
    APackage:=TLazPackage(FMainOwner);
    if PkgBoss.AddPackageDependency(APackage,Item.PackageName)=mrOk then
      ModalResult:=mrOK;
  end;
end;

function TFindUnitDialog.MainOwnerHasRequirement(PackageName: string): boolean;
var
  AProject: TProject;
  APackage: TLazPackage;
begin
  Result:=false;
  if FMainOwner=nil then exit;
  if FMainOwner is TProject then begin
    AProject:=TProject(FMainOwner);
    Result:=PackageGraph.FindDependencyRecursively(
                             AProject.FirstRequiredDependency,PackageName)<>nil;
  end else if FMainOwner is TLazPackage then begin
    APackage:=TLazPackage(FMainOwner);
    Result:=PackageGraph.FindDependencyRecursively(
                             APackage.FirstRequiredDependency,PackageName)<>nil;
  end;
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
          if FMainOwner=nil then FMainOwner:=AProject;
          OwnerNode:=InfoTreeView.Items.Add(nil,'Owner: Project');
          AddPaths(OwnerNode,'Unit search paths',AProject.ProjectDirectory,
                   AProject.CompilerOptions.GetUnitPath(true),true);

        end
        else if TObject(Owners[i]) is TLazPackage then begin
          APackage:=TLazPackage(Owners[i]);
          if FMainOwner=nil then FMainOwner:=APackage;
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

constructor TQuickFixMissingUnit.Create(aCaption: string);
begin
  Caption:=aCaption;
end;

{ TQuickFixMissingUnitAddRequirement }

constructor TQuickFixMissingUnitAddRequirement.Create(aPackageName: string);
begin
  PackageName:=aPackageName;
  Caption:='Add package '+PackageName+' as requirement';
end;

initialization
  {$I findunitdlg.lrs}

end.

