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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

 Author: Mattias Gaertner

 Abstract:
   IDE dialog showing packages needing (re)build.
}
unit IDEInfoNeedBuild;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz_AVL_Tree,
  // LCL
  Forms, Controls, StdCtrls, ButtonPanel, LCLType,
  // LazUtils
  LazUtilities,
  // IdeIntf
  IDEWindowIntf, LazIDEIntf, ProjectIntf, PackageIntf,
  // IDE
  LazarusIDEStrConsts, PackageDefs, PackageSystem, Project, InputHistory,
  EnvironmentOpts, IDEProcs, BuildManager;

type
  TINeedBuild = (
    inbNone,
    inbNo,
    inbNormal,
    inbClean
    );

  TInfoNeedBuildItem = class
  public
    Target: TObject; // TProject, TLazPackage, LazarusIDE
    Caption: string;
    Filename: string;
    NeedBuild: TINeedBuild;
    Note: string;
  end;

  { TIDEInfoNeedBuildDlg }

  TIDEInfoNeedBuildDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    MainMemo: TMemo;
    TargetComboBox: TComboBox;
    TargetLabel: TLabel;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure TargetComboBoxChange(Sender: TObject);
  private
    FIdleConnected: boolean;
    FMainTarget: TObject;
    FSkipDesignTimePackages: boolean;
    FTargetToItem: TAvlTree; // tree of TInfoNeedBuildItem sorted for Target
    FTargets: TFPList; // topologically sorted list of TInfoNeedBuildItem, last=main
    procedure FillTargets;
    function ProjectAsTarget(AProject: TProject): string;
    function IDEAsTarget: string;
    procedure SetIdleConnected(AValue: boolean);
    function CheckNeedBuild(All: boolean): boolean;// true = complete
    function GetTargets(Target: string): TFPList;
    function HaveSameTargets(BuildItems, Targets: TFPList): boolean;
    procedure ClearTargets;
    procedure SetMainTarget(AValue: TObject);
  public
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
    property MainTarget: TObject read FMainTarget write SetMainTarget;
    property SkipDesignTimePackages: boolean read FSkipDesignTimePackages
                                             write FSkipDesignTimePackages;
  end;

procedure ShowNeedBuildDialog;

function CompareInfoNeedBuildItemWithTargets(Info1, Info2: Pointer): integer;
function CompareTargetWithInfoNeedBuildItem(Target, Info: Pointer): integer;

implementation

{$R *.lfm}

procedure ShowNeedBuildDialog;
var
  IDEInfoNeedBuildDlg: TIDEInfoNeedBuildDlg;
begin
  IDEInfoNeedBuildDlg:=TIDEInfoNeedBuildDlg.Create(nil);
  try
    IDEInfoNeedBuildDlg.ShowModal;
  finally
    IDEInfoNeedBuildDlg.Free;
  end;
end;

function CompareInfoNeedBuildItemWithTargets(Info1, Info2: Pointer): integer;
var
  Item1: TInfoNeedBuildItem absolute Info1;
  Item2: TInfoNeedBuildItem absolute Info2;
begin
  Result:=ComparePointers(Item1.Target,Item2.Target);
end;

function CompareTargetWithInfoNeedBuildItem(Target, Info: Pointer): integer;
var
  Item: TInfoNeedBuildItem absolute Info;
begin
  Result:=ComparePointers(Target,Item.Target);
end;

{ TIDEInfoNeedBuildDlg }

procedure TIDEInfoNeedBuildDlg.FormCreate(Sender: TObject);
var
  Target: String;
begin
  FTargetToItem:=TAvlTree.Create(@CompareInfoNeedBuildItemWithTargets);
  FTargets:=TFPList.Create;

  Caption:=lisWhatNeedsBuilding;

  TargetLabel.Caption:=lisTarget;
  FillTargets;
  Target:=InputHistories.ViewNeedBuildTarget;
  if (Target<>'') and (TargetComboBox.Items.IndexOf(Target)>=0) then
    TargetComboBox.Text:=Target
  else
    TargetComboBox.Text:=TargetComboBox.Items[0];

  IDEDialogLayoutList.ApplyLayout(Self);
  IdleConnected:=true;
  TargetComboBox.DropDownCount:=EnvironmentOptions.DropDownCount;
end;

procedure TIDEInfoNeedBuildDlg.FormDestroy(Sender: TObject);
begin
  IdleConnected:=false;
  ClearTargets;
  FreeAndNil(FTargetToItem);
  FreeAndNil(FTargets);
  MainTarget:=nil;
end;

procedure TIDEInfoNeedBuildDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then
    ModalResult:=mrCancel;
end;

procedure TIDEInfoNeedBuildDlg.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if CheckNeedBuild(false) then
    IdleConnected:=false;
end;

procedure TIDEInfoNeedBuildDlg.TargetComboBoxChange(Sender: TObject);
begin
  IdleConnected:=true;
end;

procedure TIDEInfoNeedBuildDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  Target: TCaption;
begin
  IdleConnected:=false;
  IDEDialogLayoutList.SaveLayout(Self);
  Target:=TargetComboBox.Text;
  if Target=ProjectAsTarget(Project1) then
    Target:='';
  InputHistories.ViewNeedBuildTarget:=Target;
end;

procedure TIDEInfoNeedBuildDlg.FillTargets;
var
  sl: TStringList;
  i: Integer;
begin
  sl:=TStringList.Create;
  try
    for i:=0 to PackageGraph.Count-1 do
      sl.Add(PackageGraph[i].Name);
    sl.Sort;
    sl.Insert(0,IDEAsTarget);
    if Project1<>nil then
      sl.Insert(0,ProjectAsTarget(Project1));
    TargetComboBox.Items.Assign(sl);
  finally
    sl.Free;
  end;
end;

function TIDEInfoNeedBuildDlg.ProjectAsTarget(AProject: TProject): string;
begin
  if AProject=nil then
    Result:=''
  else
    Result:=Format(lisProject, [AProject.Title]);
end;

function TIDEInfoNeedBuildDlg.IDEAsTarget: string;
begin
  Result:=lisLazarusIDE;
end;

procedure TIDEInfoNeedBuildDlg.SetIdleConnected(AValue: boolean);
begin
  if FIdleConnected=AValue then Exit;
  if AValue and (ComponentState*[csDestroying,csLoading]<>[]) then exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

function TIDEInfoNeedBuildDlg.CheckNeedBuild(All: boolean): boolean;
var
  NewTargets: TFPList;
  i: Integer;
  Item: TInfoNeedBuildItem;
  s: String;
  Pkg: TLazPackage;
  AProject: TProject;
  NeedBuildAll: boolean;
  NeedBuild: TModalResult;
begin
  SkipDesignTimePackages:=false;
  NewTargets:=GetTargets(TargetComboBox.Text);
  try
    if not HaveSameTargets(FTargets,NewTargets) then begin
      // targets have changed
      ClearTargets;
      if NewTargets=nil then exit(true);
      for i:=0 to NewTargets.Count-1 do begin
        Item:=TInfoNeedBuildItem.Create;
        Item.Target:=TObject(NewTargets[i]);
        if Item.Target=LazarusIDE then begin
          Item.Caption:=IDEAsTarget;
          Item.Filename:=EnvironmentOptions.LazarusDirectory;
        end
        else if Item.Target is TProject then begin
          AProject:=TProject(Item.Target);
          Item.Caption:=ProjectAsTarget(AProject);
          Item.Filename:=AProject.ProjectInfoFile;
          SkipDesignTimePackages:=not (pfUseDesignTimePackages in AProject.Flags);
        end
        else if Item.Target is TLazPackage then begin
          Pkg:=TLazPackage(Item.Target);
          Pkg.Flags:=Pkg.Flags-[lpfNeedGroupCompile];
          Item.Caption:=Pkg.IDAsString;
          Item.Filename:=Pkg.Filename;
        end;
        FTargetToItem.Add(Item);
        FTargets.Add(Item);
      end;
    end;
  finally
    NewTargets.Free;
  end;
  Result:=true;

  // check
  if (FTargets.Count>0) then
    MainTarget:=TInfoNeedBuildItem(FTargets.Last).Target
  else
    MainTarget:=nil;

  i:=0;
  while i<FTargets.Count do begin
    Item:=TInfoNeedBuildItem(FTargets[i]);
    if Item.NeedBuild=inbNone then begin
      Item.NeedBuild:=inbNo;
      if Item.Target=LazarusIDE then begin
        // no check available
      end
      else if Item.Target is TProject then begin
        AProject:=TProject(Item.Target);
        Item.Note:='';
        NeedBuild:=MainBuildBoss.DoCheckIfProjectNeedsCompilation(AProject,
                                         NeedBuildAll,Item.Note);
        if NeedBuild=mrYes then begin
          if NeedBuildAll then
            Item.NeedBuild:=inbClean
          else
            Item.NeedBuild:=inbNormal;
        end;
      end
      else if Item.Target is TLazPackage then begin
        Pkg:=TLazPackage(Item.Target);
        Item.Note:='';
        NeedBuild:=PackageGraph.CheckIfPackageNeedsCompilation(
                        Pkg,SkipDesignTimePackages,true,NeedBuildAll,Item.Note);
        if NeedBuild=mrYes then begin
          PackageGraph.SetFlagDependenciesNeedBuild(Pkg);
          if NeedBuildAll then
            Item.NeedBuild:=inbClean
          else
            Item.NeedBuild:=inbNormal;
        end;
      end;
      if not All then break;
    end;
    inc(i);
  end;
  Result:=i=FTargets.Count; // true = all checked

  // update memo
  s:='';
  for i:=0 to FTargets.Count-1 do begin
    Item:=TInfoNeedBuildItem(FTargets[i]);
    s+='Target: '+Item.Caption+LineEnding;
    case Item.NeedBuild of
    inbNone: s+='checking ...';
    inbNo: s+='No build needed.';
    inbNormal: s+='Build needed.';
    inbClean: s+='Clean build needed.';
    end;
    s+=LineEnding;
    if Item.Filename<>'' then
      s+='File: '+Item.Filename+LineEnding;
    if Item.Note<>'' then
      s+='Note: '+Item.Note+LineEnding;
    s+=LineEnding;
  end;
  MainMemo.Lines.Text:=s;
end;

function TIDEInfoNeedBuildDlg.GetTargets(Target: string): TFPList;

  function GetList(Main: TObject; FirstDependency: TPkgDependency;
    ReqFlags: TPkgIntfRequiredFlags): TFPList;
  begin
    Result:=nil;
    if Main=nil then exit;
    PackageGraph.GetAllRequiredPackages(nil,FirstDependency,Result,ReqFlags);
    if Result<>nil then begin
      // PackageGraph.GetAllRequiredPackages starts with the inner nodes
      // => reverse order
      ReverseList(Result);
    end
    else
      Result:=TFPList.Create;
    Result.Add(Main);
  end;

var
  Pkg: TLazPackage;
  ReqFlags: TPkgIntfRequiredFlags;
begin
  ReqFlags:=[];
  if Target=IDEAsTarget then begin
    Result:=GetList(LazarusIDE,PackageGraph.FirstAutoInstallDependency,ReqFlags);
  end else if Target=ProjectAsTarget(Project1) then begin
    if not (pfUseDesignTimePackages in Project1.Flags) then
      Include(ReqFlags,pirSkipDesignTimeOnly);
    Result:=GetList(Project1,Project1.FirstRequiredDependency,ReqFlags);
  end else begin
    Pkg:=PackageGraph.FindPackageWithName(Target,nil);
    Result:=GetList(Pkg,Pkg.FirstRequiredDependency,ReqFlags);
  end;
end;

function TIDEInfoNeedBuildDlg.HaveSameTargets(BuildItems, Targets: TFPList
  ): boolean;
// check if BuildItems and Targets have the same targets
var
  Targets1: TFPList;
  Targets2: TFPList;
  i: Integer;
  Target: TObject;
begin
  Result:=false;
  if (BuildItems=nil)<>(Targets=nil) then exit;
  Targets1:=TFPList.Create;
  Targets2:=TFPList.Create;
  try
    // create a list of targets from BuildItems and a second list from Targets
    for i:=0 to BuildItems.Count-1 do
      Targets1.Add(TInfoNeedBuildItem(BuildItems[i]).Target);
    for i:=0 to Targets.Count-1 do begin
      Target:=TObject(Targets[i]);
      if (Target=LazarusIDE) or (Target is TProject) or (Target is TLazPackage) then
        Targets2.Add(Target);
    end;
    if Targets1.Count<>Targets2.Count then exit;
    // sort both lists
    Targets1.Sort(@ComparePointers);
    Targets2.Sort(@ComparePointers);
    // compare each item
    for i:=0 to Targets1.Count-1 do
      if Targets1[i]<>Targets2[i] then exit;
  finally
    Targets1.Free;
    Targets2.Free;
  end;
  Result:=true;
end;

procedure TIDEInfoNeedBuildDlg.ClearTargets;
begin
  FTargetToItem.FreeAndClear;
  FTargets.Clear;
end;

procedure TIDEInfoNeedBuildDlg.SetMainTarget(AValue: TObject);
begin
  if FMainTarget=AValue then Exit;
  FMainTarget:=AValue;
  //debugln(['TIDEInfoNeedBuildDlg.SetMainTarget ',DbgSName(MainTarget)]);
  if (MainTarget=LazarusIDE) then
    MainBuildBoss.SetBuildTargetIDE
  else
    MainBuildBoss.SetBuildTargetProject1(true);
end;

end.

