{ /***************************************************************************
                    showcompileropts.pas  -  Lazarus IDE unit
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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    Dialog for showing the compiler options as command line parameters.
}
unit ShowCompilerOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, Buttons, StdCtrls, ComCtrls, ExtCtrls,
  // LazUtils
  LazFileUtils, LazUTF8, LazStringUtils,
  // CodeTools
  CodeToolsCfgScript,
  // IdeIntf
  BaseIDEIntf, LazIDEIntf, IDEImagesIntf, CompOptsIntf, ProjectIntf, PackageIntf,
  // IDE
  LazarusIDEStrConsts, Project, PackageDefs,
  CompilerOptions, ModeMatrixOpts, MiscOptions;

type

  { TShowCompilerOptionsDlg }

  TShowCompilerOptionsDlg = class(TForm)
    CloseButton: TBitBtn;
    CmdLineMemo: TMEMO;
    CmdLineParamsTabSheet: TTabSheet;
    InheritedParamsTabSheet: TTabSheet;
    InhItemMemo: TMemo;
    InhSplitter: TSplitter;
    InhTreeView: TTreeView;
    MultilineCheckBox: TCheckBox;
    PageControl1: TPageControl;
    RelativePathsCheckBox: TCheckBox;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InhTreeViewSelectionChanged(Sender: TObject);
    procedure MultilineCheckBoxChange(Sender: TObject);
    procedure RelativePathsCheckBoxChange(Sender: TObject);
  private
    FCompilerOpts: TBaseCompilerOptions;
    ImageIndexInherited: Integer;
    ImageIndexRequired: Integer;
    ImageIndexPackage: Integer;
    InheritedChildDatas: TFPList; // list of PInheritedNodeData
    procedure ClearInheritedTree;
    procedure SetCompilerOpts(const AValue: TBaseCompilerOptions);
    procedure UpdateMemo;
    procedure UpdateInheritedTree;
  public
    property CompilerOpts: TBaseCompilerOptions read FCompilerOpts write SetCompilerOpts;
  end;

function ShowCompilerOptionsDialog(OwnerForm: TCustomForm;
  CompilerOpts: TBaseCompilerOptions): TModalResult;

implementation

{$R *.lfm}

type
  TInheritedNodeData = record
    FullText: string;
    Option: TInheritedCompilerOption;
  end;
  PInheritedNodeData = ^TInheritedNodeData;

function ShowCompilerOptionsDialog(OwnerForm: TCustomForm;
  CompilerOpts: TBaseCompilerOptions): TModalResult;
var
  ShowCompilerOptionsDlg: TShowCompilerOptionsDlg;
begin
  Result:=mrOk;
  LazarusIDE.PrepareBuildTarget(false,smsfsBackground);
  ShowCompilerOptionsDlg:=TShowCompilerOptionsDlg.Create(OwnerForm);
  try
    ShowCompilerOptionsDlg.CompilerOpts:=CompilerOpts;
    Result:=ShowCompilerOptionsDlg.ShowModal;
  finally
    ShowCompilerOptionsDlg.Free;
  end;
end;

{ TShowCompilerOptionsDlg }

procedure TShowCompilerOptionsDlg.RelativePathsCheckBoxChange(Sender: TObject);
begin
  UpdateMemo;
end;

procedure TShowCompilerOptionsDlg.ClearInheritedTree;
var
  i: integer;
  ChildData: PInheritedNodeData;
begin
  if InhTreeView = nil then
    exit;
  InhTreeView.BeginUpdate;
  // dispose all child data
  if InheritedChildDatas <> nil then
  begin
    for i := 0 to InheritedChildDatas.Count - 1 do
    begin
      ChildData := PInheritedNodeData(InheritedChildDatas[i]);
      Dispose(ChildData);
    end;
    InheritedChildDatas.Free;
    InheritedChildDatas := nil;
  end;
  InhTreeView.Items.Clear;
  InhTreeView.EndUpdate;
end;

procedure TShowCompilerOptionsDlg.InhTreeViewSelectionChanged(Sender: TObject);
var
  ANode: TTreeNode;
  ChildData: PInheritedNodeData;
  sl: TStrings;
begin
  ANode := InhTreeView.Selected;
  if (ANode = nil) or (ANode.Data = nil) then
  begin
    InhItemMemo.Lines.Text := lisSelectANode;
  end
  else
  begin
    ChildData := PInheritedNodeData(ANode.Data);
    if ChildData^.Option in icoAllSearchPaths then
    begin
      sl := SplitString(ChildData^.FullText, ';');
      InhItemMemo.Lines.Assign(sl);
      sl.Free;
    end
    else
      InhItemMemo.Lines.Text := ChildData^.FullText;
  end;
end;

procedure TShowCompilerOptionsDlg.MultilineCheckBoxChange(Sender: TObject);
begin
  UpdateMemo;
end;

procedure TShowCompilerOptionsDlg.FormCreate(Sender: TObject);
begin
  ImageIndexPackage := IDEImages.LoadImage('item_package');
  ImageIndexRequired := IDEImages.LoadImage('pkg_required');
  ImageIndexInherited := IDEImages.LoadImage('pkg_inherited');

  Caption:=dlgCompilerOptions;

  PageControl1.ActivePage:=CmdLineParamsTabSheet;
  CmdLineParamsTabSheet.Caption:=lisCommandLineParameters;
  RelativePathsCheckBox.Caption:=lisShowRelativePaths;
  RelativePathsCheckBox.Checked:=not MiscellaneousOptions.ShowCompOptFullFilenames;
  MultilineCheckBox.Caption:=lisShowMultipleLines;
  MultilineCheckBox.Checked:=not MiscellaneousOptions.ShowCompOptMultiLine;

  InheritedParamsTabSheet.Caption:=lisInheritedParameters;
  InhTreeView.Images := IDEImages.Images_16;
  InhItemMemo.Text := lisSelectANode;

  CloseButton.Caption:=lisBtnClose;
end;

procedure TShowCompilerOptionsDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  MiscellaneousOptions.ShowCompOptFullFilenames:=not RelativePathsCheckBox.Checked;
  MiscellaneousOptions.ShowCompOptMultiLine:=MultilineCheckBox.Checked;
  MiscellaneousOptions.Save;
end;

procedure TShowCompilerOptionsDlg.FormDestroy(Sender: TObject);
begin
  ClearInheritedTree;
end;

procedure TShowCompilerOptionsDlg.SetCompilerOpts(
  const AValue: TBaseCompilerOptions);
begin
  if FCompilerOpts=AValue then exit;
  FCompilerOpts:=AValue;
  UpdateMemo;
  UpdateInheritedTree;
end;

procedure TShowCompilerOptionsDlg.UpdateMemo;
var
  Flags: TCompilerCmdLineOptions;
  CurOptions: String;
  ParamList: TStrings;
begin
  if CompilerOpts=nil then exit;

  Flags:=CompilerOpts.DefaultMakeOptionsFlags+[ccloAddCompilerPath];
  if not RelativePathsCheckBox.Checked then
    Include(Flags,ccloAbsolutePaths);
  CurOptions := CompilerOpts.MakeOptionsString(Flags);
  if MultilineCheckBox.Checked then begin
    ParamList:=TStringList.Create;
    try
      SplitCmdLineParams(CurOptions,ParamList);
      CurOptions:=ParamList.Text;
    finally
      ParamList.Free;
    end;
    CmdLineMemo.ScrollBars:=ssAutoBoth;
  end else
    CmdLineMemo.ScrollBars:=ssAutoVertical;
  CmdLineMemo.Lines.Text:=CurOptions;
end;

procedure TShowCompilerOptionsDlg.UpdateInheritedTree;
var
  OptionsList: TFPList;
  i: integer;
  AncestorOptions: TAdditionalCompilerOptions;
  AncestorNode: TTreeNode;
  AncestorBaseOpts: TBaseCompilerOptions;
  Vars: TCTCfgScriptVariables;
  Macro: TLazBuildMacro;
  j: Integer;

  procedure AddChildNode(const NewNodeName, Value: string;
    Option: TInheritedCompilerOption);
  var
    VisibleValue: string;
    ChildNode: TTreeNode;
    ChildData: PInheritedNodeData;
  begin
    if Value = '' then
      exit;
    New(ChildData);
    ChildData^.FullText := Value;
    ChildData^.Option := Option;
    if InheritedChildDatas = nil then
      InheritedChildDatas := TFPList.Create;
    InheritedChildDatas.Add(ChildData);

    if UTF8Length(Value) > 100 then
      VisibleValue := UTF8Copy(Value, 1, 100) + '[...]'
    else
      VisibleValue := Value;
    ChildNode := InhTreeView.Items.AddChildObject(AncestorNode,
      NewNodeName + ' = "' + VisibleValue + '"', ChildData);
    ChildNode.ImageIndex := ImageIndexRequired;
    ChildNode.SelectedIndex := ChildNode.ImageIndex;
  end;

var
  SkippedPkgList: TFPList;
  AProject: TProject;
  Pkg: TLazPackage;
  t: TBuildMatrixGroupType;

  procedure AddMatrixGroupNode(Grp: TBuildMatrixGroupType);
  begin
    if AncestorNode<>nil then exit;
    AncestorNode := InhTreeView.Items.Add(nil, '');
    case Grp of
    bmgtEnvironment: AncestorNode.Text:=dlgGroupEnvironment;
    bmgtProject: AncestorNode.Text:=dlgProject;
    bmgtSession: AncestorNode.Text:=lisProjectSession;
    end;
    AncestorNode.ImageIndex := ImageIndexPackage;
    AncestorNode.SelectedIndex := AncestorNode.ImageIndex;
  end;

  procedure AddMatrixGroup(Grp: TBuildMatrixGroupType);
  var
    CustomOptions: String;
    OutDir: String;
  begin
    AncestorNode := nil;
    CustomOptions:='';
    OnAppendCustomOption(CompilerOpts,CustomOptions,[Grp]);
    if CustomOptions<>'' then begin
      AddMatrixGroupNode(Grp);
      AddChildNode(liscustomOptions, CustomOptions, icoCustomOptions);
    end;
    OutDir:='.*';
    OnGetOutputDirectoryOverride(CompilerOpts,OutDir,[Grp]);
    if OutDir<>'.*' then begin
      AddMatrixGroupNode(Grp);
      AddChildNode('Output directory', OutDir, icoNone);
    end;
    if AncestorNode<>nil then
      AncestorNode.Expand(true);
  end;

begin
  if CompilerOpts=nil then exit;
  OptionsList := nil;
  //debugln(['TCompilerInheritedOptionsFrame.UpdateInheritedTree START CompilerOpts=',DbgSName(CompilerOpts)]);
  CompilerOpts.GetInheritedCompilerOptions(OptionsList);
  SkippedPkgList:=nil;
  try
    if CompilerOpts is TProjectCompilerOptions then begin
      AProject:=TProjectCompilerOptions(CompilerOpts).LazProject;
      AProject.GetAllRequiredPackages(SkippedPkgList);
      if (SkippedPkgList<>nil)
      and (not (pfUseDesignTimePackages in AProject.Flags)) then begin
        // keep design time only packages
        for i:=SkippedPkgList.Count-1 downto 0 do
          if TLazPackage(SkippedPkgList[i]).PackageType<>lptDesignTime then
            SkippedPkgList.Delete(i);
      end;
    end;
    //debugln(['TCompilerInheritedOptionsFrame.UpdateInheritedTree END']);
    InhTreeView.BeginUpdate;
    ClearInheritedTree;
    if OptionsList <> nil then
    begin
      Vars:=GetBuildMacroValues(CompilerOpts,false);
      // add All node
      AncestorNode := InhTreeView.Items.Add(nil, lisAllInheritedOptions);
      AncestorNode.ImageIndex := ImageIndexInherited;
      AncestorNode.SelectedIndex := AncestorNode.ImageIndex;
      with CompilerOpts do
      begin
        AddChildNode(lisunitPath,
          GetInheritedOption(icoUnitPath, True), icoUnitPath);
        AddChildNode(lisincludePath,
          GetInheritedOption(icoIncludePath, True), icoIncludePath);
        AddChildNode(lisobjectPath,
          GetInheritedOption(icoObjectPath, True), icoObjectPath);
        AddChildNode(lislibraryPath,
          GetInheritedOption(icoLibraryPath, True), icoLibraryPath);
        AddChildNode(lislinkerOptions, GetInheritedOption(icoLinkerOptions, True),
          icoLinkerOptions);
        AddChildNode(liscustomOptions, GetInheritedOption(icoCustomOptions, True),
          icoCustomOptions);
      end;
      AncestorNode.Expanded := True;
      // add detail nodes
      for i := 0 to OptionsList.Count - 1 do
      begin
        AncestorOptions := TAdditionalCompilerOptions(OptionsList[i]);
        AncestorNode := InhTreeView.Items.Add(nil, '');
        AncestorNode.Text := AncestorOptions.GetOwnerName;
        AncestorNode.ImageIndex := ImageIndexPackage;
        AncestorNode.SelectedIndex := AncestorNode.ImageIndex;
        AncestorBaseOpts:=AncestorOptions.GetBaseCompilerOptions;
        with AncestorOptions.ParsedOpts do
        begin
          AddChildNode(lisunitPath,
            CreateRelativeSearchPath(GetParsedValue(pcosUnitPath),CompilerOpts.BaseDirectory),
            icoUnitPath);
          AddChildNode(lisincludePath,
            CreateRelativeSearchPath(GetParsedValue(pcosIncludePath),CompilerOpts.BaseDirectory),
            icoIncludePath);
          AddChildNode(lisobjectPath,
            CreateRelativeSearchPath(GetParsedValue(pcosObjectPath),CompilerOpts.BaseDirectory),
            icoObjectPath);
          AddChildNode(lislibraryPath,
            CreateRelativeSearchPath(GetParsedValue(pcosLibraryPath),CompilerOpts.BaseDirectory),
            icoLibraryPath);
          AddChildNode(lislinkerOptions, GetParsedValue(pcosLinkerOptions),
            icoLinkerOptions);
          AddChildNode(liscustomOptions, GetParsedValue(pcosCustomOptions),
            icoCustomOptions);
        end;
        if (AncestorBaseOpts<>nil) and (Vars<>nil) then begin
          for j:=0 to AncestorBaseOpts.BuildMacros.Count-1 do
          begin
            Macro:=AncestorBaseOpts.BuildMacros[j];
            AddChildNode(Macro.Identifier,Vars.Values[Macro.Identifier],icoNone);
          end;
        end;
        AncestorNode.Expanded := True;
      end;
      OptionsList.Free;
    end else
    begin
      InhTreeView.Items.Add(nil, lisNoCompilerOptionsInherited);
    end;
    if SkippedPkgList<>nil then begin
      for i:=0 to SkippedPkgList.Count-1 do begin
        Pkg:=TLazPackage(SkippedPkgList[i]);
        AncestorNode := InhTreeView.Items.Add(nil, '');
        AncestorNode.Text := Format(lisExcludedAtRunTime, [Pkg.Name]);
        AncestorNode.ImageIndex := ImageIndexPackage;
        AncestorNode.SelectedIndex := AncestorNode.ImageIndex;
      end;
    end;

    // add matrix options
    for t:=low(TBuildMatrixGroupType) to high(TBuildMatrixGroupType) do
      AddMatrixGroup(t);

    InhTreeView.EndUpdate;
  finally
    SkippedPkgList.Free;
  end;
end;

end.

