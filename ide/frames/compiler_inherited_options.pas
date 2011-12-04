{ /***************************************************************************
                      compileroptions.pp  -  Lazarus IDE unit
                      ---------------------------------------
                   Compiler options sets the switches for the project
                   file for the FPC compiler.


                   Initial Revision  : Sat May 10 23:15:32 CST 1999


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
unit compiler_inherited_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls,
  CodeToolsCfgScript, IDEOptionsIntf, IDEImagesIntf, ProjectIntf, CompOptsIntf,
  Project, PackageDefs, CompilerOptions, LazarusIDEStrConsts, IDEProcs;

type

  { TCompilerInheritedOptionsFrame }

  TCompilerInheritedOptionsFrame = class(TAbstractIDEOptionsEditor)
    InhItemMemo: TMemo;
    InhNoteLabel: TLabel;
    InhSplitter: TSplitter;
    InhTreeView: TTreeView;
    procedure InhTreeViewSelectionChanged(Sender: TObject);
  private
    ImageIndexInherited: Integer;
    ImageIndexRequired: Integer;
    ImageIndexPackage: Integer;
    InheritedChildDatas: TList; // list of PInheritedNodeData
    procedure ClearInheritedTree;
  public
    destructor Destroy; override;
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    procedure UpdateInheritedTree(CompilerOpts: TBaseCompilerOptions);
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

type
  TInheritedNodeData = record
    FullText: string;
    Option: TInheritedCompilerOption;
  end;
  PInheritedNodeData = ^TInheritedNodeData;


{ TCompilerInheritedOptionsFrame }

function TCompilerInheritedOptionsFrame.GetTitle: string;
begin
  Result := dlgCOInherited;
end;

procedure TCompilerInheritedOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  ImageIndexPackage := IDEImages.LoadImage(16, 'item_package');
  ImageIndexRequired := IDEImages.LoadImage(16, 'pkg_required');
  ImageIndexInherited := IDEImages.LoadImage(16, 'pkg_inherited');
  InhNoteLabel.Caption := lisAdditionalCompilerOptionsInheritedFromPackages;
  InhTreeView.Images := IDEImages.Images_16;
  InhItemMemo.Text := lisSelectANode;
end;

procedure TCompilerInheritedOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  UpdateInheritedTree(AOptions as TBaseCompilerOptions);
end;

procedure TCompilerInheritedOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin

end;

class function TCompilerInheritedOptionsFrame.SupportedOptionsClass:
TAbstractIDEOptionsClass;
begin
  Result := TBaseCompilerOptions;
end;

procedure TCompilerInheritedOptionsFrame.UpdateInheritedTree(
  CompilerOpts: TBaseCompilerOptions);
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
      InheritedChildDatas := TList.Create;
    InheritedChildDatas.Add(ChildData);

    if length(Value) > 100 then
      VisibleValue := copy(Value, 1, 100) + '[...]'
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
begin
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
            CreateRelativeSearchPath(GetParsedValue(pcosUnitPath),
            CompilerOpts.BaseDirectory),
            icoUnitPath);
          AddChildNode(lisincludePath,
            CreateRelativeSearchPath(GetParsedValue(pcosIncludePath),
            CompilerOpts.BaseDirectory),
            icoIncludePath);
          AddChildNode(lisobjectPath,
            CreateRelativeSearchPath(GetParsedValue(pcosObjectPath),
            CompilerOpts.BaseDirectory),
            icoObjectPath);
          AddChildNode(lislibraryPath,
            CreateRelativeSearchPath(GetParsedValue(pcosLibraryPath),
            CompilerOpts.BaseDirectory),
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
    end
    else
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
    InhTreeView.EndUpdate;
  finally
    SkippedPkgList.Free;
  end;
end;

destructor TCompilerInheritedOptionsFrame.Destroy;
begin
  ClearInheritedTree;
  inherited Destroy;
end;

procedure TCompilerInheritedOptionsFrame.ClearInheritedTree;
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


procedure TCompilerInheritedOptionsFrame.InhTreeViewSelectionChanged(Sender: TObject);
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

initialization
  RegisterIDEOptionsEditor(GroupCompiler, TCompilerInheritedOptionsFrame,
    CompilerOptionsInherited);
  RegisterIDEOptionsEditor(GroupPkgCompiler, TCompilerInheritedOptionsFrame,
    CompilerOptionsInherited);

end.

