unit compiler_inherited_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, IDEOptionsIntf, Project, CompilerOptions,
  LazarusIDEStrConsts, IDEImagesIntf, IDEProcs;

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
    procedure UpdateInheritedTree(CompilerOpts: TBaseCompilerOptions);
  public
    destructor Destroy; override;
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

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

begin
  OptionsList := nil;
  CompilerOpts.GetInheritedCompilerOptions(OptionsList);
  InhTreeView.BeginUpdate;
  ClearInheritedTree;
  if OptionsList <> nil then
  begin
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
      AncestorNode.Expanded := True;
    end;
    OptionsList.Free;
  end
  else
  begin
    InhTreeView.Items.Add(nil, lisNoCompilerOptionsInherited);
  end;
  InhTreeView.EndUpdate;
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
  {$I compiler_inherited_options.lrs}
  RegisterIDEOptionsEditor(GroupCompiler, TCompilerInheritedOptionsFrame,
    CompilerOptionsInherited);

end.

