{
 *****************************************************************************
  This file is part of the EducationLaz package

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner, Michael Kuhardt

  Abstract:
    Frame to setup the component palette.
}
unit EduCompPalette;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz_AVL_Tree,
  // LCL
  LCLProc, LCLType, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls,
  // LazUtils
  LazConfigStorage, AvgLvlTree,
  // IdeIntf
  FormEditingIntf, IDEOptionsIntf, IDEOptEditorIntf, ComponentReg, IDEImagesIntf,
  // Education
  EduOptions;

type
  { TEduComponentPaletteOptions }

  TEduComponentPaletteOptions = class(TEduOptionsNode)
  private
    fVisible: TStringToStringTree;
    function GetComponentVisible(ComponentName: string): boolean;
    procedure SetComponentVisible(ComponentName: string; const AValue: boolean);
    procedure VoteForVisible(AComponent: TRegisteredComponent; var Vote: integer);
  public
    constructor Create; override;
    destructor Destroy; override;
    function Load(Config: TConfigStorage): TModalResult; override;
    function Save(Config: TConfigStorage): TModalResult; override;
    procedure Apply(Enable: boolean); override;
    property ComponentVisible[ComponentName: string]: boolean read GetComponentVisible write SetComponentVisible;
  end;

  { TEduCompPaletteFrame }

  TEduCompPaletteFrame = class(TAbstractIDEOptionsEditor)
    ShowExtendedButton: TButton;
    ShowMinimalButton: TButton;
    ComponentsGroupBox: TGroupBox;
    ComponentsTreeView: TTreeView;
    HideAllButton: TButton;
    LeftPanel: TPanel;
    ShowAllButton: TButton;
    procedure ComponentsTreeViewMouseDown(Sender: TObject;
      Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure FrameClick(Sender: TObject);
    procedure HideAllButtonClick(Sender: TObject);
    procedure ShowAllButtonClick(Sender: TObject);
    procedure ShowExtendedButtonClick(Sender: TObject);
    procedure ShowMinimalButtonClick(Sender: TObject);
  private
    HideImgID: LongInt;
    ShowImgID: LongInt;
    fCompNameToImgIndex: TStringToPointerTree; // Component.ClassName to index+1 in TreeViews.Images
    procedure FillComponentTreeView;
    procedure SaveFillComponentTreeView;
    procedure ShowHideAll(aShow: boolean);
    procedure ShowSelected(extended: boolean);
  public
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
  end;

var
  EduComponentPaletteOptions: TEduComponentPaletteOptions = nil;

procedure Register;

implementation

procedure Register;
begin
  EduComponentPaletteOptions:=TEduComponentPaletteOptions.Create;
  EducationOptions.Root.Add(EduComponentPaletteOptions);
  EduOptionCompPaletteID:=RegisterIDEOptionsEditor(EduOptionID,
                            TEduCompPaletteFrame,EduOptionCompPaletteID)^.Index;
end;

{ TEduCompPaletteFrame }

procedure TEduCompPaletteFrame.ComponentsTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  Hit: THitTests;
begin
  if Button<>mbLeft then exit;
  Node:=ComponentsTreeView.GetNodeAt(X,Y);
  if (Node=nil) then exit;
  if Node.Parent=nil then exit;
  Hit:=ComponentsTreeView.GetHitTestInfoAt(X,Y);
  if [htOnIcon,htOnStateIcon]*Hit<>[] then begin
    if Node.StateIndex=ShowImgID then
      Node.StateIndex:=HideImgID
    else
      Node.StateIndex:=ShowImgID;
  end;
end;

procedure TEduCompPaletteFrame.FrameClick(Sender: TObject);
begin

end;

procedure TEduCompPaletteFrame.HideAllButtonClick(Sender: TObject);
begin
  ShowHideAll(false);
end;

procedure TEduCompPaletteFrame.ShowAllButtonClick(Sender: TObject);
begin
  ShowHideAll(true);
end;

procedure TEduCompPaletteFrame.ShowExtendedButtonClick(Sender: TObject);
begin
  ShowHideAll(false);
  ShowSelected(true);
end;

procedure TEduCompPaletteFrame.ShowMinimalButtonClick(Sender: TObject);
begin
  ShowHideAll(false);
  ShowSelected(false);
end;

procedure TEduCompPaletteFrame.FillComponentTreeView;

  function GetIconCopy(ResName: string): TCustomBitMap;
  var
    ResHandle: TLResource;
  begin
    Result := nil;
    // prevent raising exception and speedup a bit search/load
    ResHandle := LazarusResources.Find(ResName);
    if ResHandle <> nil then
      Result := CreateBitmapFromLazarusResource(ResHandle)
    else
    if FindResource(HInstance, PChar(ResName), PChar(RT_BITMAP)) <> 0 then
    begin
      Result := TBitmap.Create;
      Result.LoadFromResourceName(HInstance, ResName);
      Result.Transparent := True;
    end
    else
    if FindResource(HInstance, PChar(ResName), PChar(RT_RCDATA)) <> 0 then
      Result := CreateBitmapFromResourceName(HInstance, ResName);
  end;

  function GetCompImgIndex(ResName: string): integer;
  var
    Img: TCustomBitmap;
  begin
    if fCompNameToImgIndex=nil then
      fCompNameToImgIndex:=TStringToPointerTree.Create(false);
    if fCompNameToImgIndex.Contains(ResName) then begin
      Result:=PtrUInt(fCompNameToImgIndex[ResName])-1;
    end else begin
      // load
      Img:=GetIconCopy(ResName);
      if Img=nil then
        Img:=GetIconCopy('unregisteredcomponent');
      if Img<>nil then begin
        Result:=ComponentsTreeView.Images.Add(Img,nil);
        Img.Free;
      end else begin
        Result:=-1;
      end;
      fCompNameToImgIndex[ResName]:=Pointer(PtrUInt(Result+1));
    end;
  end;

var
  i: Integer;
  Page: TBaseComponentPage;
  PageNode: TTreeNode;
  CompNode: TTreeNode;
  CompName: String;
  Comp: TRegisteredComponent;
begin
  ComponentsTreeView.BeginUpdate;
  ComponentsTreeView.Items.Clear;
  if ComponentsTreeView.Images=nil then begin
    ComponentsTreeView.Images:=TImageList.Create(Self);
    ComponentsTreeView.Images.Width:=ComponentPaletteImageWidth;
    ComponentsTreeView.Images.Height:=ComponentPaletteImageHeight;
    ComponentsTreeView.StateImages:=IDEImages.Images_16;
  end else
    ComponentsTreeView.Images.Clear;
  ShowImgID:=IDEImages.LoadImage('menu_run');
  HideImgID:=IDEImages.LoadImage('menu_stop');
  for i:=0 to IDEComponentPalette.Comps.Count-1 do begin
    Comp:=IDEComponentPalette.Comps[i];
    Page:=Comp.RealPage;
    if (Page=nil) or (Page.PageName='') then continue;
    PageNode:=ComponentsTreeView.Items.FindTopLvlNode(Page.PageName);
    if PageNode=nil then
      PageNode:=ComponentsTreeView.Items.Add(nil,Page.PageName);
    CompName:=Comp.ComponentClass.ClassName;
    CompNode:=ComponentsTreeView.Items.AddChild(PageNode,CompName);
    CompNode.ImageIndex:=GetCompImgIndex(CompName);
    CompNode.SelectedIndex:=CompNode.ImageIndex;
    if EduComponentPaletteOptions.ComponentVisible[CompName] then
      CompNode.StateIndex:=ShowImgID
    else
      CompNode.StateIndex:=HideImgID;
    PageNode.Expanded:=true;
  end;
  ComponentsTreeView.EndUpdate;
end;

procedure TEduCompPaletteFrame.SaveFillComponentTreeView;
var
  Node: TTreeNode;
  CompName: String;
begin
  Node:=ComponentsTreeView.Items.GetFirstNode;
  while Node<>nil do begin
    if Node.Parent<>nil then begin
      CompName:=Node.Text;
      EduComponentPaletteOptions.ComponentVisible[CompName]:=
        Node.StateIndex=ShowImgID;
    end else begin

    end;
    Node:=Node.GetNext;
  end;
end;

procedure TEduCompPaletteFrame.ShowHideAll(aShow: boolean);
var
  Node: TTreeNode;
  CompName: String;
begin
  ComponentsTreeView.BeginUpdate;
  Node:=ComponentsTreeView.Items.GetFirstNode;
  while Node<>nil do begin
    if Node.Parent<>nil then begin
      CompName:=Node.Text;

      EduComponentPaletteOptions.ComponentVisible[CompName]:=aShow;
      if aShow then
        Node.StateIndex:=ShowImgID
      else
        Node.StateIndex:=HideImgID;
    end

    else begin

    end;

    Node:=Node.GetNext;
  end;
  ComponentsTreeView.EndUpdate;
end;

procedure TEduCompPaletteFrame.ShowSelected(extended: boolean);
var
  Node: TTreeNode;
  CompName: String;
  MinimalComponents: array[0..12] of String;
  ExtendedComponents: array[0..25] of String;
  i,k: integer;
begin

  MinimalComponents[0] :=  'TEdit';
  MinimalComponents[1] :=  'TButton';
  MinimalComponents[2] :=  'TCheckBox';
  MinimalComponents[3] :=  'TLabel';
  MinimalComponents[4] :=  'TListBox';
  MinimalComponents[5] :=  'TComboBox';
  MinimalComponents[6] :=  'TRadioGroup';
  MinimalComponents[7] :=  'TRadioButton';
  MinimalComponents[8] :=  'TPanel';
  MinimalComponents[9] :=  'TMainMenu';
  MinimalComponents[10] :=  'TMemo';
  MinimalComponents[11] :=  'TGroupBox';
  MinimalComponents[12] :=  'TImage';

  ExtendedComponents[0] :=  'TBitBtn';
  ExtendedComponents[1] :=  'TScrollBar';
  ExtendedComponents[2] :=  'TPopupMenu';
  ExtendedComponents[3] :=  'TCheckGroup';
  ExtendedComponents[4] :=  'TActionList';
  ExtendedComponents[5] :=  'TStringGrid';
  ExtendedComponents[6] :=  'TSpeedButton';
  ExtendedComponents[7] :=  'TTimer';
  ExtendedComponents[8] :=  'TIdleTimer';
  ExtendedComponents[9] :=  'TPageControl';
  ExtendedComponents[10] :=  'TStaticText';
  ExtendedComponents[11] :=  'TDBGrid';
  ExtendedComponents[12] :=  'TOpenDialog';
  ExtendedComponents[13] :=  'TSaveDialog';
  ExtendedComponents[14] :=  'TSelectDirectoryDialog';
  ExtendedComponents[15] :=  'TDataSource';
  ExtendedComponents[16] :=  'TDBNavigator';
  ExtendedComponents[17] :=  'TDBText';
  ExtendedComponents[18] :=  'TDBEdit';
  ExtendedComponents[19] :=  'TDBMemo';
  ExtendedComponents[20] :=  'TDBImage';
  ExtendedComponents[21] :=  'TDBListBox';
  ExtendedComponents[22] :=  'TDBLookupListBox';
  ExtendedComponents[23] :=  'TDBComboBox';
  ExtendedComponents[24] :=  'TDBLookupComboBox';
  ExtendedComponents[25] :=  'TDBCheckBox';

  ComponentsTreeView.BeginUpdate;
  Node:=ComponentsTreeView.Items.GetFirstNode;
  while Node<>nil do begin
    if Node.Parent<>nil then begin
      CompName:=Node.Text;
      for i := 0 to 12 do begin
        if (CompareText (CompName , MinimalComponents[i] )=0) then begin
            EduComponentPaletteOptions.ComponentVisible[CompName]:=true;
            Node.StateIndex:=ShowImgID;
        end;
      end;

      if extended then begin
        for k := 0 to 25 do begin
          if (CompareText (CompName , ExtendedComponents[k] )=0) then begin
            EduComponentPaletteOptions.ComponentVisible[CompName]:=true;
            Node.StateIndex:=ShowImgID;
          end;
        end;
      end;

    end;

    Node:=Node.GetNext;
  end;
  ComponentsTreeView.EndUpdate;
end;

destructor TEduCompPaletteFrame.Destroy;
begin
  FreeAndNil(fCompNameToImgIndex);
  inherited Destroy;
end;

function TEduCompPaletteFrame.GetTitle: String;
begin
  Result:=ersEduCompPaletteTitle;
end;

procedure TEduCompPaletteFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions=EducationOptions then begin
    FillComponentTreeView;
  end;
end;

procedure TEduCompPaletteFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  ShowAllButton.Caption:=ersShowAll;
  HideAllButton.Caption:=ersHideAll;
  ShowMinimalButton.Caption:=ersShowMinimal;
  ShowExtendedButton.Caption:=ersShowExtended;
  ComponentsGroupBox.Caption:=ersVisibleComponents;
end;

class function TEduCompPaletteFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=EducationIDEOptionsClass;
end;

procedure TEduCompPaletteFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions=EducationOptions then begin
    SaveFillComponentTreeView;
  end;
end;

{ TEduComponentPaletteOptions }

function TEduComponentPaletteOptions.GetComponentVisible(ComponentName: string): boolean;
begin
  Result:=fVisible[ComponentName]='1';
end;

procedure TEduComponentPaletteOptions.SetComponentVisible(
  ComponentName: string; const AValue: boolean);
begin
  if AValue then
    fVisible[ComponentName]:='1'
  else
    fVisible.Remove(ComponentName);
end;

procedure TEduComponentPaletteOptions.VoteForVisible(
  AComponent: TRegisteredComponent; var Vote: integer);
begin
  if not ComponentVisible[AComponent.ComponentClass.ClassName] then dec(Vote,100);
end;

constructor TEduComponentPaletteOptions.Create;
begin
  inherited Create;
  Name:='ComponentPalette';
  fVisible:=TStringToStringTree.Create(false);
  IDEComponentPalette.AddHandlerUpdateVisible(@VoteForVisible);
end;

destructor TEduComponentPaletteOptions.Destroy;
begin
  if IDEComponentPalette<>nil then
    IDEComponentPalette.RemoveHandlerUpdateVisible(@VoteForVisible);
  FreeAndNil(fVisible);
  inherited Destroy;
end;

function TEduComponentPaletteOptions.Load(Config: TConfigStorage): TModalResult;
var
  Cnt: LongInt;
  i: Integer;
  ComponentName: String;
begin
  fVisible.Clear;
  Cnt:=Config.GetValue('Visible/Count',0);
  for i:=1 to Cnt do begin
    ComponentName:=Config.GetValue('Visible/Item'+IntToStr(i),'');
    if ComponentName='' then continue;
    fVisible[ComponentName]:='1';
  end;
  Result:=inherited Load(Config);
end;

function TEduComponentPaletteOptions.Save(Config: TConfigStorage): TModalResult;
var
  Node: TAvlTreeNode;
  Item: PStringToStringItem;
  Cnt: Integer;
begin
  Cnt:=0;
  Node:=fVisible.Tree.FindLowest;
  while Node<>nil do begin
    inc(Cnt);
    Item:=PStringToStringItem(Node.Data);
    Config.SetDeleteValue('Visible/Item'+IntToStr(Cnt),Item^.Name,'');
    Node:=fVisible.Tree.FindSuccessor(Node);
  end;
  Config.SetDeleteValue('Visible/Count',Cnt,0);
  Result:=inherited Save(Config);
end;

procedure TEduComponentPaletteOptions.Apply(Enable: boolean);
begin
  inherited Apply(Enable);
  if IDEComponentPalette<>nil then
    IDEComponentPalette.Update(False);
end;

{$R *.lfm}

end.
