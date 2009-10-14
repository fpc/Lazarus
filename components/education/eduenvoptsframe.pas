{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the EducationLaz package                            *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Dialog to setup the education package.
}
unit EduEnvOptsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, AvgLvlTree,
  FormEditingIntf, LazConfigStorage, IDEOptionsIntf, ComponentReg,
  IDEImagesIntf,
  EduOptions;

const
  EnvOptionsEducation = 2000;

type

  { TEduGeneralOptions }

  TEduGeneralOptions = class(TEduOptionsNode)
  private
    FEnabled: boolean;
    procedure SetEnabled(const AValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    function Load(Config: TConfigStorage): TModalResult; override;
    function Save(Config: TConfigStorage): TModalResult; override;
    property Enabled: boolean read FEnabled write SetEnabled default true;
  end;

  { TEduComponentPaletteOptions }

  TEduComponentPaletteOptions = class(TEduOptionsNode)
  private
    fVisible: TStringToStringTree;
    function GetComponentVisible(ComponentName: string): boolean;
    procedure SetComponentVisible(ComponentName: string; const AValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    function Load(Config: TConfigStorage): TModalResult; override;
    function Save(Config: TConfigStorage): TModalResult; override;
    procedure Apply(Enable: boolean); override;
    property ComponentVisible[ComponentName: string]: boolean read GetComponentVisible write SetComponentVisible;
  end;

  { TEduEnvFrame }

  TEduEnvFrame = class(TAbstractIDEOptionsEditor)
    EnableCheckBox: TCheckBox;
    ComponentsGroupBox: TGroupBox;
    ComponentsTreeView: TTreeView;
    procedure ComponentsTreeViewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FrameClick(Sender: TObject);
  private
    HideImgID: LongInt;
    ShowImgID: LongInt;
    procedure FillComponentTreeView;
    procedure SaveFillComponentTreeView;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

var
  EduEnvFrame: TEduEnvFrame;
  EduGeneralOptions: TEduGeneralOptions = nil;
  EduComponentPaletteOptions: TEduComponentPaletteOptions = nil;

procedure Register;

implementation

procedure Register;
begin
  RegisterIDEOptionsGroup(EduOptionID,TEduOptions);
  RegisterIDEOptionsEditor(EduOptionID,TEduEnvFrame,EduOptionGeneralID);
  EduGeneralOptions:=TEduGeneralOptions.Create;
  EducationOptions.Root.Add(EduGeneralOptions);
  EduComponentPaletteOptions:=TEduComponentPaletteOptions.Create;
  EducationOptions.Root.Add(EduComponentPaletteOptions);

  // load options
  EducationOptions.Load;
end;

{ TEduEnvFrame }

procedure TEduEnvFrame.FrameClick(Sender: TObject);
begin

end;

procedure TEduEnvFrame.ComponentsTreeViewMouseDown(Sender: TObject;
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
  if htOnStateIcon in Hit then begin
    if Node.StateIndex=ShowImgID then
      Node.StateIndex:=HideImgID
    else
      Node.StateIndex:=ShowImgID;
  end;
end;

procedure TEduEnvFrame.FillComponentTreeView;
var
  i: Integer;
  Page: TBaseComponentPage;
  j: Integer;
  Comp: TRegisteredComponent;
  PageNode: TTreeNode;
  CompNode: TTreeNode;
  ResHandle: TLResource;
  Image: TCustomBitmap;
  CompName: String;
begin
  if ComponentsTreeView.Images=nil then begin
    ComponentsTreeView.Images:=TImageList.Create(Self);
    ComponentsTreeView.Images.Width:=ComponentPaletteImageWidth;
    ComponentsTreeView.Images.Height:=ComponentPaletteImageHeight;
    ComponentsTreeView.StateImages:=IDEImages.Images_16;
  end else
    ComponentsTreeView.Images.Clear;
  ShowImgID:=IDEImages.LoadImage(16,'menu_run');
  HideImgID:=IDEImages.LoadImage(16,'menu_stop');
  ComponentsTreeView.BeginUpdate;
  ComponentsTreeView.Items.Clear;
  for i:=0 to IDEComponentPalette.Count-1 do begin
    Page:=IDEComponentPalette[i];
    if Page.PageName='' then continue;
    PageNode:=ComponentsTreeView.Items.Add(nil,Page.PageName);
    for j:=0 to Page.Count-1 do begin
      Comp:=Page[j];
      CompName:=Comp.ComponentClass.ClassName;
      CompNode:=ComponentsTreeView.Items.AddChild(PageNode,CompName);
      ResHandle := LazarusResources.Find(CompName);
      if ResHandle <> nil then
        Image := CreateBitmapFromLazarusResource(ResHandle)
      else
        Image := nil;
      if Image = nil then
        Image := CreateBitmapFromLazarusResource('default');
      CompNode.ImageIndex:=ComponentsTreeView.Images.Add(Image,nil);
      CompNode.SelectedIndex:=CompNode.ImageIndex;
      if EduComponentPaletteOptions.ComponentVisible[CompName] then
        CompNode.StateIndex:=ShowImgID
      else
        CompNode.StateIndex:=HideImgID;
    end;
    PageNode.Expanded:=true;
  end;
  ComponentsTreeView.EndUpdate;
end;

procedure TEduEnvFrame.SaveFillComponentTreeView;
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

constructor TEduEnvFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  ComponentsGroupBox.Caption:='Visible components';
  FillComponentTreeView;
end;

destructor TEduEnvFrame.Destroy;
begin
  inherited Destroy;
end;

function TEduEnvFrame.GetTitle: String;
begin
  Result:='General';
end;

procedure TEduEnvFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  EnableCheckBox.Caption:='Enable education settings';
end;

procedure TEduEnvFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  EnableCheckBox.Checked:=EduGeneralOptions.Enabled;
end;

procedure TEduEnvFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  EduGeneralOptions.Enabled:=EnableCheckBox.Checked;
  SaveFillComponentTreeView;

  if EducationOptions.Save<>mrOk then
    DebugLn(['TEduEnvFrame.WriteSettings Failed']);
  EducationOptions.Apply(EduGeneralOptions.Enabled);
end;

class function TEduEnvFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := nil;
end;

{ TEduGeneralOptions }

procedure TEduGeneralOptions.SetEnabled(const AValue: boolean);
begin
  if FEnabled=AValue then exit;
  FEnabled:=AValue;
  Changed;
end;

constructor TEduGeneralOptions.Create;
begin
  inherited Create;
  Name:='General';
  FEnabled:=true;
end;

destructor TEduGeneralOptions.Destroy;
begin
  if EduGeneralOptions=Self then EduGeneralOptions:=nil;
  inherited Destroy;
end;

function TEduGeneralOptions.Load(Config: TConfigStorage): TModalResult;
begin
  FEnabled:=Config.GetValue('Enabled',True);
  Result:=inherited Load(Config);
end;

function TEduGeneralOptions.Save(Config: TConfigStorage): TModalResult;
begin
  Config.SetDeleteValue('Enabled',Enabled,true);
  Result:=inherited Save(Config);
end;

{ TEduComponentPaletteOptions }

function TEduComponentPaletteOptions.GetComponentVisible(ComponentName: string
  ): boolean;
begin
  Result:=fVisible[ComponentName]='1';
end;

procedure TEduComponentPaletteOptions.SetComponentVisible(
  ComponentName: string; const AValue: boolean);
begin
  if AValue then
    fVisible[ComponentName]:='1'
  else
    fVisible.Delete(ComponentName);
end;

constructor TEduComponentPaletteOptions.Create;
begin
  inherited Create;
  Name:='ComponentPalette';
  fVisible:=TStringToStringTree.Create(false);
end;

destructor TEduComponentPaletteOptions.Destroy;
begin
  FreeAndNil(fVisible);
  inherited Destroy;
end;

function TEduComponentPaletteOptions.Load(Config: TConfigStorage
  ): TModalResult;
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

function TEduComponentPaletteOptions.Save(Config: TConfigStorage
  ): TModalResult;
var
  Node: TAvgLvlTreeNode;
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
var
  i: Integer;
  Page: TBaseComponentPage;
  j: Integer;
  Comp: TRegisteredComponent;
begin
  inherited Apply(Enable);
  for i:=0 to IDEComponentPalette.Count-1 do begin
    Page:=IDEComponentPalette[i];
    for j:=0 to Page.Count-1 do begin
      Comp:=Page[j];
      Comp.Visible:=(not Enable) or ComponentVisible[Comp.ComponentClass.ClassName];
    end;
  end;
end;

initialization
  {$I eduenvoptsframe.lrs}

end.

