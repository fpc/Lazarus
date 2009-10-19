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
    Frame to setup the component palette.
}
unit EduCompPalette;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, AvgLvlTree,
  FormEditingIntf, LazConfigStorage, IDEOptionsIntf, ComponentReg,
  IDEImagesIntf, LazIDEIntf,
  EduOptions;

type
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

  { TEduCompPaletteFrame }

  TEduCompPaletteFrame = class(TAbstractIDEOptionsEditor)
    ComponentsGroupBox: TGroupBox;
    ComponentsTreeView: TTreeView;
    procedure ComponentsTreeViewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    HideImgID: LongInt;
    ShowImgID: LongInt;
    procedure FillComponentTreeView;
    procedure SaveFillComponentTreeView;
  public
    function GetTitle: String; override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
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
  RegisterIDEOptionsEditor(EduOptionID,TEduCompPaletteFrame,EduOptionCompPaletteID);
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
  if htOnStateIcon in Hit then begin
    if Node.StateIndex=ShowImgID then
      Node.StateIndex:=HideImgID
    else
      Node.StateIndex:=ShowImgID;
  end;
end;

procedure TEduCompPaletteFrame.FillComponentTreeView;
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
      Image.Free;
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

function TEduCompPaletteFrame.GetTitle: String;
begin
  Result:='Component palette';
end;

procedure TEduCompPaletteFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin

end;

procedure TEduCompPaletteFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  ComponentsGroupBox.Caption:='Visible components';
  FillComponentTreeView;
end;

class function TEduCompPaletteFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=nil;
end;

procedure TEduCompPaletteFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  SaveFillComponentTreeView;
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
  {$I educomppalette.lrs}

end.

