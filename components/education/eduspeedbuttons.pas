{
 *****************************************************************************
 *  This file is part of the EducationLaz package
 *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,
 *  for details about the license.
 *****************************************************************************

  Author: Michael Kuhardt

  Abstract:
    Frame to setup SpeedButtons
}
unit EduSpeedButtons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz_AVL_Tree,
  // LCL
  Controls, Graphics, LResources, Forms, StdCtrls, ExtCtrls, Dialogs,
  ComCtrls, Buttons,
  // LazUtils
  LazConfigStorage, AvgLvlTree,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, LazIDEIntf, IDEImagesIntf,
  // Education
  EduOptions;

type

  { EduSpeedButtonsOptions }

  TEduSpeedButtonsOptions = class(TEduOptionsNode)
  private
    fVisible: TStringToStringTree;
    function GetButtonVisible(ButtonName: string): boolean;
    procedure SetButtonVisible(ButtonName: string; const AValue: boolean);

  public
    constructor Create; override;
    destructor Destroy; override;
    function Load(Config: TConfigStorage): TModalResult; override;
    function Save(Config: TConfigStorage): TModalResult; override;
    function GetToolBar(tbName: string): TToolBar;
    procedure Apply(Enable: boolean); override;
    property ButtonVisible[ButtonName: string]: boolean read GetButtonVisible write SetButtonVisible;

  end;

  { TEduSpeedButtonsFrame }

  TEduSpeedButtonsFrame = class(TAbstractIDEOptionsEditor)
    ShowSelectionButton: TButton;
    ShowAllButton: TButton;
    HideAllButton: TButton;
    SpeedButtonsGroupBox: TGroupBox;
    Panel: TPanel;
    SpeedButtonsTreeView: TTreeView;
    procedure HideAllButtonClick(Sender: TObject);
    procedure ShowAllButtonClick(Sender: TObject);
    procedure ShowSelectionButtonClick(Sender: TObject);
    procedure SpeedButtonsTreeViewMouseDown(Sender: TObject;
      Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure FrameClick(Sender: TObject);
  private
    HideImgID: LongInt;
    ShowImgID: LongInt;
    procedure FillSpeedButtonsTreeView;
    procedure SaveFillSpeedButtonsTreeView;
    procedure ShowHideAll(aShow: boolean);
    procedure ShowSelected;
    function GetImageForSpeedBtn(btnName: String): TCustomBitmap;
  public
    function GetTitle: String; override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
  end;

var
  EduSpeedButtonsOptions: TEduSpeedButtonsOptions = nil;

procedure Register;

implementation

procedure Register;
begin
  EduSpeedButtonsOptions:=TEduSpeedButtonsOptions.Create;
  EducationOptions.Root.Add(EduSpeedButtonsOptions);
  EduSpeedButtonsOptionsID:=RegisterIDEOptionsEditor(EduOptionID,
                         TEduSpeedButtonsFrame,EduSpeedButtonsOptionsID)^.Index;
end;

{ TEduSpeedButtonsOptions }

constructor TEduSpeedButtonsOptions.Create;

begin
  inherited Create;
  Name:='SpeedButtons';
  fVisible:=TStringToStringTree.Create(false);
end;

destructor TEduSpeedButtonsOptions.Destroy;
begin
  FreeAndNil(fVisible);
  inherited Destroy;
end;

function TEduSpeedButtonsOptions.GetButtonVisible(ButtonName: string): boolean;
begin
  Result:=fVisible[ButtonName]='1';
end;

procedure TEduSpeedButtonsOptions.SetButtonVisible(
  ButtonName: string; const AValue: boolean);
begin
  if AValue then
    fVisible[ButtonName]:='1'
  else
    fVisible.Remove(ButtonName);
end;

function TEduSpeedButtonsOptions.Load(Config: TConfigStorage): TModalResult;
var
  Cnt: LongInt;
  i: Integer;
  ButtonName: String;

begin

  fVisible.Clear;
  Cnt:=Config.GetValue('Visible/Count',0);
  for i:=1 to Cnt do begin
    ButtonName:=Config.GetValue('Visible/Item'+IntToStr(i),'');
    if ButtonName='' then continue;
    fVisible[ButtonName]:='1';
  end;
  Result:=inherited Load(Config);
end;

function TEduSpeedButtonsOptions.Save(Config: TConfigStorage): TModalResult;
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

procedure TEduSpeedButtonsOptions.Apply(Enable: boolean);
var
  i: Integer;
  curButton: TToolButton;
  Bar: TToolBar;
begin
  Bar := GetToolBar('tbStandard');
  if Assigned(Bar) then
    for i:=0 to Bar.ButtonCount-1 do begin
      curButton:=Bar.Buttons[i];
      if Assigned(curButton) and (curButton.Name <> '') then
        curButton.Visible:=(not Enable) or ButtonVisible[curButton.Name];
    end;

  Bar := GetToolBar('tbViewDebug');
  if Assigned(Bar) then
    for i:=0 to Bar.ButtonCount-1 do begin
      curButton:=Bar.Buttons[i];
      if Assigned(curButton) and (curButton.Name <> '') then
        curButton.Visible:=(not Enable) or ButtonVisible[curButton.Name];
    end;
end;

function TEduSpeedButtonsOptions.GetToolBar(tbName: string): TToolBar;
var
  AComponent: TComponent;
begin
  if (tbName='tbStandard') or (tbName='tbViewDebug')then begin
    AComponent:=LazarusIDE.OwningComponent.FindComponent(tbName);
    if AComponent is TToolBar then
      Result:=TToolBar(AComponent)
    else
      Result:=nil;
  end;
end;

{ TEduSpeedButtonsFrame }

procedure TEduSpeedButtonsFrame.SpeedButtonsTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  Hit: THitTests;
begin
  if Button<>mbLeft then exit;
  Node:=SpeedButtonsTreeView.GetNodeAt(X,Y);
  if (Node=nil) then exit;
  if Node.Parent=nil then exit;
  Hit:=SpeedButtonsTreeView.GetHitTestInfoAt(X,Y);
  if [htOnIcon,htOnStateIcon]*Hit<>[] then begin
    if Node.StateIndex=ShowImgID then
      Node.StateIndex:=HideImgID
    else
      Node.StateIndex:=ShowImgID;
  end;
end;

procedure TEduSpeedButtonsFrame.ShowHideAll(aShow: boolean);
var
  Node: TTreeNode;
  ButtonName: String;
begin
  SpeedButtonsTreeView.BeginUpdate;
  Node:=SpeedButtonsTreeView.Items.GetFirstNode;
  while Node<>nil do begin
    if Node.Parent<>nil then begin
      ButtonName:=Node.Text;

      EduSpeedButtonsOptions.ButtonVisible[ButtonName]:=aShow;
      if aShow then
        Node.StateIndex:=ShowImgID
      else
        Node.StateIndex:=HideImgID;
    end

    else begin

    end;

    Node:=Node.GetNext;
  end;
  SpeedButtonsTreeView.EndUpdate;
end;

procedure TEduSpeedButtonsFrame.ShowSelected;
var
  Node: TTreeNode;
  ButtonName: String;
  SelectedButtons: array[0..9] of String;
  i: integer;
begin

  SelectedButtons[0] :=  'NewFormSpeedBtn';
  SelectedButtons[1] :=  'OpenFileSpeedBtn';
  SelectedButtons[2] :=  'PauseSpeedButton';
  SelectedButtons[3] :=  'RunSpeedButton';
  SelectedButtons[4] :=  'SaveAllSpeedBtn';
  SelectedButtons[5] :=  'StepIntoSpeedButton';
  SelectedButtons[6] :=  'StepOverpeedButton';
  SelectedButtons[7] :=  'StopSpeedButton';
  SelectedButtons[8] :=  'ToggleFormSpeedBtn';
  SelectedButtons[9] :=  'EduNewSingleFileProgramBtn';

  SpeedButtonsTreeView.BeginUpdate;
  Node:=SpeedButtonsTreeView.Items.GetFirstNode;
  while Node<>nil do begin
    if Node.Parent<>nil then begin
      ButtonName:=Node.Text;
      for i := 0 to 9 do begin
        if (CompareText (ButtonName , SelectedButtons[i] )=0) then begin
            EduSpeedButtonsOptions.ButtonVisible[ButtonName]:=true;
            Node.StateIndex:=ShowImgID;
        end;
      end;

    end;

    Node:=Node.GetNext;
  end;
  SpeedButtonsTreeView.EndUpdate;
end;

procedure TEduSpeedButtonsFrame.HideAllButtonClick(Sender: TObject);
begin
  ShowHideAll(false);
end;

procedure TEduSpeedButtonsFrame.ShowAllButtonClick(Sender: TObject);
begin
  ShowHideAll(true);
end;

procedure TEduSpeedButtonsFrame.ShowSelectionButtonClick(Sender: TObject);
begin
  ShowHideAll(false);
  ShowSelected;
end;

procedure TEduSpeedButtonsFrame.FrameClick(Sender: TObject);
begin

end;

function TEduSpeedButtonsFrame.GetImageForSpeedBtn(btnName: String) :TCustomBitmap;
begin
   if (CompareText(btnName,'NewUnitSpeedBtn')=0) or (CompareText(btnName,'EduNewSingleFileProgramBtn')=0) then begin
    Result := CreateBitmapFromResourceName(HInstance, 'item_unit');
  end
  else if (CompareText(btnName,'OpenFileSpeedBtn')=0) then begin
    Result := CreateBitmapFromResourceName(HInstance, 'laz_open');
  end
  else if (CompareText(btnName,'SaveSpeedBtn')=0) then begin
    Result := CreateBitmapFromResourceName(HInstance, 'laz_save');
  end
  else if (CompareText(btnName,'SaveAllSpeedBtn')=0) then begin
    Result := CreateBitmapFromResourceName(HInstance, 'menu_save_all');
  end
  else if (CompareText(btnName,'NewFormSpeedBtn')=0) then begin
    Result := CreateBitmapFromResourceName(HInstance, 'item_form');
  end
  else if (CompareText(btnName,'ToggleFormSpeedBtn')=0) then begin
    Result := CreateBitmapFromResourceName(HInstance, 'menu_view_toggle_form_unit');
  end
  else if (CompareText(btnName,'ViewUnitsSpeedBtn')=0) then begin
    Result := CreateBitmapFromResourceName(HInstance, 'menu_view_units');
  end
  else if (CompareText(btnName,'ViewFormsSpeedBtn')=0) then begin
    Result := CreateBitmapFromResourceName(HInstance, 'menu_view_forms');
  end
  else if (CompareText(btnName,'RunSpeedButton')=0) then begin
    Result := CreateBitmapFromResourceName(HInstance, 'menu_run');
  end
  else if (CompareText(btnName,'PauseSpeedButton')=0) then begin
    Result := CreateBitmapFromResourceName(HInstance, 'menu_pause');
  end
  else if (CompareText(btnName,'StopSpeedButton')=0) then begin
    Result := CreateBitmapFromResourceName(HInstance, 'menu_stop');
  end
  else if (CompareText(btnName,'StepIntoSpeedButton')=0) then begin
    Result := CreateBitmapFromResourceName(HInstance, 'menu_stepinto');
  end
  else if (CompareText(btnName,'StepOverpeedButton')=0) then begin
    Result := CreateBitmapFromResourceName(HInstance, 'menu_stepover');
  end
  else
    result := nil;
end;

procedure TEduSpeedButtonsFrame.FillSpeedButtonsTreeView;
var
  i: Integer;
  curNode: TTreeNode;
  CategoryNode: TTreeNode;
  curButton: TToolButton;
  Image: TCustomBitmap;
  StandardBar, ViewDebugBar: TToolBar;
begin
  StandardBar:=EduSpeedButtonsOptions.GetToolBar('tbStandard');
  ViewDebugBar:=EduSpeedButtonsOptions.GetToolBar('tbViewDebug');
  if (StandardBar=Nil) or (ViewDebugBar=Nil) then Exit;

  SpeedButtonsTreeView.BeginUpdate;
  SpeedButtonsTreeView.Items.Clear;

  if SpeedButtonsTreeView.Images=nil then begin
    SpeedButtonsTreeView.Images:=TImageList.Create(Self);
    SpeedButtonsTreeView.Images.Width:=StandardBar.ButtonWidth;
    SpeedButtonsTreeView.Images.Height:=StandardBar.ButtonHeight;
    SpeedButtonsTreeView.StateImages:=IDEImages.Images_16;
  end else

    SpeedButtonsTreeView.Images.Clear;
    ShowImgID:=IDEImages.LoadImage('menu_run');
    HideImgID:=IDEImages.LoadImage('menu_stop');

    CategoryNode:=SpeedButtonsTreeView.Items.Add(nil,'Standard Buttons');
    for i:=0 to StandardBar.ButtonCount-1 do begin

      curButton:=StandardBar.Buttons[i];
      if NOT(curButton.Name = '') then begin
        curNode:=SpeedButtonsTreeView.Items.AddChild(CategoryNode,curButton.Name);
        Image := GetImageForSpeedBtn(curButton.Name);
        if (Image = nil) then
           Image := CreateBitmapFromResourceName(HInstance, 'default');
        curNode.ImageIndex:=SpeedButtonsTreeView.Images.Add(Image,nil);
        Image.Free;
        curNode.SelectedIndex:=curNode.ImageIndex;

        if (EduSpeedButtonsOptions.ButtonVisible[curButton.Name]) then
          curNode.StateIndex:=ShowImgID
        else
          curNode.StateIndex:=HideImgID;
      end;
    end;
    CategoryNode.Expanded:=true;

    CategoryNode:=SpeedButtonsTreeView.Items.Add(nil,'Debug Buttons');
    for i:=0 to ViewDebugBar.ButtonCount-1 do begin

      curButton:=ViewDebugBar.Buttons[i];
      if NOT(curButton.Name = '') then begin
        curNode:=SpeedButtonsTreeView.Items.AddChild(CategoryNode,curButton.Name);
        Image := GetImageForSpeedBtn(curButton.Name);
        if (Image = nil) then
           Image := CreateBitmapFromResourceName(HInstance, 'default');
        curNode.ImageIndex:=SpeedButtonsTreeView.Images.Add(Image,nil);
        Image.Free;
        curNode.SelectedIndex:=curNode.ImageIndex;

        if (EduSpeedButtonsOptions.ButtonVisible[curButton.Name]) then
          curNode.StateIndex:=ShowImgID
        else
          curNode.StateIndex:=HideImgID;
      end;
    end;
    CategoryNode.Expanded:=true;
    SpeedButtonsTreeView.EndUpdate;

end;

procedure TEduSpeedButtonsFrame.SaveFillSpeedButtonsTreeView;
var
  Node: TTreeNode;
  ButtonName: String;
begin
  Node:=SpeedButtonsTreeView.Items.GetFirstNode;
  while Node<>nil do begin
    if Node.Parent<>nil then begin
      ButtonName:=Node.Text;
      EduSpeedButtonsOptions.ButtonVisible[ButtonName]:=
        Node.StateIndex=ShowImgID;
    end else begin

    end;
    Node:=Node.GetNext;
  end;
end;

function TEduSpeedButtonsFrame.GetTitle: String;
begin
  Result:=ersEduSBTitle;
end;

procedure TEduSpeedButtonsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions=EducationOptions then begin
    FillSpeedButtonsTreeView;
  end;
end;

procedure TEduSpeedButtonsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  ShowAllButton.Caption:=ersShowAll;
  HideAllButton.Caption:=ersHideAll;
  ShowSelectionButton.Caption:=ersShowSelection;
  SpeedButtonsGroupBox.Caption:=ersVisibleSpeedButtons;
end;

class function TEduSpeedButtonsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=EducationIDEOptionsClass;
end;

procedure TEduSpeedButtonsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions=EducationOptions then begin
    SaveFillSpeedButtonsTreeView;
  end;
end;

{$R *.lfm}

end.
