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
}

{
 Property editor for TListView objects

 Author: Olivier Guilbaud  (golivier@free.fr)
 
 History
   01/28/2003 OG - Create
   18/02/2003 OG - First release
   19/02/2003 OG - Add ObjInspStrConsts unit
   24/02/2003 OG - Replace TListBox with TTreeView
                   Include suItems property
                   
   ToDo :
     Select the first item on show editor ... dont work :o(
}
unit ListViewPropEdit;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, Menus, PropEdits, ComponentEditors, LCLProc,
  ObjInspStrConsts;

Implementation

Type
  {TMenuItemsPropertyEditorDlg}
  
  TListViewItemsPropertyEditorDlg = Class(TForm)
    procedure LeftGroupBoxResize(Sender: TObject);
    procedure ListViewItemsPropertyEditorDlgResize(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure RightGroupBoxResize(Sender: TObject);
  private
    edtLabel : TEdit;
    edtIndex : TEdit;
    TV       : TTreeView;
    fBuild   : Boolean;
    Panel1: TPanel;
    OkButton: TBitBtn;
    CancelButton: TBitBtn;
    LeftGroupBox: TGroupBox;
    NewButton: TButton;
    SubButton: TButton;
    DeleteButton: TButton;
    RightGroupBox: TGroupBox;
    CaptionLabel: TLabel;
    ImgIndexLabel: TLabel;

    Procedure btnAddOnClick(Sender : TObject);
    Procedure btnDelOnClick(Sender : TObject);
    procedure btnAddSubOnClick(Sender : TObject);
    Procedure LBOnClick(Sender: TObject);
    procedure EdtLabelOnChange(Sender: TObject);
    procedure EdtIndexOnChange(Sender: TObject);
    
    procedure OnDlgShow(Sender: TObject);
    procedure RefreshEdts;

  public
    constructor Create(TheOwner: TComponent); override;
  end;

  TListViewComponentEditor = class(TDefaultComponentEditor)
  protected
    procedure DoShowEditor;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  {TListViewItemsPropertyEditor
   Property editor for the Items properties of TListView object.
   Brings up the dialog for editing items}
  TListViewItemsPropertyEditor = Class(TClassPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

//This function find the Designer of aComponent
function GetDesignerOfComponent(aComponent : TComponent): TComponentEditorDesigner;
var
  OwnerForm: TCustomForm;
begin
  Result:=nil;
  if (aComponent is TCustomForm) and (TCustomForm(aComponent).Parent=nil) then
      OwnerForm:=TCustomForm(aComponent)
  else
  begin
    OwnerForm:=TCustomForm(aComponent.Owner);
    if OwnerForm=nil then
    begin
      raise Exception.Create('TComponentInterface.GetDesigner: '
        +aComponent.Name+' Owner=nil');
    end;

    if not (OwnerForm is TCustomForm) then
    begin
      raise Exception.Create('TComponentInterface.GetDesigner: '
          +aComponent.Name+' OwnerForm='+OwnerForm.ClassName);
    end;
    Result:=TComponentEditorDesigner(OwnerForm.Designer);
  end;
end;

{ TListViewItemsPropertyEditor }

procedure TListViewItemsPropertyEditor.Edit;
Var DI : TComponentEditorDesigner;
    Ds : TBaseComponentEditor;
    LV : TCustomListView;
begin
  LV:=TListItems(GetOrdValue).Owner;
  DI:=GetDesignerOfComponent(LV);
  If Assigned(DI) then
  begin
    Ds:=GetComponentEditor(LV,DI);
    If Assigned(Ds) then
      Ds.ExecuteVerb(0);
  end;
end;

function TListViewItemsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog,paReadOnly,paRevertable];
end;

{ TListViewComponentEditor }

procedure TListViewComponentEditor.DoShowEditor;
Var Dlg     : TListViewItemsPropertyEditorDlg;
    LV      : TListView;
    C       : TPersistent;
    i,j     : Integer;
    Li      : TListItem;
    Hook    : TPropertyEditorHook;
    TN,TN2  : TTreeNode;
begin
  Dlg:=TListViewItemsPropertyEditorDlg.Create(Application);
  try
    C:=GetComponent;
    if C is TListView then LV:=TListView(C);
    if C is TListItems then LV:=TListView(TListItems(C).Owner);
    GetHook(Hook);

    if Assigned(LV) then
    begin
      //Initialize the listbox items with ListView items
      for i:=0 to LV.Items.Count-1 do
      begin
        Dlg.fBuild:=True;
        TN:=Dlg.TV.Items.add(nil,LV.Items.Item[i].Caption);
        TN.ImageIndex:=LV.Items[i].ImageIndex;

        //sub items
        for j:=0 to LV.Items.Item[i].SubItems.Count-1 do
        begin
          TN2:=Dlg.TV.Items.AddChild(TN,LV.Items.Item[i].SubItems.Strings[j]);
          TN2.ImageIndex:=LV.Items.Item[i].SubItemImages[j];
        end;
      end;
      
      //ShowEditor
      if (Dlg.ShowModal=mrOk) then
      begin
        LV.BeginUpdate;
        try
          //Clear items
          LV.Items.Clear;
      
          //Recreate new items or modify
          for i:=0 to Dlg.TV.Items.Count-1 do
          begin
            TN:=Dlg.TV.Items.Items[i];
            If not Assigned(TN.Parent) then
            begin
              Li:=LV.Items.Add;
              Li.Caption:=TN.Text;;
              Li.ImageIndex:=TN.ImageIndex;
              
              //Sub items if exists
              for j:=0 to TN.Count-1 do
              begin
                TN2:=TN.Items[j];
                Li.SubItems.Add(TN2.Text);
                Li.SubItemImages[j]:=TN.ImageIndex;
              end;
            end;
          end;
        finally
          LV.EndUpdate;
          if Assigned(Hook) then
            Hook.Modified(Self);
        end;
      end;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TListViewComponentEditor.ExecuteVerb(Index: Integer);
begin
  If Index=0 then
    DoShowEditor;
end;

function TListViewComponentEditor.GetVerb(Index: Integer): string;
begin
  Result:='';
  If Index=0 then
    Result:=sccsLvEdtCaption;
end;

function TListViewComponentEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

{ TListViewItemsPropertyEditorDlg }
constructor TListViewItemsPropertyEditorDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnShow:=@OnDlgShow;
  
  fBuild:=False;
  
  //Size of window
  Height:=261;
  Width :=640;
  Position := poScreenCenter;
  Caption  := sccsLvEdtCaption;
  
  Panel1:=TPanel.Create(Self);
  With Panel1 do begin
    Name:='Panel1';
    Caption:='';
    Parent:=Self;
    Height:=41;
    Align :=alBottom;
    OnResize:=@Panel1Resize;
  end;

  //Btn Ok
  OkButton:=TBitBtn.Create(Self);
  With OkButton do begin
    Name:='OkButton';
    Parent:=Panel1;
    Left  :=Parent.ClientWidth-200;
    Top   :=8;
    Width :=90;
    Kind  :=bkOk;
  end;

  //Button cancel
  CancelButton:=TBitBtn.Create(Self);
  With CancelButton do begin
    Name:='CancelButton';
    Parent:=Panel1;
    Left  :=Parent.ClientWidth-100;
    Top   :=8;
    Width :=90;
    Kind  :=bkCancel;
  end;

  //Left group box
  LeftGroupBox:=TGroupBox.Create(self);
  With LeftGroupBox do begin
    Name:='LeftGroupBox';
    Parent :=Self;
    Width  :=329;
    Top    :=3;
    Left   :=3;
    Height :=Parent.ClientHeight-40;
    Caption:=sccsLvEdtGrpLCaption;
    OnResize:=@LeftGroupBoxResize;
  end;

  NewButton:=TButton.Create(Self);
  with NewButton do begin
    Name:='NewButton';
    Parent :=LeftGroupBox;
    Left   :=192;
    Width  :=121;
    Top    :=22;
    Caption:=sccsLvEdtBtnAdd;
    OnClick:=@btnAddOnClick;
  end;

  SubButton:=TButton.Create(self);
  With SubButton do begin
    Name:='SubButton';
    Parent :=LeftGroupBox;
    Enabled:=False;
    Left   :=NewButton.Left;
    Width  :=NewButton.Width;
    Top    :=NewButton.Top+NewButton.Height+30;
    Caption:=sccsLvEdtBtnAddSub;
    OnClick:=@btnAddSubOnClick;
  end;

  DeleteButton:=TButton.Create(self);
  With DeleteButton do
  begin
    Name:='DeleteButton';
    Parent :=LeftGroupBox;
    Left   :=NewButton.Left;
    Width  :=NewButton.Width;
    Top    :=SubButton.Top+SubButton.Height+30;
    Caption:=sccsLvEdtBtnDel;
    OnClick:=@btnDelOnClick;
  end;

  TV:=TTreeView.Create(self);
  With TV do
  begin
    Name:='Tv';
    Parent  :=LeftGroupBox;
    Top     :=3;
    Left    :=3;
    Width   :=NewButton.Left-Left-Left;
    Height  :=Parent.ClientHeight-Top-Top;
    
    //Options of TV
    RightClickSelect:=True;
    ReadOnly:=True;
    ShowButtons:=False;
    AutoExpand:=True;
    HideSelection:=False;
    
    OnClick :=@LBOnClick;
  end;

  //Right group box
  RightGroupBox:=TGroupBox.Create(self);
  With RightGroupBox do
  begin
    Name:='RightGroupBox';
    Parent :=Self;
    Width  :=297;
    Top    :=0;
    Left   :=339;
    Height :=217;
    Caption:=sccsLvEdtGrpRCaption;
    OnResize:=@RightGroupBoxResize;
  end;

  CaptionLabel:=TLabel.Create(self);
  With CaptionLabel do
  begin
    Name:='CaptionLabel';
    Parent :=RightGroupBox;
    Left   :=16;
    Top    :=32;
    Caption:=sccsLvEdtlabCaption;
  end;

  ImgIndexLabel:=TLabel.Create(self);
  With ImgIndexLabel do
  begin
    Name:='ImgIndexLabel';
    Parent :=RightGroupBox;
    Left   :=16;
    Top    :=72;
    Width  :=90;
    Caption:=sccsLvEdtImgIndexCaption;
  end;

  EdtLabel:= TEdit.Create(self);
  With EdtLabel do
  begin
    Parent:=RightGroupBox;
    Left  :=134;
    Text  :='';
    Width :=155;
    Top   :=24;
    
    OnChange:=@EdtLabelOnChange;
  end;
  
  EdtIndex:= TEdit.Create(self);
  With EdtIndex do
  begin
    Parent:=RightGroupBox;
    Left  :=134;
    Text  :='';
    Width :=43;
    Top   :=64;
    
    OnChange:=@EdtIndexOnChange;
  end;
  
  OnResize:=@ListViewItemsPropertyEditorDlgResize;
  ListViewItemsPropertyEditorDlgResize(nil);
end;

//Initialze the TEdit with selected node
procedure TListViewItemsPropertyEditorDlg.RefreshEdts;
Var TN : TTreeNode;
begin
  TN:=TV.Selected;
  fbuild:=True;
  try
    if Assigned(TN) then
    begin
      edtLabel.Text:=TN.Text;
      edtIndex.Text:=IntToStr(TN.ImageIndex);
      edtLabel.Enabled:=True;
      edtIndex.Enabled:=True;
      SubButton.Enabled  :=True;
    end
    else
    begin
      EdtLabel.Text:='';
      EdtIndex.Text:='';
      SubButton.Enabled:=False;
      edtLabel.Enabled:=False;
      edtIndex.Enabled:=False;
    end;
  finally
    fbuild:=false;
  end;
end;

procedure TListViewItemsPropertyEditorDlg.LeftGroupBoxResize(Sender: TObject);
begin
  NewButton:=TButton.Create(Self);
  with NewButton do begin
    Left   :=192;
    Width  :=121;
    Top    :=22;
  end;

  With SubButton do begin
    Left   :=NewButton.Left;
    Width  :=NewButton.Width;
    Top    :=NewButton.Top+NewButton.Height+30;
  end;

  With DeleteButton do begin
    Left   :=NewButton.Left;
    Width  :=NewButton.Width;
    Top    :=SubButton.Top+SubButton.Height+30;
  end;

  With TV do
  begin
    Top     :=3;
    Left    :=3;
    Width   :=NewButton.Left-Left-Left;
    Height  :=Parent.ClientHeight-Top-Top;
  end;
end;

procedure TListViewItemsPropertyEditorDlg.ListViewItemsPropertyEditorDlgResize(
  Sender: TObject);
begin
  With Panel1 do begin
    Top:=Parent.ClientHeight-41;
  end;

  With LeftGroupBox do begin
    Width  :=329;
    Top    :=0;
    Left   :=0;
    Height :=Panel1.Top-Top;
  end;

  With RightGroupBox do
  begin
    Width  :=297;
    Top    :=0;
    Left   :=339;
    Height :=Panel1.Top-Top;
  end;
end;

procedure TListViewItemsPropertyEditorDlg.Panel1Resize(Sender: TObject);
begin
  With OkButton do begin
    Left  :=Parent.ClientWidth-200;
    Top   :=8;
    Width :=90;
  end;

  With CancelButton do begin
    Left  :=Parent.ClientWidth-100;
    Top   :=8;
    Width :=90;
  end;
end;

procedure TListViewItemsPropertyEditorDlg.RightGroupBoxResize(Sender: TObject);
begin
  With CaptionLabel do
  begin
    Left   :=16;
    Top    :=32;
  end;

  With ImgIndexLabel do
  begin
    Left   :=16;
    Top    :=72;
    Width  :=90;
  end;

  With EdtLabel do
  begin
    Left  :=134;
    Text  :='';
    Width :=155;
    Top   :=24;
  end;

  With EdtIndex do
  begin
    Left  :=134;
    Text  :='';
    Width :=43;
    Top   :=64;
  end;
end;

//Créate new item
procedure TListViewItemsPropertyEditorDlg.btnAddOnClick(Sender: TObject);
Var TN : TTreeNode;
begin
  fBuild:=True;
  try
    TN:=TV.Items.Add(nil,sccsLvEdtBtnAdd);
    TN.ImageIndex:=-1;
    TV.Selected:=TN;
    
    RefreshEdts;
  finally
    fbuild:=False;
  end;
  
  //Select the label editor
  if EdtLabel.CanFocus then
  begin
    EdtLabel.SetFocus;
    EdtLabel.SelectAll;
  end;
end;

//Delete the selected item
procedure TListViewItemsPropertyEditorDlg.btnDelOnClick(Sender: TObject);
Var TN,TN2 : TTreeNode;
begin
  TN:=TV.Selected;
  If Assigned(TN) then
  begin
    TN2:=TN.GetPrev;
    TN.Delete;
    TV.Selected:=TN2;
    
    RefreshEdts;
  end;
end;

//Add an sub item
procedure TListViewItemsPropertyEditorDlg.btnAddSubOnClick(Sender: TObject);
Var TN,TN2 : TTreeNode;
begin
  TN:=TV.Selected;
  If Assigned(TN) then
  begin
    If Assigned(TN.Parent) then
        TN:=TN.Parent;

    TN2:=TV.Items.AddChild(TN,sccsLvEdtBtnAdd);
    TN2.ImageIndex:=-1;
    TV.Selected:=TN2;

    RefreshEdts;
  end;

  //Select the label editor
  if EdtLabel.CanFocus then
  begin
    EdtLabel.SetFocus;
    EdtLabel.SelectAll;
  end;
end;


//Modify the TEdit for the Label and Image index
procedure TListViewItemsPropertyEditorDlg.LBOnClick(Sender: TObject);
begin
  RefreshEdts;
end;

//Refrsh the label list
procedure TListViewItemsPropertyEditorDlg.EdtLabelOnChange(Sender: TObject);
Var TN : TTreeNode;
begin
  if fBuild then Exit;
  TN:=TV.Selected;
  if Assigned(TN) then
    TN.Text:=edtLabel.Text;
end;

//Refresh the index list
procedure TListViewItemsPropertyEditorDlg.EdtIndexOnChange(Sender: TObject);
Var i,E : Integer;
    TN  : TTreeNode;
begin
  if fBuild then Exit;
  TN:=TV.Selected;
  if Assigned(TN) then
  begin
    Val(edtIndex.Text,i,E);
    if E<>0 then i:=-1;
    TN.ImageIndex:=i;
  end;
end;

//Initialize the dialog
procedure TListViewItemsPropertyEditorDlg.OnDlgShow(Sender: TObject);
Var TN : TTReeNode;
begin
  DebugLn('TListViewItemsPropertyEditorDlg.OnDlgShow ',
    IntToStr(Panel1.Top),' ',BoolToStr(Panel1.Align=alBottom));
  TN:=TV.TopItem;
  If Assigned(TN) then
  begin
    TV.Selected:=TN;
    RefreshEdts;
  end;
end;

initialization
  //Register TListViewItemsPropertyEditor
  RegisterPropertyEditor(ClassTypeInfo(TListItems), TListView,'Items',
    TListViewItemsPropertyEditor);

  //Register a component editor for TListView
  RegisterComponentEditor(TListView,TListViewComponentEditor);
end.
