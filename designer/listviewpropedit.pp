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

 Author: Olivier guilbaud  (golivier@free.fr)
 
 History
   01/28/2003 OG - Create
   18/02/2003 OG - First release
}
unit ListViewPropEdit;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources, ComCtrls,
  StdCtrls, Buttons, ExtCtrls,Menus,PropEdits,ComponentEditors,LCLProc,LMessages;

Implementation
Const
  sccsLvEdtCaption        = 'ListView editor';
  sccsLvEdtGrpLCaption    = ' Items ';
  sccsLvEdtGrpRCaption    = ' Item property ';
  sccsLvEdtlabCaption     = 'Label';
  sccsLvEdtImgIndexCaption= 'Image index';
  sccsLvEdtBtnAdd         = 'New';
  sccsLvEdtBtnDel         = 'Delete';
  
Type
 {TMenuItemsPropertyEditorDlg}
  TListViewItemsPropertyEditorDlg = Class(TForm)
  private
    edtLabel : TEdit;
    edtIndex : TEdit;
    LB       : TListBox;
    LstIndex : TStringList;
    
    fBuild   : Boolean;
    
    Procedure btnAddOnClick(Sender : TObject);
    Procedure btnDelOnClick(Sender : TObject);
    Procedure LBOnClick(Sender: TObject);
    procedure EdtLabelOnChange(Sender: TObject);
    procedure EdtIndexOnChange(Sender: TObject);
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
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
Var Dlg : TListViewItemsPropertyEditorDlg;
    LV  : TListView;
    C   : TPersistent;
    i   : Integer;
    Li  : TListItem;
    Hook: TPropertyEditorHook;
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
        Dlg.LB.Items.Add(LV.Items.Item[i].Caption);
        Dlg.LstIndex.Add(IntToStr(LV.Items[i].ImageIndex));
      end;
      if LV.Items.Count>0 then
      begin
        Dlg.LB.ItemIndex:=0;
        Dlg.LB.OnClick(nil);
      end;

      //ShowEditor
      if (Dlg.ShowModal=mrOk) then
      begin
        LV.BeginUpdate;
        try
          //Clear items
          LV.Items.Clear;
      
          //Recreate new items or modify
          for i:=0 to Dlg.LB.Items.Count-1 do
          begin
            Li:=LV.Items.Add;
            Li.Caption:=Dlg.LB.Items.Strings[i];
            Li.ImageIndex:=StrToInt(Dlg.LstIndex.Strings[i]);
          end;
        finally
          LV.EndUpdate;
          if Assigned(Hook) then
             Hook.Modified;
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
constructor TListViewItemsPropertyEditorDlg.Create(aOwner: TComponent);
Var Cmp : TWinControl;
begin
  inherited Create(aOwner);
  
  LstIndex:=TStringList.Create;
  fBuild:=False;
  
  //Sise of window
  Height:=261;
  Width :=640;
  BorderStyle:=bsSingle;
  Position :=poScreenCenter;
  Caption  :=sccsLvEdtCaption;
  
  Cmp:=TPanel.Create(self);
  With TPanel(Cmp) do
  begin
    Parent:=Self;
    Height:=41;
    Align :=alBottom;
  end;

  //Bnt cancel
  With TBitBtn.Create(self) do
  begin
    Left  :=533;
    Width :=91;
    Top   :=8;
    Kind  :=bkCancel;
    Parent:=Cmp;
  end;

  //Bnt Ok
  With TBitBtn.Create(self) do
  begin
    Left  :=437;
    Width :=91;
    Top   :=8;
    Kind  :=bkOk;
    Parent:=Cmp;
  end;

  //Left group box
  Cmp:=TGroupBox.Create(self);
  With TgroupBox(Cmp) do
  begin
    Width  :=329;
    Top    :=0;
    Left   :=3;
    Height :=217;
    Parent :=Self;
    Caption:=sccsLvEdtGrpLCaption
  end;
  
  With TButton.Create(self) do
  begin
    Parent :=Cmp;
    Left   :=192;
    Width  :=121;
    Top    :=32;
    Caption:=sccsLvEdtBtnAdd;
    OnClick:=@btnAddOnClick;
  end;

  With TButton.Create(self) do
  begin
    Parent :=Cmp;
    Left   :=192;
    Width  :=121;
    Top    :=72;
    Caption:=sccsLvEdtBtnDel;
    OnClick:=@btnDelOnClick;
  end;

  LB:=TListBox.Create(self);
  With LB do
  begin
    Parent  :=Cmp;
    Top     :=3;
    Width   :=164;
    Left    :=5;
    Height  :=190;
    ExtendedSelect:=True;
    OnClick :=@LBOnClick;
  end;
  
  //Right group box
  Cmp:=TGroupBox.Create(self);
  With TgroupBox(Cmp) do
  begin
    Width  :=297;
    Top    :=0;
    Left   :=339;
    Height :=217;
    Parent :=Self;
    Caption:=sccsLvEdtGrpRCaption
  end;

  With TLabel.Create(self) do
  begin
    Parent :=cmp;
    Left   :=16;
    Top    :=32;
    Caption:=sccsLvEdtlabCaption;
  end;

  With TLabel.Create(self) do
  begin
    Parent :=cmp;
    Left   :=16;
    Top    :=72;
    Caption:=sccsLvEdtImgIndexCaption;
  end;

  EdtLabel:= TEdit.Create(self);
  With EdtLabel do
  begin
    Parent:=Cmp;
    Left  :=104;
    Text  :='';
    Width :=185;
    Top   :=24;
    
    OnChange:=@EdtLabelOnChange;
  end;
  
  EdtIndex:= TEdit.Create(self);
  With EdtIndex do
  begin
    Parent:=Cmp;
    Left  :=104;
    Text  :='';
    Width :=73;
    Top   :=64;
    
    OnChange:=@EdtIndexOnChange;
  end;
end;

destructor TListViewItemsPropertyEditorDlg.Destroy;
begin
  LstIndex.Free;
  inherited Destroy;
end;

//Créate new item
procedure TListViewItemsPropertyEditorDlg.btnAddOnClick(Sender: TObject);
begin
  fBuild:=True;
  try
    LB.Items.Add(sccsLvEdtBtnAdd);
    LstIndex.Add('-1');
    LB.ItemIndex:=LB.Items.Count-1;
    
    edtLabel.Text:=LB.Items.Strings[LB.ItemIndex];
    edtIndex.Text:=LstIndex.Strings[LB.ItemIndex];
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
Var i : Integer;
begin
  If LB.ItemIndex<>-1 then
  begin
    i:=LB.ItemIndex;
    LB.Items.Delete(i);
    LstIndex.Delete(i);
    if LB.Items.Count=0 then
      i:=-1
    else
    begin
      If i>LB.Items.Count-1 then
        i:=LB.Items.Count-1;
    end;
    
    try
      if i=-1 then
      begin
        EdtLabel.Text:='';
        EdtIndex.Text:='';
      end;
      
      LB.ItemIndex:=i;
    except
    end;
    LBOnClick(nil);
  end;
end;


//Modify the TEdit for the Label and Image index
procedure TListViewItemsPropertyEditorDlg.LBOnClick(Sender: TObject);
begin
  If LB.ItemIndex<>-1 then
  begin
    fBuild:=True;
    try
      edtLabel.Text:=LB.Items.Strings[LB.ItemIndex];
      edtIndex.Text:=LstIndex.Strings[LB.ItemIndex];
    finally
      fBuild:=False;
    end;
  end;
end;

//Refrsh the label list
procedure TListViewItemsPropertyEditorDlg.EdtLabelOnChange(Sender: TObject);
Var i : Integer;
begin
  If (LB.ItemIndex<>-1) and not fBuild then
  begin
    i:=LB.ItemIndex;
    LB.Items.Strings[LB.ItemIndex]:=edtLabel.Text;
    LB.ItemIndex:=i;
  end;
end;

//Refresh the index list
procedure TListViewItemsPropertyEditorDlg.EdtIndexOnChange(Sender: TObject);
Var i,E : Integer;
begin
  If (LB.ItemIndex<>-1) and not fBuild then
  begin
    Val(edtIndex.Text,i,E);
    if E<>0 then i:=-1;
    LstIndex.Strings[LB.ItemIndex]:=IntToStr(i);
  end;
end;

initialization
  //Initialization of properties Items of TMainMenu and TPopupMenu
  RegisterPropertyEditor(ClassTypeInfo(TListItems), TListView,'Items',
    TListViewItemsPropertyEditor);

  //Register a component editor for with mouse right clic, the popup
  RegisterComponentEditor(TListView,TListViewComponentEditor);
end.
