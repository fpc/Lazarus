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
@author(Olivier guilbaud (OG) <golivier@free.fr>)
@created(24/02/2003)
@lastmod(25/02/2003)

Property editor for TImageList objects

History
 26-Feb-2003 OG - Update for use assign.
 27-feb-2003 OG - If possible zoom x2 the selected image.
                - Fix the superposition of images
 
Todo :
  - Rogne and truncate image capability
}

unit ImageListEditor;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, Menus, PropEdits, ComponentEditors, LCLProc,
  LMessages, ObjInspStrConsts,ImgList;



const
  //See @link  TGraphicPropertyEditorForm.LoadBTNCLICK for explanations
  FormatsSupported: Array[0..1] of String =('.xpm',
                                             '.bmp');

Type
  {TMenuItemsPropertyEditorDlg}
  
  //Editor dialog
  TImageListEditorDlg = Class(TForm)
  private
    fImg: TImage;
    fLv : TListView;
    fImgL: TImageList;
    fBtnAdd  : TButton;
    fBtnDel  : TButton;
    fBtnClear: TButton;
    fDirName : String;
    FModified: boolean;
    
    procedure OnLVLSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure OnClickAdd(Sender: TObject);
    procedure OnClickDel(Sender: TObject);
    procedure OnClickClear(Sender: TObject);
    procedure SetModified(const AValue: boolean);
  public
    constructor Create(aOwner: TComponent); override;

    //Assign an List images at editor and initialise the
    //TListView component
    Procedure AssignImageList(aImgL: TImageList);
    property Modified: boolean read FModified write SetModified;
  end;

  //Editor call by Lazarus with 1 verbe only
  TImageListComponentEditor = class(TDefaultComponentEditor)
  protected
    procedure DoShowEditor;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

Implementation

//If Select item, preview the image
procedure TImageListEditorDlg.OnLVLSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
Var Bmp: TBitMap;
begin
  if Assigned(Item) and Selected then
  begin
    if (Item.ImageIndex>=0) then
    begin
      //Clear old image
      fImg.Picture.BitMap.Canvas.Brush.Color:=clWhite;
      fImg.Picture.BitMap.Canvas.FillRect(Rect(0,0,fImg.Width,fImg.Height));

      //Draw new image
      Bmp:=TBitMap.Create;
      fImgL.GetBitmap(Item.ImageIndex,Bmp);

      fImg.Picture.BitMap:=Bmp;
      Bmp.Free;

      fBtnDel.Enabled:=True;
      fImg.Visible:=True;
      fBtnClear.Enabled:=True;
      fImg.Invalidate;
    end;
  end;
end;

//Select new image file and add in list
procedure TImageListEditorDlg.OnClickAdd(Sender: TObject);
Var OpenDlg: TOpenDialog;
    Ext    : String;
    FileName: String;
    AListItem: TListItem;
    i      : Integer;
    Bmp    : TBitMap;
begin
 Opendlg := TOpenDialog.Create(Self);
 Try
   Opendlg.Options:=[ofextensiondifferent, ofpathmustexist, offilemustexist, ofenablesizing];
   Opendlg.DefaultExt:='.xpm';
   Opendlg.Filter:='*.xpm';
   OpenDlg.InitialDir:=fDirName; //last rirectory
   
   If OpenDlg.Execute then
   begin
     FileName:=OpenDlg.FileName;
     Ext:=ExtractFileExt(FileName);

     //Check if the file is supported
     For i:=Low(FormatsSupported) to High(FormatsSupported) do
     begin
       If AnsiCompareText(Ext,FormatsSupported[I]) = 0 then
       begin
         fImg.Picture.LoadFromFile(FileName);
         fDirName:=ExtractFilePath(FileName); //save the directory
         Break;
       end;
     end;

     //If the image is loaded, then add to list
     If Assigned(fImg.Picture.Graphic) then
     begin
       If not fImg.Picture.Graphic.Empty then
       begin
         Bmp:=TBitMap.Create;
         Bmp.LoadFromFile(FileName);
         Modified:=true;
         i:=fImgL.Add(Bmp,nil);
         AListItem:=fLV.Items.Add;
         AListItem.Caption:=IntToStr(i);
         AListItem.ImageIndex:=i;
         fLV.Selected:=AListItem;
       end;
    end;
   end;
 finally
   OpenDlg.Free;
 end;
end;

//Delete the selected image and refresh screen
procedure TImageListEditorDlg.OnClickDel(Sender: TObject);
Var IL : TListItem;
    i,j: Integer;
begin
  If Assigned(fLV.Selected) then
  begin
    Modified:=true;
    fImgL.Delete(fLV.Selected.ImageIndex);
    IL:=flv.Selected;
    i:=Il.Index;
    fLv.Items.Delete(Il.Index);
    
    //Select an new item
    if (fLv.Items.Count<>0) then
    begin
      for j:=i to fLv.Items.Count-1 do
      begin
        fLv.Items.Item[j].ImageIndex:=fLv.Items.Item[j].ImageIndex-1;
        fLv.Items.Item[j].Caption:=IntToStr(fLv.Items.Item[j].ImageIndex);
      end;
      
      if i>fLv.Items.Count-1 then
        Dec(i);
        
      Il:=fLv.Items.Item[i];
      fLv.Selected:=IL;
    end
    else
    begin
      fBtnDel.Enabled:=False;
      fImg.Visible:=False;
      fBtnClear.Enabled:=False;
    end;
  end;
end;

//Delete all images of list  and items for TListView
procedure TImageListEditorDlg.OnClickClear(Sender: TObject);
begin
  if (fImgL.Count>0)
  and (MessageDlg(sccsILConfirme,mtConfirmation,[mbYes,mbNo],0)=mrYes) then
  begin
    Modified:=true;
    fImgL.Clear;
    while fLV.Items.Count<>0 do
      fLV.Items.Delete(0);
    fBtnDel.Enabled:=False;
    fBtnClear.Enabled:=False;
    fImg.Visible:=False;
  end;
end;

procedure TImageListEditorDlg.SetModified(const AValue: boolean);
begin
  if FModified=AValue then exit;
  FModified:=AValue;
end;

{ TImageListEditorDlg }
constructor TImageListEditorDlg.Create(aOwner: TComponent);
Var Cmp: TWinControl;
begin
  inherited Create(aOwner);
  BorderStyle:=bssingle;
  
  //Temporary list
  fImgL:=TImageList.Create(self);

  //Default directory
  fDirName:=ExtractFilePath(ParamStr(0));
  
  //Sise of window
  Height:=289;
  Width :=579;
  BorderStyle:=bsSingle;
  Position :=poScreenCenter;
  Caption  :=sccsILEdtCaption;
  
  //Bnt Ok
  With TBitBtn.Create(self) do
  begin
    Left  :=448;
    Width :=99;
    Top   :=16;
    Kind  :=bkOk;
    Parent:=self;
  end;

  //Bnt Cancel
  With TBitBtn.Create(self) do
  begin
    Left  :=448;
    Width :=99;
    Top   :=56;
    Kind  :=bkCancel;
    Parent:=self;
  end;

  //Top group box
  Cmp:=TGroupBox.Create(self);
  With TgroupBox(Cmp) do
  begin
    Width  :=416;
    Top    :=6;
    Left   :=8;
    Height :=130;
    Parent :=Self;
    Caption:=sccsILCmbImgSel
  end;

  //TShape for best view
  with TShape.Create(self) do
  begin
    Parent      :=Cmp;
    Left        :=11;
    Width       :=98;
    Top         :=6;
    Height      :=98;
  end;

  //Image for preview a selected image item
  fImg:=TImage.Create(self);
  With fImg do
  begin
    Parent      :=Cmp;
    Transparent :=False;
    Left        :=12;
    Width       :=97;
    Top         :=7;
    Height      :=97;
  end;

  //bottom group box
  Cmp:=TGroupBox.Create(self);
  With TgroupBox(Cmp) do
  begin
    Width  :=562;
    Top    :=144;
    Left   :=8;
    Height :=141;
    Parent :=Self;
    Caption:=sccsILCmbImgList
  end;

  fLV :=TListView.Create(self);
  With fLV do
  begin
    Parent :=Cmp;
    Left   :=3;
    Width  :=411;
    Top    :=1;
    Height :=118;
    SmallImages:=fImgL;
    ScrollBars:=sshorizontal;
    fLV.OnSelectItem:=@OnLVLSelectItem;
  end;

  fBtnAdd:=TButton.Create(self);
  With fBtnAdd do
  begin
    Parent  :=Cmp;
    Top     :=1;
    Width   :=112;
    Left    :=430;
    Height  :=25;
    Caption :=sccsILBtnAdd;
    OnClick :=@OnClickAdd;
  end;

  fBtnDel:=TButton.Create(self);
  With fBtnDel do
  begin
    Parent  :=Cmp;
    Top     :=34;
    Width   :=112;
    Left    :=430;
    Height  :=25;
    Enabled :=False;
    Caption :=sccsLvEdtBtnDel; //Same caption
    OnClick :=@OnClickDel;
  end;

  fBtnClear:=TButton.Create(self);
  With fBtnClear do
  begin
    Parent  :=Cmp;
    Top     :=66;
    Width   :=112;
    Left    :=430;
    Height  :=25;
    Enabled :=False;
    Caption :=sccsILBtnClear;
    OnClick :=@OnClickClear;
  end;
end;

//Assign an List images at editor
procedure TImageListEditorDlg.AssignImageList(aImgL: TImageList);
Var IL: TListItem;
    i : Integer;
begin
  If Assigned(aImgL) then
  begin
    //Clear all existing images
    fImgL.Clear;
    while fLV.Items.Count<>0 do
      fLV.Items.Delete(0);

    fImgL.Assign(aImgL);

    for i:=0 to fImgL.Count-1 do
    begin
      IL:=fLV.Items.Add;
      Il.ImageIndex:=i;
      IL.Caption:=IntToStr(i);
    end;
    
    //If possible zoom the selected image
    if (fImgL.Width<97) and (fImgL.Height<97) then
    begin
      fImg.Width  :=fImgL.Width;
      fImg.Height :=fImgL.Height;
      fImg.Stretch:=false;// scaling is not yet supported for transparent images

      //Center the image
      fImg.Left:=12+(97-fImg.Width);
      fImg.Top := 7+(97-fImg.Height);
    end
    else
    begin
      //Restore the default position
      fImg.Width  :=97;
      fImg.Height :=97;
      fImg.Top    :=7;
      fImg.Left   :=12;
      fImg.Stretch:=false;// scaling is not yet supported for transparent images
    end;
    
    fBtnDel.Enabled:=(fImgL.Count<>0);
    fBtnClear.Enabled:=(fImgL.Count<>0);
    fImg.Visible:=(fImgL.Count<>0);
    //Select the first item
    if (fImgL.Count<>0) then
      fLV.Selected:=fLV.Items.Item[0];

  end;
end;


{ TImageListComponentEditor }

procedure TImageListComponentEditor.DoShowEditor;
Var Dlg: TImageListEditorDlg;
    Hook: TPropertyEditorHook;
    aImg: TImageList;
begin
  Dlg:=TImageListEditorDlg.Create(Application);
  try
    If GetComponent is TImageList then
    begin
      aImg:=TImageList(GetComponent);
      GetHook(Hook);
      Dlg.AssignImageList(aImg);

      //ShowEditor
      if (Dlg.ShowModal=mrOK) and Dlg.Modified then
      begin
      
        //Apply the modifications
        writeln('TImageListComponentEditor.DoShowEditor A ',aImg.Count,' ',aImg.Width,',',aImg.Height);
        aImg.Assign(Dlg.fImgL);
        writeln('TImageListComponentEditor.DoShowEditor B ',aImg.Count,' ',aImg.Width,',',aImg.Height);

        //not work :o( aImg.AddImages(Dlg.fImgL);
        if Assigned(Hook) then
          Hook.Modified(Self);
      end;
    end;
  finally
    Dlg.Free;
  end;
  writeln('TImageListComponentEditor.DoShowEditor END ');
end;

procedure TImageListComponentEditor.ExecuteVerb(Index: Integer);
begin
 DoShowEditor;
end;

function TImageListComponentEditor.GetVerb(Index: Integer): string;
begin
  Result:=sccsILEdtCaption+' ...';
end;

function TImageListComponentEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

initialization
  //Register a component editor for TImageList
  RegisterComponentEditor(TImageList,TImageListComponentEditor);
end.
