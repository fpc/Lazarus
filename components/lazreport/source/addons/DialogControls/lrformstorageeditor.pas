{ LazReport dialogs control

  Copyright (C) 2012-2013 alexs alexs75.at.yandex.ru

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit lrFormStorageEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, ExtCtrls, LR_Class;

type

  { TlrFormStorageEditorForm }

  TlrFormStorageEditorForm = class(TfrObjEditorForm)
    ButtonPanel1: TButtonPanel;
    ListBoxSaved: TListBox;
    ListBoxObjects: TListBox;
    ListBoxProps: TListBox;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure ListBoxObjectsClick(Sender: TObject);
    procedure ListBoxPropsDblClick(Sender: TObject);
    procedure ListBoxSavedDblClick(Sender: TObject);
  private
    //
  public
    procedure ShowEditor({%H-}t: TfrView); override;
  end;

implementation
uses LRDialogControls, lrFormStorage, PropEdits, typinfo, FPCAdds;

{$R *.lfm}

{ TlrFormStorageEditorForm }

procedure TlrFormStorageEditorForm.FormCreate(Sender: TObject);
begin
  //
end;

procedure TlrFormStorageEditorForm.ListBoxObjectsClick(Sender: TObject);
var
  t: TfrView;

  TypeInfo: PTypeInfo;
  TypeData: PTypeData;
  PropInfo: PPropInfo;
  PropData: ^TPropData;
  CurCount: integer;
begin
  ListBoxProps.Items.Clear;

  if (ListBoxObjects.ItemIndex<0) or (ListBoxObjects.Items.Count = 0) then exit;

  T:=ListBoxObjects.Items.Objects[ListBoxObjects.ItemIndex] as TfrView;
  if not Assigned(T) then exit;


  // read all properties and remove doubles
  TypeInfo:=T.ClassInfo;
  repeat
    // read all property infos of current class
    TypeData:=GetTypeData(TypeInfo);
    // skip unitname
    PropData:=AlignToPtr(PByte(@TypeData^.UnitName)+Length(TypeData^.UnitName)+1);
    // read property count
    CurCount:=PWord(PropData)^;
    PropInfo:=PPropInfo(@PropData^.PropList);

    // read properties
    while CurCount>0 do
    begin
      ListBoxProps.Items.Add(PropInfo^.Name);
      PropInfo:=PPropInfo(AlignToPtr(pointer(@PropInfo^.Name)+PByte(@PropInfo^.Name)^+1));
      dec(CurCount);
    end;
    TypeInfo:=TypeData^.ParentInfo;
    if TypeInfo=nil then
      break;
  until false;
end;

procedure TlrFormStorageEditorForm.ListBoxPropsDblClick(Sender: TObject);
var
  S:string;
begin
  S:=ListBoxObjects.Items[ListBoxObjects.ItemIndex]+'.'+ListBoxProps.Items[ListBoxProps.ItemIndex];
  if ListBoxSaved.Items.IndexOf(S)<0 then
    ListBoxSaved.Items.Add(S);
end;

procedure TlrFormStorageEditorForm.ListBoxSavedDblClick(Sender: TObject);
begin
  if (ListBoxSaved.ItemIndex>-1) and (ListBoxSaved.ItemIndex < ListBoxSaved.Items.Count) then
    ListBoxSaved.Items.Delete(ListBoxSaved.ItemIndex);
end;

type
  THackObj = class(TfrObject);

procedure TlrFormStorageEditorForm.ShowEditor(t: TfrView);
var
  P:TfrObject;
  i:integer;
  FPage:TfrPage;
begin
  FPage:=THackObj(t).OwnerPage;

  if (FPage is TfrPageDialog) then
  begin
    for i:=0 to FPage.Objects.Count-1 do
    begin
      P:=TfrObject(FPage.Objects[i]);
      if P is TlrVisualControl then
      begin
        ListBoxObjects.Items.Add(P.Name);
        ListBoxObjects.Items.Objects[ListBoxObjects.Items.Count-1]:=P;
      end;
    end;
  end;

  ListBoxSaved.Items.Assign(TLRFormStorage(T).StoredProperties);
  if ShowModal = mrOk then
  begin
    TLRFormStorage(T).StoredProperties.Assign(ListBoxSaved.Items);
    frDesigner.Modified := True;
  end;
end;

end.

