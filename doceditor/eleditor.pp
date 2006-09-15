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

  Author: Michael Van Canneyt
}
unit ElEditor;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, DOM, xmlread, xmlwrite, Forms, Controls, ExtCtrls,
     ComCtrls, StdCtrls, Dialogs, Menus, fpdeutil, Lazdemsg, Lazdeopts,
     GraphType, ActnList, LResources;

Type

  { TCustomElementEditor }
  TGetElementListEvent = Procedure (List : TStrings) of Object;
  TGetInitialDirEvent = function: string of object;

  { TCustomElementEditor }

  TCustomElementEditor = Class(TPanel)
  private
    Felement : TDomElement;
    FGetElementList: TGetElementListEvent;
    FSavedNode,
    FModified : Boolean;
    FTargetFileName: string;
    FGetInitialDir: TGetInitialDirEvent;
    FChangeEvent: TNotifyEvent;
    FOnChangeCount: Integer;
    procedure SetModified(const AValue: Boolean);
  Protected
    Function GetCurrentSelection : String; virtual; abstract;
    Procedure SetElement (Value : TDomElement); virtual;
    Function GetInitialDir: String;
    procedure LockOnChange;
    procedure UnLockOnChange;
  Public
    Procedure Refresh; virtual; abstract;
    Function  TestSave(S : String) : Boolean; virtual; abstract;
    Function  CurrentXML : String; virtual; abstract;
    Function  Save : Boolean; virtual; abstract;
    Function  CanInsertTag(TagTYpe : TTagType) : Boolean; virtual; abstract;
    Procedure DeleteElement; virtual; abstract;
    Procedure InsertTag (tagName : String); virtual; abstract;
    Procedure InsertTag (TagType : TTagType); virtual; abstract;
    Procedure InsertLink(LinkTarget,LinkText : String); virtual; abstract;
    Procedure InsertTable(Cols,Rows : Integer; UseHeader : Boolean); virtual; abstract;
    procedure InsertPrintShortLink(pLinkTarget: string); virtual; abstract;
    Property  Element : TDomElement Read FElement Write SetElement;
    Property  CurrentSelection : String Read GetCurrentSelection;
    Property  Modified : Boolean Read FModified Write SetModified;
    Property  SavedNode : Boolean Read FSavedNode Write FSavedNode;
    Property  TargetFileName: String read FTargetFileName write FTargetFileName;
    Property  OnGetElementList : TGetElementListEvent Read FGetElementList Write FGetElementList;
    Property  OnGetInitialDir: TGetInitialDirEvent read FGetInitialdir write FGetInitialDir;
    Property  OnChange: TNotifyEvent read FChangeEvent write FChangeEvent;
  end;
  
  { TElementEditor }

  TElementEditor = Class(TCustomElementEditor)
  Private
    FExampleNodes : TList;
    FShortNode,
    FDescrNode,
    FErrorsNode,
    FSeeAlsoNode : TDomElement;
    FLabel : TLabel;
    FShortEntry : TEdit;
    FDescrMemo,
    FErrorsMemo : TMemo;
    FSplit1,
    FSplit2,
    FSplit3 : TSplitter;
    TBExamples : TToolbar;
    BAddExample,
    BEditExample,
    BDeleteExample : TToolButton;
    TBSeeAlso : TToolbar;
    BAddSeeAlso,
    BEditSeeAlso,
    BDeleteSeeAlso : TToolButton;
    ILElements : TImageList;
    FSeeAlso,
    FExamples : TListBox;
    FCurrentEditable : TWinControl;
    Procedure GetNodes;
    Function  CurrentEditable : TWinControl;
    procedure OnEnterControl(Sender : TObject);
    Procedure OnTextModified(Sender : TObject);
    Procedure DoAddExample(Sender : TObject);
    Procedure DoEditExample(Sender : TObject);
    Procedure DoDeleteExample(Sender : TObject);
    Procedure DoAddSeeAlso(Sender : TObject);
    Procedure DoEditSeeAlso(Sender : TObject);
    Procedure DoDeleteSeeAlso(Sender : TObject);
    Function EditLink(Var Value : String) : Boolean;
  Public
    Constructor Create (AOwner : TComponent); override;
    Destructor destroy; override;
    Procedure Refresh;override;
    Function GetCurrentSelection : String; override;
    Procedure SetElement (Value : TDomElement);override;
    Function  TestSave(S : String) : Boolean;override;
    Function  CurrentXML : String; override;
    Function  Save : Boolean; override;
    Function  CanInsertTag(TagTYpe : TTagType) : Boolean; override;
    Procedure DeleteElement; override;
    Procedure InsertTag (tagName : String); override;
    Procedure InsertTag (TagType : TTagType); override;
    Procedure InsertLink(LinkTarget,LinkText : String); override;
    Procedure InsertTable(Cols,Rows : Integer; UseHeader : Boolean); override;
    procedure InsertPrintShortLink(pLinkTarget: string); override;
  end;


implementation

uses frmexample,frmLink;

{ TCustomElementEditor }

procedure TCustomElementEditor.SetModified(const AValue: Boolean);
begin
  FModified := AValue;
  if (FOnChangeCount=0) and FModified and Assigned(FChangeEvent) then
    FChangeEvent(Self);
end;

procedure TCustomElementEditor.SetElement(Value: TDomElement);

begin
  FElement:=Value;
end;

function TCustomElementEditor.GetInitialDir: String;
begin
  result := '';
  if Assigned(FGetInitialDir) then
    result := FGetInitialdir();
end;

procedure TCustomElementEditor.LockOnChange;
begin
  FOnChangeCount := 1;
end;

procedure TCustomElementEditor.UnLockOnChange;
begin
  FOnChangeCount := 0;
end;

{ ---------------------------------------------------------------------
  TElementEditor
  ---------------------------------------------------------------------}


Constructor TElementEditor.Create(AOwner : TComponent);

Var
  P0,P1,P2,P3,P4 : TPanel;
  L : Tlabel;

begin
  LockOnChange;
  Inherited;
  FExampleNodes:=TList.create;
  ILElements:=TImageList.Create(Self);
  ILElements.Height:=22;
  ILElements.Width:=22;
  ILElements.AddFromLazarusResource('Insert_16N');
  ILElements.AddFromLazarusResource('Edit_16N');
  ILElements.AddFromLazarusResource('Delete_16N');
  P0:=TPanel.Create(Self);
  With P0 do
    begin
    Parent:=Self;
    Align:=alTop;
    Height:=75;
    BevelOuter:=bvNone;
    end;
  FLabel:=TLabel.Create(Self);
  With FLabel do
    begin
    parent:=P0;
    Caption:='<New element>';
    Align:=alTop;
    end;
  L:=TLabel.Create(self);
  With L do
    begin
    L.Parent:=P0;
    L.Align:=alTop;
    L.Caption:=SShortDescription;
    end;
  FShortEntry:=TEdit.Create(Self);
  With FShortEntry do
    begin
    Parent:=P0;
    Align:=alTop;
    height:=24;
    OnEnter:=@OnEnterControl;
    OnChange:=@OnTextModified;
    end;
  // Description
  L:=TLabel.Create(self);
  With L do
    begin
    Parent:=P0;
    Align:=alBottom;
    Caption:=SDescription;
    end;
  FDescrMemo:=TMemo.Create(Self);
  With FDescrMemo do
    begin
    Parent:=Self;
    Align:=alTop;
    OnEnter:=@OnEnterControl;
    OnChange:=@OnTextModified;
    Height:=150;
    end;
  FSPlit1:=TSplitter.Create(Self);
  With FSplit1 do
    begin
    Parent:=Self;
    Align:=alTop;
    end;
  P1:=TPanel.Create(Self);
  With P1 do
    begin
    Parent:=Self;
    Align:=alClient;
    BevelOuter:=bvNone;
    end;
  // Errors
  L:=TLabel.Create(Self);
  With L do
    begin
    Parent:=P1;
    Align:=alTop;
    Caption:=SErrors;
    end;
  FErrorsMemo:=TMemo.Create(Self);
  With FErrorsMemo do
    begin
    Parent:=P1;
    Align:=alTop;
    Height:=50;
    OnEnter:=@OnEnterControl;
    OnChange:=@OnTextModified;
    end;
  FSPlit2:=TSplitter.Create(Self);
  With FSplit2 do
    begin
    Parent:=P1;
    Align:=alTop;
    end;
  // See Also
  P2:=TPanel.Create(Self);
  With P2 do
    begin
    Parent:=P1;
    Align:=alClient;
    BevelOuter:=bvNone;
    end;
  P4:=TPanel.Create(Self);
  With P4 do
    begin
    Parent:=P2;
    Align:=altop;
    BevelOuter:=bvNone;
    Height:=24;
    end;
  L:=Tlabel.Create(Self);
  With L do
    begin
    Parent:=P4;
    Align:=AlClient;
    Caption:=SSeeAlso;
    end;
  TBSeeAlso:=TToolbar.Create(Self);
  With TBSeeAlso do
    begin
    PArent:=P4;
    Align:=alRight;
    Width:=100;
    Transparent := True;
    Images:=ILElements;
    end;
  BAddSeeAlso:=TToolButton.Create(Self);
  With BAddSeeAlso do
    begin
    Parent:=TBSeeAlso;
    OnClick:=@DoAddSeeAlso;
    ImageIndex:=0;
    Hint := SHintToolbarAdd;
    end;
  BEditSeeAlso:=TToolButton.Create(Self);
  With BEditSeeAlso do
    begin
    Parent:=TBSeeAlso;
    OnClick:=@DoEditSeeAlso;
    ImageIndex:=1;
    Hint := SHintToolbarEdit;
    end;
  BDeleteSeeAlso:=TToolButton.Create(Self);
  With BDeleteSeeAlso do
    begin
    Parent:=TBSeeAlso;
    OnClick:=@DoDeleteSeeAlso;
    ImageIndex:=2;
    Hint := SHintToolbarDelete;
    end;
  FSeeAlso:=TListBox.Create(Self);
  With FSeealso do
    begin
    Parent:=P2;
    Align:=alTop;
    Height:=50;
    OnEnter:=@OnEnterControl;
    end;
  FSPlit3:=TSplitter.Create(Self);
  With FSplit3 do
    begin
    Parent:=P2;
    Align:=alTop;
    end;
  // Examples.
  P3:=TPanel.Create(Self);
  With P3 do
    begin
    Parent:=P2;
    Align:=alClient;
    BevelOuter:=bvNone;
    end;
  P4:=TPanel.Create(Self);
  With P4 do
    begin
    Parent:=P3;
    Align:=altop;
    BevelOuter:=bvNone;
    Height:=24;
    end;
  TBExamples:=TToolbar.Create(Self);
  With TBExamples do
    begin
    PArent:=P4;
    Align:=alRight;
    Width:=100;
    Transparent := True;
    Images:=ILElements;
    end;
  BAddExample:=TToolButton.Create(Self);
  With BAddExample do
    begin
    Parent:=TBexamples;
    OnClick:=@DoAddExample;
    ImageIndex:=0;
    Hint := SHintToolbarAdd;
    end;
  BEditExample:=TToolButton.Create(Self);
  With BEditExample do
    begin
    Parent:=TBexamples;
    OnClick:=@DoEditExample;
    ImageIndex:=1;
    Hint := SHintToolbarEdit;
    end;
  BDeleteExample:=TToolButton.Create(Self);
  With BDeleteExample do
    begin
    Parent:=TBexamples;
    OnClick:=@DoDeleteExample;
    ImageIndex:=2;
    Hint := SHintToolbarDelete;
    end;
  L:=Tlabel.Create(Self);
  With L do
    begin
    Parent:=P4;
    Align:=AlClient;
    Caption:=SCodeExample;
    end;
  FExamples:=TListBox.Create(Self);
  With FExamples do
    begin
    Parent:=P3;
    Align:=AlClient;
    OnEnter:=@OnEnterControl;
    end;
end;

destructor TElementEditor.destroy;
begin
  FreeAndNil(FExampleNodes);
  inherited destroy;
end;

Procedure TElementEditor.SetElement (Value : TDomElement);

begin
  If (Value<>FElement) then
    If (not Modified) or Save then
      begin
      Inherited;
      Refresh;
      end;
end;

Procedure TElementEditor.DeleteElement;

begin
  Element:=Nil;
end;

Procedure TElementEditor.Refresh;

Var
  S : TSTringStream;

  Function NodeToString(E : TDomElement) : String;

  Var
    N : TDomNode;

  begin
    If (E=Nil) then
      Result:=''
    else
      begin
      S.Seek(0,soFromBeginning);
      S.Size:=0;
      N:=E.FirstChild;
      While Assigned(N) do
        begin
        WriteXml(N,S);
        N:=N.NextSibling;
        end;
      Result:=S.Datastring;
    end;
  end;

  Function RemoveLineFeeds(S : String) : String;

  Var
    I : Integer;

  begin
    Result:=S;
    For I:=1 to Length(Result) do
      If Result[i] in [#10,#13] then
        Result[i]:=' ';
  end;

Var
  I: Integer;
  N : TDomNode;

begin
  GetNodes;
  If Assigned(Felement) then
    FLabel.Caption := Format(SDataForElement,[FElement['name']])
  else
    FLabel.Caption := SNoElement;
  S := TStringStream.Create('');
  LockOnChange;
  Try
    FShortEntry.Text:=RemoveLineFeeds(NodeToString(FShortNode));
    FDescrMemo.Text:=NodeToString(FDescrNode);
    FErrorsMemo.Text:=NodeToString(FErrorsNode);
    FSeeAlso.Items.Clear;
    If Assigned(FSeeAlsoNode) then
      begin
      N:=FSeeAlsoNode.FirstChild;
      While N<>Nil do
        begin
        If IsLinkNode(N) then
          FSeeAlso.Items.Add(TDomElement(N)['id']);
        N:=N.NextSibling;
        end;
      end;
    FExamples.Items.Clear;
    For I:=0 to FExampleNodes.Count-1 do
      FExamples.Items.Add(TDomElement(FExampleNodes[i])['file']);
    FModified:=False;
  Finally
    S.Free;
    UnLockOnChange;
  end;
end;

Function TElementeditor.TestSave(S : String) : Boolean;

Const
  Head = '<?xml version="1.0" encoding="ISO-8859-1"?><fpdoc-descriptions>';
  Tail = '</fpdoc-descriptions>';
  SErrorSaving = 'There is an error in the documentation nodes:'+LineEnding+
                 '%s'+LineEnding+
                 'Please correct it first and try saving again.';

Var
  D : TXMLDocument;
  SS : TStringStream;

begin
  Result:=Length(S)=0;
  If Not Result then
    begin
    SS:=TStringStream.Create(Head+S+Tail);
    D:=nil;
    Try
      Try
        ReadXmlFile(D,SS);
        Result:=True;
      except
        On E : Exception do
           MessageDlg(Format(SErrorSaving,[E.Message]),mtError,[mbOK],0)
      end;
    finally
      D.Free;
      SS.Free;
    end;
    end;
end;

Function  TElementEditor.CurrentXML : String;

  Function GetNodeString(NodeName,Value : String) : String;

  begin
    Result:='';
    If (Value<>'') Then
      Result:=Format('<%s>%s</%s>',[NodeName,Value,NodeName])
    else If Not SkipEmptyNodes then
      result:='<'+NodeName+'/>';
  end;

Var
  I : Integer;
  S : String;

begin
  Result:='';
  If Not Assigned(FElement) then
    Exit;
  Result:=GetNodeString('short',Trim(FShortEntry.Text));
  Result:=Result+GetNodeString('descr',trim(FDescrMemo.Text));
  Result:=Result+GetNodeString('errors',trim(FErrorsMemo.Text));
  S:='';
  for I:=0 to FSeeAlso.Items.Count-1 do
    if Trim(FSeeAlso.Items[i])<>'' then
      S:=S+'<link id="'+Trim(FSeeAlso.Items[i])+'"/>';
  Result:=Result+GetNodeString('seealso',S);
  S:='';
  for I:=0 to FExamples.Items.Count-1 do
    if Trim(FExamples.Items[i])<>'' then
      S:=S+'<example file="'+Trim(FExamples.Items[i])+'"/>';
  Result:=Result+S;
  //Result:=Result+GetNodeString('example',S);
end;

Function TElementEditor.Save : Boolean;

Var
  SS : TStringStream;
  S : String;
  N,NN : TDomNode;

begin
  Result:=Not Assigned(FElement);
  if Not Result then
    begin
    Result:=False;
    S:=CurrentXML;
    If TestSave(S) then
      begin
      SS:=TStringStream.Create(S);
      Try
        // Free child nodes.
        N:=FElement.FirstChild;
        While N<>Nil do
          begin
          NN:=N.NextSibling;
          If not (IsElementNode(N) or IsModuleNode(N) or IsTopicNode(N)) then
            FElement.RemoveChild(N);
          N:=NN;
          end;
        // Read them again from stream.
        SS.Seek(0,soFromBeginning);
        ReadXMLFragment(FElement,SS);
        FModified:=False;
        // We must get the nodes back, because they were deleted !
        GetNodes;
        Result:=True;
        FSavedNode:=True;
      Finally
        SS.Free;
      end;
      end;
    end;
end;

function TElementEditor.CanInsertTag(TagTYpe: TTagType): Boolean;
begin
  Result:=(FCurrentEditable is TCustomEdit)
          and ((TagType<>ttTable) or (FCurrentEditable is TMemo));
end;

Procedure TElementEditor.InsertTag (tagName : String);

Var
  S : String;

begin
  If Assigned(CurrentEditable) and (CurrentEditable is TCustomEdit) then
    With TCustomEdit(CurrentEditable)do
      begin
      S:=SelText;
      S:=Format('<%s>%s</%s>',[TagName,S,TagName]);
      Seltext:=S;
      SelLength:=Length(S);
      Modified:=True;
      end;
end;

Procedure TElementEditor.InsertTag(TagType : TTagType);

begin
  InsertTag(TagNames[TagTYpe]);
end;

Procedure TElementEditor.InsertLink(LinkTarget,LinkText : String);

begin
  If (CurrentEditable<>Nil) and (CurrentEditable is TCustomEdit) then
    With TCustomEdit(CurrentEditable) do
      begin
      If (LinkText<>'') then
        SelText:='<link id="'+LinkTarget+'">'+LinkText+'</link>'
      else
        SelText:='<link id="'+LinkTarget+'"/>';
      end
  else if (CurrentEditable=FSeeAlso) then
    FSeeAlso.Items.add(LinkTarget);
  Modified:=True;
end;

Procedure TElementEditor.InsertTable(Cols,Rows : Integer; UseHeader : Boolean);

Var
  I : Integer;
  R,T : String;

begin
  If (CurrentEditable is TMemo) then
    begin
    R:='<tr>';
    For I:=1 to Cols do
      R:=R+'<td></td>';
    R:=R+'</tr>'+lineEnding;
    T:='';
    If UseHeader then
      begin
      Dec(Rows);
      T:='<th>';
      For I:=1 to Cols do
        T:=T+'<td></td>';
      T:=T+'</th>'+lineEnding;
      end;
    For I:=1 to rows do
      T:=T+R;
    T:=LineEnding+'<table>'+LineEnding+T+'</table>'+LineEnding;
    With TMemo(CurrentEditable) do
      SelText:=t;
    end;
end;


procedure TElementEditor.InsertPrintShortLink(pLinkTarget: string);
begin
  { Should be Limit insert only to Long Description edit box? }
  if (CurrentEditable <> nil) and (CurrentEditable is TCustomEdit) then
  begin
    (CurrentEditable as TCustomEdit).SelText :=
        Format('<%s id="%s"/>', [TagNames[ttPrintShort], pLinkTarget]);
  end;
  Modified := True;
end;


Procedure TElementEditor.GetNodes;

Var
  Node : TDomNode;
  S : String;

begin
  FShortNode:=Nil;
  FDescrNode:=Nil;
  FErrorsNode:=Nil;
  FSeeAlsoNode:=Nil;
  FExampleNodes.Clear;
  If Assigned(FElement) then
    begin
    Node:=FElement.FirstChild;
    While Assigned(Node) do
      begin
      If (Node.NodeType=ELEMENT_NODE) then
        begin
        S:=Node.NodeName;
        If S='short' then
          FShortNode:=TDomElement(Node)
        else if S='descr' then
          FDescrNode:=TDomElement(Node)
        else if S='errors' then
          FErrorsNode:=TDomElement(Node)
        else if S='seealso' then
          FSeeAlsoNode:=TDomElement(Node)
        else if S='example' then
          FExampleNodes.Add(Node);
        end;
      Node:=Node.NextSibling;
      end;
    end;
end;

Function TElementEditor.CurrentEditable : TWinControl;

begin
  Result:=FCurrentEditable;
end;

procedure TElementEditor.OnEnterControl(Sender: TObject);
begin
  if Sender=nil then ;
  FCurrentEditable:=Sender as TWinControl;
end;

Procedure TElementEditor.OnTextModified(Sender : TObject);

begin
  if Sender=nil then ;
  Modified:=True;
end;

procedure TElementEditor.DoAddExample(Sender: TObject);

begin
  if Sender=nil then ;
  With TExampleForm.Create(Self) do
    Try
      ExampleName:='example.pp';
      ExampleDir:= GetInitialDir;
      If ShowModal=mrOK then
        begin
          if FExamples.Items.IndexOf(ExampleName)<0 then
          begin
            FExamples.Items.Add(ExampleName);
            Modified:=True;
          end;
        end;
    Finally
      Free;
    end;
end;

procedure TElementEditor.DoEditExample(Sender: TObject);

begin
  if Sender=nil then ;
  With FExamples do
    begin
    If ItemIndex<>-1 then
      With TExampleForm.Create(Self) do
        Try
          ExampleName:=Items[ItemIndex];
          If ShowModal=mrOK then
            begin
            Items[ItemIndex]:=ExampleName;
            Modified:=True;
            end;
        Finally
          Free;
        end;
    end;
end;

procedure TElementEditor.DoDeleteExample(Sender: TObject);

begin
  if Sender=nil then ;
  With FExamples do
    begin
    If ItemIndex<>-1 then
      begin
      Items.Delete(ItemIndex);
      Modified:=True;
      end;
    end;
end;

Function TElementEditor.EditLink(Var Value : String) : Boolean;

begin
  With TLinkForm.Create(Self) do
    try
      If Assigned(OnGetElementList) then
        begin
        CBTarget.Items.BeginUpdate;
        Try
          OnGetElementList(CBTarget.Items);
        Finally
          CBTarget.Items.EndUpdate;
        end;
        end;
      CBTarget.Text:=Value;
      Result:=ShowModal=mrOK;
      If Result then
        Value:=CBTarget.Text;
    Finally
      Free;
    end;
end;

procedure TElementEditor.DoAddSeeAlso(Sender: TObject);

Var
  S : String;

begin
  if Sender=nil then ;
  S:='';
  If EditLink(S) then
    begin
      if FSeeAlso.Items.IndexOf(S)<0 then
        begin
          FSeeAlso.Items.Add(S);
          Modified:=True;
        end;
    end;
end;

procedure TElementEditor.DoEditSeeAlso(Sender: TObject);

Var
  S : String;

begin
  if Sender=nil then ;
  With FSeeAlso do
    begin
    If (ItemIndex>=0) then
      S:=Items[ItemIndex]
    else
      S:='';
    If EditLink(S) then
      begin
      If (ItemIndex>=0) then
        Items[ItemIndex]:=S
      else
        Items.Add(S);
      Modified:=True;
      end;
    end;
end;

procedure TElementEditor.DoDeleteSeeAlso(Sender: TObject);
begin
  if Sender=nil then ;
  With FSeeAlso do
    If (ItemIndex<>-1) then
      begin
      Items.Delete(ItemIndex);
      Modified:=True;
      end;
end;

Function TElementEditor.GetCurrentSelection : String;

begin
  If (CurrentEditable=Nil) or not (CurrentEditable is TCustomEdit) then
    Result:=''
  else
    Result:=TCustomEdit(CurrentEditable).SelText;
end;

initialization
  {$i icons.lrs}

end.

