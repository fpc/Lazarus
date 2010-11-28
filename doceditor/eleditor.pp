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

type
  TCustomElementEditorNew = class(TFrame)

  end;

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
    TBLink : TToolbar;
    TBEditLink : TToolbutton;
    //ILElements : TImageList;
    FSeeAlso,
    FExamples : TListBox;
    FCurrentEditable : TWinControl;
    FElementLink : String;
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
    Procedure DoEditElementLink(Sender : TObject);
    Function EditLink(Var Value,ALinkText : String) : Boolean;
    procedure ShowElementCaption;
  Public
    Constructor Create (AOwner : TComponent); override;
    Destructor Destroy; override;
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

uses frmexample, frmLink, StrUtils, LCLProc, FrmMain;

Function JoinLinkText(L,T : String): String;
begin
  Result:=L;
  If (T<>'') then
    Result:=Result+'|'+T;
end;

Procedure SplitLinkText(LT : String; Var L,T : String);

Var
  P : Integer;

begin
  P:=Pos('|',LT);
  If (P=0) then
    begin
    L:=LT;
    T:='';
    end
  else
    begin
    T:=LT;
    L:=Copy(LT,1,P-1);
    Delete(T,1,P);
    end;
end;

{$R *.lfm}

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
{  ILElements:=TImageList.Create(Self);
  ILElements.Height:=22;
  ILElements.Width:=22;
  ILElements.AddLazarusResource('Insert_16N');
  ILElements.AddLazarusResource('Edit_16N');
  ILElements.AddLazarusResource('Delete_16N');}

  P0:=TPanel.Create(Self);
  with P0 do
  begin
    Parent:=Self;
    Align:=alTop;
    Height:=85;
    BevelOuter:=bvNone;
    AutoSize:=true;
  end;
  P1:=TPanel.Create(Self);
  with P1 do
  begin
    Parent:=P0;
    Align:=alClient;
    BevelOuter:=bvNone;
    AutoSize:=true;
  end;
  FLabel:=TLabel.Create(Self);
  With FLabel do
  begin
    parent:=P1;
    Caption:='<New element>';
    Align:=alTop;
  end;
  TBLink:=TToolbar.Create(Self);
  With TBLink do
    begin
    PArent:=P0;
    Align:=alRight;
    Width:=50;
    Transparent := True;
    Images:=MainForm.ILElements; //ILElements;
    end;
  TBEditLink:=TToolbutton.Create(Self);
  With TBEditLink do
    begin
    Parent:=TBLink;
    OnClick:=@DoEditElementLink;
    ImageIndex:=1;
    Hint := SHintEditElementLink;
    end;
  L:=TLabel.Create(self);
  With L do
    begin
    L.Parent:=P1;
    Top := 15;
    L.Align:=alTop;
    L.Caption:=SShortDescription;
    end;
  FShortEntry:=TEdit.Create(Self);
  With FShortEntry do
  begin
    Parent:=P1;
    Top := 35;
    Align:=alTop;
    height:=24;
    OnEnter:=@OnEnterControl;
    OnChange:=@OnTextModified;
  end;
  // Description
  L:=TLabel.Create(self);
  With L do
  begin
//    Parent:=P0;
    Parent:=Self;
    Align:=alTop;
    Caption:=SDescription;
  end;

  FDescrMemo:=TMemo.Create(Self);
  with FDescrMemo do
  begin
    Parent:=Self;
    Top := 80;
    Align:=alTop;
    OnEnter:=@OnEnterControl;
    OnChange:=@OnTextModified;
    Height:=150;
    ScrollBars:=ssAutoBoth;
    PopupMenu:=MainForm.PopupMenu1;
  end;
  FSPlit1:=TSplitter.Create(Self);
  With FSplit1 do
    begin
    Parent:=Self;
    Top := 85;
    Align:=alTop;
    end;
  P1:=TPanel.Create(Self);
  With P1 do
    begin
    Parent:=Self;
    Top := 90;
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
    Top := 15;
    Align:=alTop;
    Height:=50;
    OnEnter:=@OnEnterControl;
    OnChange:=@OnTextModified;
    ScrollBars:=ssAutoBoth;
    end;
  FSPlit2:=TSplitter.Create(Self);
  With FSplit2 do
    begin
    Parent:=P1;
    Top := 70;
    Align:=alTop;
    end;
  // See Also
  P2:=TPanel.Create(Self);
  With P2 do
    begin
    Parent:=P1;
    Top := 75;
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
    Images:=MainForm.ILElements; //ILElements;
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
    OnDblClick:=@DoEditSeeAlso;
    end;
  FSPlit3:=TSplitter.Create(Self);
  With FSplit3 do
    begin
    Parent:=P2;
    Top := 55;
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
    Images:=MainForm.ILElements;//ILElements;
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

Procedure TElementEditor.ShowElementCaption;


Var
  ST : String;

begin
  If Assigned(Felement) then
  begin
  ST:=Format(SDataForElement,[FElement['name']]);
  If (FElementLink<>'') then
    ST:=ST+SLinksTo+FElementLink;
  end
else
  ST := SNoElement;
FLabel.Caption:=ST;
end;

Procedure TElementEditor.Refresh;

  function RemoveLFAfterTags(S : String) : String;
  
    function RemoveLF(S, Tag : string; LenTag : integer) : string;
    var
      Remove : Integer;
    begin
      Remove := pos(Tag, S);
      while Remove <> 0 do begin
        inc(Remove, LenTag);
        while (Remove <= length(S)) and (S[Remove] in [#10, #13]) do
          delete(S, Remove, 1);
        Remove := PosEx(Tag, S, Remove);
      end;
      Result := S;
    end;
    
  const
    Link = '</link>' + LineEnding;
    LenLink = length(Link) - length(LineEnding);
    Parag = '<p/>' + LineEnding;
    LenParag = length(Parag);
    Bold = '</b>' + LineEnding;
    LenBold = length(Bold);
  begin
    Result := RemoveLF(S, Link, LenLink);
    Result := RemoveLF(Result, Parag, LenParag);
    Result := RemoveLF(Result, Bold, LenBold);
  end;

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
      Result:=RemoveLFAfterTags(S.Datastring);
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
  ShowElementCaption;
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
          FSeeAlso.Items.Add(JoinLinkText(TDomElement(N)['id'],NodeToString(TDomElement(N))));
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
  I,P : Integer;
  S,L,LT : String;

begin
  Result:='';
  If Not Assigned(FElement) then
    Exit;
  Result:=GetNodeString('short',Trim(FShortEntry.Text));
  Result:=Result+GetNodeString('descr',trim(FDescrMemo.Text));
  Result:=Result+GetNodeString('errors',trim(FErrorsMemo.Text));
  S:='';
  for I:=0 to FSeeAlso.Items.Count-1 do
    begin
    LT:=Trim(FSeeAlso.Items[i]);
    if (LT<>'') then
      begin
      SplitLinkText(LT,L,LT);
      If (LT<>'') then
        S:=S+'<link id="'+L+'">'+LT+'</link>'
      else
        S:=S+'<link id="'+L+'"/>';
      end;
    end;
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
  SS : TStream;
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
        If (FElementLink<>'') then
          FElement['link']:=FElementLink;
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

procedure TElementEditor.InsertTag (tagName : String);
var
  S : String;
  SS:integer;
begin
  If Assigned(CurrentEditable) and (CurrentEditable is TCustomEdit) then
    with TCustomEdit(CurrentEditable)do
    begin
      S:=SelText;
      SS:=SelStart;
      S:=Format('<%s>%s</%s>',[TagName,S,TagName]);
      Seltext:=S;
      SelStart:=SS;
      SelLength:=UTF8Length(S);
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
    FElementLink:=FElement['link'];
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

Function TElementEditor.EditLink(Var Value,ALinkText : String) : Boolean;

begin
  With TLinkForm.Create(Self) do
    try
      Caption:=SInsertLink;
      If Assigned(OnGetElementList) then
        begin
        Links.BeginUpdate;
        Try
          OnGetElementList(Links);
        Finally
          Links.EndUpdate;
        end;
        end;
      Link:=Value;
      LinkText:=ALinkText;
      Result:=ShowModal=mrOK;
      If Result then
        begin
        Value:=CBTarget.Text;
        ALinkText:=LinkText;
        end;
    Finally
      Free;
    end;
end;

procedure TElementEditor.DoAddSeeAlso(Sender: TObject);

Var
  S,T : String;

begin
  if Sender=nil then ;
  S:='';
  T:='';
  If EditLink(S,T) then
    begin
    S:=JoinLinkText(S,T);
      if FSeeAlso.Items.IndexOf(S)<0 then
        begin
          FSeeAlso.Items.Add(S);
          Modified:=True;
        end;
    end;
end;

procedure TElementEditor.DoEditSeeAlso(Sender: TObject);

Var
  S,T : String;
  P : Integer;

begin
  if Sender=nil then ;
  With FSeeAlso do
    begin
    If (ItemIndex>=0) then
      begin
      SplitLinkText(Items[ItemIndex],S,T);
      end
    else
      begin
      S:='';
      T:='';
      end;
    If EditLink(S,T) then
      begin
      S:=JoinLinkText(S,T);
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

procedure TElementEditor.DoEditElementLink(Sender: TObject);
begin
  With TLinkForm.Create(Self) do
    try
      Caption:=SHintEditElementLink;
      If Assigned(OnGetElementList) then
        begin
        Links.BeginUpdate;
        Try
          OnGetElementList(Links);
        Finally
          Links.EndUpdate;
        end;
        end;
      Link:=FElementLink;
      EnableLinkText:=False;
      If ShowModal=mrOK then
        begin
        FElementLink:=Link;
        ShowElementCaption;
        end;
    Finally
      Free;
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



