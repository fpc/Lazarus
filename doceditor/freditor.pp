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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Michael Van Canneyt
  Changed to Frame by Vladislav V. Sudarikov 
}
unit freditor;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, StrUtils,
  Forms, Controls, ExtCtrls, Buttons, StdCtrls, Dialogs, Menus,
  DOM, xmlread, xmlwrite, fpdeutil, Lazdemsg, Lazdeopts,
  GraphType, ActnList, LResources, LazUTF8;

Type

  { TElemEditorFrame }
  TGetElementListEvent = Procedure (List : TStrings) of Object;
  TGetInitialDirEvent = function: string of object;

  { TElemEditorFrame }

  TElemEditorFrame = Class(TFrame)
    edtShortEntry: TEdit;
    GroupBox1: TGroupBox;
    lblDescr: TLabel;
    lblErrors: TLabel;
    lblExamples: TLabel;
    lblNewElem: TLabel;
    lblSeeAlso: TLabel;
    lblShortDescr: TLabel;
    lbxExamples: TListBox;
    lbxSeeAlso: TListBox;
    memDescr: TMemo;
    memErrors: TMemo;
    pnlDescr: TPanel;
    pnlErrors: TPanel;
    pnlExamples: TPanel;
    pnlSeeAlso: TPanel;
    pnlShorDescr: TPanel;
    spbAddExample: TSpeedButton;
    spbAddSeeAlso: TSpeedButton;
    spbEditExample: TSpeedButton;
    spbEditElementLink: TSpeedButton;
    spbDeleteExample: TSpeedButton;
    spbDeleteSeeAlso: TSpeedButton;
    spbEditSeeAlso: TSpeedButton;
    Splitter1: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    Procedure OnEnterControl(Sender : TObject);
    Procedure OnTextModified(Sender : TObject);
    Procedure DoEditSeeAlso(Sender: TObject);
    Procedure DoDeleteSeeAlso(Sender: TObject);
    Procedure DoAddExample(Sender : TObject);
    Procedure DoEditExample(Sender : TObject);
    Procedure DoDeleteExample(Sender : TObject);
    Procedure DoAddSeeAlso(Sender : TObject);
    Procedure DoEditElementLink(Sender : TObject);
  private
    FElementLink : String;
    Felement : TDomElement;
    FCurrentEditable : TWinControl;
    FGetElementList: TGetElementListEvent;
    FSavedNode,
    FModified : Boolean;
    FTargetFileName: string;
    FGetInitialDir: TGetInitialDirEvent;
    FChangeEvent: TNotifyEvent;
    FOnChangeCount: Integer;
    Function EditLink(Var Value,ALinkText : String) : Boolean;
    procedure SetModified(const AValue: Boolean);
    procedure ShowElementCaption;
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
    procedure InsertItemizeList(ItemsCount: Integer); virtual; abstract;
    procedure InsertEnumerateList(ItemsCount: Integer); virtual; abstract;

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

  TElementEditor = Class(TElemEditorFrame)
  Private
    FExampleNodes : TList;
    FShortNode,
    FDescrNode,
    FErrorsNode,
    FSeeAlsoNode : TDomElement;
    Procedure GetNodes;
    Function  CurrentEditable : TWinControl;
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
    procedure InsertItemizeList(ItemsCount: Integer); override;
    procedure InsertEnumerateList(ItemsCount: Integer); override;
  end;

implementation

uses frmexample, frmLink, FrmMain;

Function JoinLinkText(lblShortDescr,T : String): String;
begin
  Result:=lblShortDescr;
  If (T<>'') then
    Result:=Result+'|'+T;
end;

Procedure SplitLinkText(LT : String; out lblShortDescr,T : String);

Var
  P : Integer;

begin
  P:=Pos('|',LT);
  If (P=0) then
    begin
    lblShortDescr:=LT;
    T:='';
    end
  else
    begin
    T:=LT;
    lblShortDescr:=Copy(LT,1,P-1);
    Delete(T,1,P);
    end;
end;

{$R *.lfm}

{ TElemEditorFrame }

procedure TElemEditorFrame.SetModified(const AValue: Boolean);
begin
  FModified := AValue;
  if (FOnChangeCount=0) and FModified and Assigned(FChangeEvent) then
    FChangeEvent(Self);
end;

procedure TElemEditorFrame.SetElement(Value: TDomElement);

begin
  FElement:=Value;
end;

function TElemEditorFrame.GetInitialDir: String;
begin
  result := '';
  if Assigned(FGetInitialDir) then
    result := FGetInitialdir();
end;

procedure TElemEditorFrame.LockOnChange;
begin
  FOnChangeCount := 1;
end;

procedure TElemEditorFrame.UnLockOnChange;
begin
  FOnChangeCount := 0;
end;

{ ---------------------------------------------------------------------
  TElementEditor
  ---------------------------------------------------------------------}


Constructor TElementEditor.Create(AOwner : TComponent);
begin
  LockOnChange;
  Inherited;
  FExampleNodes:=TList.create;
  lblShortDescr.Caption := SShortDescription;
  lblDescr.Caption := sDescription;
  lblErrors.Caption := SErrors;
  lblSeeAlso.Caption := SSeeAlso;
  lblExamples.Caption := SCodeExample;
  spbAddExample.Hint := SHintToolbarAdd;
  spbAddSeeAlso.Hint := SHintToolbarAdd;
  spbDeleteExample.Hint := SHintToolbarDelete;
  spbDeleteSeeAlso.Hint := SHintToolbarDelete;
  spbEditElementLink.Hint := SHintToolbarEdit;
  spbEditExample.Hint := SHintToolbarEdit;
  spbEditSeeAlso.Hint := SHintToolbarEdit;
  memDescr.PopupMenu:=MainForm.PopupMenu1;
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

Procedure TElemEditorFrame.ShowElementCaption;
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
  lblNewElem.Caption:=ST;
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
    For i := Length(Result) downto 1 do
      If Result[i] in [#10,#13] then
        Delete(Result, i, 1);
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
    edtShortEntry.Text:=RemoveLineFeeds(NodeToString(FShortNode));
    memDescr.Text:=NodeToString(FDescrNode);
    memErrors.Text:=NodeToString(FErrorsNode);
    lbxSeeAlso.Items.Clear;
    If Assigned(FSeeAlsoNode) then
      begin
      N:=FSeeAlsoNode.FirstChild;
      While N<>Nil do
        begin
        If IsLinkNode(N) then
          lbxSeeAlso.Items.Add(JoinLinkText(TDomElement(N)['id'],NodeToString(TDomElement(N))));
        N:=N.NextSibling;
        end;
      end;
    lbxExamples.Items.Clear;
    For I:=0 to FExampleNodes.Count-1 do
      lbxExamples.Items.Add(TDomElement(FExampleNodes[i])['file']);
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
  S,L,LT,T : String;

begin
  Result:='';
  If Not Assigned(FElement) then
    Exit;
  Result:=GetNodeString('short',Trim(edtShortEntry.Text));
  Result:=Result+GetNodeString('descr',trim(memDescr.Text));
  Result:=Result+GetNodeString('errors',trim(memErrors.Text));
  S:='';
  for I:=0 to lbxSeeAlso.Items.Count-1 do
    begin
    LT:=Trim(lbxSeeAlso.Items[i]);
    if (LT<>'') then
      begin
      SplitLinkText(LT,L,T);
      If (T<>'') then
        S:=S+'<link id="'+L+'">'+T+'</link>'
      else
        S:=S+'<link id="'+L+'"/>';
      end;
    end;
  Result:=Result+GetNodeString('seealso',S);
  S:='';
  for I:=0 to lbxExamples.Items.Count-1 do
    if Trim(lbxExamples.Items[i])<>'' then
      S:=S+'<example file="'+Trim(lbxExamples.Items[i])+'"/>';
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
  If CurrentEditable is TCustomEdit then
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
  else if (CurrentEditable=lbxSeeAlso) then
    lbxSeeAlso.Items.add(LinkTarget);
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

procedure TElementEditor.InsertItemizeList(ItemsCount: Integer);
var
  I: Integer;
  R: String;
begin
  If (CurrentEditable is TMemo) then
    begin
    R:='<ul>'+lineEnding;
    for I:=1 to ItemsCount do
      R:=R+'<li></li>'+lineEnding;
    R:=R+'</ul>'+lineEnding;
    With TMemo(CurrentEditable) do
      SelText:=R;
    end;
end;

procedure TElementEditor.InsertEnumerateList(ItemsCount: Integer);
var
  I: Integer;
  R: String;
begin
  if (CurrentEditable is TMemo) then
  begin
  R:='<ol>'+lineEnding;
  for I:=1 to ItemsCount do
    R:=R+'<li></li>'+lineEnding;
  R:=R+'</ol>'+lineEnding;
  with TMemo(CurrentEditable) do
    SelText:=R;
  end;
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

procedure TElemEditorFrame.OnEnterControl(Sender: TObject);
begin
  if Sender=nil then ;
  FCurrentEditable:=Sender as TWinControl;
end;

Procedure TElemEditorFrame.OnTextModified(Sender : TObject);
begin
  if Sender=nil then ;
  Modified:=True;
end;

procedure TElemEditorFrame.DoAddExample(Sender: TObject);

begin
  if Sender=nil then ;
  With TExampleForm.Create(Self) do
    Try
      Caption := SInsertExampleCode;
      EFileName.Text := 'example.pp';
      LEFileName.Caption := SCodeExample;
      EFileName.InitialDir := GetInitialDir;
      If ShowModal=mrOK then
        begin
          if lbxExamples.Items.IndexOf(ExampleName)<0 then
          begin
            lbxExamples.Items.Add(ExampleName);
            Modified:=True;
          end;
        end;
    Finally
      Free;
    end;
end;

procedure TElemEditorFrame.DoEditExample(Sender: TObject);

begin
  if Sender=nil then ;
  With lbxExamples do
    begin
    If ItemIndex<>-1 then
      With TExampleForm.Create(Self) do
        Try
          Caption := SInsertExampleCode;
          EFileName.Text := Items[ItemIndex];
          LEFileName.Caption := SCodeExample;
          EFileName.InitialDir := GetInitialDir;
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

procedure TElemEditorFrame.DoDeleteExample(Sender: TObject);

begin
  if Sender=nil then ;
  With lbxExamples do
    begin
    If ItemIndex<>-1 then
      begin
      Items.Delete(ItemIndex);
      Modified:=True;
      end;
    end;
end;

Function TElemEditorFrame.EditLink(Var Value,ALinkText : String) : Boolean;
begin
  With TLinkForm.Create(Self) do
    try
      Caption:=SInsertLink;
      LLinkTarget.Caption := SLinkTarget;
      LELinkText.Caption := SLinkText;
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

procedure TElemEditorFrame.DoAddSeeAlso(Sender: TObject);
Var
  S,T : String;
begin
  if Sender=nil then ;
  S:='';
  T:='';
  If EditLink(S,T) then
    begin
    S:=JoinLinkText(S,T);
      if lbxSeeAlso.Items.IndexOf(S)<0 then
        begin
          lbxSeeAlso.Items.Add(S);
          Modified:=True;
        end;
    end;
end;

procedure TElemEditorFrame.DoEditSeeAlso(Sender: TObject);
Var
  S,T : String;
begin
  if Sender=nil then ;
  With lbxSeeAlso do
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

procedure TElemEditorFrame.DoDeleteSeeAlso(Sender: TObject);
begin
  if Sender=nil then ;
  With lbxSeeAlso do
    If (ItemIndex<>-1) then
      begin
      Items.Delete(ItemIndex);
      Modified:=True;
      end;
end;

procedure TElemEditorFrame.DoEditElementLink(Sender: TObject);
begin
  With TLinkForm.Create(Self) do
    try
      Caption:=SHintEditElementLink;
      LLinkTarget.Caption := SLinkTarget;
      LELinkText.Caption := SLinkText;
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

end.



