{
/***************************************************************************
                             LazDocFrm.pas
                             -------------

 ***************************************************************************/

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
  see for todo list: http://wiki.lazarus.freepascal.org/index.php/LazDoc
}

unit LazDocFrm;

{$mode objfpc}{$H+}

{ $define dbgLazDoc}

interface

uses
  Classes, SysUtils, StrUtils,
  LCLProc, LResources, StdCtrls, Buttons, ComCtrls, Controls, Dialogs,
  ExtCtrls, Forms, Graphics,
  SynEdit,
  CodeToolManager, CodeCache,
  Laz_DOM, Laz_XMLRead, Laz_XMLWrite,
  IDEHelpIntf, HelpIntf,
  IDEProcs, LazarusIDEStrConsts;

const
  SHORT = 1;
  DESCR = 2;
  ERRORS = 4;
  SEEALSO = 5;
  EXAMPLE = 6;
  NODEITEMS = EXAMPLE;  //always make nodeitems equal to higest element

type
  TFPDocNode = array [1..NODEITEMS] of String;

  { TLazDocForm }

  TLazDocForm = class(TForm)
    AddLinkButton: TButton;
    BrowseExampleButton: TButton;
    ExampleEdit: TEdit;
    LinkIdComboBox: TComboBox;
    DeleteLinkButton: TButton;
    DescrMemo: TMemo;
    LinkTextEdit: TEdit;
    LinkListBox: TListBox;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    ShortEdit: TEdit;
    ErrorsMemo: TMemo;
    PageControl: TPageControl;
    DescrTabSheet: TTabSheet;
    ErrorsTabSheet: TTabSheet;
    ShortTabSheet: TTabSheet;
    BoldFormatButton: TSpeedButton;
    ItalicFormatButton: TSpeedButton;
    InsertCodeTagButton: TSpeedButton;
    InsertRemarkButton: TSpeedButton;
    InsertVarTagButton: TSpeedButton;
    ExampleTabSheet: TTabSheet;
    UnderlineFormatButton: TSpeedButton;
    SeeAlsoTabSheet: TTabSheet;
    procedure AddLinkButtonClick(Sender: TObject);
    procedure BrowseExampleButtonClick(Sender: TObject);
    procedure DeleteLinkButtonClick(Sender: TObject);
    procedure DocumentationTagChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormatButtonClick(Sender: TObject);
    procedure LinkChange(Sender: TObject);
    procedure LinkListBoxClick(Sender: TObject);
  private
    FChanged:   Boolean;
    FCurrentElement: TPascalHelpContextList;
    FDoc: TXMLdocument;
    FDocFileName: String;
    FLinkIndex: Integer;
    function ElementFromNode(Node: TDOMNode): TFPDocNode;
    function ExtractFuncProc(const startpos: TPoint; const keyword: String;
      src: tStrings): String;
    function GetFirstChildValue(n: TDOMNode): String;
    function GetFirstElement: TDOMNode;
    function GetModuleNode: TDOMNode;
    function GetNearestSourceElement(const SrcFilename: string;
      const CaretPos: TPoint): TPascalHelpContextList;
    function MakeLink: String;
    function NodeByPascalContext(const AContext: TPascalHelpContextList): TDOMNode;
    function GetElementName(const AContext: TPascalHelpContextList): string;
    procedure GetElementList;
    procedure InsertElement(const ElementName: String);
    procedure Save;
    procedure SetDocFileName(const Value: String);
    procedure UpdateCaption;
  public
    procedure Reset;
    procedure UpdateLazDoc(const SrcFilename: string; const Caret: TPoint);
    property DocFileName: String read FDocFileName write SetDocFileName;
    property Doc: TXMLdocument read FDoc;
  end;

var
  LazDocForm: TLazDocForm;

procedure DoShowLazDoc;

implementation

{ TLazDocForm }

procedure DoShowLazDoc;
begin
  if LazDocForm = Nil then
    Application.CreateForm(TLazDocForm, LazDocForm);

  LazDocForm.Show;
end;

function TLazDocForm.GetModuleNode: TDOMNode;
var
  n: TDOMNode;
begin
  //get first node
  n := doc.FindNode('fpdoc-descriptions');

  //proceed to package (could there be more packages in one file??)
  n := n.FirstChild;

  //proceed to module  (could there be more modules in one file??)
  n := n.FirstChild;
  while n.NodeName <> 'module' do
    n := n.NextSibling;

  Result := n;
end;

function TLazDocForm.GetFirstElement: TDOMNode;
var
  Node: TDOMNode;
begin
  //get first module node
  Node := GetModuleNode;

  //proceed to element
  Node := Node.FirstChild;
  while Node.NodeName <> 'element' do
    Node := Node.NextSibling;

  Result := Node;
end;

procedure TLazDocForm.GetElementList;
var
  n: TDOMNode;
begin
  LinkIdComboBox.Clear;

  if not Assigned(doc) then
  begin
    {$ifdef dbgLazDoc}
    DebugLn('TLazDocForm.GetElementList: document is not set');
    {$endif}

    Exit;
  end;

  //get first element node
  n := GetFirstElement;

  //search all elements
  while Assigned(n) do
  begin
    //showmessage(TDomElement(n)['name']);
    LinkIdComboBox.Items.Add(TDomElement(n)['name']);

    n := n.NextSibling;

    if not Assigned(n) then
      Exit;

    while n.NodeName = '#comment' do
      n := n.NextSibling;
  end;
end;

procedure TLazDocForm.SetDocFileName(const Value: String);
begin
  LinkIdComboBox.Clear;

  if FileExistsCached(Value) and (Value <> FDocFileName) then
  begin
    // reset Self
    Reset;

    FDocFileName := Value;

    if Assigned(doc) then
      FreeAndNil(doc);

    ReadXMLFile(doc, FDocFileName);

    UpdateCaption;

    GetElementList;

    {$ifdef dbgLazDoc}
    DebugLn('TLazDocForm.SetDocFileName: document is set: ' + Value);
    {$endif}
  end;
end;

procedure TLazDocForm.FormCreate(Sender: TObject);
begin
  Caption := lisLazDocMainFormCaption;

  FLinkIndex := -1;

  with PageControl do
  begin
    Page[0].Caption := lisLazDocShortTag;
    Page[1].Caption := lisLazDocDescrTag;
    Page[2].Caption := lisLazDocErrorsTag;
    Page[3].Caption := lisLazDocSeeAlsoTag;
    Page[4].Caption := lisLazDocExampleTag;
    PageIndex := 0;
  end;

  BoldFormatButton.Hint := lisLazDocHintBoldFormat;
  ItalicFormatButton.Hint := lisLazDocHintItalicFormat;
  UnderlineFormatButton.Hint := lisLazDocHintUnderlineFormat;
  InsertCodeTagButton.Hint := lisLazDocHintInsertCodeTag;
  InsertRemarkButton.Hint := lisLazDocHintRemarkTag;
  InsertVarTagButton.Hint := lisLazDocHintVarTag;

  AddLinkButton.Caption := lisLazDocAddLinkButton;
  DeleteLinkButton.Caption := lisLazDocDeleteLinkButton;

  BrowseExampleButton.Caption := lisLazDocBrowseExampleButton;

  Reset;
end;

procedure TLazDocForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fDoc);
  FreeAndNil(FCurrentElement);
end;

procedure TLazDocForm.FormResize(Sender: TObject);
begin
  LinkIdComboBox.Width := (AddLinkButton.Left - LinkIdComboBox.Left - 8) div 2;
  LinkTextEdit.Left    := LinkIdComboBox.Left + LinkIdComboBox.Width + 4;
  LinkTextEdit.Width   := LinkIdComboBox.Width;
end;

procedure TLazDocForm.FormatButtonClick(Sender: TObject);

  procedure InsertTag(starttag, endtag: String);
  begin
    if PageControl.ActivePage.Caption = lisLazDocDescrTag then
      DescrMemo.SelText := starttag + DescrMemo.SelText + endtag;
    if PageControl.ActivePage.Caption = lisLazDocErrorsTag then
      ErrorsMemo.SelText := starttag + ErrorsMemo.SelText + endtag;
  end;

begin
  case TSpeedButton(Sender).Tag of
    //bold
    0:
      InsertTag('<b>', '</b>');
    //italic
    1:
      InsertTag('<i>', '</i>');
    //underline
    2:
      InsertTag('<u>', '</u>');
    //codetag
    3:
      InsertTag('<p><code>', '</code></p>');
    //remarktag
    4:
      InsertTag('<p><remark>', '</remark></p>');
    //vartag
    5:
      InsertTag('<var>', '</var>');
  end;
end;

procedure TLazDocForm.LinkChange(Sender: TObject);
begin
  if FLinkIndex = -1 then
    Exit;

  LinkListBox.Items.Strings[FLinkIndex] := MakeLink;
end;

procedure TLazDocForm.LinkListBoxClick(Sender: TObject);
var
  strTmp: String;
  intTmp: Integer;
  intStart: Integer;
begin
  //split the link into Id and Text
  FLinkIndex := LinkListBox.ItemIndex;

  if FLinkIndex = -1 then
    Exit;

  intStart := PosEx('"', LinkListBox.Items[FLinkIndex], 1);

  intTmp := PosEx('"', LinkListBox.Items[FLinkIndex], intStart + 1);

  LinkIdComboBox.Text := Copy(LinkListBox.Items[FLinkIndex],
    intStart + 1, intTmp - intStart - 1);

  strTmp := Copy(LinkListBox.Items[FLinkIndex], intTmp + 2,
    Length(LinkListBox.Items[FLinkIndex]));

  if strTmp = '>' then
    LinkTextEdit.Text := ''
  else
    LinkTextEdit.Text := Copy(strTmp, 1, Length(strTmp) - Length('</link>'));
end;

procedure TLazDocForm.InsertElement(const ElementName: String);
var
  n: TDOMNode;
  child: TDOMNode;
begin
  Exit;

  //preparations being made for adding nodes
  //having to finalize adding comment

  //get first module node
  n := GetModuleNode;

  //TODO: insert element comment (important or not!!)
  child := doc.CreateComment('test');
  n.AppendChild(child);

  child := doc.CreateElement('element');
  TDOMElement(child).SetAttribute('name', ElementName);
  child.AppendChild(doc.CreateElement('short'));
  child.AppendChild(doc.CreateElement('descr'));
  child.AppendChild(doc.CreateElement('errors'));
  child.AppendChild(doc.CreateElement('seealso'));
  child.AppendChild(doc.CreateElement('example'));
  n.AppendChild(child);

  WriteXMLFile(doc, FDocFileName);
end;

function TLazDocForm.NodeByPascalContext(
  const AContext: TPascalHelpContextList): TDOMNode;
var
  Node: TDOMNode;
  ElementName: String;
begin
  Result := Nil;

  if not Assigned(doc) then Exit;

  // get first element node
  ElementName:=GetElementName(AContext);

  if ElementName='' then exit;
  //DebugLn('TLazDocForm.NodeByPascalContext ElementName="',ElementName,'"');

  // search elements for ElementName
  Node:=GetFirstElement;
  while Node<>nil do begin
    if (Node is TDomElement)
    and (CompareText(TDomElement(Node).GetAttribute('name'),ElementName)=0) then
    begin
      break;
    end;
    Node:=Node.NextSibling;
  end;
  
  if Node=nil then begin
    // no element found
    if not Assigned(Node) then begin
      // if there is no node, then fpdoc has not created an element
      InsertElement(ElementName);
      Exit;
    end;
  end;

  {$ifdef dbgLazDoc}
  DebugLn('TLazDocForm.NodeByPascalContext: element node found where name is: ' +
    ElementName);
  {$endif}

  Result := Node;
end;

function TLazDocForm.GetElementName(const AContext: TPascalHelpContextList
  ): string;
var
  Level: Integer;
begin
  // get first element node
  Level:=0;
  Result:='';
  while (Level<AContext.Count) do begin
    case AContext.Items[Level].Descriptor of
    pihcProperty,pihcProcedure,pihcVariable,pihcType,pihcConst:
      begin
        if Result<>'' then Result:=Result+'.';
        Result:=Result+AContext.Items[Level].Context;
      end;
    pihcFilename: ;
    pihcSourceName: ;
    else
      DebugLn('TLazDocForm.NodeByPascalContext unsupported type: "',AContext.Items[Level].Context,'"');
      exit; // unsupported type
    end;
    inc(Level);
  end;
end;

function TLazDocForm.GetFirstChildValue(n: TDOMNode): String;
begin
  if Assigned(n.FirstChild) then
  begin
    {$ifdef dbgLazDoc}
    DebugLn('TLazDocForm.GetFirstChildValue: retrieving node ' +
      n.NodeName + '=' + n.FirstChild.NodeValue);
    {$endif}

    Result := n.FirstChild.NodeValue;
  end
  else
  begin
    {$ifdef dbgLazDoc}
    DebugLn('TLazDocForm.GetFirstChildValue: retrieving empty node ' +
      n.NodeName);
    {$endif}

    Result := '';
  end;
end;

function TLazDocForm.ElementFromNode(Node: TDOMNode): TFPDocNode;
var
  S: String;
begin
  Node := Node.FirstChild;
  while Assigned(Node) do
  begin
    if (Node.NodeType = ELEMENT_NODE) then
    begin
      S := Node.NodeName;

      if S = 'short' then
        Result[SHORT] := GetFirstChildValue(Node);

      if S = 'descr' then
        Result[DESCR] := GetFirstChildValue(Node);

      if S = 'errors' then
        Result[ERRORS] := GetFirstChildValue(Node);

      if S = 'seealso' then
        Result[SEEALSO] := GetFirstChildValue(Node);

      if S = 'example' then
        Result[EXAMPLE] := GetFirstChildValue(Node);
    end;
    Node := Node.NextSibling;
  end;
end;

function TLazDocForm.ExtractFuncProc(const startpos: TPoint;
  const keyword: String; src: tStrings): String;
var
  xpos: Integer;
  ypos: Integer;
begin
  xpos := Succ(startpos.x + length(keyword));
  ypos := startpos.y;

  result := '';
  while (src[ypos][xpos] <> '(') and (src[ypos][xpos] <> ';') and
    (src[ypos][xpos] <> ':') do
  begin
    Result := Result + src[ypos][xpos];
    Inc(xpos);
    if xpos > length(src[ypos]) then
    begin
      xpos := 0;
      Inc(ypos);
    end;
  end;
end;

function TLazDocForm.GetNearestSourceElement(const SrcFilename: string;
  const CaretPos: TPoint): TPascalHelpContextList;
begin
  Result:=LazarusHelp.ConvertSourcePosToPascalHelpContext(CaretPos,SrcFilename);

  //if Result<>nil then
  //  DebugLn('TLazDocForm.GetNearestSourceElement Result=',Result.AsString);
end;

procedure TLazDocForm.UpdateCaption;
var
  strCaption: String;
begin
  strCaption := lisLazDocMainFormCaption + ' - ';

  if FCurrentElement <> nil then
    strCaption := strCaption + GetElementName(FCurrentElement) + ' - '
  else
    strCaption := strCaption + lisLazDocNoTagCaption + ' - ';

  if FDocFileName <> '' then
    Caption := strCaption + FDocFileName
  else
    Caption := strCaption + lisLazDocNoTagCaption;
end;

procedure TLazDocForm.Reset;
begin
  FreeAndNil(Doc);
  FreeAndNil(FCurrentElement);
  FDocFileName    := '';
  UpdateCaption;

  //clear all element editors/viewers
  ShortEdit.Clear;
  DescrMemo.Clear;
  ErrorsMemo.Clear;
  LinkIdComboBox.Text := '';
  LinkTextEdit.Clear;
  LinkListBox.Clear;
  ExampleEdit.Clear;

  FChanged := False;
end;

procedure TLazDocForm.UpdateLazDoc(const SrcFilename: string;
  const Caret: TPoint);
var
  dn: TFPDocNode;
  n:  TDOMNode;
  EnabledState: Boolean;
  NewElement: TPascalHelpContextList;
begin
  if not Assigned(doc) then
  begin
    {$ifdef dbgLazDoc}
    DebugLn('TLazDocForm.UpdateLazDoc: document is not set');
    {$endif}

    Exit;
  end;

  // save the current changes to documentation
  Save;

  NewElement:=GetNearestSourceElement(SrcFilename, Caret);
  // avoid circles and overhead
  if (NewElement<>nil) and (FCurrentElement<>nil)
  and (NewElement.IsEqual(FCurrentElement)) then begin
    NewElement.Free;
    exit;
  end;

  FCurrentElement := NewElement;

  UpdateCaption;

  n := NodeByPascalContext(FCurrentElement);

  EnabledState := Assigned(n);

  if Assigned(n) then
  begin
    dn := ElementFromNode(n);

    ShortEdit.Text := dn[SHORT];
    DescrMemo.Lines.Text := ConvertLineEndings(dn[DESCR]);
    ErrorsMemo.Lines.Text := ConvertLineEndings(dn[ERRORS]);
    LinkListBox.Items.Text := ConvertLineEndings(dn[SEEALSO]);
    LinkIdComboBox.Text := '';
    LinkTextEdit.Clear;
    ExampleEdit.Text := Copy(dn[EXAMPLE], 16, Length(dn[EXAMPLE]) - 19);
  end
  else
  begin
    ShortEdit.Text := lisLazDocNoDocumentation;
    DescrMemo.Lines.Text := lisLazDocNoDocumentation;
    ErrorsMemo.Lines.Text := lisLazDocNoDocumentation;
    LinkIdComboBox.Text := lisLazDocNoDocumentation;
    LinkTextEdit.Text := lisLazDocNoDocumentation;
    LinkListBox.Clear;
    ExampleEdit.Text := lisLazDocNoDocumentation;
  end;

  FChanged := False;

  ShortEdit.Enabled := EnabledState;
  DescrMemo.Enabled := EnabledState;
  ErrorsMemo.Enabled := EnabledState;
  LinkIdComboBox.Enabled := EnabledState;
  LinkTextEdit.Enabled := EnabledState;
  LinkListBox.Enabled := EnabledState;
  AddLinkButton.Enabled := EnabledState;
  DeleteLinkButton.Enabled := EnabledState;
  ExampleEdit.Enabled := EnabledState;
  BrowseExampleButton.Enabled := EnabledState;
end;

function ToUnixLineEnding(const s: String): String;
var
  p: Integer;
begin
  Result:=s;
  p:=1;
  while (p<=length(s)) do begin
    if not (s[p] in [#10,#13]) then begin
      inc(p);
    end else begin
      // line ending
      if (p<length(s)) and (s[p+1] in [#10,#13]) and (s[p]<>s[p+1]) then begin
        // double character line ending
        Result:=copy(Result,1,p-1)+#10+copy(Result,p+2,length(Result));
      end else if s[p]=#13 then begin
        // single char line ending #13
        Result[p]:=#10;
      end;
      inc(p);
    end;
  end;
end;

procedure TLazDocForm.Save;
var
  Node: TDOMNode;
  S: String;
  NodeWritten: array [1..NODEITEMS] of Boolean;
  i: Integer;

  procedure CheckAndWriteNode(NodeName: String; NodeText: String;
    NodeIndex: Integer);
  var
    child: TDOMNode;
  begin
    {$ifdef dbgLazDoc}
    DebugLn('TLazDocForm.Save[CheckAndWriteNode]: checking element: ' +
      NodeName);
    {$endif}

    if S = NodeName then
    begin
      if not Assigned(Node.FirstChild) then
      begin
        child := doc.CreateTextNode(ToUnixLineEnding(NodeText));
        Node.AppendChild(child);
      end
      else
        Node.FirstChild.NodeValue := ToUnixLineEnding(NodeText);
      NodeWritten[NodeIndex] := True;
    end;
  end;

  procedure InsertNodeElement(ElementName: String; ElementText: String);
  var
    child: TDOMNode;
  begin
    {$ifdef dbgLazDoc}
    DebugLn('TLazDocForm.Save[InsertNodeElement]: inserting element: ' +
      ElementName);
    {$endif}

    child := doc.CreateElement(ElementName);
    child.AppendChild(doc.CreateTextNode(ToUnixLineEnding(ElementText)));
    Node.AppendChild(child);
  end;

begin
  // nothing changed, so exit
  if not FChanged then
    Exit;

  Node := NodeByPascalContext(FCurrentElement);

  if not Assigned(Node) then
    Exit;

  // reset all nodes
  for i := 1 to NODEITEMS do
    NodeWritten[i] := False;

  // write all known nodes to XML
  Node := Node.FirstChild;
  while Assigned(Node) do
  begin
    if (Node.NodeType = ELEMENT_NODE) then
    begin
      S := Node.NodeName;

      CheckAndWriteNode('short', ShortEdit.Text, SHORT);
      CheckAndWriteNode('descr', DescrMemo.Text, DESCR);
      CheckAndWriteNode('errors', ErrorsMemo.Text, ERRORS);
      CheckAndWriteNode('seealso', LinkListBox.Text, SEEALSO);
      CheckAndWriteNode('example', '<example file="' +
        ExampleEdit.Text + '"/>', EXAMPLE);
    end;
    Node := Node.NextSibling;
  end;

  // add new nodes to XML if not already updated
  Node := NodeByPascalContext(FCurrentElement);
  for i := 1 to NODEITEMS do
    if NodeWritten[i] = False then
      case i of
        SHORT:
          InsertNodeElement('short', ShortEdit.Text);
        DESCR:
          InsertNodeElement('descr', DescrMemo.Text);
        ERRORS:
          InsertNodeElement('errors', ErrorsMemo.Text);
        SEEALSO:
          InsertNodeElement('seealso', LinkListBox.Text);
        EXAMPLE:
          InsertNodeElement('example', '<example file="' +
            ExampleEdit.Text + '"/>');
      end;

  WriteXMLFile(doc, FDocFileName);

  FChanged := False;
end;

procedure TLazDocForm.DocumentationTagChange(Sender: TObject);
begin
  FChanged := True;
end;

function TLazDocForm.MakeLink: String;
begin
  if Trim(LinkTextEdit.Text) = '' then
    Result := '<link id="' + Trim(LinkIdComboBox.Text) + '"/>'
  else
    Result := '<link id="' + Trim(LinkIdComboBox.Text) + '">' +
      LinkTextEdit.Text + '</link>';
end;

procedure TLazDocForm.AddLinkButtonClick(Sender: TObject);
begin
  FLinkIndex := -1;

  if Trim(LinkIdComboBox.Text) <> '' then
  begin
    LinkListBox.Items.Add(MakeLink);

    FChanged := True;
  end;
end;

procedure TLazDocForm.BrowseExampleButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    ExampleEdit.Text := SetDirSeparators(ExtractRelativepath(
      ExtractFilePath(FDocFileName), OpenDialog.FileName));
end;

procedure TLazDocForm.DeleteLinkButtonClick(Sender: TObject);
begin
  if FLinkIndex >= 0 then
  begin
    LinkListBox.Items.Delete(FLinkIndex);

    FLinkIndex := -1;

    FChanged := True;
  end;
end;

initialization
  {$I lazdocfrm.lrs}

end.
