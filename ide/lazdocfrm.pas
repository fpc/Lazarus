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
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  Dialogs,
  DOM,
  ExtCtrls,
  Forms,
  Graphics,
  IDEProcs,
  LazarusIDEStrConsts,
  LCLProc,
  LResources,
  StdCtrls,
  StrUtils,
  SynEdit,
  SysUtils,
  XMLread,
  XMLwrite;

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
    procedure FormResize(Sender: TObject);
    procedure FormatButtonClick(Sender: TObject);
    procedure LinkChange(Sender: TObject);
    procedure LinkListBoxClick(Sender: TObject);
  private
    { private declarations }
    FLinkIndex: Integer;
    FChanged:   Boolean;
    FDocFileName: String;
    FCurrentElement: String;
    FLastElement: String;
    function GetModuleNode: TDOMNode;
    function GetFirstElement: TDOMNode;
    procedure GetElementList;
    function MakeLink: String;
    procedure SetDocFileName(Value: String);
    procedure InsertElement(ElementName: String);
    function NodeByName(ElementName: String): TDOMNode;
    function GetFirstChildValue(n: TDOMNode): String;
    function ElementFromNode(Node: TDOMNode): TFPDocNode;
    function ExtractFuncProc(startpos: tpoint; keyword: String;
      src: tStrings): String;
    function GetNearestSourceElement(source: tStrings;
      caretpos: tpoint): String;
    procedure SetCaption;
    procedure Save;
  public
    { public declarations }
    procedure Reset;
    procedure UpdateLazDoc(source: TStrings; pos: TPoint);
    property DocFileName: String read FDocFileName write SetDocFileName;
  end;

var
  LazDocForm: TLazDocForm;
  doc: TXMLdocument = Nil;
//maybe better to make it a member field of TLazFormDoc

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
  n: TDOMNode;
begin
  //get first module node
  n := GetModuleNode;

  //proceed to element
  n := n.FirstChild;
  while n.NodeName <> 'element' do
    n := n.NextSibling;

  Result := n;
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

procedure TLazDocForm.SetDocFileName(Value: String);
begin
  LinkIdComboBox.Clear;

  if FileExists(Value) and (Value <> FDocFileName) then
  begin
    //reset Self
    Reset;

    FDocFileName := Value;

    if Assigned(doc) then
      FreeAndNil(doc);

    ReadXMLFile(doc, FDocFileName);

    SetCaption;

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

  Resize;
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

procedure TLazDocForm.InsertElement(ElementName: String);
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

function TLazDocForm.NodeByName(ElementName: String): TDOMNode;
var
  n: TDOMNode;
begin
  Result := Nil;

  if not Assigned(doc) then
  begin
    {$ifdef dbgLazDoc}
    DebugLn('TLazDocForm.NodeByName: document is not set');
    {$endif}

    Exit;
  end;

  //get first element node
  n := GetFirstElement;

  //search elements for ElementName
  while Assigned(n) and (TDomElement(n)['name'] <> ElementName) do
  begin
    n := n.NextSibling;

    //no element found
    if not Assigned(n) then
    begin
      InsertElement(ElementName);
      Exit;
    end;

    while n.NodeName = '#comment' do
      n := n.NextSibling;
  end;

  {$ifdef dbgLazDoc}
  DebugLn('TLazDocForm.NodeByName: element node found where name is: ' +
    ElementName);
  {$endif}

  Result := n;
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

function TLazDocForm.ExtractFuncProc(startpos: tpoint;
  keyword: String; src: tStrings): String;
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

function TLazDocForm.GetNearestSourceElement(source: tStrings;
  caretpos: tpoint): String;
var
  xpos: Integer;
  ypos: Integer;
begin
  //find preceding keyword
  xpos := Succ(caretpos.x);
  ypos := caretpos.y;
  while (xpos > 0) or (ypos > 0) do
  begin
    Dec(xpos);

    if xpos < 0 then
    begin
      Dec(ypos);
      xpos := length(source[ypos]);
    end;

    //check for keywords
    if PosEx('procedure', source[ypos], xpos) = 1 then
    begin
      Result := ExtractFuncProc(Point(xpos, ypos), 'procedure', source);
      Exit;
    end;
    if PosEx('function', source[ypos], xpos) = 1 then
    begin
      Result := ExtractFuncProc(Point(xpos, ypos), 'function', source);
      Exit;
    end;
    if PosEx('constructor', source[ypos], xpos) = 1 then
    begin
      Result := ExtractFuncProc(Point(xpos, ypos), 'constructor', source);
      Exit;
    end;
    if PosEx('desctructor', source[ypos], xpos) = 1 then
    begin
      Result := ExtractFuncProc(Point(xpos, ypos), 'desctructor', source);
      Exit;
    end;
  end;
end;

procedure TLazDocForm.SetCaption;
var
  strCaption: String;
begin
  strCaption := lisLazDocMainFormCaption + ' - ';

  if FCurrentElement <> '' then
    strCaption := strCaption + FCurrentElement + ' - '
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
  FCurrentElement := '';
  FDocFileName    := '';
  SetCaption;

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

procedure TLazDocForm.UpdateLazDoc(source: TStrings; pos: TPoint);
var
  dn: TFPDocNode;
  n:  TDOMNode;
  EnabledState: Boolean;
begin
  if not Assigned(doc) then
  begin
    {$ifdef dbgLazDoc}
    DebugLn('TLazDocForm.UpdateLazDoc: document is not set');
    {$endif}

    Exit;
  end;

  //save the current changes to documentation
  Save;

  FCurrentElement := GetNearestSourceElement(source, pos);

  //do not continue if FCurrentElement=FLastElement
  //or FCurrentElement is empty (J. Reyes)
  if (FCurrentElement = FLastElement) or (FCurrentElement = '') then
    Exit;

  SetCaption;

  FLastElement := FCurrentElement;

  n := NodeByName(FCurrentElement);

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
  n: TDOMNode;
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
      if not Assigned(n.FirstChild) then
      begin
        child := doc.CreateTextNode(ToUnixLineEnding(NodeText));
        n.AppendChild(child);
      end
      else
        n.FirstChild.NodeValue := ToUnixLineEnding(NodeText);
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
    n.AppendChild(child);
  end;

begin
  //nothing changed, so exit
  if not FChanged then
    Exit;

  n := NodeByName(FCurrentElement);

  if not Assigned(n) then
    Exit;

  //reset all nodes
  for i := 1 to NODEITEMS do
    NodeWritten[i] := False;

  //write all known nodes to XML
  n := n.FirstChild;
  while Assigned(n) do
  begin
    if (n.NodeType = ELEMENT_NODE) then
    begin
      S := n.NodeName;

      CheckAndWriteNode('short', ShortEdit.Text, SHORT);
      CheckAndWriteNode('descr', DescrMemo.Text, DESCR);
      CheckAndWriteNode('errors', ErrorsMemo.Text, ERRORS);
      CheckAndWriteNode('seealso', LinkListBox.Text, SEEALSO);
      CheckAndWriteNode('example', '<example file="' +
        ExampleEdit.Text + '"/>', EXAMPLE);
    end;
    n := n.NextSibling;
  end;

  //add new nodes to XML if not already updated
  n := NodeByName(FCurrentElement);
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

finalization
  FreeAndNil(doc)

end.
