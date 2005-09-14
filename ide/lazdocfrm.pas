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
unit LazDocFrm;

{$mode objfpc}{$H+}

{$define dbgLazDoc}

interface

uses
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  Dialogs,
  DOM,
  Forms,
  Graphics,
  LCLProc,
  LResources,
  StdCtrls,
  StrUtils,
  SynEdit,
  SysUtils,
  XMLread,
  XMLwrite;

type
  TFPDocNode = record
    Short: String;
    Descr: String;
    Errors: String;
  end;

  { TLazDocForm }

  TLazDocForm = class(TForm)
    DescrMemo: TMemo;
    ShortEdit: TEdit;
    ErrorsMemo: TMemo;
    PageControl: TPageControl;
    DescrTabSheet: TTabSheet;
    ErrorsTabSheet: TTabSheet;
    ShortTabSheet: TTabSheet;
    procedure DescrMemoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FDocFileName: String;
    FCurrentElement: String;
    FLastElement: String;
    procedure SetDocFileName(Value: String);
    function NodeByName(ElementName: String): TDOMNode;
    function GetFirstChildValue(n: TDOMNode): String;
    function ElementFromNode(Node: TDOMNode): TFPDocNode;
    function ExtractFuncProc(startpos: tpoint; keyword: String;
      src: tStrings): String;
    function GetNearestSourceElement(source: tStrings;
      caretpos: tpoint): String;
    procedure SetCaption;
  public
    { public declarations }
    procedure UpdateLazDoc(source: TStrings; pos: TPoint);
    property DocFileName: String read FDocFileName write SetDocFileName;
  end;

const
  MAINFORMCAPTION = 'LazDoc editor';
  NODOCUMENTATION = 'Documentation entry does not exist';

var
  LazDocForm: TLazDocForm;
  doc: TXMLdocument;

procedure DoShowLazDoc;

implementation

{ TLazDocForm }

procedure DoShowLazDoc;
begin
  if LazDocForm = Nil then
    LazDocForm := TLazDocForm.Create(Nil);

  LazDocForm.Show;
end;

procedure TLazDocForm.SetDocFileName(Value: String);
begin
  if FileExists(Value) and (Value <> FDocFileName) then
  begin

    FDocFileName := Value;

    if not Assigned(doc) then
      doc := TXMLDocument.Create;

    ReadXMLFile(doc, FDocFileName);

    //clear all element editors/viewers
    ShortEdit.Clear;
    DescrMemo.Clear;
    ErrorsMemo.Clear;

    SetCaption;

    {$ifdef dbgLazDoc}
    DebugLn('TLazDocForm.SetDocFileName: document is set: ' + Value);
    {$endif}

  end;
end;

procedure TLazDocForm.FormCreate(Sender: TObject);
begin
  Caption := MAINFORMCAPTION;
  
  with PageControl do
  begin
    Page[0].Caption := 'Short';
    Page[1].Caption := 'Description';
    Page[2].Caption := 'Errors';
    PageIndex := 0;
  end;

  //clear all element editors/viewers
  ShortEdit.Clear;
  DescrMemo.Clear;
  ErrorsMemo.Clear;
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

  //get first node
  n := doc.FindNode('fpdoc-descriptions');

  //proceed to package (could there be more packages in one file??)
  n := n.FirstChild;

  //proceed to module  (could there be more modules in one file??)
  n := n.FirstChild;
  while n.NodeName <> 'module' do
    n := n.NextSibling;

  //proceed to element
  n := n.FirstChild;
  while n.NodeName <> 'element' do
    n := n.NextSibling;

  //search elements for ElementName
  while Assigned(n) and (TDomElement(n)['name'] <> ElementName) do
  begin
    n := n.NextSibling;

    if not Assigned(n) then
      Exit;

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
    DebugLn('TLazDocForm.GetFirstChildValue: retrieving node ' + n.NodeName + '=' + n.FirstChild.NodeValue);
    {$endif}

    Result := n.FirstChild.NodeValue
  end
  else
  begin
    {$ifdef dbgLazDoc}
    DebugLn('TLazDocForm.GetFirstChildValue: retrieving empty node ' + n.NodeName);
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
        Result.Short := GetFirstChildValue(Node);

      if S = 'descr' then
        Result.Descr := GetFirstChildValue(Node);

      if S='errors' then
        Result.Errors := GetFirstChildValue(Node);
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
  while (src[ypos][xpos] <> '(') and (src[ypos][xpos] <> ';') and (src[ypos][xpos] <> ':') do
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
  end;
end;

procedure TLazDocForm.SetCaption;
var
  strCaption: String;
begin
  strCaption := MAINFORMCAPTION + ' - ';

  if FCurrentElement <> '' then
    strCaption := strCaption + FCurrentElement + ' - '
  else
    strCaption := strCaption + '<NONE> - ';

  Caption := strCaption + FDocFileName;
end;

procedure TLazDocForm.UpdateLazDoc(source: TStrings; pos: TPoint);
var
  dn: TFPDocNode;
  n: TDOMNode;
  EnabledState: boolean;
begin
  if not Assigned(doc) then
  begin
    {$ifdef dbgLazDoc}
    DebugLn('TLazDocForm.UpdateLazDoc: document is not set');
    {$endif}

    Exit;
  end;

  FCurrentElement := GetNearestSourceElement(source, pos);

  SetCaption;

  //do not continue if FCurrentElement=FLastElement
  //or FCurrentElement is empty (J. Reyes)
  if (FCurrentElement = FLastElement) or (FCurrentElement = '') then
    Exit;
    
  FLastElement := FCurrentElement;

  n := NodeByName(FCurrentElement);

  EnabledState := Assigned(n);

  ShortEdit.Enabled := True;
  DescrMemo.Enabled := True;
  ErrorsMemo.Enabled := True;

  //detach the update method, because changing the text already
  //commits to XML. Fix later, hack now ;)
  ShortEdit.OnChange := nil;
  DescrMemo.OnChange := nil;
  ErrorsMemo.OnChange := nil;

  if Assigned(n) then
  begin
    dn := ElementFromNode(n);

    ShortEdit.Text := dn.Short;
    DescrMemo.Lines.Text := ConvertLineEndings(dn.Descr);
    ErrorsMemo.Lines.Text := ConvertLineEndings(dn.Errors);
  end
  else
  begin
    ShortEdit.Text := NODOCUMENTATION;
    DescrMemo.Lines.Text := NODOCUMENTATION;
    ErrorsMemo.Lines.Text := NODOCUMENTATION;
  end;
  
  //attach the update method again
  ShortEdit.OnChange := @DescrMemoChange;
  DescrMemo.OnChange := @DescrMemoChange;
  ErrorsMemo.OnChange := @DescrMemoChange;

  ShortEdit.Enabled := EnabledState;
  DescrMemo.Enabled := EnabledState;
  ErrorsMemo.Enabled := EnabledState;
end;

procedure TLazDocForm.DescrMemoChange(Sender: TObject);
var
  n: TDOMNode;
  S: String;
  child: TDOMNode;
  function ToUnixLineEnding(s: string): string;
  begin
    if LineEnding=#10 then
      Result := s
    else
      Result := StringReplace(s, LineEnding, #10, [rfReplaceAll]);
  end;
begin
  n := NodeByName(FCurrentElement);

  if not Assigned(n) then
    Exit;

  n := n.FirstChild;
  while Assigned(n) do
  begin
    if (n.NodeType = ELEMENT_NODE) then
    begin
      S := n.NodeName;

      if S = 'short' then
      begin
        if not Assigned(n.FirstChild) then
        begin
          child := doc.CreateTextNode(ShortEdit.Text);
          n.AppendChild(child);
        end
        else
          n.FirstChild.NodeValue := ShortEdit.Text;
      end;

      if S = 'descr' then
      begin
        if not Assigned(n.FirstChild) then
        begin
          child := doc.CreateTextNode(ToUnixLineEnding(DescrMemo.Lines.Text));
          n.AppendChild(child);
        end
        else
          n.FirstChild.NodeValue := ToUnixLineEnding(DescrMemo.Lines.Text);
      end;

      if S = 'errors' then
      begin
        if not Assigned(n.FirstChild) then
        begin
          child := doc.CreateTextNode(ToUnixLineEnding(ErrorsMemo.Lines.Text));
          n.AppendChild(child);
        end
        else
          n.FirstChild.NodeValue := ToUnixLineEnding(ErrorsMemo.Lines.Text);
      end;

    end;
    n := n.NextSibling;
  end;

  WriteXMLFile(doc, FDocFileName);
end;

initialization
  {$I lazdocfrm.lrs}

end.

