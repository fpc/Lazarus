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

  Author: Donald Ziesig

  Abstract

  TTemplateExpander provides xml templates to replace the hard-coded
  pascal snippets used by TCodeCompletionCodeTool.

  The xml file fragment for the Setter method:

  <templates>
     *
     *
     *
  <template name="SetterMethod">
  procedure <ClassName>.<AccessParam/>(<PropVarName/>: <PropType/>);<br/>
  begin
  <indent/>if <VarName/>=<PropVarName/> then Exit;<br/>
  <indent/><VarName/>:=<PropVarName/>;<br/>
  end;<br/>
  </template>
     *
     *
     *
  </templates>

  produces pascal:

  procedure TMyClass.SetMyVar(AValue: MyType);
  begin
    if AValue=MyVar then Exit;
    MyVar:=AValue;
  end;

===============================================================================

  The following xml tags are implemented:

  <if var="SomeBool"> ... </if>      generates pascal code if string value of
                                     the argument named "SomeBool" is "true".

  <ifnot var="SomeBool" ... </ifnot> generates pascal code if string value of
                                     the argument named "SomeBool" is not "true"

  <else> ... </else>                 must immediately follow </if> or </ifnot>.
                                     generates pascal code if the negation of
                                     the previous tag is true.

  <count var="SomeVar"> ... </count> generates pascal code for zero or more
                                     values of the string argument "SomeVar".
                                     The values are encoded as a single string
                                     of the form "Arg0?Arg1?...ArgN?" (Yeah, I
                                     know.  See the comments below about a hack.)

  <indent/>                          is replaced by the appropriate Indent string.

  <br/>                              is replaced by the appropriate LineEnd string.

  <SomeVar/>                         is replaced by the string element of ArgVal
                                     which corresponds to the string element of
                                     ArgName.

}
unit CodeCompletionTemplater;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, laz2_DOM, laz2_XMLRead, LazFileUtils,
  CodeCache, FileProcs;

type

  { TTemplateExpander }

  TTemplateExpander = class
  private
    FCode: TCodeBuffer;
    fCodeChangeStep: Integer;
    procedure LoadCode;
    procedure SetCode(AValue: TCodeBuffer);
  protected
    XMLDoc : TXMLDocument;
    Root   : TDOMNode;

    function ArgCount(Args: String): Integer;
    function ArgN(Args: String; Index : Integer) : String;

    function ExpandTemplate(Template: TDOMNode;
                            LineEnd, Indent : String;
                            ArgName: array of String;
                            ArgVal: array of const;
                            CountIndex: Integer = -1): String;

    function FindTemplate(TemplateName: String): TDOMNode;
  public
    constructor Create;
    destructor  Destroy; override;

    function Expand(TemplateName : String;
                    LineEnd, Indent : String;
                    ArgName : array of String;
                    ArgVal  : array of const): String;

    function TemplateExists(TemplateName: String): Boolean;

    property Doc: TXMLDocument read XMLDoc;
    property Code: TCodeBuffer read FCode write SetCode;
    procedure ReloadCode;
  end;

var
  CTTemplateExpander : TTemplateExpander; // will be set by CodeToolBoss

implementation

{ TTemplateExpander }

//
// ArgCount and ArgN are part of a hack to overcome the fact that
// "array of const" can not contain another array.
//
// instead of a convenient ['a',...,['x1,'x2'],...,'z'] we must use something
// like ['a',...,'x1?x2?',...'z'] (which is what I chose just for simplicity)
//

function TTemplateExpander.ArgCount(Args: String): Integer;
var
  I : Integer;
begin
  Result := 0;
  for I := 1 to Length(Args) do
    if Args[I] = '?' then
      Inc(Result);
end;

function TTemplateExpander.ArgN(Args: String; Index: Integer): String;
var
  I : Integer;
  P : Integer;
  S : String;
begin
  S := Args;
  for I := 0 to pred(Index) do
    begin
      P := Pos('?',S);
      S := Copy(S,P+1,65535);
    end;
  P := Pos('?',S);
  Result := Copy(S,1,P-1);
end;

constructor TTemplateExpander.Create;
begin
  fCodeChangeStep:=CTInvalidChangeStamp;
end;

destructor TTemplateExpander.Destroy;
begin
  FCode:=nil;
  FreeAndNil(XMLDoc);
end;

function TTemplateExpander.Expand(TemplateName: String; LineEnd,
  Indent: String; ArgName: array of String; ArgVal: array of const): String;
var
  Template : TDOMNode;
begin
  Template := FindTemplate(TemplateName);
  if Template = nil then
    raise Exception.Create('Template "' + TemplateName + '" not found in TemplateExpander.');
  Result := ExpandTemplate(Template, LineEnd, Indent, ArgName, ArgVal);
end;

function TTemplateExpander.ExpandTemplate(Template: TDOMNode; LineEnd,
  Indent: String; ArgName: array of String; ArgVal: array of const;
  CountIndex : Integer): String;

// Sequential search of ArgName array to return corresponding element
// of the ArgVal array (appropriately processed if it simulates being
// an array itself.
function GetArgValue(Name : String; Index : Integer = -1): String;
var
  I : Integer;
  S : String;
begin
  for I := 0 to pred(Length(ArgName)) do
    if ArgName[I] = Name then
      begin
        S := AnsiString(ArgVal[I].VAnsiString);
        if (Index < 0) or (Pos('?',S) = 0) then
          Result := S
        else
          Result :=  ArgN(S, Index);
        exit;
      end;
  raise Exception.Create('ExpandTemplate could not find Argument named "' + Name + '"');
end;

function GetBoolArgValue(Name : String): Boolean;
var
  I : Integer;
begin
  for I := 0 to pred(Length(ArgName)) do
    if ArgName[I] = Name then
      begin
        Result :=  ArgVal[I].VBoolean;
        exit;
      end;
  raise Exception.Create('ExpandTemplate could not find Argument named "' + Name + '"');
end;

function GetNodeValue(Node : TDOMNode; Required : Boolean = True): String;
var
  Len : Integer;
begin
  Result := '';
  Len := Node.Attributes.Length;
  if Required then
    if Len = 0 then
      raise Exception.Create('Missing attribute tag for node "' + Node.NodeName + '"');
  if Len > 0 then
    Result := Node.Attributes.Item[0].NodeValue;
end;

var
  Node : TDOMNode;
  N : String;
  S : String;
  Name : String;
  R : String; // for debugger
  PrevNode : TDOMNode;
  CommentFlag : Boolean;
  CountArgs : String;
  NArgs : Integer;
  I     : Integer;
begin
  R := '';
  PrevNode := nil;
  Node := Template.FirstChild;
  while Node <> nil do
    begin
      N := Node.NodeName;
      S := Node.NodeValue;
      CommentFlag := False;
// plain text in the xml file is copied directly to the output (almost).
      if N = '#text' then
        begin
          if Pos(#10, S) = 1 then  // Hack to work around XML parser that leaves
            S := Copy(S,2,65535);  // A new-line when text appears in first
          R := R + S;              // column of the XML file.
        end
// indent the text using the string argument Indent
      else if N = 'indent' then
        begin
          Name := GetNodeValue(Node, False);
          if Name = '' then
            R := R + Indent
          else
            R := R + GetArgValue(Name);
        end
// add the line break using the string argument LineEnd
      else if N = 'br' then
         R := R + LineEnd
// process the xml 'if' tag
      else if (N = 'if') then
        begin
          Name := GetNodeValue(Node); //Node.Attributes.Item[0].NodeValue;
          if GetBoolArgValue(Name) then
            R := R + ExpandTemplate(Node, LineEnd,Indent,ArgName,ArgVal);
        end
// process the xml 'ifnot' tag
      else if (N = 'ifnot') then
        begin
          Name := GetNodeValue(Node); //Node.Attributes.Item[0].NodeValue;
          if not GetBoolArgValue(Name) then
            R := R + ExpandTemplate(Node, LineEnd,Indent,ArgName,ArgVal);
        end
// process the xml 'else' tag.  This is sneaky.  The else tag must (almost)
// immediately follow the closing of either the 'if' or 'ifnot' tags.  (The
// exception allows comments to intervene)
//
// The original implementation used separate 'else' and 'elsenot' tags, but
// the xml file got so confusing at times that it was better to add some
// nasty looking code here to make the xml neater.
      else if N = 'else' then
        begin
          if PrevNode = nil then
            raise Exception.Create('Expander: "else" without "if" or "ifnot"');
          if PrevNode.NodeName = 'if' then
            begin
              Name := GetNodeValue(PrevNode); //PrevNode.Attributes.Item[0].NodeValue;
              if GetBoolArgValue(Name) then
                R := R + ExpandTemplate(Node, LineEnd,Indent,ArgName,ArgVal);
            end
          else if PrevNode.NodeName = 'ifnot' then
            begin
              Name := GetNodeValue(PrevNode); //PrevNode.Attributes.Item[0].NodeValue;
              if not GetBoolArgValue(Name) then
                R := R + ExpandTemplate(Node, LineEnd,Indent,ArgName,ArgVal);
            end
          else
            raise Exception.Create('Expander:  mis-placed "else" following ' + PrevNode.NodeName);

         end
// process the xml 'count' tag.  This implements multiple lines to be generated
// from array or list data in the pascal code.  This was originally needed to
// implement the 'AssignMethod' template.
      else if N = 'count' then
        begin
          Name := GetNodeValue(Node); //Node.Attributes.Item[0].NodeValue;
          CountArgs := GetArgValue(Name);
          NArgs := ArgCount(CountArgs);
          for I := 0 to pred(Nargs) do
            R := R + ExpandTemplate(Node, LineEnd,Indent,ArgName,ArgVal, I);
        end
// process all other xml tags (less comment) as requests for the pascal variable
// specified by the tag name:  e.g., <ClassName/>  will look for an argument name
// of ClassName in the ArgNames array and get the corresponding value from the
// ArgVals array;
      else if N <> '#comment' then
        R := R +  GetArgValue(N, CountIndex)
{$IFDEF DebugTemplate }
      else
        begin
          R := R + '{ ' + Node.NodeValue + ' }';
          CommentFlag := True;
        end;
{$ELSE DebugTemplate}
      else
        CommentFlag := True;
{$ENDIF DebugTemplate}
// ignore the comment nodes in subsequent processing.
      if not CommentFlag then PrevNode := Node;
      Node := Node.NextSibling;
    end;
  Result := R;
end;

function TTemplateExpander.FindTemplate(TemplateName : String): TDOMNode;
var
  N : String;
begin
  if not Assigned(Root) then
    begin
      Result := nil;
      exit;
    end;
  if Root.NodeName <> 'templates' then
    raise Exception.Create('Root node of codetools TemplateExpander = "' + Root.NodeName + '", "templates" expected.');

// Sequential search of list of templates.
  Result := Root.FirstChild;
  while Result <> nil do
    begin
      N := Result.NodeName;
      if N <> '#comment' then  // ignores first level comments
        begin
          if N <> 'template' then
            raise Exception.Create('template node of codetools TemplateExpander = "' + N + '", "template" expected.');
          if Result.Attributes.Item[0].NodeValue = TemplateName then
            break;
        end;
      Result := Result.NextSibling;
    end;
end;

procedure TTemplateExpander.LoadCode;
var
  ms: TMemoryStream;
begin
  if Code=nil then begin
    fCodeChangeStep:=CTInvalidChangeStamp;
    exit;
  end;
  fCodeChangeStep:=Code.ChangeStep;
  Root:=nil;
  FreeAndNil(XMLDoc);
  ms:=TMemoryStream.Create;
  try
    Code.SaveToStream(ms);
    ms.Position:=0;
    ReadXMLFile(XMLDoc, ms);
    Root := XMLDoc.DocumentElement;
  finally
    ms.Free;
  end;
end;

procedure TTemplateExpander.SetCode(AValue: TCodeBuffer);
begin
  if FCode=AValue then Exit;
  FCode:=AValue;
  LoadCode;
end;

function TTemplateExpander.TemplateExists(TemplateName: String): Boolean;
begin
  ReloadCode;
  Result := FindTemplate(TemplateName) <> nil;
end;

procedure TTemplateExpander.ReloadCode;
begin
  if Code=nil then exit;
  if Code.ChangeStep=fCodeChangeStep then exit;
  LoadCode;
end;

end.

