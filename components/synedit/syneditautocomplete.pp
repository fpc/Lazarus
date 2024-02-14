{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditAutoComplete.pas, released 2000-06-25.

The Initial Author of the Original Code is Michael Hieke.
Portions written by Michael Hieke are Copyright 2000 Michael Hieke.
Portions written by Cyrille de Brebisson (from mwCompletionProposal.pas) are
Copyright 1999 Cyrille de Brebisson.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditAutoComplete;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, StrUtils,
  LCLIntf, LCLType, Controls,
  LazStringUtils, LazUTF8,
  SynEdit, SynEditKeyCmds, SynEditPlugins;

const
  CodeTemplateMacroMagic = '$(EnableMakros)';
  CodeTemplateEnableMacros = 'EnableMakros';
  CodeTemplateKeepSubIndent = 'KeepSubIndent';
  CodeTemplateAttributesStartMagic = '$(AttributesStart)';
  CodeTemplateAttributesEndMagic = '$(AttributesEnd)';

type
  TCustomSynAutoComplete = class;
  
  TOnTokenNotFound = procedure(Sender: TObject; AToken: string; 
                           AEditor: TCustomSynEdit; var Index:integer) of object;
  TOnExecuteCompletion = procedure(ASynAutoComplete: TCustomSynAutoComplete;
                                   Index: integer) of object;

  // Data for a single template item.

  { TCodeTemplate }

  TCodeTemplate = class
  private    // The Key is Stored in a TCodeTemplateList string item.
    fValue: string;
    fComment: string;
    fAttributes: TStringList;  // List of attributes.
  public
    constructor Create;
    constructor Create(aTemplate: TCodeTemplate);
    constructor Create(aValue, aComment: string);
    destructor Destroy; override;
    procedure SetBooleanAttribute(aName: string; aValue: Boolean);
    procedure SetValueWithoutLastEOL(aValue: string);
    property Value: string read fValue write fValue;
    property Comment: string read fComment write fComment;
    property Attributes: TStringList read fAttributes;
  end;

  { TCodeTemplateList }

  TCodeTemplateList = class(TStringListUTF8Fast)
  private
    function GetTemplate(Index: Integer): TCodeTemplate;
    procedure SetTemplate(Index: Integer; AValue: TCodeTemplate);
  public
    property Objects[Index: Integer]: TCodeTemplate read GetTemplate write SetTemplate;
  end;

  { TCustomSynAutoComplete }

  TCustomSynAutoComplete = class(TLazSynMultiEditPlugin)
  private
    fOnTokenNotFound: TOnTokenNotFound;
    fIndentToTokenStart: boolean;
    FOnExecuteCompletion: TOnExecuteCompletion;
  protected
    fCodeTemplSource: TStrings;
    fCodeTemplates: TCodeTemplateList;
    fEditor: TCustomSynEdit;
    fEditors: TList;
    fEOTokenChars: string;
    fCaseSensitive: boolean;
    fParsed: boolean;
    procedure CompletionListChanged(Sender: TObject);
    function GetCodeTemplates: TCodeTemplateList;
    procedure ParseCompletionList; virtual;
    procedure SetCodeTemplSource(Value: TStrings); virtual;
    procedure SynEditCommandHandler(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand;
      var AChar: TUTF8Char;
      Data: pointer; HandlerData: pointer);
    procedure DoEditorAdded(AValue: TCustomSynEdit); override;
    procedure DoEditorRemoving(AValue: TCustomSynEdit); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddCompletion(AToken: string; ATemplate: TCodeTemplate);
    procedure AddCompletion(AToken, AValue, AComment: string);
    procedure Execute(AEditor: TCustomSynEdit); virtual;
    procedure ExecuteCompletion(AToken: string; AEditor: TCustomSynEdit); virtual;
  public
    property CodeTemplSource: TStrings read fCodeTemplSource write SetCodeTemplSource;
    property CaseSensitive: boolean read fCaseSensitive write fCaseSensitive;
    property CodeTemplates: TCodeTemplateList read GetCodeTemplates;
    property EndOfTokenChr: string read fEOTokenChars write fEOTokenChars;
    property OnTokenNotFound: TOnTokenNotFound read fOnTokenNotFound write fOnTokenNotFound;
    property IndentToTokenStart: boolean read fIndentToTokenStart write fIndentToTokenStart;
    property OnExecuteCompletion: TOnExecuteCompletion read FOnExecuteCompletion
                                                     write FOnExecuteCompletion;
  end;

  { TSynEditAutoComplete }

  TSynEditAutoComplete = class(TCustomSynAutoComplete)
  published
    property CodeTemplSource;
    property CaseSensitive;
    property Editor;
    property EndOfTokenChr;
    property OnTokenNotFound;
    property IndentToTokenStart;
    property OnExecuteCompletion;
  end;

implementation

{ TCodeTemplate }

constructor TCodeTemplate.Create;
begin
  inherited Create;
  fAttributes := TStringListUTF8Fast.Create;
  fAttributes.UseLocale := False;
  fAttributes.OwnsObjects := True;
end;

constructor TCodeTemplate.Create(aTemplate: TCodeTemplate);
begin
  Create;
  fValue := aTemplate.Value;
  fComment := aTemplate.Comment;
  fAttributes.Assign(aTemplate.Attributes);
end;

constructor TCodeTemplate.Create(aValue, aComment: string);
begin
  Create;
  fValue := aValue;
  fComment := aComment;
end;

destructor TCodeTemplate.Destroy;
begin
  fAttributes.Free;
  inherited Destroy;
end;

procedure TCodeTemplate.SetBooleanAttribute(aName: string; aValue: Boolean);
var
  i: Integer;
begin
  if aValue then
    fAttributes.Values[aName] := 'true'
  else begin
    i := fAttributes.IndexOfName(aName);
    if i >= 0 then
      fAttributes.Delete(i);
  end;
end;

procedure TCodeTemplate.SetValueWithoutLastEOL(aValue: string);
var
  l: Integer;
begin                        // Remove last EOL from Value.
  if aValue <> '' then begin
    l := Length(aValue);
    if aValue[l] in [#10,#13] then begin
      Dec(l);
      if (l > 0) and (aValue[l] in [#10,#13])
      and (aValue[l] <> aValue[l+1]) then
        Dec(l);
      SetLength(aValue, l);
    end;
  end;
  fValue := aValue;
end;

{ TCodeTemplateList }

function TCodeTemplateList.GetTemplate(Index: Integer): TCodeTemplate;
begin
  Result := TCodeTemplate(inherited GetObject(Index));
end;

procedure TCodeTemplateList.SetTemplate(Index: Integer; AValue: TCodeTemplate);
begin
  inherited PutObject(Index, AValue);
end;

{ TCustomSynAutoComplete }

procedure TCustomSynAutoComplete.AddCompletion(AToken: string; ATemplate: TCodeTemplate);
var
  NewTemplate: TCodeTemplate;
begin
  if aToken = '' then Exit;
  if not fParsed then
    ParseCompletionList;
  NewTemplate := TCodeTemplate.Create(ATemplate);
  fCodeTemplates.AddObject(aToken, NewTemplate);
end;

procedure TCustomSynAutoComplete.AddCompletion(AToken, AValue, AComment: string);
var
  NewTemplate: TCodeTemplate;
begin
  if aToken = '' then Exit;
  if not fParsed then
    ParseCompletionList;
  NewTemplate := TCodeTemplate.Create(AValue, AComment);
  fCodeTemplates.AddObject(aToken, NewTemplate);
end;

procedure TCustomSynAutoComplete.CompletionListChanged(Sender: TObject);
begin
  fParsed := FALSE;
end;

constructor TCustomSynAutoComplete.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCodeTemplSource := TStringList.Create;
  TStringList(fCodeTemplSource).OnChange := @CompletionListChanged;
  fCodeTemplates := TCodeTemplateList.Create;
  fEditors := TList.Create;
  fEOTokenChars := '()[]{}.';
end;

destructor TCustomSynAutoComplete.Destroy;
begin
  fEditors.Free;
  fCodeTemplates.Free;
  fCodeTemplSource.Free;
  inherited Destroy;
end;

procedure TCustomSynAutoComplete.Execute(AEditor: TCustomSynEdit);
var
  s: string;
  i, j: integer;
begin
  if AEditor <> nil then begin
    // get token
    s := AEditor.LineText;
    j := AEditor.LogicalCaretXY.x;
    i := j - 1;
    if i <= Length(s) then begin
      while (i > 0) and (s[i] > ' ') and (Pos(s[i], fEOTokenChars) = 0) do
        Dec(i);
      Inc(i);
      s := Copy(s, i, j - i);
      ExecuteCompletion(s, AEditor);
    end else begin
      i:=-1;
      if Assigned(OnTokenNotFound) then
        OnTokenNotFound(Self,'',AEditor,i);
      if i>=0 then
        ExecuteCompletion(fCodeTemplates[i], AEditor);
    end;
  end;
end;

procedure TCustomSynAutoComplete.ExecuteCompletion(AToken: string;
  AEditor: TCustomSynEdit);
var
  i, j, Len, IndentLen, TokenStartX: integer;
  s: string;
  IdxMaybe, NumMaybe: integer;
  p, p2: TPoint;
  NewCaretPos: boolean;
  Temp: TStringList;
  Template: TCodeTemplate;
begin
  if not fParsed then
    ParseCompletionList;
  Len := Length(AToken);
  if (Len=0) and Assigned(OnTokenNotFound) then
  begin
    i := 0;
    OnTokenNotFound(Self,AToken,AEditor,i);
  end;
  if (Len > 0) and (AEditor <> nil) and not AEditor.ReadOnly
    and (fCodeTemplates.Count > 0)
  then begin
    // find completion for this token - not all chars necessary if unambiguous
    i := fCodeTemplates.Count - 1;
    IdxMaybe := -1;
    NumMaybe := 0;
    if fCaseSensitive then begin
      while i > -1 do begin
        s := fCodeTemplates[i];
        if s = AToken then
          break
        else if LazStartsStr(AToken, s) then begin
          Inc(NumMaybe);
          IdxMaybe := i;
        end;
        Dec(i);
      end;
    end else begin
      while i > -1 do begin
        s := fCodeTemplates[i];
        if AnsiCompareText(s, AToken) = 0 then
          break
        else if AnsiCompareText(Copy(s, 1, Len), AToken) = 0 then begin
          Inc(NumMaybe);
          IdxMaybe := i;
        end;
        Dec(i);
      end;
    end;
    if (i = -1) and (NumMaybe = 1) then
      i := IdxMaybe;
    if (i < 0) and Assigned(fOnTokenNotFound) then
      fOnTokenNotFound(Self,AToken,AEditor,i);
    if i > -1 then begin
      // select token in editor
      p := AEditor.LogicalCaretXY;
      if Assigned(OnExecuteCompletion) then
        OnExecuteCompletion(Self,i)
      else begin
        AEditor.BeginUpdate;
        try
          TokenStartX:=p.x;
          s:=AEditor.Lines[p.y-1];
          if TokenStartX>length(s) then TokenStartX:=length(s);
          while (TokenStartX > 1) and (s[TokenStartX-1] > ' ')
          and (Pos(s[TokenStartX-1], fEOTokenChars) = 0) do
            Dec(TokenStartX);
          AEditor.BlockBegin := Point(TokenStartX, p.y);
          AEditor.BlockEnd := p;
          // indent the completion string if necessary, determine the caret pos
          if IndentToTokenStart then begin
            IndentLen := p.x - Len - 1;
          end else begin
            // indent the same as the first line
            IndentLen:=1;
            if (p.y>0) and (p.y<=AEditor.Lines.Count) then begin
              s:=AEditor.Lines[p.y-1];
              while (IndentLen<p.x)
              and ((IndentLen>length(s)) or (s[IndentLen]<=' ')) do
                inc(IndentLen);
            end;
            dec(IndentLen);
          end;
          p := AEditor.BlockBegin;
          NewCaretPos := FALSE;
          Temp := TStringList.Create;
          try
            Template := fCodeTemplates.Objects[i];
            s:=Template.fValue;
            Temp.Text := s;
            if (s<>'') and (s[length(s)] in [#10,#13]) then
              Temp.Add('');

            // indent lines
            if (IndentLen > 0) and (Temp.Count > 1) then
            begin
              s := StringOfChar(' ', IndentLen);
              for i := 1 to Temp.Count - 1 do
                Temp[i] := s + Temp[i];
            end;
            // find first '|' and use it as caret position
            for i := 0 to Temp.Count - 1 do
            begin
              s := Temp[i];
              j := Pos('|', s);
              if j > 0 then
              begin
                Delete(s, j, 1);
                Temp[i] := s;
  //              if j > 1 then
  //                Dec(j);
                NewCaretPos := TRUE;
                Inc(p.y, i);
                if i = 0 then
  //                Inc(p.x, j)
                  Inc(p.x, j - 1)
                else
                  p.x := j;
                break;
              end;
            end;
            s := Temp.Text;
            // strip the trailing #13#10 that was appended by the stringlist
            i := Length(s);
            if (i>=1) and (s[i] in [#10,#13]) then begin
              dec(i);
              if (i>=1) and (s[i] in [#10,#13]) and (s[i]<>s[i+1]) then
                dec(i);
              SetLength(s, i);
            end;
          finally
            Temp.Free;
          end;
          // replace the selected text and position the caret
          p2 := AEditor.BlockBegin;
          AEditor.SelText := '';
          AEditor.SetTextBetweenPoints(p2, p2, s, [], scamEnd);
          if NewCaretPos then
            AEditor.LogicalCaretXY := p;
          AEditor.EnsureCursorPosVisible;
        finally
          AEditor.EndUpdate;
        end;
      end;
    end;
  end;
end;

function TCustomSynAutoComplete.GetCodeTemplates: TCodeTemplateList;
begin
  if not fParsed then
    ParseCompletionList;
  Result := fCodeTemplates;
end;

procedure TCustomSynAutoComplete.ParseCompletionList;
var
  sKey, sValue, sComment: string;

  procedure SaveEntry;
  var
    StartI, EndI: Integer;
    Template: TCodeTemplate;
  begin
    Template := TCodeTemplate.Create;
    Assert(not LazStartsStr(CodeTemplateMacroMagic, sValue), 'SaveEntry: Found '+CodeTemplateMacroMagic);
    if LazStartsStr(CodeTemplateAttributesStartMagic, sValue) then
    begin
      // Start of attributes
      StartI := Length(CodeTemplateAttributesStartMagic) + 1;
      while (StartI <= Length(sValue)) and (sValue[StartI] in [#10,#13]) do
        Inc(StartI);
      EndI := PosEx(CodeTemplateAttributesEndMagic, sValue, StartI);
      if EndI = 0 then
        raise Exception.Create('TCustomSynAutoComplete.ParseCompletionList: "'
                              + CodeTemplateAttributesEndMagic + '" not found.');
      Template.Attributes.Text := Copy(sValue, StartI, EndI-StartI);
      // Start of value
      StartI := EndI + Length(CodeTemplateAttributesEndMagic);
      while (StartI <= Length(sValue)) and (sValue[StartI] in [#10,#13]) do
        Inc(StartI);
      Template.Value := Copy(sValue, StartI, Length(sValue));
    end;
    Template.Comment := sComment;
    fCodeTemplates.AddObject(sKey, Template);
    sKey := '';
    sValue := '';
    sComment := '';
  end;

var
  BorlandDCI: boolean;
  i, j, Len: integer;
  S: string;
  TemplateStarted: Boolean;
begin
  fCodeTemplates.Clear;
  sKey := '';
  sValue := '';
  sComment := '';
  if fCodeTemplSource.Count > 0 then begin
    S := fCodeTemplSource[0];
    BorlandDCI := (S <> '') and (S[1] = '[');
    TemplateStarted:=false;
    for i := 0 to fCodeTemplSource.Count - 1 do begin
      S := fCodeTemplSource[i];
      Len := Length(S);
      if BorlandDCI then begin
        // the style of the Delphi32.dci file
        if (Len > 0) and (S[1] = '[') then begin
          // save last entry
          if sKey <> '' then
            SaveEntry;
          // new completion entry
          j := 2;
          while (j <= Len) and (S[j] > ' ') do
            Inc(j);
          sKey := Copy(S, 2, j - 2);
          // start of comment in DCI file
          while (j <= Len) and (S[j] <= ' ') do
            Inc(j);
          if (j <= Len) and (S[j] = '|') then
            Inc(j);
          while (j <= Len) and (S[j] <= ' ') do
            Inc(j);
          sComment := Copy(S, j, Len);
          if sComment[Length(sComment)] = ']' then
            SetLength(sComment, Length(sComment) - 1);
          TemplateStarted:=true;
        end else begin
          if not TemplateStarted then
            sValue := sValue + LineEnding;
          TemplateStarted:=false;
          sValue := sValue + S;
        end;
      end else begin
        // the original style
        if (Len > 0) and (S[1] <> '=') then begin
          // save last entry
          if sKey <> '' then
            SaveEntry;
          // new completion entry
          sKey := S;
          TemplateStarted:=true;
        end else if (Len > 0) and (S[1] = '=') then begin
          if not TemplateStarted then
            sValue := sValue + LineEnding;
          TemplateStarted:=false;
          sValue := sValue + Copy(S, 2, Len);
        end;
      end;
    end;
    if sKey <> '' then
      SaveEntry;
  end;
  fCodeTemplates.Sort;
  fParsed:=true;
end;

procedure TCustomSynAutoComplete.SetCodeTemplSource(Value: TStrings);
begin
  fCodeTemplSource.Assign(Value);
  fParsed := FALSE;
end;

procedure TCustomSynAutoComplete.SynEditCommandHandler(Sender: TObject;
  AfterProcessing: boolean; var Handled: boolean;
  var Command: TSynEditorCommand;
  var AChar: TUTF8Char; Data: pointer;
  HandlerData: pointer);
begin
  if not AfterProcessing and not Handled and (Command = ecAutoCompletion) then
  begin
    Handled := TRUE;
    Execute(Sender as TCustomSynEdit);
  end;
end;

procedure TCustomSynAutoComplete.DoEditorAdded(AValue: TCustomSynEdit);
begin
  inherited DoEditorAdded(AValue);
  AValue.RegisterCommandHandler(@SynEditCommandHandler, nil);
end;

procedure TCustomSynAutoComplete.DoEditorRemoving(AValue: TCustomSynEdit);
begin
  inherited DoEditorRemoving(AValue);
  AValue.UnregisterCommandHandler(@SynEditCommandHandler);
end;

end.

