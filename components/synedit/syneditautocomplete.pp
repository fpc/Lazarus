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
  LCLIntf, LCLType, LCLProc,
  Classes, SynEdit, SynEditKeyCmds, SynEditPlugins,
  Controls;

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

  { TCustomSynAutoComplete }

  TCustomSynAutoComplete = class(TLazSynMultiEditPlugin)
  private
    fOnTokenNotFound: TOnTokenNotFound;
    fIndentToTokenStart: boolean;
    FOnExecuteCompletion: TOnExecuteCompletion;
    fAttributes: TFPList;// list of TStrings
    function GetCompletionAttributes(Index: integer): TStrings;
    procedure ClearAttributes;
  protected
    fAutoCompleteList: TStrings;
    fCompletions: TStrings;
    fCompletionComments: TStrings;
    fCompletionValues: TStrings;
    fEditor: TCustomSynEdit;
    fEditors: TList;
    fEOTokenChars: string;
    fCaseSensitive: boolean;
    fParsed: boolean;
    procedure CompletionListChanged(Sender: TObject);
    function GetCompletions: TStrings;
    function GetCompletionComments: TStrings;
    function GetCompletionValues: TStrings;
    procedure ParseCompletionList; virtual;
    procedure SetAutoCompleteList(Value: TStrings); virtual;
    procedure SynEditCommandHandler(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand;
      var AChar: TUTF8Char;
      Data: pointer; HandlerData: pointer);
    procedure DoEditorAdded(AValue: TCustomSynEdit); override;
    procedure DoEditorRemoving(AValue: TCustomSynEdit); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddCompletion(AToken, AValue, AComment: string;
                            TheAttributes: TStrings = nil);
    procedure DeleteCompletion(Index: integer);
    procedure Execute(AEditor: TCustomSynEdit); virtual;
    procedure ExecuteCompletion(AToken: string; AEditor: TCustomSynEdit);
      virtual;
  public
    property AutoCompleteList: TStrings read fAutoCompleteList
      write SetAutoCompleteList;
    property CaseSensitive: boolean read fCaseSensitive write fCaseSensitive;
    property Completions: TStrings read GetCompletions;
    property CompletionComments: TStrings read GetCompletionComments;
    property CompletionValues: TStrings read GetCompletionValues;
    property EndOfTokenChr: string read fEOTokenChars write fEOTokenChars;
    property CompletionAttributes[Index: integer]: TStrings
                                                   read GetCompletionAttributes;
    property OnTokenNotFound: TOnTokenNotFound
      read fOnTokenNotFound write fOnTokenNotFound;
    property IndentToTokenStart: boolean
      read fIndentToTokenStart write fIndentToTokenStart;
    property OnExecuteCompletion: TOnExecuteCompletion read FOnExecuteCompletion
                                                     write FOnExecuteCompletion;
  end;

  { TSynEditAutoComplete }

  TSynEditAutoComplete = class(TCustomSynAutoComplete)
  published
    property AutoCompleteList;
    property CaseSensitive;
    property Editor;
    property EndOfTokenChr;
    property OnTokenNotFound;
    property IndentToTokenStart;
    property OnExecuteCompletion;
  end;

implementation

uses
  SysUtils, Menus;

{ TCustomSynAutoComplete }

procedure TCustomSynAutoComplete.AddCompletion(AToken, AValue, AComment: string;
  TheAttributes: TStrings);
begin
  if AToken <> '' then begin
    if not fParsed then
      ParseCompletionList;

    fCompletions.Add(AToken);
    fCompletionComments.Add(AComment);
    fCompletionValues.Add(AValue);
    if TheAttributes=nil then
      TheAttributes:=TStringList.Create;
    fAttributes.Add(TheAttributes);
  end;
end;

procedure TCustomSynAutoComplete.CompletionListChanged(Sender: TObject);
begin
  fParsed := FALSE;
end;

constructor TCustomSynAutoComplete.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fAutoCompleteList := TStringList.Create;
  TStringList(fAutoCompleteList).OnChange :=
     @CompletionListChanged;
  fCompletions := TStringList.Create;
  fCompletionComments := TStringList.Create;
  fCompletionValues := TStringList.Create;
  fEditors := TList.Create;
  fEOTokenChars := '()[]{}.';
  fAttributes:=TFPList.Create;
end;

function TCustomSynAutoComplete.GetCompletionAttributes(Index: integer
  ): TStrings;
begin
  Result:=TStrings(fAttributes[Index]);
end;

procedure TCustomSynAutoComplete.ClearAttributes;
var
  i: Integer;
begin
  for i:=0 to fAttributes.Count-1 do
    TObject(fAttributes[i]).Free;
  fAttributes.Clear;
end;

procedure TCustomSynAutoComplete.DeleteCompletion(Index: integer);
begin
  fCompletions.Delete(Index);
  fCompletionComments.Delete(Index);
  fCompletionValues.Delete(Index);
  TObject(fAttributes[Index]).Free;
  fAttributes.Delete(Index);
end;

destructor TCustomSynAutoComplete.Destroy;
begin
  fEditors.Free;
  fCompletions.Free;
  fCompletionComments.Free;
  fCompletionValues.Free;
  fAutoCompleteList.Free;
  ClearAttributes;
  fAttributes.Free;
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
        ExecuteCompletion(FCompletions[i], AEditor);
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
    and (fCompletions.Count > 0)
  then begin
    // find completion for this token - not all chars necessary if unambiguous
    i := fCompletions.Count - 1;
    IdxMaybe := -1;
    NumMaybe := 0;
    if fCaseSensitive then begin
      while i > -1 do begin
        s := fCompletions[i];
        if AnsiCompareStr(s, AToken) = 0 then
          break
        else if AnsiCompareStr(Copy(s, 1, Len), AToken) = 0 then begin
          Inc(NumMaybe);
          IdxMaybe := i;
        end;
        Dec(i);
      end;
    end else begin
      while i > -1 do begin
        s := fCompletions[i];
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
            Temp.Text := fCompletionValues[i];
            s:=fCompletionValues[i];
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

function TCustomSynAutoComplete.GetCompletions: TStrings;
begin
  if not fParsed then
    ParseCompletionList;
  Result := fCompletions;
end;

function TCustomSynAutoComplete.GetCompletionComments: TStrings;
begin
  if not fParsed then
    ParseCompletionList;
  Result := fCompletionComments;
end;

function TCustomSynAutoComplete.GetCompletionValues: TStrings;
begin
  if not fParsed then
    ParseCompletionList;
  Result := fCompletionValues;
end;

procedure TCustomSynAutoComplete.ParseCompletionList;

  procedure RemoveFirstLine(var Pattern: string);
  var
    i: Integer;
  begin
    // remove first line (i.e. macro enabled flag)
    i:=1;
    while (i<=length(Pattern)) and (not (Pattern[i] in [#10,#13])) do inc(i);
    if (i<length(Pattern)) and (Pattern[i+1] in [#10,#13])
    and (Pattern[i+1]<>Pattern[i]) then
      inc(i);
    delete(Pattern,1,i);
  end;

var
  BorlandDCI: boolean;
  i, j, Len: integer;
  s, sCompl, sComment, sComplValue: string;
  TemplateStarted: Boolean;

  procedure SaveEntry;
  var
    CurAttributes: TStrings;
    Lines: TStringList;
    LastLineHasEnding: boolean;
    l: Integer;
  begin
    fCompletions.Add(sCompl);
    sCompl := '';
    fCompletionComments.Add(sComment);
    sComment := '';
    CurAttributes:=TStringList.Create;
    if copy(sComplValue,1,length(CodeTemplateMacroMagic))=CodeTemplateMacroMagic
    then begin
      RemoveFirstLine(sComplValue);
      CurAttributes.Values[CodeTemplateEnableMacros]:='true';
    end else if copy(sComplValue,1,length(CodeTemplateAttributesStartMagic))
      =CodeTemplateAttributesStartMagic
    then begin
      Lines:=TStringList.Create;
      Lines.Text:=sComplValue;
      LastLineHasEnding:=(sComplValue<>'') and (sComplValue[length(sComplValue)] in [#10,#13]);
      Lines.Delete(0);
      while (Lines.Count>0) and (Lines[0]<>CodeTemplateAttributesEndMagic) do
      begin
        CurAttributes.Add(Lines[0]);
        Lines.Delete(0);
      end;
      if Lines.Count>0 then
        Lines.Delete(0);
      sComplValue:=Lines.Text;
      if not LastLineHasEnding then begin
        l:=length(sComplValue);
        if (l>0) and (sComplValue[l] in [#10,#13]) then begin
          dec(l);
          if (l>0) and (sComplValue[l] in [#10,#13])
          and (sComplValue[l]<>sComplValue[l+1]) then
            dec(l);
          sComplValue:=copy(sComplValue,1,l);
        end;
      end;
      Lines.Free;
    end;
    fCompletionValues.Add(sComplValue);
    sComplValue := '';
    fAttributes.Add(CurAttributes);
  end;

begin
  fCompletions.Clear;
  fCompletionComments.Clear;
  fCompletionValues.Clear;
  ClearAttributes;

  if fAutoCompleteList.Count > 0 then begin
    s := fAutoCompleteList[0];
    BorlandDCI := (s <> '') and (s[1] = '[');

    sCompl := '';
    sComment := '';
    sComplValue := '';
    TemplateStarted:=false;
    for i := 0 to fAutoCompleteList.Count - 1 do begin
      s := fAutoCompleteList[i];
      Len := Length(s);
      if BorlandDCI then begin
        // the style of the Delphi32.dci file
        if (Len > 0) and (s[1] = '[') then begin
          // save last entry
          if sCompl <> '' then
            SaveEntry;
          // new completion entry
          j := 2;
          while (j <= Len) and (s[j] > ' ') do
            Inc(j);
          sCompl := Copy(s, 2, j - 2);
          // start of comment in DCI file
          while (j <= Len) and (s[j] <= ' ') do
            Inc(j);
          if (j <= Len) and (s[j] = '|') then
            Inc(j);
          while (j <= Len) and (s[j] <= ' ') do
            Inc(j);
          sComment := Copy(s, j, Len);
          if sComment[Length(sComment)] = ']' then
            SetLength(sComment, Length(sComment) - 1);
          TemplateStarted:=true;
        end else begin
          if not TemplateStarted then
            sComplValue := sComplValue + #13#10;
          TemplateStarted:=false;
          sComplValue := sComplValue + s;
        end;
      end else begin
        // the original style
        if (Len > 0) and (s[1] <> '=') then begin
          // save last entry
          if sCompl <> '' then
            SaveEntry;
          // new completion entry
          sCompl := s;
          TemplateStarted:=true;
        end else if (Len > 0) and (s[1] = '=') then begin
          if not TemplateStarted then
            sComplValue := sComplValue + #13#10;
          TemplateStarted:=false;
          sComplValue := sComplValue + Copy(s, 2, Len);
        end;
      end;
    end;
    if sCompl <> '' then                                                        //mg 2000-11-07
      SaveEntry;
  end;
  fParsed:=true;
end;

procedure TCustomSynAutoComplete.SetAutoCompleteList(Value: TStrings);
begin
  fAutoCompleteList.Assign(Value);
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

