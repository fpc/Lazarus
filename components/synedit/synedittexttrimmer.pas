{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

-------------------------------------------------------------------------------}

unit SynEditTextTrimmer;

{$I synedit.inc}

interface

uses
LCLProc,
  Classes, SysUtils, SynEditTypes, SynEditTextBase, SynEditTextBuffer,
  SynEditMiscClasses, SynEditPointClasses;

type

  { TSynEditStringTrimmingList }

  TSynEditStringTrimmingList = class(TSynEditStringsLinked)
  private
    fCaret: TSynEditCaret;
    fUndoList: TSynEditUndoList;
    fSpaces: String;
    fLineText: String;
    fLineIndex: Integer;
    fEnabled: Boolean;
    fLockCount: Integer;
    fLockList : TStringList;
    procedure DoCaretChanged(Sender : TObject);
    procedure SetEnabled(const AValue : Boolean);
    function  TrimLine(const S : String; Index: Integer; RealUndo: Boolean = False) : String;
    function  Spaces(Index: Integer) : String;
    procedure DoLinesChanged(Index, N: integer);
    procedure TrimAfterLock;
  protected
    function  GetExpandedString(Index: integer): string; override;
    function  GetLengthOfLongestLine: integer; override;
    function  Get(Index: integer): string; override;
    function  GetObject(Index: integer): TObject; override;
    procedure Put(Index: integer; const S: string); override;
    procedure PutObject(Index: integer; AObject: TObject); override;
  public
    constructor Create(ASynStringSource: TSynEditStrings; ACaret: TSynEditCaret);
    destructor Destroy; override;

    function Add(const S: string): integer; override;
    procedure AddStrings(AStrings: TStrings); override;
    procedure Clear; override;
    procedure Delete(Index: integer); override;
    procedure DeleteLines(Index, NumLines: integer);  override;
    procedure Insert(Index: integer; const S: string); override;
    procedure InsertLines(Index, NumLines: integer); override;
    procedure InsertStrings(Index: integer; NewStrings: TStrings); override;
    procedure Exchange(Index1, Index2: integer); override;
    property ExpandedStrings[Index: integer]: string read GetExpandedString;
    property LengthOfLongestLine: integer read GetLengthOfLongestLine;
  public
    procedure Lock;
    procedure UnLock;
    procedure ForceTrim; // for redo; redo can not wait for UnLock
    procedure UndoRealSpaces(Item: TSynEditUndoItem);
    property Enabled : Boolean read fEnabled write SetEnabled;
    property UndoList: TSynEditUndoList read fUndoList write fUndoList;
  end;

implementation


{ TSynEditStringTrimmingList }

constructor TSynEditStringTrimmingList.Create(ASynStringSource : TSynEditStrings; ACaret: TSynEditCaret);
begin
  fCaret := ACaret;
  fCaret.AddChangeHandler(@DoCaretChanged);
  fLockList := TStringList.Create;
  fLineIndex:= -1;
  fSpaces := '';
  fEnabled:=false;
  Inherited Create(ASynStringSource);
end;

destructor TSynEditStringTrimmingList.Destroy;
begin
  fCaret.RemoveChangeHandler(@DoCaretChanged);
  FreeAndNil(fLockList);
  inherited Destroy;
end;

procedure TSynEditStringTrimmingList.DoCaretChanged(Sender : TObject);
var
  s: String;
begin
  if (fLineIndex = TSynEditCaret(Sender).LinePos - 1) then exit;
  if (length(fSpaces) > 0) and (fLineIndex > 0)
  and (fLineIndex <= fSynStrings.Count)
  and (fLockCount = 0) then begin
    s := fSynStrings[fLineIndex];
    fSynStrings[fLineIndex] := s;                                               // trigger OnPutted, so the line gets repainted
    fUndoList.AddChange(crTrimSpace, Point(1+length(s), fLineIndex+1),
      Point(1+length(s)+length(fSpaces), fLineIndex+1), fSpaces, smNormal);
  end;
  fLineIndex := TSynEditCaret(Sender).LinePos - 1;
  fSpaces := '';
end;

procedure TSynEditStringTrimmingList.SetEnabled(const AValue : Boolean);
begin
  if fEnabled = AValue then exit;
  fEnabled:=AValue;
  fLockList.Clear;
  fLockCount:=0;
  if fEnabled and (fLineIndex >= 0) and (fLineIndex < fSynStrings.Count) then
    fSynStrings[fLineIndex] := TrimLine(fSynStrings[fLineIndex], fLineIndex);
end;

procedure TSynEditStringTrimmingList.UndoRealSpaces(Item: TSynEditUndoItem);
var
  i: Integer;
begin
  fSynStrings.Strings[Item.fChangeStartPos.y-1]
    := copy(fSynStrings.Strings[Item.fChangeStartPos.y-1],
            1, Item.fChangeStartPos.x-1) + Item.fChangeStr;
  if (fLineIndex = Item.fChangeStartPos.y-1) then fSpaces := '';
  i := fLockList.IndexOfObject(TObject(Pointer(Item.fChangeStartPos.y-1)));
  if i >= 0 then fLockList.Delete(i);
end;

function TSynEditStringTrimmingList.TrimLine(const S: String; Index: Integer;
         RealUndo: Boolean = False): String;
var
  l, i:integer;
  temp: String;
begin
  if (not fEnabled) or (fUndoList.IsLocked) then exit(s);
  if RealUndo then begin
    temp := fSynStrings.Strings[Index];
    l := length(temp);
    i:= l;
    while (i>0) and (temp[i] in [#9, ' ']) do dec(i);
    // Add RealSpaceUndo
    if i < l then
      fUndoList.AddChange(crTrimRealSpace, Point(i+1, Index+1),
                          Point(l, Index+1), copy(temp, i+1, l-i), smNormal);
  end;

  l := length(s);
  i := l;
  while (i>0) and (s[i] in [#9, ' ']) do dec(i);
  temp := copy(s, i+1, l-i);
  if i=l then
    result := s   // No need to make a copy
  else
    result := copy(s, 1, i);

  if fLockCount > 0 then begin
    i := fLockList.IndexOfObject(TObject(pointer(Index)));
    if i < 0 then
      fLockList.AddObject(temp, TObject(pointer(Index)))
    else
      fLockList[i] := temp;
  end
  else if (fLineIndex = Index) then begin
    fSpaces := temp;
    fLineText:=result;
  end;
end ;

function TSynEditStringTrimmingList.Spaces(Index : Integer) : String;
var
  i : Integer;
begin
  if fLockCount > 0 then begin
    i := fLockList.IndexOfObject(TObject(Pointer(Index)));
    if i < 0 then
      result := ''
    else
      result := fLockList[i];
    exit;
  end;
  if Index <> fLineIndex then exit('');
  if (fLineIndex < 0) or (fLineIndex >= fSynStrings.Count)
    or (fLineText <> fSynStrings[fLineIndex]) then begin
    fSpaces:='';
    fLineText:='';
  end;
  Result:= fSpaces;
end;

procedure TSynEditStringTrimmingList.DoLinesChanged(Index, N : integer);
var
  i, j: Integer;
begin
  if  fLockCount > 0 then begin
    for i := fLockList.Count-1 downto 0 do begin
      j := Integer(Pointer(fLockList.Objects[i]));
      if (j >= Index) and (j < Index - N) then
        fLockList.Delete(i)
      else if j > Index then
        fLockList.Objects[i] := TObject(Pointer(j + N));
    end;
  end else begin
    if (fLineIndex >= Index) and (fLineIndex < Index - N) then
      fLineIndex:=-1
    else if fLineIndex > Index then
      inc(fLineIndex, N);
  end;
end;

procedure TSynEditStringTrimmingList.Lock;
begin
  if (fLockCount = 0) and (fLineIndex >= 0) then
    fLockList.AddObject(Spaces(fLineIndex), TObject(Pointer(fLineIndex)));
  inc(fLockCount);
end;

procedure TSynEditStringTrimmingList.UnLock;
begin
  dec(fLockCount);
  if (fLockCount = 0) then TrimAfterLock;
end;

procedure TSynEditStringTrimmingList.TrimAfterLock;
var
  i, index, llen, slen: Integer;
  ltext: String;
begin
  i := fLockList.IndexOfObject(TObject(Pointer(fLineIndex)));
  if i >= 0 then begin
    fSpaces:= fLockList[i];
    if (fLineIndex >= 0) and (fLineIndex < fSynStrings.Count) then
      fLineText := fSynStrings[fLineIndex];
    fLockList.Delete(i);
  end;
  for i := 0 to fLockList.Count-1 do begin
    index := Integer(Pointer(fLockList.Objects[i]));
    slen := length(fLockList[i]);
    if (slen > 0) and (index >= 0) and (index < fSynStrings.Count) then begin
      ltext := fSynStrings[index];
      llen := length(ltext);
      fSynStrings[index] := ltext;                                            // trigger OnPutted, so the line gets repainted
      fUndoList.AddChange(crTrimSpace, Point(1+llen, index+1),
        Point(1+llen+slen, index+1), fLockList[i], smNormal);
    end;
  end;
  fLockList.Clear;
end;

procedure TSynEditStringTrimmingList.ForceTrim;
begin
  TrimAfterLock;
end;

// Lines
function TSynEditStringTrimmingList.GetExpandedString(Index : integer) : string;
begin
  Result:= fSynStrings.ExpandedStrings[Index] + Spaces(Index);
end;

function TSynEditStringTrimmingList.GetLengthOfLongestLine : integer;
var
  i: Integer;
begin
  Result:= fSynStrings.LengthOfLongestLine;
  if (fLineIndex >= 0) and (fLineIndex < Count) then begin
    i:= length(ExpandedStrings[fLineIndex]);
    if (i > Result) then Result := i;
  end;
end;

function TSynEditStringTrimmingList.Get(Index : integer) : string;
begin
  Result:= fSynStrings.Strings[Index] + Spaces(Index);
end;

function TSynEditStringTrimmingList.GetObject(Index : integer) : TObject;
begin
  Result:= fSynStrings.Objects[Index];
end;

procedure TSynEditStringTrimmingList.Put(Index : integer; const S : string);
begin
  fSynStrings.Strings[Index]:= TrimLine(S, Index, True);
end;

procedure TSynEditStringTrimmingList.PutObject(Index : integer; AObject : TObject);
begin
  fSynStrings.Objects[Index]:= AObject;
end;

function TSynEditStringTrimmingList.Add(const S : string) : integer;
var
  c : Integer;
begin
  c := fSynStrings.Count;
  DoLinesChanged(c, 1);
  Result := fSynStrings.Add(TrimLine(S, c));
end;

procedure TSynEditStringTrimmingList.AddStrings(AStrings : TStrings);
var
  i, c : Integer;
begin
  c := fSynStrings.Count;
  DoLinesChanged(c, AStrings.Count);
  for i := 0 to AStrings.Count-1 do
    AStrings[i] := TrimLine(AStrings[i], c + i);
  fSynStrings.AddStrings(AStrings);
end;

procedure TSynEditStringTrimmingList.Clear;
begin
  fSynStrings.Clear;
  fLineIndex:=-1;
end;

procedure TSynEditStringTrimmingList.Delete(Index : integer);
begin
  fSynStrings.Delete(Index);
  DoLinesChanged(Index, -1);
end;

procedure TSynEditStringTrimmingList.DeleteLines(Index, NumLines : integer);
begin
  fSynStrings.DeleteLines(Index, NumLines);
  DoLinesChanged(Index, -NumLines);
end;

procedure TSynEditStringTrimmingList.Insert(Index : integer; const S : string);
begin
  DoLinesChanged(Index, 1);
  fSynStrings.Insert(Index, TrimLine(S, Index));
end;

procedure TSynEditStringTrimmingList.InsertLines(Index, NumLines : integer);
begin
  DoLinesChanged(Index, NumLines);
  fSynStrings.InsertLines(Index, NumLines);
end;

procedure TSynEditStringTrimmingList.InsertStrings(Index : integer; NewStrings : TStrings);
var
  i : Integer;
begin
  DoLinesChanged(Index, NewStrings.Count);
  for i := 0 to NewStrings.Count-1 do
    NewStrings[i] := TrimLine(NewStrings[i], Index+i, True);
  fSynStrings.InsertStrings(Index, NewStrings);
end;

procedure TSynEditStringTrimmingList.Exchange(Index1, Index2 : integer);
begin
  fSynStrings.Exchange(Index1, Index2);
  if fLineIndex = Index1 then
    fLineIndex := Index2
  else if fLineIndex = Index2 then
    fLineIndex := Index1;
end;

end.

