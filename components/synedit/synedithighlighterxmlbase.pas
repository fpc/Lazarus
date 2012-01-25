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

This file was added to the Lazarus branch of SynEdit.
The original Author is M Friebe
}

(* Provides folding for Xml and Html *)
unit SynEditHighlighterXMLBase;

interface

{$I SynEdit.inc}

uses
  SysUtils, Classes, math, LCLType,
  SynEditTextBase,
  SynEditHighlighter, SynEditHighlighterFoldBase;

type

  TSynXmlRangeInfo = record
    ElementOpenList: Array of String; // List of words opened in this line (and still open at the end of line)
    ElementCloseList: Array of Smallint; // include close, for open on same line
  end;

  { TSynHighlighterXmlRangeList }

  TSynHighlighterXmlRangeList = class(TSynHighlighterRangeList)
  private
    FItemOffset: Integer;
    function GetXmlRangeInfo(Index: Integer): TSynXmlRangeInfo;
    procedure SetXmlRangeInfo(Index: Integer; const AValue: TSynXmlRangeInfo);
  protected
    procedure SetCapacity(const AValue: Integer); override;
  public
    constructor Create;
    procedure Move(AFrom, ATo, ALen: Integer); override;
    property XmlRangeInfo[Index: Integer]: TSynXmlRangeInfo
      read GetXmlRangeInfo write SetXmlRangeInfo;
  end;


  TSynCustomXmlHighlighter = class(TSynCustomFoldHighlighter)
  private
    FXmlRangeInfo: TSynXmlRangeInfo;
    FXmlRangeInfoChanged: Boolean;
    FXmlRangeInfoOpenPos: integer;
    FXmlRangeInfoClosePos: integer;
  protected
    function  CreateRangeList(ALines: TSynEditStringsBase): TSynHighlighterRangeList; override;
    function  UpdateRangeInfoAtLine(Index: Integer): Boolean; override; // Returns true if range changed

    function  StartXmlCodeFoldBlock(ABlockType: Integer): TSynCustomCodeFoldBlock;
    function  StartXmlNodeCodeFoldBlock(ABlockType: Integer; OpenPos: Integer;
                                        AName: String): TSynCustomCodeFoldBlock;
    procedure EndXmlCodeFoldBlock;
    procedure EndXmlNodeCodeFoldBlock(ClosePos: Integer = -1; AName: String = '');
  public
    procedure SetLine({$IFDEF FPC}const {$ENDIF}NewValue: string; LineNumber:Integer); override;
  public
    function FoldOpenCount(ALineIndex: Integer; AType: Integer = 0): integer; override;
    function FoldCloseCount(ALineIndex: Integer; AType: Integer = 0): integer; override;
    function FoldNestCount(ALineIndex: Integer; AType: Integer = 0): integer; override;
    // TODO: make private
    function MinimumFoldLevel(ALineIndex: Integer): integer; override;
    function EndFoldLevel(ALineIndex: Integer): integer; override;
  end;

implementation

const
  MaxFoldNestDeep = 500;

function TSynCustomXmlHighlighter.CreateRangeList(ALines: TSynEditStringsBase): TSynHighlighterRangeList;
begin
  Result := TSynHighlighterXmlRangeList.Create;
end;

function TSynCustomXmlHighlighter.UpdateRangeInfoAtLine(Index: Integer): Boolean;
var
  InfoOpenLenChanged, InfoCloseLenChanged: Boolean;
begin
  Result := inherited UpdateRangeInfoAtLine(Index);
  InfoOpenLenChanged := Length(FXmlRangeInfo.ElementOpenList) <> FXmlRangeInfoOpenPos;
  InfoCloseLenChanged := Length(FXmlRangeInfo.ElementCloseList) <> FXmlRangeInfoClosePos;
  if FXmlRangeInfoChanged or InfoOpenLenChanged or InfoCloseLenChanged then begin
    Result := True;
    if InfoOpenLenChanged then
      SetLength(FXmlRangeInfo.ElementOpenList, FXmlRangeInfoOpenPos);
    if InfoCloseLenChanged then
      SetLength(FXmlRangeInfo.ElementCloseList, FXmlRangeInfoClosePos);
    TSynHighlighterXmlRangeList(CurrentRanges).XmlRangeInfo[LineIndex] := FXmlRangeInfo; // Store on this line
    FXmlRangeInfoChanged := False;
  end;
end;

procedure TSynCustomXmlHighlighter.SetLine({$IFDEF FPC}const {$ENDIF}NewValue: string;
  LineNumber:Integer);
begin
  inherited;
  FXmlRangeInfo := TSynHighlighterXmlRangeList(CurrentRanges).XmlRangeInfo[LineIndex]; // From this line, not from the previous line
  FXmlRangeInfoChanged := False;
  FXmlRangeInfoOpenPos := 0;
  FXmlRangeInfoClosePos := 0;
end;

function TSynCustomXmlHighlighter.FoldOpenCount(ALineIndex: Integer; AType: Integer): integer;
begin
  If AType <> 0 then exit(0);
  Result := EndFoldLevel(ALineIndex) - MinimumFoldLevel(ALineIndex);
end;

function TSynCustomXmlHighlighter.FoldCloseCount(ALineIndex: Integer; AType: Integer): integer;
begin
  If AType <> 0 then exit(0);
  Result := EndFoldLevel(ALineIndex - 1) - MinimumFoldLevel(ALineIndex);
end;

function TSynCustomXmlHighlighter.FoldNestCount(ALineIndex: Integer; AType: Integer): integer;
begin
  If AType <> 0 then exit(0);
  Result := EndFoldLevel(ALineIndex);
end;

function TSynCustomXmlHighlighter.MinimumFoldLevel(ALineIndex: Integer): integer;
var
  r: TSynCustomHighlighterRange;
begin
  if (ALineIndex < 0) or (ALineIndex >= CurrentLines.Count) then
    exit(0);
  r := TSynCustomHighlighterRange(CurrentRanges[ALineIndex]);
  if (r <> nil) and (Pointer(r) <> NullRange) then
    Result := r.MinimumCodeFoldBlockLevel
  else
    Result := 0;
end;

function TSynCustomXmlHighlighter.EndFoldLevel(ALineIndex: Integer): integer;
var
  r: TSynCustomHighlighterRange;
begin
  if (ALineIndex < 0) or (ALineIndex >= CurrentLines.Count) then
    exit(0);
  r := TSynCustomHighlighterRange(CurrentRanges[ALineIndex]);
  if (r <> nil) and (Pointer(r) <> NullRange) then
    Result := r.CodeFoldStackSize
  else
    Result := 0;
end;

function TSynCustomXmlHighlighter.StartXmlCodeFoldBlock(ABlockType: Integer): TSynCustomCodeFoldBlock;
var
  FoldBlock: Boolean;
  p: PtrInt;
begin
  FoldBlock :=  FFoldConfig[ABlockType].Enabled;
  p := 0;
  if not FoldBlock then
    p := PtrInt(GetFoldConfigInternalCount);
  Result := StartCodeFoldBlock(p + Pointer(PtrInt(ABlockType)), FoldBlock);
end;

function TSynCustomXmlHighlighter.StartXmlNodeCodeFoldBlock(ABlockType: Integer;
  OpenPos: Integer; AName: String): TSynCustomCodeFoldBlock;
var
  i: Integer;
begin
  If IsScanning then begin
    AName := LowerCase(AName);
    i := Length(FXmlRangeInfo.ElementOpenList);
    if (FXmlRangeInfoOpenPos < i) then begin
      if (FXmlRangeInfo.ElementOpenList[FXmlRangeInfoOpenPos] <> AName) then begin
        FXmlRangeInfo.ElementOpenList[FXmlRangeInfoOpenPos] := AName;
        FXmlRangeInfoChanged := true; // TODO:if this node closes on the same line, it may not be amodified ....
      end;
    end else begin     // append - modified will be deteced by the new length
      SetLength(FXmlRangeInfo.ElementOpenList, FXmlRangeInfoOpenPos + 10);
      FXmlRangeInfo.ElementOpenList[FXmlRangeInfoOpenPos] := AName;
    end;
  end;
  inc(FXmlRangeInfoOpenPos);
  result := StartXmlCodeFoldBlock(ABlockType);
end;

procedure TSynCustomXmlHighlighter.EndXmlCodeFoldBlock;
var
  DecreaseLevel: Boolean;
begin
  DecreaseLevel := TopCodeFoldBlockType < Pointer(PtrInt(GetFoldConfigInternalCount));
  EndCodeFoldBlock(DecreaseLevel);
end;

procedure TSynCustomXmlHighlighter.EndXmlNodeCodeFoldBlock(ClosePos: Integer = -1; AName: String = '');
var
  cnt, i, k, lvl: Integer;
  LInfo: Array of String;
begin
  AName := LowerCase(AName);

  cnt := 0;
  If IsScanning then begin
    if (AName = '') and (CodeFoldRange.CodeFoldStackSize > 0) then begin
      cnt := 1;
    end
    else begin
      cnt := 1;
      i := FXmlRangeInfoOpenPos;
      while i > 0 do begin
        if (FXmlRangeInfo.ElementOpenList[i-1] = AName) then
          break;
        dec(i);
        inc(cnt);
      end;

      if i = 0 then begin
        i := LineIndex - 1;
        lvl := EndFoldLevel(i);
        while i >= 0 do begin
          if MinimumFoldLevel(i) < lvl then begin
            LInfo := TSynHighlighterXmlRangeList(CurrentRanges).XmlRangeInfo[i].ElementOpenList;
            k := length(LInfo) - Max(EndFoldLevel(i) - lvl, 0) - 1;
            while (k >= 0) do begin
              if (LInfo[k] = AName) then
                break;
              inc(cnt);
              dec(k);
              dec(lvl);
            end;
            if k >= 0 then break;
          end;
          dec(i);
        end;

        if (i < 0) or (cnt > CodeFoldRange.CodeFoldStackSize ) then cnt := 0; // never opened, do not close
      end;
    end;

    i := Length(FXmlRangeInfo.ElementCloseList);
    if (FXmlRangeInfoClosePos < i) then begin
      if (FXmlRangeInfo.ElementCloseList[FXmlRangeInfoClosePos] <> cnt) then begin
        FXmlRangeInfo.ElementCloseList[FXmlRangeInfoClosePos] := cnt;
        FXmlRangeInfoChanged := true;
      end;
    end else begin  // append - modified will be deteced by the new length
      SetLength(FXmlRangeInfo.ElementCloseList, FXmlRangeInfoClosePos + 10);
      FXmlRangeInfo.ElementCloseList[FXmlRangeInfoClosePos] := cnt;
    end;
  end
  else begin
    if FXmlRangeInfoClosePos < length(FXmlRangeInfo.ElementCloseList) then
      cnt := FXmlRangeInfo.ElementCloseList[FXmlRangeInfoClosePos]
    else
      cnt := 0;
  end;
  inc(FXmlRangeInfoClosePos);

  for i := 1 to cnt do begin
    if FXmlRangeInfoOpenPos > 0 then
      dec(FXmlRangeInfoOpenPos);
    EndXmlCodeFoldBlock;
  end;
end;

{ TSynHighlighterXmlRangeList }

function TSynHighlighterXmlRangeList.GetXmlRangeInfo(Index: Integer): TSynXmlRangeInfo;
begin
  if (Index < 0) or (Index >= Count) then begin
    Result.ElementOpenList := nil;
    exit;
  end;
  Result := TSynXmlRangeInfo((ItemPointer[Index] + FItemOffset)^);
end;

procedure TSynHighlighterXmlRangeList.SetXmlRangeInfo(Index: Integer;
  const AValue: TSynXmlRangeInfo);
begin
  TSynXmlRangeInfo((ItemPointer[Index] + FItemOffset)^) := AValue;
end;

procedure TSynHighlighterXmlRangeList.SetCapacity(const AValue: Integer);
var
  i: LongInt;
begin
  for i := AValue to Capacity-1 do
    with TSynXmlRangeInfo((ItemPointer[i] + FItemOffset)^) do begin
      ElementOpenList := nil;
      ElementCloseList := nil;
    end;
  inherited SetCapacity(AValue);
end;

constructor TSynHighlighterXmlRangeList.Create;
begin
  inherited;
  FItemOffset := ItemSize;
  ItemSize := FItemOffset + SizeOf(TSynXmlRangeInfo);
end;

procedure TSynHighlighterXmlRangeList.Move(AFrom, ATo, ALen: Integer);
var
  i: LongInt;
begin
  if ATo > AFrom then
    for i:= Max(AFrom + ALen, ATo) to ATo + ALen - 1 do // move forward
      with TSynXmlRangeInfo((ItemPointer[i] + FItemOffset)^) do begin
        ElementOpenList := nil;
        ElementCloseList := nil;
      end
  else
    for i:= ATo to Min(ATo + ALen , AFrom) - 1 do // move backward
      with TSynXmlRangeInfo((ItemPointer[i] + FItemOffset)^) do begin
        ElementOpenList := nil;
        ElementCloseList := nil;
      end;
  inherited Move(AFrom, ATo, ALen);
end;

end.



