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
  SysUtils, Classes, math, LCLType, SynEditTextBase, SynEditHighlighter,
  SynEditHighlighterFoldBase, LazEditHighlighterUtils, LazEditHighlighter, LazEditTypes,
  LazListClasses;

type

  TSynXmlRangeInfo = record
    ElementOpenList: Array of String; // List of words opened in this line (and still open at the end of line)
    ElementCloseList: Array of Smallint; // include close, for open on same line
  end;

  TSynXmlFullRangeInfo = record
    Rng: Pointer;
    Info: TSynXmlRangeInfo;
  end;

  { TSynHighlighterXmlRangeList }

  TSynHighlighterXmlRangeList = class(
    specialize TGenInitLazHighlighterLineRangeShiftList<TSynXmlFullRangeInfo,
      specialize TLazListAspectMemInitManagedRefCnt<TSynXmlFullRangeInfo>
    >
  )
  private
    function GetXmlRangeInfo(Index: Integer): TSynXmlRangeInfo;
    procedure SetXmlRangeInfo(Index: Integer; const AValue: TSynXmlRangeInfo);
  public
    property XmlRangeInfo[Index: Integer]: TSynXmlRangeInfo
      read GetXmlRangeInfo write SetXmlRangeInfo;
  end;


  { TSynCustomXmlHighlighter }

  TSynCustomXmlHighlighter = class(TSynCustomFoldHighlighter)
  private const
    XML_BRACKET_KIND_TOKEN_COUNT = 6;
    XML_BRACKET_KIND_TOKENS: array [Boolean, 0..XML_BRACKET_KIND_TOKEN_COUNT-1] of string =
      ( ('>', ')', ']', '}', '''', '"'),
        ('<', '(', '[', '{', '''', '"')
      );
  private
    FXmlRangeInfo: TSynXmlRangeInfo;
    FXmlRangeInfoChanged: Boolean;
    FXmlRangeInfoOpenPos: integer;
    FXmlRangeInfoClosePos: integer;
  protected
    function  CreateRangeList(ALines: TLazEditStringsBase): TLazHighlighterLineRangeList; override;
    function  UpdateRangeInfoAtEOL: Boolean; override; // Returns true if range changed

    function  StartXmlCodeFoldBlock(ABlockType: Integer): Boolean;
    function  StartXmlNodeCodeFoldBlock(ABlockType: Integer; OpenPos: Integer;
                                        AName: String): Boolean;
    procedure EndXmlCodeFoldBlock;
    procedure EndXmlNodeCodeFoldBlock(ClosePos: Integer = -1; AName: String = '');
    function GetBracketKinds(AnIndex: integer; AnOpeningToken: boolean): String; override;
  public
    procedure InitForScaningLine; override;
    function BracketKindCount: integer; override;
    function GetBracketContextAt(const ALineIdx: TLineIdx; const ALogX: IntPos;
      const AByteLen: Integer; const AKind: integer; var AFlags: TLazEditBracketInfoFlags; out
      AContext, ANestLevel: Integer; var InternalInfo: PtrUInt): Boolean; override;
  end;

implementation

function TSynCustomXmlHighlighter.CreateRangeList(ALines: TLazEditStringsBase): TLazHighlighterLineRangeList;
begin
  Result := TSynHighlighterXmlRangeList.Create;
end;

function TSynCustomXmlHighlighter.UpdateRangeInfoAtEOL: Boolean;
var
  InfoOpenLenChanged, InfoCloseLenChanged: Boolean;
begin
  Result := inherited UpdateRangeInfoAtEOL;
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

procedure TSynCustomXmlHighlighter.InitForScaningLine;
begin
  inherited;
  FXmlRangeInfo := TSynHighlighterXmlRangeList(CurrentRanges).XmlRangeInfo[LineIndex]; // From this line, not from the previous line
  FXmlRangeInfoChanged := False;
  FXmlRangeInfoOpenPos := 0;
  FXmlRangeInfoClosePos := 0;
end;

function TSynCustomXmlHighlighter.BracketKindCount: integer;
begin
  Result := XML_BRACKET_KIND_TOKEN_COUNT;
end;

function TSynCustomXmlHighlighter.GetBracketContextAt(const ALineIdx: TLineIdx;
  const ALogX: IntPos; const AByteLen: Integer; const AKind: integer;
  var AFlags: TLazEditBracketInfoFlags; out AContext, ANestLevel: Integer;
  var InternalInfo: PtrUInt): Boolean;
begin
  if LineIndex <> ALineIdx then
    StartAtLineIndex(ALineIdx);
  NextToLogX(ALogX, True);

  AContext := GetTokenKind;
  ANestLevel := 0;
  AFlags := AFlags + [bfUnknownNestLevel];
  Result := True;

  if AKind <> 0 then
    AFlags := AFlags + [bfNoLanguageContext];
  if AKind >= 4 then
    AFlags := AFlags + [bfSingleLine, bfUniform, bfNotNestable] - [bfOpen];
end;

function TSynCustomXmlHighlighter.StartXmlCodeFoldBlock(ABlockType: Integer): Boolean;
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

function TSynCustomXmlHighlighter.StartXmlNodeCodeFoldBlock(ABlockType: Integer; OpenPos: Integer;
  AName: String): Boolean;
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
    if (AName = '') and (CurrentCodeFoldBlockLevel > 0) then begin
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
        lvl := FoldBlockEndLevel(i);
        while i >= 0 do begin
          if FoldBlockMinLevel(i) < lvl then begin
            LInfo := TSynHighlighterXmlRangeList(CurrentRanges).XmlRangeInfo[i].ElementOpenList;
            k := length(LInfo) - Max(FoldBlockEndLevel(i) - lvl, 0) - 1;
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

        if (i < 0) or (cnt > CurrentCodeFoldBlockLevel) then cnt := 0; // never opened, do not close
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

function TSynCustomXmlHighlighter.GetBracketKinds(AnIndex: integer; AnOpeningToken: boolean
  ): String;
begin
  Result := XML_BRACKET_KIND_TOKENS[AnOpeningToken, AnIndex]

end;

{ TSynHighlighterXmlRangeList }

function TSynHighlighterXmlRangeList.GetXmlRangeInfo(Index: Integer): TSynXmlRangeInfo;
begin
  if (Index < 0) or (Index >= Count) then begin
    Result.ElementOpenList := nil;
    exit;
  end;
  Result := ItemPointer[Index]^.Info;
end;

procedure TSynHighlighterXmlRangeList.SetXmlRangeInfo(Index: Integer;
  const AValue: TSynXmlRangeInfo);
begin
  ItemPointer[Index]^.Info := AValue;
end;

end.



