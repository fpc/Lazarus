{
 *****************************************************************************
  This file is part of the LazEdit package from the Lazarus IDE.

  This content of this file is licensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 or 2.0 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
  Used units may have to be amended according to the choice.
 *****************************************************************************
}
unit LazEditHighlighter;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  // LazEdit
  LazEditTextAttributes;

type

  TLazEditHighlighterAttributes = class(TLazEditTextAttribute)
  published
    property Foreground;
    property Background;
    property FrameColor;

    property ForePriority;
    property BackPriority;
    property FramePriority;

    property FrameStyle;
    property FrameEdges;

    property Style;
    property BoldPriority;
    property ItalicPriority;
    property UnderlinePriority;
    property StrikeOutPriority;

    property OnChange;
  end;
  TLazEditHighlighterAttributesClass = class of TLazEditHighlighterAttributes;

  TLazEditHighlighterAttributesModifier = class(TLazEditTextAttributeModifier)
  published
    property Foreground;
    property Background;
    property FrameColor;

    property ForePriority;
    property BackPriority;
    property FramePriority;

    property FrameStyle;
    property FrameEdges;

    property Style;
    property BoldPriority;
    property ItalicPriority;
    property UnderlinePriority;
    property StrikeOutPriority;

    property OnChange;
  published
    property BackAlpha;
    property ForeAlpha;
    property FrameAlpha;

    property StyleMask;
  end;
  TLazEditHighlighterAttributesModifierClass = class of TLazEditHighlighterAttributesModifier;

  TLazEditHighlighterAttributes_Eol = class(TLazEditHighlighterAttributes)
  published
    property ExtendPastEol;
  end;

  TLazEditHighlighterAttributesModifier_Eol = class(TLazEditHighlighterAttributesModifier)
  published
    property ExtendPastEol;
  end;

  { TLazEditCustomHighlighter }

  TLazEditCustomHighlighter = class(TLazEditAttributeOwner)
  strict private
    FTokenAttributeMergeResult: TLazEditTextAttributeMergeResult;
    FTokenAttributeList: TLazCustomEditTextAttributeArray;
  protected
    procedure MergeModifierToTokenAttribute(
      var ACurrentResultAttrib: TLazCustomEditTextAttribute;
      AModifierAttrib: TLazCustomEditTextAttribute;
      AModifierLeftCol: integer = -2;
      AModifierRightCol: integer = -2;
      ASkipResultBounds: boolean = False
    );
  public
    destructor Destroy; override;

    function GetTokenPos: Integer; virtual; abstract; // 0-based
    function GetTokenLen: Integer; virtual; abstract;
    (* GetTokenAttribute / GetEndOfLineAttribute
       The base attribute
     * GetTokenAttributeEx / GetEndOfLineAttributeEx
       The final attribute with merged modifiers (if HL has modifiers)
    *)
    function GetTokenAttribute: TLazEditTextAttribute; virtual; abstract;
    function GetTokenAttributeEx: TLazCustomEditTextAttribute; virtual;
    function GetTokenAttributeList: TLazCustomEditTextAttributeArray;
    function GetEndOfLineAttribute: TLazEditTextAttribute; virtual; // valid after line was scanned to EOL
    function GetEndOfLineAttributeEx: TLazCustomEditTextAttribute; virtual; // valid after line was scanned to EOL
  end;




implementation

{ TLazEditCustomHighlighter }

procedure TLazEditCustomHighlighter.MergeModifierToTokenAttribute(
  var ACurrentResultAttrib: TLazCustomEditTextAttribute;
  AModifierAttrib: TLazCustomEditTextAttribute; AModifierLeftCol: integer;
  AModifierRightCol: integer; ASkipResultBounds: boolean);
var
  tp: Integer;
  b: TLazEditDisplayTokenBound;
begin
  if FTokenAttributeMergeResult = nil then
    FTokenAttributeMergeResult := TLazEditTextAttributeMergeResult.Create;

  if ACurrentResultAttrib <> FTokenAttributeMergeResult then begin
    if FTokenAttributeList <> nil then
      FTokenAttributeList[0] := ACurrentResultAttrib;
    // first call, replace the callers result
    FTokenAttributeMergeResult.Assign(ACurrentResultAttrib);
    if not ASkipResultBounds then begin
      tp := GetTokenPos+1;
      FTokenAttributeMergeResult.SetFrameBoundsLog(tp, tp+GetTokenLen);
    end;
    ACurrentResultAttrib := FTokenAttributeMergeResult;
  end;

  if FTokenAttributeList <> nil then begin
    tp := Length(FTokenAttributeList);
    SetLength(FTokenAttributeList, tp+1);
    FTokenAttributeList[tp] := AModifierAttrib;
  end;

  if AModifierLeftCol <> -2 then begin
    b.Init(-1, AModifierLeftCol);
    AModifierAttrib.StartX := b;
  end;
  if AModifierRightCol <> -2 then begin
    b.Init(-1, AModifierRightCol);
    AModifierAttrib.EndX := b;
  end;

  FTokenAttributeMergeResult.Merge(AModifierAttrib);
end;

destructor TLazEditCustomHighlighter.Destroy;
begin
  inherited Destroy;
  FTokenAttributeMergeResult.Free;
end;

function TLazEditCustomHighlighter.GetTokenAttributeEx: TLazCustomEditTextAttribute;
var
  tp: Integer;
begin
  Result := GetTokenAttribute;
  if Result <> nil then begin
    // Highlighter that need different bounds must implement their own GetTokenAttributeEx;
    tp := GetTokenPos+1;
    Result.SetFrameBoundsLog(tp, tp+GetTokenLen);
  end;
end;

function TLazEditCustomHighlighter.GetTokenAttributeList: TLazCustomEditTextAttributeArray;
var
  r: TLazCustomEditTextAttribute;
begin
  SetLength(FTokenAttributeList,1);
  FTokenAttributeList[0] := nil;
  r := GetTokenAttributeEx;
  if FTokenAttributeList[0] = nil then begin
    if r = nil then
      SetLength(FTokenAttributeList, 0)
    else
      FTokenAttributeList[0] := r;
  end;
  Result := FTokenAttributeList;
  SetLength(FTokenAttributeList, 0)
end;

function TLazEditCustomHighlighter.GetEndOfLineAttribute: TLazEditTextAttribute;
begin
  Result := nil;
end;

function TLazEditCustomHighlighter.GetEndOfLineAttributeEx: TLazCustomEditTextAttribute;
begin
  Result := GetEndOfLineAttribute;
end;

end.

