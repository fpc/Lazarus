{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynMemo.pas, released 2000-04-07.
The Original Code is based on mwCustomEdit.pas by Martin Waldenburg, part of
the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
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
  - a handler for the different EM_XXX messages has to be implemented to make
    this more compatible with TMemo
-------------------------------------------------------------------------------}

unit SynMemo;

{$I synedit.inc}

interface

uses
  Classes,
{$IFDEF SYN_CLX}
  Qt,
  Types,
{$ELSE}
  {$IFDEF SYN_LAZARUS}
  LCLIntf, Controls,
  {$ELSE}
  Windows,
  {$ENDIF}
{$ENDIF}
  SynEdit;

//SelStart and SelEnd are now in TCustomSynEdit                                 //DDH Addition

type

  { TCustomSynMemo }

  TCustomSynMemo = class(TCustomSynEdit)
  public
    function CharIndexToRowCol(Index: integer): TPoint;                         //as 2000-11-09
    function RowColToCharIndex(RowCol: TPoint): integer;                        //as 2000-11-09
    procedure Append(const Value: String);
    procedure Clear;
  end;

  TSynMemo = class(TCustomSynMemo)
{begin}                                                                         //mh 2000-09-23
  public
    // TCustomSynMemo properties
{end}                                                                           //mh 2000-09-23
  published
    // inherited properties
    property Align;
    {$IFDEF SYN_LAZARUS}
    property BorderSpacing;
    {$ENDIF}
{$IFDEF SYN_COMPILER_4_UP}
    property Anchors;
    property Constraints;
{$ENDIF}
    property Color;
  {$IFNDEF SYN_CLX}
    {$IFNDEF SYN_LAZARUS}
    property Ctl3D;
    {$ENDIF}
  {$ENDIF}
    property Enabled;
    property Font;
    property Height;
    property Name;
    property ParentColor;
  {$IFNDEF SYN_CLX}
    {$IFNDEF SYN_LAZARUS}
    property ParentCtl3D;
    {$ENDIF}
  {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property Visible;
    property Width;
    // inherited events
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
{$IFDEF SYN_COMPILER_4_UP}
{$IFNDEF SYN_CLX}
{$IFNDEF SYN_LAZARUS}
    property OnEndDock;
{$ENDIF}
{$ENDIF}
{$ENDIF}
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFDEF SYN_COMPILER_4_UP}
{$IFNDEF SYN_CLX}
{$IFNDEF SYN_LAZARUS}
    property OnStartDock;
{$ENDIF}
{$ENDIF}
{$ENDIF}
    property OnStartDrag;
    // TCustomSynEdit properties
    property BookMarkOptions;
    property BorderStyle default bsSingle;
    property ExtraLineSpacing;
    property Gutter;
    property HideSelection;
    property Highlighter;
    property InsertCaret;
    property InsertMode;
    property Keystrokes;
    property MouseActions;
    property MouseSelActions;
    property Lines;
    property MaxLeftChar;
    property MaxUndo;
    property Options;
    property Options2;
    property OverwriteCaret;
    property ReadOnly;
    property RightEdge;
    property RightEdgeColor;
    {$IFNDEF SYN_LAZARUS}
    property SearchEngine;
    {$ENDIF}
    property ScrollBars;
    property SelectedColor;
    property SelectionMode;
    property TabWidth;
    property WantTabs;
    // TCustomSynEdit events
    property OnChange;
    property OnClearBookmark;                                                   // djlp 2000-08-29
    property OnCommandProcessed;
    property OnDropFiles;
    property OnGutterClick;
    {$IFNDEF SYN_LAZARUS}
    property OnGutterGetText;
    property OnGutterPaint;
    {$ENDIF}
    property OnPaint;
    property OnPlaceBookmark;
    property OnProcessCommand;
    property OnProcessUserCommand;
    property OnReplaceText;
    property OnSpecialLineColors;
    property OnStatusChange;
    {$IFNDEF SYN_LAZARUS}
    property OnPaintTransient;
    {$ENDIF}
  end;

implementation

uses
  {$IFNDEF SYN_LAZARUS}
  SynEditStrConst,
  {$ENDIF}
  SynEditMiscProcs;

{ TCustomSynMemo }

function TCustomSynMemo.CharIndexToRowCol(Index: integer): TPoint;
var
  x, y, Chars: integer;
  e: string;
  LineEndLen: Integer;
begin
  x := 0;
  y := 0;
  e:=LineEnding;
  LineEndLen:=length(e);
  Chars := 0;
  while y < TextBuffer.Count do begin
    x := Length(TextBuffer[y]);
    if Chars + x + LineEndLen > Index then begin
      x := Index - Chars;
      break;
    end;
    Inc(Chars, x + LineEndLen);
    x := 0;
    Inc(y);
  end;
  Result := Point(x + 1, y + 1);
end;

function TCustomSynMemo.RowColToCharIndex(RowCol: TPoint): integer;
var
  i: integer;
  e: string;
  LineEndLen: Integer;
begin
  Result := 0;
  RowCol.y := Min(TextBuffer.Count, RowCol.y) - 1;
  e:=LineEnding;
  LineEndLen:=length(e);
  for i := 0 to RowCol.y - 1 do
    Result := Result + Length(TextBuffer[i]) + LineEndLen;
  Result := Result + RowCol.x;
end;

procedure TCustomSynMemo.Append(const Value: String);
begin
  Lines.Append(Value);
end;

procedure TCustomSynMemo.Clear;
begin
  Lines.Clear;
end;

end.

