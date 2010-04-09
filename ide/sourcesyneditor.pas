{
/***************************************************************************
                               SourceSynEditor
                             -------------------

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

  Abstract:
    SynEdit extensions for the IDE
    - DebugMarks: Mark lines with debug info
}
unit SourceSynEditor;

{$mode objfpc}{$H+}

interface

{$I ide.inc}

uses
  Classes, SysUtils, Graphics, SynEdit, SynEditMiscClasses, SynGutter,
  SynGutterLineNumber, SynGutterCodeFolding, SynGutterMarks, SynGutterChanges,
  SynEditTextBuffer, SynEditFoldedView, SynTextDrawer;

type

  TIDESynGutterMarks = class;

  { TIDESynEditor }

  TIDESynEditor = class(TSynEdit)
  private
    function GetIDEGutterMarks: TIDESynGutterMarks;
  protected
    function CreateGutter(AOwner : TSynEditBase; AFoldedLinesView: TSynEditFoldedView;
                          ATextDrawer: TheTextDrawer): TSynGutter; override;
  public
    property IDEGutterMarks: TIDESynGutterMarks read GetIDEGutterMarks;
    property TextBuffer;
  end;

  { TIDESynGutter }

  TIDESynGutter = class(TSynGutter)
  protected
    procedure CreateDefaultGutterParts; override;
  end;

  { TIDESynDebugMarkInfo }

  TIDESynDebugMarkInfo = class(TSynManagedStorageMem)
  private
    FRefCount: Integer;
    function GetSrcLineToMarkLine(SrcIndex: Integer): Integer;
    procedure SetSrcLineToMarkLine(SrcIndex: Integer; const AValue: Integer);
  protected
    function ItemSize: Integer; override;
  public
    constructor Create;
    procedure IncRefCount;
    procedure DecRefCount;
    // Index is the Current line in editor (including source modification)
    // Result is the original Line as known by the debugger
    property SrcLineToMarkLine[SrcIndex: Integer]: Integer
             read GetSrcLineToMarkLine write SetSrcLineToMarkLine; default;
    property RefCount: Integer read FRefCount;
  end;

  { TIDESynGutterMarks }

  TIDESynGutterMarks = class(TSynGutterMarks)
  private
    FDebugMarkInfo: TIDESynDebugMarkInfo;
  protected
    Procedure PaintLine(aScreenLine: Integer; Canvas : TCanvas; AClip : TRect); override;
  public
    procedure SetDebugMarks(AFirstLinePos, ALastLinePos: Integer);
    procedure ClearDebugMarks;
    function HasDebugMarks: Boolean;
  end;

implementation

{ TIDESynEditor }

function TIDESynEditor.GetIDEGutterMarks: TIDESynGutterMarks;
begin
  Result := TIDESynGutterMarks(Gutter.Parts.ByClass[TIDESynGutterMarks, 0]);
end;

function TIDESynEditor.CreateGutter(AOwner: TSynEditBase;
  AFoldedLinesView: TSynEditFoldedView; ATextDrawer: TheTextDrawer): TSynGutter;
begin
  Result := TIDESynGutter.Create(AOwner, AFoldedLinesView, ATextDrawer);
end;

{ TIDESynGutter }

procedure TIDESynGutter.CreateDefaultGutterParts;
begin
  with TIDESynGutterMarks.Create(Parts) do
    Name := 'SynGutterMarks1';
  with TSynGutterLineNumber.Create(Parts) do
    Name := 'SynGutterLineNumber1';
  with TSynGutterChanges.Create(Parts) do
    Name := 'SynGutterChanges1';
  with TSynGutterSeparator.Create(Parts) do
    Name := 'SynGutterSeparator1';
  with TSynGutterCodeFolding.Create(Parts) do
    Name := 'SynGutterCodeFolding1';
end;

procedure TIDESynGutterMarks.PaintLine(aScreenLine: Integer; Canvas: TCanvas; AClip: TRect);
var
  aGutterOffs, TxtIdx: Integer;
  HasAnyMark: Boolean;

  procedure DrawDebugMark(Line: Integer);
  var
    itop : Longint;
    LineHeight: LongInt;
  begin
    iTop := 0;
    if Line < 0 then Exit;
    if Assigned(FBookMarkOpt.BookmarkImages) and
       (DebugMarksImageIndex <= FBookMarkOpt.BookmarkImages.Count) and
       (DebugMarksImageIndex >= 0) then
    begin
      LineHeight := TSynEdit(SynEdit).LineHeight;
      if not FBookMarkOpt.DrawBookmarksFirst then
        aGutterOffs := AClip.Left
      else
      if aGutterOffs = 0 then
        aGutterOffs := FBookMarkOpt.BookmarkImages.Width + AClip.Left;
      if LineHeight > FBookMarkOpt.BookmarkImages.Height then
        iTop := (LineHeight - FBookMarkOpt.BookmarkImages.Height) div 2;
      with FBookMarkOpt do
        BookmarkImages.Draw(Canvas, LeftMargin + aGutterOffs,
                            iTop + Line * LineHeight, DebugMarksImageIndex, True);

      Inc(aGutterOffs, FBookMarkOpt.BookmarkImages.Width);
    end
  end;

begin
  aGutterOffs := 0;
  HasAnyMark := PaintMarks(aScreenLine, Canvas, AClip, aGutterOffs);
  TxtIdx := FFoldView.TextIndex[aScreenLine];
  if (not HasAnyMark) and (HasDebugMarks) and (FDebugMarkInfo.SrcLineToMarkLine[TxtIdx] > 0)
  then
    DrawDebugMark(aScreenLine);
end;

{ TIDESynGutterMarks }

procedure TIDESynGutterMarks.SetDebugMarks(AFirstLinePos, ALastLinePos: Integer);
var
  i: LongInt;
begin
  if FDebugMarkInfo = nil then begin
    FDebugMarkInfo := TIDESynDebugMarkInfo(TIDESynEditor(SynEdit).TextBuffer.Ranges[ClassType]);
    if FDebugMarkInfo = nil then begin
      FDebugMarkInfo := TIDESynDebugMarkInfo.Create;
      TIDESynEditor(SynEdit).TextBuffer.Ranges[ClassType] := FDebugMarkInfo;
    end
    else
      FDebugMarkInfo.IncRefCount;
  end;

  for i := AFirstLinePos - 1 to ALastLinePos - 1 do
    FDebugMarkInfo[i] := i;
  TSynEdit(SynEdit).InvalidateGutter;
end;

procedure TIDESynGutterMarks.ClearDebugMarks;
begin
  if FDebugMarkInfo = nil then exit;
  FDebugMarkInfo.DecRefCount;
  if FDebugMarkInfo.RefCount = 0 then begin
    TIDESynEditor(SynEdit).TextBuffer.Ranges[ClassType] := nil;
    FreeAndNil(FDebugMarkInfo);
  end;
  FDebugMarkInfo := nil;
  TSynEdit(SynEdit).InvalidateGutter;
end;

function TIDESynGutterMarks.HasDebugMarks: Boolean;
begin
  if FDebugMarkInfo = nil then begin
    FDebugMarkInfo := TIDESynDebugMarkInfo(TIDESynEditor(SynEdit).TextBuffer.Ranges[ClassType]);
    if FDebugMarkInfo <> nil then TSynEdit(SynEdit).InvalidateGutter;
  end;
  Result := FDebugMarkInfo <> nil;
end;

{ TIDESynDebugMarkInfo }

function TIDESynDebugMarkInfo.GetSrcLineToMarkLine(SrcIndex: Integer): Integer;
begin
  Result := Integer(ItemPointer[SrcIndex]^);
end;

procedure TIDESynDebugMarkInfo.SetSrcLineToMarkLine(SrcIndex: Integer; const AValue: Integer);
begin
  Integer(ItemPointer[SrcIndex]^) := AValue;
end;

function TIDESynDebugMarkInfo.ItemSize: Integer;
begin
  Result := SizeOf(Integer);
end;

constructor TIDESynDebugMarkInfo.Create;
begin
  Inherited;
  FRefCount := 1;
end;

procedure TIDESynDebugMarkInfo.IncRefCount;
begin
  inc(FRefCount);
end;

procedure TIDESynDebugMarkInfo.DecRefCount;
begin
  dec(FRefCount);
end;

end.

