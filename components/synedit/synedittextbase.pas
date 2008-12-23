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
unit SynEditTextBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEditTypes;
  
type

  { TSynEditStrings }

  TSynEditStrings = class(TStrings)
  protected
    function GetFoldEndLevel(Index: integer): integer; virtual; abstract;
    function GetFoldMinLevel(Index: integer): integer; virtual; abstract;
    procedure SetFoldEndLevel(Index: integer; const AValue: integer); virtual; abstract;
    procedure SetFoldMinLevel(Index: integer; const AValue: integer); virtual; abstract;
    function GetRange(Index: integer): TSynEditRange; virtual; abstract;
    procedure PutRange(Index: integer; ARange: TSynEditRange); virtual; abstract;
    function GetExpandedString(Index: integer): string; virtual; abstract;
    function GetLengthOfLongestLine: integer; virtual; abstract;
    procedure SetTextStr(const Value: string); override;
  public
    procedure DeleteLines(Index, NumLines: integer); virtual; abstract;
    procedure InsertLines(Index, NumLines: integer); virtual; abstract;
    procedure InsertStrings(Index: integer; NewStrings: TStrings); virtual; abstract;
    procedure ClearRanges(ARange: TSynEditRange); virtual; abstract;
  public
    property ExpandedStrings[Index: integer]: string read GetExpandedString;
    property LengthOfLongestLine: integer read GetLengthOfLongestLine;
    property Ranges[Index: integer]: TSynEditRange read GetRange write PutRange;
    property FoldMinLevel[Index: integer]: integer read GetFoldMinLevel
                                                   write SetFoldMinLevel;
    property FoldEndLevel[Index: integer]: integer read GetFoldEndLevel
                                                   write SetFoldEndLevel;
  end;


implementation


{ TSynEditStrings }

procedure TSynEditStrings.SetTextStr(const Value : string);
var
  StartPos: Integer;
  p: Integer;
  Len: Integer;
  sl: TStringList;
begin
  BeginUpdate;
  sl:=TStringList.Create;
  try
    Clear;
    p:=1;
    StartPos:=p;
    Len:=length(Value);
    while p<=Len do begin
      if not (Value[p] in [#10,#13]) then begin
        inc(p);
      end else begin
        sl.Add(copy(Value,StartPos,p-StartPos));
        inc(p);
        if (p<=Len) and (Value[p] in [#10,#13]) and (Value[p-1]<>Value[p]) then
          inc(p);
        StartPos:=p;
      end;
    end;
    if StartPos<=Len then
      sl.Add(copy(Value,StartPos,Len-StartPos+1));
    AddStrings(sl);
  finally
    sl.Free;
    EndUpdate;
  end;
end;

end.

