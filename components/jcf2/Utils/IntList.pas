{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is IntList.pas, released April 2000.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

unit IntList;

{ AFS 2 April 2K
  A old concept, and a generic class
  implemented here because I needed it in the LineBreaker

  * Change by JuMa to use TIntegerList from LazUtils as a base class. *

  This class could be extended in mnay ways (e.g. math ops on the list like sum, min, max, avg etc)
  Fns to add without duplicates, Sort, add another int list (and intersection, difference) etc
  But I don't need any of that right now.
}


{$I JcfGlobal.inc}

interface

uses Classes, SysUtils, IntegerList;

type

  TIntList = class(TIntegerList)
  public
    procedure ChangeValue(const liIndex, liDelta: integer);
    function IndexOfMax: integer;
    { to use it as a stack }
    function Top: integer;
    function Pop: integer;
  end;

implementation

{ TIntList }

procedure TIntList.ChangeValue(const liIndex, liDelta: integer);
begin
  { can fall out of bounds, easiest to ignore it here }
  if (liIndex < 0) or (liIndex >= Count) then
    exit;

  Items[liIndex] := Items[liIndex] + liDelta;
end;

function TIntList.IndexOfMax: integer;
var
  liItem, liMax: integer;
  liLoop: integer;
begin
  Result := -1;
  liMax  := Low(integer);

  for liLoop := 0 to Count - 1 do
  begin
    liItem := Items[liLoop];
    { the >= is a hack kinda but for the purposes of the linebreaker,
      in case of a tie, take the last item }

    if liItem >= liMax then
    begin
      liMax  := liItem;
      Result := liLoop;
    end;
  end;
end;

function TIntList.Pop: integer;
begin
  if Count > 0 then
  begin
    Result := Items[Count - 1];
    Delete(Count - 1);
  end
  else
    Result := 0;
end;

function TIntList.Top: integer;
begin
  if Count > 0 then
    Result := Items[Count - 1]
  else
    Result := 0;
end;

end.
