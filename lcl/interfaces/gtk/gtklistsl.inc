{******************************************************************************
                                 gtklistsl.inc
                 TGtkListStringList and TGtkCListStringList
                 
 ******************************************************************************
 
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{$IFOPT H+} 
  {$DEFINE H_PLUS} 
{$ELSE}
  {$H+}
  {$UNDEF H_PLUS}
{$ENDIF} 

{*************************************************************}
{                      Default compare function               }
{*************************************************************}

function DefaultCompareFunc(a, b : gpointer) : gint; cdecl;
var AStr, BStr : PChar;
begin
  gtk_label_get(PGtkLabel(PGtkBin(a)^.child), @AStr);
  gtk_label_get(PGtkLabel(PGtkBin(b)^.child), @BStr);
//writeln('DefaultCompareFunc "',AStr,'" "',BStr,'"');
  Result:= strcomp(AStr, BStr);
end;

{*************************************************************}
{                      TGtkListStringList methods             }
{*************************************************************}

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Create
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
constructor TGtkListStringList.Create(List : PGtkList);
begin
  inherited Create;
  if List = nil then raise Exception.Create(
    'TGtkListStringList.Create Unspecified list widget');
  FGtkList:= List;
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.SetSorted
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.SetSorted(Val : boolean);
begin
  if Val <> FSorted then begin
    FSorted:= Val;
    if FSorted then Sort;
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Sort
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.Sort;
var sl: TStringList;
begin
  // sort internally (sorting in the widget would be slow and unpretty ;)
  sl:=TStringList.Create;
  sl.Assign(Self);
  sl.Sort; // currently this is quicksort -> 
             // Disadvantages: - worst case on sorted list
             //                - not keeping order
             // ToDo: replace by mergesort and add customsort
  Assign(sl);
  sl.Free;
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Assign
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.Assign(Source : TPersistent);
var Counter : integer;
begin
  if (Source=Self) or (Source=nil) 
  or ((Source is TGtkListStringList) 
  and (TGtkListStringList(Source).FGtkList=FGtkList)) then exit;
//writeln('[TGtkListStringList.Assign] A ',Source.Classname,' Self=',HexStr(Cardinal(Self),8),' Source=',HexStr(Cardinal(Source),8));
  { Do not call inherited Assign as it does things we do not want to happen }
  if Source is TStrings then begin
    Clear;
    for Counter:= TStrings(Source).Count - 1 downto 0 do begin
      Insert(0, TStrings(Source)[Counter]);
    end;
  end else inherited Assign(Source);
//writeln('[TGtkListStringList.Assign] END ',Source.Classname);
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Get
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
function TGtkListStringList.Get(Index : integer) : string;
var Item : PChar;
      ALabel : PGtkWidget;
      ListItem : PGtkListItem;
begin
//writeln('[TGtkListStringList.Get] Index=',Index,' Count=',Count);
  if (Index < 0) or (Index >= Count) then raise Exception.Create('Out of bounds.')
  else begin
    if FGtkList=nil then begin
      Result:='';
      exit;
    end;
    ListItem:= g_list_nth_data(FGtkList^.children, Index);
    ALabel:= PGtkBin(ListItem)^.child;
    if ALabel = nil then
      Result:= ''
    else begin
      Item:=nil;
      gtk_label_get (PGtkLabel(ALabel), @Item);
      Result:= StrPas(Item);
    end;  
  end;      
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.GetCount
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
function TGtkListStringList.GetCount: integer;
begin
  if FGtkList^.children = nil then
    Result:= 0
  else
    Result:= g_list_length(FGtkList^.children);
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Clear
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.Clear;
begin
  gtk_list_clear_items(FGtkList, 0, Count);
end;  

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Delete
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.Delete(Index : integer);
begin
  gtk_list_clear_items(FGtkList, Index, Index + 1);
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Insert
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.Insert(Index : integer; const S : string);
var li : PGtkWidget;
  l, m, r, cmp: integer;
begin
  if FSorted then begin
    l:=0;
    r:=Count-1;
    m:=l;
    while (l<=r) do begin
      m:=(l+r) shr 1;
      cmp:=AnsiCompareText(S,Strings[m]);

      if cmp<0 then
        r:=m-1
      else if cmp>0 then
        l:=m+1
      else
        break;
    end;
    if (m<Count) and (AnsiCompareText(S,Strings[m])>0) then
      inc(m);
    Index:=m;
  end;
  if (Index < 0) or (Index > Count) then 
    raise Exception.Create('TGtkListStringList.Insert: Index '+IntToStr(Index)
      +' out of bounds. Count='+IntToStr(Count));

  // ToDo:
  //  - CustomDraw needs expose/draw events
  //  - Icons
  //  - measure item
  
  li:=gtk_list_item_new_with_label(PChar(S));
  gtk_widget_show(li);
  gtk_list_insert_items(FGtkList, g_list_append(nil, li), Index);
//writeln('[TGtkListStringList.Insert] END Index=',Index,' Count=',Count,' ',S,',',Count);
end;



{*************************************************************}
{                      TGtkCListStringList methods            }
{*************************************************************}

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.Create
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
constructor TGtkCListStringList.Create(List : PGtkCList);
begin
  inherited Create;
  if List = nil then raise Exception.Create('Unspecified list widget');
  FGtkCList:= List;
end;

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.SetSorted
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkCListStringList.SetSorted(Val : boolean);
begin
  if Val <> FSorted then begin
    FSorted:= Val;
    gtk_clist_set_auto_sort(FGtkCList, Val);
    if Val then Sort;
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.Sort
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkCListStringList.Sort;
begin
  gtk_clist_sort(FGtkCList);
end;

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.Assign
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkCListStringList.Assign(Source : TPersistent);
var
  Counter : integer;
begin
  { Do not call inherited Assign as it does things we do not want to happen }
  if Source is TStrings 
  then begin
    Clear;
    for Counter:= TStrings(Source).Count - 1 downto 0 do 
      InsertObject(0, TStrings(Source)[Counter], TStrings(Source).Objects[Counter]);
  end 
  else inherited Assign(Source);
end;

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.Clear
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkCListStringList.Clear;
begin
  gtk_clist_clear(FGtkCList);
end;

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.Delete
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkCListStringList.Delete(Index : integer);
begin
  gtk_clist_remove(FGtkCList, Index);
end;

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.Get
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
function TGtkCListStringList.Get(Index : integer) : string;
var 
  Item : PChar;
begin
  if (Index < 0) or (Index >= Count) 
  then raise Exception.Create('Out of bounds.')
  else begin
    Item := nil;
    gtk_clist_get_text(FGtkCList, Index, 0, @Item);
    Result:= StrPas(Item);
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.GetCount
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
function TGtkCListStringList.GetCount : integer;
begin
  Result:= FGtkCList^.rows;
end;

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.GetObject
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
function TGtkCListStringList.GetObject(Index: Integer): TObject;
begin
  pointer(Result) := gtk_clist_get_row_data(FGtkCList, Index);
end;

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.Insert
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkCListStringList.Insert(Index : integer; const S : string);
type
  TCSArr = record  
    Arr: array[0..15] of PChar;
    Str: array[0..0] of Char;
  end;
var
  CS: ^TCSArr;
  CSize: integer;
  K:     integer;
begin
  CSize := sizeof(TCSArr)+length(S)+1;
  GetMem(CS, CSize);
  FillChar(CS^, sizeof(TCSArr), 0);
  StrPCopy(CS^.Str, S);
  CS^.Arr[0] := @CS^.Str;
  for K := 1 to 15 do begin
    CS^.Arr[K] := StrScan(CS^.Arr[K-1], #9);
    if Assigned(CS^.Arr[K]) then begin
      CS^.Arr[K][0] := #0;
      inc(integer(CS^.Arr[K]));
    end else
      break;
  end;
  gtk_clist_insert(FGtkCList, Index, PPGChar(CS));
  FreeMem(CS);
end;

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.PutObject
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkCListStringList.PutObject(Index: Integer; AObject: TObject);
begin
  gtk_clist_set_row_data(FGtkCList, Index, AObject);
end;

{$IFDEF H_PLUS}
  {$UNDEF H_PLUS}
{$ELSE}
  {$H-}
{$ENDIF}

{ =============================================================================

  $Log$
  Revision 1.7  2002/09/07 20:30:50  lazarus
  Make TComboboxes sort again, including in OI

  Revision 1.6  2002/08/29 00:07:02  lazarus
  MG: fixed TComboBox and InvalidateControl

  Revision 1.5  2002/05/10 06:05:57  lazarus
  MG: changed license to LGPL

  Revision 1.4  2001/11/27 15:06:13  lazarus
  MG: added multi language syntax hilighting

  Revision 1.3  2001/10/31 10:38:26  lazarus
  MG: fixed sorted customlistbox

  Revision 1.2  2001/09/30 08:34:52  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.1  2000/07/13 10:28:29  michael
  + Initial import

  Revision 1.2  2000/04/13 21:25:16  lazarus
  MWE:
    ~ Added some docu and did some cleanup.
  Hans-Joachim Ott <hjott@compuserve.com>:
    * TMemo.Lines works now.
    + TMemo has now a property Scrollbar.
    = TControl.GetTextBuf revised :-)
    + Implementation for CListBox columns added
    * Bug in TGtkCListStringList.Assign corrected.

  Revision 1.1  2000/03/30 22:51:42  lazarus
  MWE:
    Moved from ../../lcl

  Revision 1.3  2000/03/04 00:05:21  lazarus
  MWE: added changes from Hans (HJO)

}
