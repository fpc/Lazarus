(******************************************************************************
                                 gtklistsl.inc
                 TGtkListStringList and TGtkCListStringList
                 
 ******************************************************************************)

{$IFOPT H+} 
  {$DEFINE H_PLUS} 
{$ELSE}
  {$H+}
{$ENDIF} 

{*************************************************************}
{                      Default compare function               }
{*************************************************************}

function DefaultCompareFunc(a, b : gpointer) : gint; cdecl;
var AStr, BStr : PChar;
begin
  gtk_label_get(PGtkLabel(PGtkBin(a)^.child), @AStr);
  gtk_label_get(PGtkLabel(PGtkBin(b)^.child), @BStr);
  Result:= strcomp(AStr, BStr);
end;

{*************************************************************}
{                      TGtkListStringList methods                                          }
{*************************************************************}

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Create
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
constructor TGtkListStringList.Create(List : PGtkList);
begin
  inherited Create;
  if List = nil then raise Exception.Create('Unspecified list widget');
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
    Sort;
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Sort
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.Sort;
begin
  FGtkList^.children:= g_list_sort(FGtkList^.children, @DefaultCompareFunc);
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Assign
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.Assign(Source : TPersistent);
var Counter : integer;
begin
  { Do not call inherited Assign as it does things we do not want to happen }
  if Source is TStrings then begin
    Clear;
    for Counter:= TStrings(Source).Count - 1 downto 0 do Insert(0, TStrings(Source)[Counter]);
  end else inherited Assign(Source);
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
  if (Index < 0) or (Index >= Count) then raise Exception.Create('Out of bounds.')
  else begin
      ListItem:= g_list_nth_data(FGtkList^.children, Index);
      ALabel:= PGtkBin(ListItem)^.child;
      if ALabel = nil then Result:= ''
      else begin
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
  if FGtkList^.children = nil then Result:= 0
  else Result:= g_list_length(FGtkList^.children);
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
begin
  li:= gtk_list_item_new_with_label(PChar(S));
  gtk_widget_show(li);
  gtk_list_insert_items(FGtkList, g_list_append(nil, li), Index);
  if FSorted then Sort;
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
  CSize := sizeof(TCSArr)+length(S);
  getmem(CS, CSize);
  fillchar(CS^, sizeof(TCSArr), 0);
  StrPCopy(CS^.Str, S);
  CS^.Arr[0] := @CS^.Str;
  for K := 1 to 15
  do begin
    CS^.Arr[K] := StrScan(CS^.Arr[K-1], #9);
    if Assigned(CS^.Arr[K])
    then begin
      CS^.Arr[K][0] := #0;
      inc(integer(CS^.Arr[K]));
    end else break;
  end;
  gtk_clist_insert(FGtkCList, Index, PPGChar(CS));
  freemem(CS, CSize);
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
