(******************************************************************************
                                     TMemoStrings
 ******************************************************************************)

{------------------------------------------------------------------------------
  Method: TMemoStrings.Get
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
function TMemoStrings.Get(Index : Integer): String;
var
  TempStrings: TStringList;
begin
  If Assigned(FMemo) and (Index >= 0)
  then begin
    TempStrings := TStringList.Create;
    TempStrings.Text := FMemo.Text;
    if Index < TempStrings.Count
    then Result := TempStrings[Index]
    else Result := '';
    TempStrings.Free;
  end else Result := '';
end;

{------------------------------------------------------------------------------
  Method: TMemoStrings.GetCount
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
function TMemoStrings.GetCount: Integer;
var
  TempStrings: TStringList;
begin
  If Assigned(FMemo)
  then begin
    TempStrings := TStringList.Create;
    TempStrings.Text := FMemo.Text;
    Result := TempStrings.Count;
    TempStrings.Free;
  end else Result := 0;
end;

{------------------------------------------------------------------------------
  Method: TMemoStrings.Create
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
constructor TMemoStrings.Create(AMemo: TCustomMemo);
begin
  inherited Create;
  FMemo := AMemo;
end;

{------------------------------------------------------------------------------
  Method: TMemoStrings.Clear
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TMemoStrings.Clear;
begin
  FMemo.Text := '';
end;

{------------------------------------------------------------------------------
  Method: TMemoStrings.Delete
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TMemoStrings.Delete(index : Integer);
var
  TempStrings: TStringList;
begin
  If Assigned(FMemo) and (Index >= 0)
  then begin
    TempStrings := TStringList.Create;
    TempStrings.Text := FMemo.Text;
    if Index < TempStrings.Count
    then begin
      TempStrings.Delete(Index);
      FMemo.Text := TempStrings.Text;
    end;
    TempStrings.Free;
  end;
end;

{------------------------------------------------------------------------------
  Method: TMemoStrings.Insert
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TMemoStrings.Insert(index : Integer; const S: String);
var
  TempStrings: TStringList;
  St: string;
begin
  If Assigned(FMemo) and (Index >= 0)
  then begin
    TempStrings := TStringList.Create;
    TempStrings.Text := FMemo.Text;
    if Index <= TempStrings.Count
    then begin
      TempStrings.Insert(Index, S);
      FMemo.Text := TempStrings.Text;
    end;
    TempStrings.Free;
  end;
end;

{ =============================================================================

  $Log$
  Revision 1.1  2000/07/13 10:28:26  michael
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


}
