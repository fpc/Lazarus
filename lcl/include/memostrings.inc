{%MainUnit ../stdctrls.pp}

{******************************************************************************
                                     TMemoStrings
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

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
procedure TMemoStrings.Insert(Index: Integer; const S: String);
var
  TempStrings: TStringList;
  Cnt: Integer;
  LastLine: String;
  CurText: String;
begin
  If Assigned(FMemo) and (Index >= 0)
  then begin
    TempStrings := TStringList.Create;
    CurText := FMemo.Text;
    TempStrings.Text := CurText;
    Cnt:=TempStrings.Count;
    //debugln('TMemoStrings.Insert A Index=',dbgs(Index),' Cnt=',dbgs(Cnt),' S="',S,'" FMemo.HandleAllocated=',dbgs(FMemo.HandleAllocated));
    if Index=Cnt then begin
      // IMPORTANT:
      // TControl.FCaption is only updated with FMemo.Text:=s calls
      // That's why, FMemoWidgetClass.AppendText does not update
      // TControl.FCaption and can not be used during loading.
      if (FMemo.HandleAllocated) and (not (csLoading in FMemo.ComponentState))
      then begin
        LastLine:=S+LineEnding;
        if (CurText<>'') and (not (CurText[length(CurText)] in [#10,#13])) then
          LastLine:=LineEnding+LastLine;
        //debugln('TMemoStrings.Insert AppendText LastLine="',LastLine,'"');
        FMemoWidgetClass.AppendText(FMemo, LastLine);
      end else begin
        TempStrings.Insert(Index, S);
        FMemo.Text:=TempStrings.Text;
      end;
    end
    else if Index < Cnt then
    begin
      TempStrings.Insert(Index, S);
      FMemo.Text := TempStrings.Text;
    end;
    TempStrings.Free;
  end;
end;

// included by stdctrls.pp
