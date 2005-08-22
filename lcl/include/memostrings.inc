{%MainUnit ../stdctrls.pp}

{******************************************************************************
                                     TMemoStrings
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

{
  $Log$
  Revision 1.12  2005/01/01 14:38:36  mattias
  fixed loading TMemo.Lines - During Loading

  Revision 1.11  2004/09/10 17:59:57  micha
  convert LM_APPENDTEXT to interface method

  Revision 1.10  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.9  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.7  2003/04/16 06:27:12  mattias
  fixed appending text to TMemo

  Revision 1.6  2003/04/15 08:54:27  mattias
  fixed TMemo.WordWrap

  Revision 1.5  2003/04/08 00:09:03  mattias
  added LM_APPENDTEXT from hernan

  Revision 1.4  2002/05/10 06:05:53  lazarus
  MG: changed license to LGPL

  Revision 1.3  2002/04/18 08:09:03  lazarus
  MG: added include comments

  Revision 1.2  2001/06/14 14:57:58  lazarus
  MG: small bugfixes and less notes

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
