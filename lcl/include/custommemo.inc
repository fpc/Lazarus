// included by stdctrls.pp

(******************************************************************************
                                     TCustomMemo
 ******************************************************************************)
{------------------------------------------------------------------------------
  Method: TCustomMemo.Append
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TCustomMemo.Append(Value : String);
begin
//TODO: Custommemo.append
end;

{------------------------------------------------------------------------------
  Method: TCustomMemo.SetLines
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TCustomMEmo.SetLines(Value : TStrings);
begin
   if Value <> nil then
   begin
      Assert(False, 'Trace:Assigning Lines');
      FLines.Assign(Value);
      Assert(False, 'Trace:Assigned Lines');
   end;
end;

{------------------------------------------------------------------------------
  Method: TCustomMemo.Create
  Params: 
  Returns: 

  Constructor for the class
 ------------------------------------------------------------------------------}
constructor TCustomMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csMemo;
  FWordWrap := True;
  FFont := TFont.Create;
  FLines := TMemoStrings.Create(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomMemo.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCustomMemo.Destroy;
begin
  FLines.Free;
  FFont.Free;
  inherited destroy;
end;

{------------------------------------------------------------------------------
  Method: TCustomMemo.SetWordWrap
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TCustomMemo.SetWordWrap(Value : Boolean);
begin
   FWordWrap := Value;
//TODO:Call sendmessage
end;

// included by stdctrls.pp

{ =============================================================================

  $Log$
  Revision 1.5  2002/04/18 08:09:03  lazarus
  MG: added include comments

  Revision 1.4  2002/03/25 17:59:20  lazarus
  GTK Cleanup
  Shane

  Revision 1.3  2001/11/12 07:56:56  lazarus
  MG: free FFont on destroy

  Revision 1.2  2001/01/10 20:12:29  lazarus
  Added the Nudge feature to the IDE.
  Shane

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.4  2000/06/16 13:33:21  lazarus
  Created a new method for adding controls to the toolbar to be dropped onto the form!
  Shane

  Revision 1.3  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

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

