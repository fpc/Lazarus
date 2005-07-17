{%MainUnit ../stdctrls.pp}

{******************************************************************************
                                     TCustomMemo
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
  //FLines := TMemoStrings.Create(Self);
  FLines := TStringList.Create; //TODO finish TTextStrings and add them here instead of TStringList
  //TMemoStrings(FLines).MemoWidgetClass := TWSCustomMemoClass(WidgetSetClass);
  FVertScrollbar := TMemoScrollBar.Create(Self, sbVertical);
  FHorzScrollbar := TMemoScrollBar.Create(Self, sbHorizontal);
  SetInitialBounds(0,0,150,90);
end;

{------------------------------------------------------------------------------
  Method: TCustomMemo.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCustomMemo.Destroy;
begin
  FreeThenNil(FLines);
  FreeThenNil(FVertScrollbar);
  FreeThenNil(FHorzScrollbar);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TCustomMemo.Append
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TCustomMemo.Append(const Value : String);
begin
  Lines.Add(Value);
end;

{------------------------------------------------------------------------------
  procedure TCustomMemo.Clear;
 ------------------------------------------------------------------------------}
procedure TCustomMemo.Clear;
begin
  Lines.Clear;
end;

{------------------------------------------------------------------------------
  procedure TCustomMemo.SetHorzScrollBar(const AValue: TMemoScrollBar);
 ------------------------------------------------------------------------------}
procedure TCustomMemo.SetHorzScrollBar(const AValue: TMemoScrollBar);
begin
  if FHorzScrollBar=AValue then exit;
  FHorzScrollBar:=AValue;
end;

{------------------------------------------------------------------------------
  procedure TCustomMemo.SetVertScrollBar(const AValue: TMemoScrollBar);
 ------------------------------------------------------------------------------}
procedure TCustomMemo.SetVertScrollBar(const AValue: TMemoScrollBar);
begin
  if FVertScrollBar=AValue then exit;
  FVertScrollBar:=AValue;
end;

{------------------------------------------------------------------------------
  function TCustomMemo.StoreScrollBars: boolean;
 ------------------------------------------------------------------------------}
function TCustomMemo.StoreScrollBars: boolean;
begin
  Result:=true;
end;

procedure TCustomMemo.CreateHandle;
var
  NewStrings : TStrings;
begin
  //DebugLn('[TCustomMemo.CreateHandle] A ',FLines.ClassName);
  inherited CreateHandle;
  //DebugLn('[TCustomMemo.CreateHandle] B ',DbgSName(Self),' ',FLines.ClassName,' FLines.Count=',dbgs(FLines.Count));
  
  // fetch/create the interface item list
  NewStrings := TWSCustomMemoClass(WidgetSetClass).GetStrings(Self);
  // copy the items (text)
  NewStrings.Assign(Lines);

  // free old items
  FLines.Free;

  // new item list is the interface item list
  FLines:= NewStrings;

  //DebugLn('[TCustomMemo.CreateHandle] END ',DbgSName(Self),' ',FLines.ClassName,' FLines.Count=',dbgs(FLines.Count));
end;

procedure TCustomMemo.DestroyHandle;
var
  NewStrings : TStringList;

begin
  if Assigned(FLines) then begin
    // create internal item list
    NewStrings:= TStringList.Create;

    // copy items (text+objects) from the interface items list
    NewStrings.Assign(Lines);

    FLines.Free;
    // new item list is the internal item list
    FLines:= NewStrings;
  end;
  inherited DestroyHandle;
end;

function TCustomMemo.RealGetText: TCaption;
begin
  Result := Lines.Text;
  //debugln('TCustomMemo.RealGetText "',Result,'"');
end;

procedure TCustomMemo.RealSetText(const Value: TCaption);
begin
  //debugln('TCustomMemo.RealSetText "',Value,'"');
  Lines.Text := Value;
end;

{------------------------------------------------------------------------------
  Method: TCustomMemo.SetLines
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TCustomMemo.SetLines(const Value : TStrings);
begin
  if (Value <> nil) then
    FLines.Assign(Value);
end;

{------------------------------------------------------------------------------
  procedure TCustomMemo.SetScrollbars(const Value : TScrollStyle);
 ------------------------------------------------------------------------------}
procedure TCustomMemo.SetScrollbars(const Value : TScrollStyle);
begin
  if Value <> FScrollbars then begin
    FScrollbars:= Value;
    if HandleAllocated and (not (csLoading in ComponentState)) then
      TWSCustomMemoClass(WidgetSetClass).SetScrollbars(Self, Value);
  end;
end;

{------------------------------------------------------------------------------
  procedure TCustomMemo.InitializeWnd;
 ------------------------------------------------------------------------------}
procedure TCustomMemo.InitializeWnd;
begin
  inherited InitializeWnd;
end;

{------------------------------------------------------------------------------
  procedure TCustomMemo.Loaded;
 ------------------------------------------------------------------------------}
procedure TCustomMemo.Loaded;
begin
  inherited Loaded;

  if HandleAllocated then begin
    TWSCustomMemoClass(WidgetSetClass).SetScrollbars(Self, FScrollbars);
    TWSCustomMemoClass(WidgetSetClass).SetWordWrap(Self, FWordWrap);
  end;
end;

{------------------------------------------------------------------------------
  function TCustomMemo.WordWrapIsStored: boolean;
 ------------------------------------------------------------------------------}
function TCustomMemo.WordWrapIsStored: boolean;
begin
  Result:=not WordWrap;
end;

procedure TCustomMemo.ControlKeyDown(var Key: Word; Shift: TShiftState);
begin
  if not ReadOnly then begin
    if (Key=VK_RETURN) and (Shift=[]) then exit;
    if (Key=VK_TAB) and (Shift-[ssShift]=[]) then exit;
  end;
  inherited ControlKeyDown(Key, Shift);
end;

{------------------------------------------------------------------------------
  Method: TCustomMemo.SetWordWrap
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TCustomMemo.SetWordWrap(const Value : boolean);
begin
  if Value <> FWordWrap then begin
    //DebugLn('TCustomMemo.SetWordWrap ',Name,' Old=',FWordWrap,' New=',Value);
    FWordWrap := Value;
    if HandleAllocated and (not (csLoading in ComponentState)) then
      TWSCustomMemoClass(WidgetSetClass).SetWordWrap(Self, Value);
  end;    
end;

// included by stdctrls.pp

{ =============================================================================

  $Log$
  Revision 1.35  2005/07/17 01:05:04  mattias
  started basic docking anchoring and made Contributors.txt ASCII to show everywhere

  Revision 1.34  2005/04/05 19:41:19  mattias
  accelerated TMemo on gtk, gtk2 and win32 interface  from Andrew Haines

  Revision 1.33  2005/03/10 09:06:34  mattias
  TCustomMemo: check for navigations keys only if not ReadOnly

  Revision 1.32  2005/01/01 14:38:36  mattias
  fixed loading TMemo.Lines - During Loading

  Revision 1.31  2004/11/15 12:37:52  mattias
  added check handleallocated on loading memo

  Revision 1.30  2004/09/22 16:13:01  micha
  convert LM_SETPROPERTIES message to interface methods for TCustomMemo

  Revision 1.29  2004/09/10 18:58:21  micha
  convert LM_ATTACHMENU to interface method

  Revision 1.28  2004/08/26 19:09:34  mattias
  moved navigation key handling to TApplication and added options for custom navigation

  Revision 1.27  2004/07/03 11:11:08  mattias
  TGTKListStringList now keeps selection on Put and Move

  Revision 1.26  2004/05/15 20:17:09  mattias
  replaced WMSize by DoSetBounds

  Revision 1.25  2004/05/14 12:53:25  mattias
  improved grids e.g. OnPrepareCanvas   patch from Jesus

  Revision 1.24  2004/05/11 12:16:47  mattias
  replaced writeln by debugln

  Revision 1.23  2004/04/18 23:55:39  marc
  * Applied patch from Ladislav Michl
  * Changed the way TControl.Text is resolved
  * Added setting of text to TWSWinControl

  Revision 1.22  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.21  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.19  2004/02/09 19:52:52  mattias
  implemented ByteOrder for TLazIntfImage and added call of to LM_SETFONT

  Revision 1.18  2004/01/27 17:36:01  mattias
  fixed switching menus in menueditor

  Revision 1.17  2003/11/01 18:58:15  mattias
  added clipboard support for TCustomEdit from Colin

  Revision 1.16  2003/06/13 11:58:46  mattias
  fixed readonly of properties

  Revision 1.15  2003/04/15 08:54:27  mattias
  fixed TMemo.WordWrap

  Revision 1.14  2003/03/31 10:57:40  mattias
  fixes for current fpc 1.1

  Revision 1.13  2003/03/31 10:49:50  mattias
  fixed parsing records

  Revision 1.12  2003/03/29 17:20:05  mattias
  added TMemoScrollBar

  Revision 1.11  2003/03/28 23:03:38  mattias
  started TMemoScrollbar

  Revision 1.10  2003/03/17 08:51:09  mattias
  added IsWindowVisible

  Revision 1.9  2003/03/11 07:46:43  mattias
  more localization for gtk- and win32-interface and lcl

  Revision 1.8  2002/10/30 13:50:26  lazarus
  MG: fixed message without handle

  Revision 1.7  2002/09/05 10:12:07  lazarus

  New dialog for multiline caption of TCustomLabel.
  Prettified TStrings property editor.
  Memo now has automatic scrollbars (not fully working), WordWrap and Scrollbars property
  Removed saving of old combo text (it broke things and is not needed). Cleanups.

  Revision 1.6  2002/05/10 06:05:52  lazarus
  MG: changed license to LGPL

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

