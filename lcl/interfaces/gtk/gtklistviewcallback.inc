// included by gtkcallback.inc

{
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
//----------------------
//LM_HSCROLL
function gtkLVHScroll(AList: PGTKCList; AData: gPointer): GBoolean; cdecl;
var
  Adjustment: PGTKAdjustment;
begin
  {$IfNDef Win32}
  Adjustment := gtk_clist_get_hadjustment(AList);
  {$Else}
  Adjustment := nil;
  {$EndIf}
  if Adjustment = nil
  then Result := False
  else Result := GTKHScrollCB(Adjustment, AData);
end;

//----------------------
//LM_VSCROLL
function gtkLVVScroll(AList: PGTKCList; AData: gPointer): GBoolean; cdecl;
var
  Adjustment: PGTKAdjustment;
begin
  {$IfNDef Win32}
  Adjustment := gtk_clist_get_vadjustment(AList);
  {$Else}
  Adjustment := nil;
  {$EndIf}
  if Adjustment = nil
  then Result := False
  else Result := GTKVScrollCB(Adjustment, AData);
end;

//----------------------
//HDN_ENDTRACK
//HDN_TRACK
function gtkLVAbortColumnResize(AList: PGTKCList; AData: gPointer): GBoolean; cdecl;
begin             
  //TODO: implement
  Result := False;
end;
     
//----------------------
//HDN_ENDTRACK
//HDN_TRACK
//HDN_ITEMCHANGED
//HDN_ITEMCHANGING 
function gtkLVResizeColumn(AList: PGTKCList; AColumn, AWidth: Integer; AData: gPointer): GBoolean; cdecl;
begin
  //TODO: implement
  Result := False;
end;

//----------------------
//HDN_ITEMCLICK
//LVN_COLUMNCLICK 
function gtkLVClickColumn(AList: PGTKCList; AColumn: Integer; AData: gPointer): GBoolean; cdecl;
var
  msg: TLMNotify;
  NM: TNMListView;
begin
  EventTrace('click-column', Adata);
  msg.Msg := CN_NOTIFY;

  FillChar(NM, SizeOf(NM), 0);
  NM.hdr.hwndfrom := longint(AList);
  NM.hdr.code := LVN_COLUMNCLICK;
  NM.iItem := -1;
  NM.iSubItem := AColumn;
  msg.NMHdr := @NM;
  Result := DeliverMessage(AData, msg) = 0;
end;

//----------------------
//LVN_DELETEITEM 
//LVN_INSERTITEM
function gtkLVRowMove(AList: PGTKCList; AnOldIdx, ANewIdx: Integer; AData: gPointer): GBoolean; cdecl;
var
  msg: TLMNotify;
  NM: TNMListView;
  r: Boolean;
begin
  // Simulate move by remove and insert

  EventTrace('row-move', Adata);
  msg.Msg := CN_NOTIFY;

  FillChar(NM, SizeOf(NM), 0);
  NM.hdr.hwndfrom := longint(AList);
  NM.hdr.code := LVN_DELETEITEM;
  NM.iItem := AnOldIdx;
  msg.NMHdr := @NM;
  r := DeliverMessage(AData, msg) = 0;
  
  NM.hdr.code := LVN_INSERTITEM;
  NM.iItem := ANewIdx;
  Result := (DeliverMessage(AData, msg) = 0) and r;
end;

//----------------------
//LVN_ITEMCHANGED
//LVN_ITEMCHANGING 
function gtkLVSelectRow(AList: PGTKCList; ARow, AColumn: Integer; AEvent: PGDKEventButton; AData: gPointer): GBoolean; cdecl;
var
  msg: TLMNotify;
  NM: TNMListView;
begin
  EventTrace('select-row', Adata);
  msg.Msg := CN_NOTIFY;

  FillChar(NM, SizeOf(NM), 0);
  NM.hdr.hwndfrom := longint(AList);
  NM.hdr.code := LVN_ITEMCHANGED;
  NM.iItem := ARow;
  NM.iSubItem := AColumn;
  NM.uNewState := LVIS_SELECTED;
  NM.uChanged := LVIF_STATE;
  msg.NMHdr := @NM;
  Result := DeliverMessage(AData, msg) = 0;
end;

function gtkLVUnSelectRow(AList: PGTKCList; ARow, AColumn: Integer; AEvent: PGDKEventButton; AData: gPointer): GBoolean; cdecl;
var
  msg: TLMNotify;
  NM: TNMListView;
begin
  EventTrace('unselect-row', Adata);
  msg.Msg := CN_NOTIFY;

  FillChar(NM, SizeOf(NM), 0);
  NM.hdr.hwndfrom := longint(AList);
  NM.hdr.code := LVN_ITEMCHANGED;
  NM.iItem := ARow;
  NM.iSubItem := AColumn;
  NM.uOldState := LVIS_SELECTED;
  NM.uChanged := LVIF_STATE;
  msg.NMHdr := @NM;
  Result := DeliverMessage(AData, msg) = 0;
end;

function gtkLVToggleFocusRow(AList: PGTKCList; AData: gPointer): GBoolean; cdecl;
//var
  //msg: TLMNotify;
  //NM: TNMListView;
begin
  EventTrace('toggle-focus-row', Adata);
  //msg.Msg := CN_NOTIFY;
(*
  TODO:
  Do we need this?
*)
  Result := False;
end;

function gtkLVSelectAll(AList: PGTKCList; AData: gPointer): GBoolean; cdecl;
var
  msg: TLMNotify;
  NM: TNMListView;      
  ListView: TListView;
  n: Integer;
begin
  EventTrace('select-all', Adata);
  msg.Msg := CN_NOTIFY;

  ListView := TObject(AData) as TListView;

  FillChar(NM, SizeOf(NM), 0);
  NM.hdr.hwndfrom := longint(AList);
  NM.hdr.code := LVN_ITEMCHANGED;
  for n := 0 to Listview.Items.Count - 1 do 
  begin
    if ListView.Items[n].Selected
    then Continue;
    NM.iItem := n;
    NM.iSubItem := -1;
    NM.uNewState := LVIS_SELECTED;
    NM.uChanged := LVIF_STATE;
    msg.NMHdr := @NM;
    Result := DeliverMessage(AData, msg) = 0;
  end;
end;

function gtkLVUnSelectAll(AList: PGTKCList; AData: gPointer): GBoolean; cdecl;
begin
  EventTrace('unselect-all', Adata);
  Result:=true;
end;

function gtkLVEndSelection(AList: PGTKCList; AData: gPointer): GBoolean; cdecl;
begin
  EventTrace('end-selection', Adata);
  Result:=true;
end;
         
// included by gtkcallback.inc

