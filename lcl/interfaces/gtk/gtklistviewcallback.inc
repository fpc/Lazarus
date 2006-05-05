{%MainUnit gtkcallback.inc}

{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
//----------------------

//----------------------
//HDN_ENDTRACK
//HDN_TRACK
function gtkLVAbortColumnResize(AList: PGTKCList; AData: gPointer): GBoolean; cdecl;
begin             
  //TODO: implement
  if (AList=nil) or (AData=nil) then ;
  Result := False;
end;
     
//----------------------
//HDN_ENDTRACK
//HDN_TRACK
//HDN_ITEMCHANGED
//HDN_ITEMCHANGING 
function gtkLVResizeColumn(AList: PGTKCList; AColumn, AWidth: Integer;
  AData: gPointer): GBoolean; cdecl;
begin
  //TODO: implement
  if (AList=nil) or (AData=nil) or (AColumn=0) or (AWidth=0) then ;
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
  NM.hdr.hwndfrom := PtrInt(AList);
  NM.hdr.code := LVN_COLUMNCLICK;
  NM.iItem := -1;
  NM.iSubItem := AColumn;
  msg.NMHdr := @NM;
  Result := DeliverMessage(AData, msg) = 0;
end;

// included by gtkcallback.inc
