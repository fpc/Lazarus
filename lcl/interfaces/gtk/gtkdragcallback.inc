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
//DRAG CALLBACK FUNCTIONS

Function edit_drag_data_received(widget : pgtkWidget;
			          Context : pGdkDragContext;
			          X : Integer;
			          Y : Integer;
			          seldata : pGtkSelectionData;
			          info : Integer;
			          time : Integer;
                                  data : pointer) : GBoolean; cdecl;
Var
  Texts : String;
Begin
  Result:=false;
  Assert(False, 'Trace:***********Drag Data Received*******************');
  if Seldata^.Length > 0 then
  Begin
    Texts := strpas(Seldata^.data);
    Assert(False, 'Trace:' + Texts);
    Assert(False, 'Trace:0');
    TEdit(Data).Caption := Texts;
    Assert(False, 'Trace:1');
  end;
  gtk_drag_finish(Context,false,false,time);
end;

Function edit_source_drag_data_get(widget : pgtkWidget;
			          Context : pGdkDragContext;
			          Selection_data : pGtkSelectionData;
			          info : Integer;
			          time : Integer;
                                  data : pointer) : GBoolean; cdecl;
var
  strTemp : PChar;
  Texts : String;
Begin
  Result:=false;
  if (info = TARGET_ROOTWIN) then
    Assert(False, 'Trace:I WAS DROPPED ON THE ROOTWIN')
  else
  Begin
    Assert(False, 'Trace:*********Setting Data************');
    Texts := TCustomEdit(data).Text;
    Assert(False, 'Trace:0');
    strTemp := StrAlloc(length(Texts) + 1);
    try
      StrPCopy(strTemp, Texts);
      Assert(False, 'Trace:1');
      gtk_selection_data_set(selection_data,selection_data^.target,
                    8,
                    strtemp,
                    length(Texts)+1);
      Assert(False, 'Trace:2');
    finally
      strDispose(strTemp);
    end;
    Assert(False,'Trace:3');
  end;
end;


Function Edit_source_drag_data_delete (widget : pGtkWidget;
			                context : pGdkDragContext;
			                data : gpointer): gBoolean ; cdecl;
begin
  Assert(False, 'Trace:***************');
  Assert(False, 'Trace:DELETE THE DATA');
  Result:=false;
end;

// included by gtkcallback.inc

