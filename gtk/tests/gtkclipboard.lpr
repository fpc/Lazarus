{
  Test gtk application for clipboard.
  
  
}
program GtkClipboard;

{$mode objfpc}{$H+}

uses
  Classes, gtk, glib, gdk;

const
  GdkTrue = {$IFDEF Gtk2}true{$ELSE}1{$ENDIF};
  GdkFalse = {$IFDEF Gtk2}false{$ELSE}0{$ENDIF};

var
  window: PGtkWidget;
  button: PGtkWidget;

function GdkAtomToStr(const Atom: TGdkAtom): string;
var
  p: Pgchar;
begin
  p:=gdk_atom_name(Atom);
  Result:=p;
  if p<>nil then g_free(p);
end;

procedure button_button_release(Widget: PGtkWidget; Event: PGdkEventButton;
  Data: Pointer); cdecl;
var
  Time: LongWord;
begin
  if Widget=nil then ;
  if Data=nil then ;
  // try to receive the primary selection
  Time:=Event^.time;
  //Time:=1001;
  gtk_selection_convert(Window,GDK_SELECTION_PRIMARY,
                        gdk_atom_intern('TARGETS',GdkFalse),Time);
end;

procedure window_selection_received(Widget: PGtkWidget;
  SelectionData: PGtkSelectionData; Time: guint; Data: Pointer); cdecl;
var
  Atoms: PTGdkAtom;
  i: Integer;
  Count: Integer;
begin
  if Widget=nil then ;
  if Time=0 then ;
  if Data=nil then ;
  
  // get the atom names
  writeln('Selection=',GdkAtomToStr(SelectionData^.selection),
    ' Target=',GdkAtomToStr(SelectionData^.target),
    ' Type=',GdkAtomToStr(SelectionData^.thetype),
    ' Format=',SelectionData^.data,
    ' Length=',SelectionData^.length);

  if (SelectionData^.data<>nil) and (SelectionData^.length>0) then begin
    writeln('TARGETS:');
    Atoms:=PTGdkAtom(SelectionData^.data);
    Count:=SelectionData^.format div 8;
    for i:=0 to Count-1 do begin
      writeln('  ',i,'/',Count,' ',GdkAtomToStr(Atoms[i]));
    end;
  end else begin
    writeln('no TARGETS');
  end;
end;

begin
  gtk_init(@argc,@argv);
  
  // window
  window:=gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(PGtkWindow(window),'Clipboard test');
  gtk_signal_connect(PGtkObject(Window),'delete_event',
    TGtkSignalFunc(@gtk_exit),nil);
  gtk_signal_connect(PGtkObject(Window),'selection_received',
    TGtkSignalFunc(@window_selection_received),nil);

  // button
  button:=gtk_button_new_with_label('Get targets');
  gtk_container_add(PGtkContainer(Window),button);
  gtk_signal_connect(PGtkObject(button),'button_release_event',
    TGtkSignalFunc(@button_button_release),nil);

  gtk_widget_show_all(window);
  gtk_main;
end.

