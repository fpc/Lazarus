program CheckButtonOnFixed;

{$mode objfpc}{$H+}

uses
  Classes, Gtk2, Gdk2, Glib2;

function gtkExposeEventAfter(Widget: PGtkWidget; Event : PGDKEventExpose;
  Data: gPointer): GBoolean; cdecl;
var
  children: PGList;
begin
  Result:=false;
  writeln('gtkExposeEventAfter ',PChar(Data));

  // Some widgets in gtk2 don't have their own exclusive "windows" so a synthetic event must be sent
  // MG: That is already done by the gtk2. For which widgets does this not work?
  //     Enabling this results in double painting, which is slower and
  //     wrong for anitaliased text.
  if GTK_IS_FIXED(Widget) then begin
    children := gtk_container_get_children(PGtkContainer(Widget));
    while children <> nil do begin
      if (children^.data <> nil) then begin
        if GTK_WIDGET_NO_WINDOW(PGtkWidget(children^.data)) then
          gtk_container_propagate_expose(PGtkContainer(Widget), PGtkWidget(children^.data), Event);
        if children^.next = nil then break;
        children := children^.next;
      end;
    end;
    g_list_free(children);
  end;
end;

var
  GtkWindow: PGtkWidget;
  GtkFixed: PGtkWidget;
  GtkCheckButton: PGtkWidget;
  FixedName: String;
  CheckButtonName: String;
begin
  gtk_init(@ARGC,@ARGV);
  GtkWindow:=gtk_window_new(GTK_WINDOW_TOPLEVEL);
  GtkFixed := gtk_fixed_new ();
  gtk_fixed_set_has_window(PGtkFixed(GtkFixed), True);
  FixedName:='GtkFixed';
  g_signal_connect_after(PGtkObject(GtkFixed), 'expose-event',
                         TGTKSignalFunc(@gtkExposeEventAfter), PChar(FixedName));

  gtk_container_add (PGtkContainer(GtkWindow), GtkFixed);
  
  GtkCheckButton:=gtk_check_button_new_with_label('Second');
  CheckButtonName:='GtkCheckButton';
  g_signal_connect_after(PGtkObject(GtkCheckButton), 'expose-event',
                         TGTKSignalFunc(@gtkExposeEventAfter), PChar(CheckButtonName));
  gtk_fixed_put(PGtkFixed(GtkFixed),GtkCheckButton,10,10);
  
  gtk_widget_show_all(GtkWindow);
  gtk_main;
end.

