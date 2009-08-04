program stutter;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, gtk2, gdk2, gdk2pixbuf, glib2;

var
  LastCall: TDateTime;
  FrameCount: Integer;

function gtkExposeEventAfter(Widget: PGtkWidget; Event : PGDKEventExpose;
  Data: gPointer): GBoolean; cdecl;
var
  children: PGList;
  n: TDateTime;
begin
  Result:=false;
  inc(FrameCount);
  n:=Now;
  if (n-LastCall)*86400>1 then begin
    writeln('gtkExposeEventAfter ',FrameCount);
    FrameCount:=0;
    LastCall:=LastCall+(1/86400);
  end;
end;

var
  GtkWindow: PGtkWidget;
  GtkFixed: PGtkWidget;
  GtkCheckButton: PGtkWidget;
  FixedName: String;
  CheckButtonName: String;
begin
  FrameCount:=0;
  LastCall:=Now;

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
  gtk_fixed_put(PGtkFixed(GtkFixed),GtkCheckButton,300,300);


  gtk_widget_show_all(GtkWindow);

  repeat
    // process all gtk messages
    gtk_main_iteration_do(True);
    // idle
    // invalidate
    gtk_widget_queue_clear_area(GtkFixed,0,0,1000,1000);
    gtk_widget_queue_draw_area(GtkFixed,0,0,1000,1000);
  until false;
  //gtk_main;
end.

