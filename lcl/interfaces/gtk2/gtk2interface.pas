{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet 
  werden!
  Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit GTK2Interface; 

interface

uses
  Gtk2Int, Interfaces, GtkDef, GTKGlobals, GtkInt, GtkMsgQueue, GTKProc, 
  GTKWinapiWindow, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('GTK2Interface', @Register); 
end.
