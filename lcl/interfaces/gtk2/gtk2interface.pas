{  This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package GTK2Interface 0.0.
 }

unit GTK2Interface; 

interface

uses
  Gtk2Int, Interfaces, GTKDef, GTKGlobals, GtkInt, GtkMsgQueue, GTKProc, 
    GTKWinapiWindow, LazarusPackageIntf; 

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('GTK2Interface', @Register); 
end.
