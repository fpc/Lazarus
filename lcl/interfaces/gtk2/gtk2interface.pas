{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package GTK2Interface 0.0.
}

unit GTK2Interface; 

interface

uses

      Interfaces, Gtk2Int, GTKGlobals, GtkMsgQueue, GTKDef, GtkInt, GTKProc, GTKWinapiWindow, LazarusPackageIntf; 

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage ( 'GTK2Interface', @Register )
end.
