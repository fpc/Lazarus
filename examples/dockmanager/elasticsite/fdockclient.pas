unit fDockClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs; 

type
  TDockingClient = class(TForm)
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  DockingClient: TDockingClient;

implementation

initialization
  {$I fdockclient.lrs}

end.


