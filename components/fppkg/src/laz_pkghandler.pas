unit laz_pkghandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pkghandler, pkgglobals, pkgmessages;

procedure Laz_ExecuteAction(const APackageName,AAction:string);

implementation

procedure Laz_ExecuteAction(const APackageName,AAction:string);
var
  pkghandlerclass : TPackageHandlerClass;
begin
  // Create action handler class
  pkghandlerclass:=GetPkgHandler(AAction);
  With pkghandlerclass.Create(nil,APackageName) do
    try
      Log(vlDebug,SLogRunAction+' start',[AAction]);
      Execute;
      Log(vlDebug,SLogRunAction+' end',[AAction]);
    finally
      Free;
    end;
end;

end.

