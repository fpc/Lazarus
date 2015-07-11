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
      {$IF FPC_FULLVERSION > 20602}
      Log(llDebug,SLogRunAction+' start',[AAction]);
      Execute;
      Log(llDebug,SLogRunAction+' end',[AAction]);
      {$ELSE}
      Log(vlDebug,SLogRunAction+' start',[AAction]);
      Execute;
      Log(vlDebug,SLogRunAction+' end',[AAction]);
      {$ENDIF}
    finally
      Free;
    end;
end;

end.

