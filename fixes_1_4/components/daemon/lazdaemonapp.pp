{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit lazdaemonapp;

interface

uses daemonapp;

Type
  TLazDaemonApplication = Class(TCustomDaemonApplication)
    Procedure CreateDaemonInstance(Var ADaemon : TCustomDaemon; DaemonDef : TDaemonDef); override;
  end;

implementation

uses classes,lresources;

Procedure TLazDaemonApplication.CreateDaemonInstance(Var ADaemon : TCustomDaemon; DaemonDef : TDaemonDef); 

begin
  ADaemon:=DaemonDef.DaemonClass.Create(Self);
end;

Initialization
  RegisterInitComponentHandler(TComponent,@InitLazResourceComponent);
  RegisterDaemonApplicationClass(TLazDaemonApplication)
end.
