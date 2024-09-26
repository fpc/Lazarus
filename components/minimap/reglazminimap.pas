unit RegLazMiniMap;

{$mode objfpc}{$H+}

interface

uses
  CtrlMiniMap, Forms;

procedure register;

implementation

uses fraMiniMapConfig, IDEOptionsIntf, IDEOptEditorIntf;

var
  MiniMapOptionsFrameID : integer = 2100;

procedure register;

begin
  MiniMapController:=TMinimapController.Create(Application);
  MiniMapController.ConfigFrame:=TMiniMapConfigFrame;
  MiniMapController.LoadConfig;
  // add IDE options frame
  MiniMapOptionsFrameID:=RegisterIDEOptionsEditor(GroupEnvironment,TMiniMapConfigFrame,
                                              MiniMapOptionsFrameID)^.Index;
end;


end.

