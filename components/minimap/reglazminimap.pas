unit RegLazMiniMap;

{$mode objfpc}{$H+}

interface

uses
  CtrlMiniMap, Forms;

procedure Register;

implementation

uses fraMiniMapConfig, IDEOptionsIntf, IDEOptEditorIntf;

var
  MiniMapOptionsFrameID : integer = 2100;

procedure Register;

begin
  MiniMapController:=TMinimapController.Create(Application);
  MiniMapController.ConfigFrame:=TMiniMapConfigFrame;
  MiniMapController.LoadConfig;
  // add IDE options frame
  MiniMapOptionsFrameID:=RegisterIDEOptionsEditor(GroupEditor,TMiniMapConfigFrame,
                                              MiniMapOptionsFrameID)^.Index;
end;


end.

