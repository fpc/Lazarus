unit LvlGraph_Dep_Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, LvlGraphCtrl;

type

  { TForm1 }

  TForm1 = class(TForm)
    LvlGraphControl1: TLvlGraphControl;
    Panel1: TPanel;
    Splitter1: TSplitter;
    TIPropertyGrid1: TTIPropertyGrid;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  with LvlGraphControl1 do begin
    Graph.GetEdge('-Project-','LCL',true);
    Graph.GetEdge('-Project-','Cody',true);
    Graph.GetEdge('Cody','IDEIntf',true);
    Graph.GetEdge('IDEIntf','LCL',true);
    Graph.GetEdge('IDEIntf','LazControls',true);
    Graph.GetEdge('LazControls','LCL',true);
    Graph.GetEdge('LCL','LCLBase',true);
    Graph.GetEdge('LCLBase','LazUtils',true);
    Graph.GetEdge('LazUtils','FCL',true);
    Graph.GetEdge('Cody','CodeTools',true);
    Graph.GetEdge('CodeTools','LazUtils',true);
    Graph.GetEdge('Cody','LazUtils',true);
    Graph.GetEdge('-Project-','OpenGLControl',true);
    Graph.GetEdge('OpenGLControl','LCL',true);
  end;

  TIPropertyGrid1.TIObject:=LvlGraphControl1;
end;

end.

