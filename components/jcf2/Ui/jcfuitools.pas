unit JcfUiTools;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, System.UITypes, ParseTreeNode;

type

  TJcfUIBase=class
  public
    procedure UpdateGUI(aCounter:integer=0;aUpdateInterval:integer=512);virtual;abstract;
    function MessageDlgUI(const aMessage:string):TModalResult;virtual;abstract;
    procedure ShowErrorMessageUI(const aMessage:string);virtual;abstract;
    procedure SetWaitCursorUI;virtual;abstract;
    procedure RestoreCursorUI;virtual;abstract;
    procedure ShowParseTreeUI(const pcRoot: TParseTreeNode);virtual;abstract;
    function  OpenDocumentUI(const aPath:string):boolean;virtual;abstract;
  end;

procedure SetJcfUiClass(AUI:TJcfUIBase);
function GetUI:TJcfUIBase;

implementation

var
  gJcfUI:TJcfUIBase=nil;

procedure SetJcfUiClass(AUI:TJcfUIBase);
begin
  if Assigned(gJcfUI) and (gJcfUI<>AUI) then
    gJcfUI.Free;
  gJcfUI:=aUI;
end;

function GetUI:TJcfUIBase;
begin
  if gJcfUI=nil then
    raise Exception.Create('JCF User interface class not assigned, you must call SetJcfUiClass(TJcfUI_xxx.Create) at program start.');
  result:=gJcfUI;
end;

finalization
  gJcfUI.Free;
  gJcfUI:=nil;
end.

