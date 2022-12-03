{
  Demonstrating how to run a thread, that produces output shown in the IDE
  Messages window.
}
unit DemoInternalCompilerReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IDECommands, MenuIntf, LazIDEIntf, IDEMsgIntf,
  IDEExternToolIntf, ProjectIntf;

type

  { TMyPascalCompiler }

  TMyPascalCompiler = class(TThread)
  protected
    procedure Execute; override;
  public
    Filename: string;
    Tool: TAbstractExternalTool;
  end;

procedure Register;

implementation

procedure RunMyPascalCompiler(Sender: TObject);
var
  Title: String;
  View: TExtToolView;
  Tool: TAbstractExternalTool;
  MyThread: TMyPascalCompiler;
  Proj: TLazProject;
begin
  Title:='Running my internal demo compiler';

  // add a view (a block of lines in the IDE's Messages window)
  View:=IDEMessagesWindow.CreateView(Title);

  // add a tool - the connection between the IDE and the thread
  Tool:=ExternalToolList.Add(Title);
  Tool.AddView(View);
  // adding a parser for the compiler messages
  Tool.AddParser(IDEFPCParser);
  if not Tool.InitParsers then exit;

  // create the thread
  MyThread:=TMyPascalCompiler.Create(true);
  MyThread.FreeOnTerminate:=true;
  MyThread.Tool:=Tool;
  Tool.Reference(MyThread,'RunMyPascalCompiler');
  Tool.UserThread:=MyThread;

  // set WorkerDirectory needed by the parser
  Proj:=LazarusIDE.ActiveProject;
  if (Proj<>nil) and (Proj.MainFile<>nil) then
    MyThread.Filename:=Proj.MainFile.Filename
  else
    MyThread.Filename:='test123.pas';
  Tool.WorkerDirectory:=ExtractFilePath(MyThread.Filename);
  if Tool.WorkerDirectory='' then
    Tool.WorkerDirectory:=GetCurrentDir;

  // start thread
  MyThread.Start;
end;

procedure Register;
var
  CmdCatTools: TIDECommandCategory;
  RunMyCompilerCmd: TIDECommand;
  MenuCaption, MenuName: String;
begin
  MenuName:='RunInternalDemoCompiler';
  MenuCaption:='Run internal demo compiler';

  // create a command
  CmdCatTools:=IDECommandList.FindCategoryByName(CommandCategoryToolMenuName);
  if CmdCatTools=nil then
    raise Exception.Create('DemoInternalCompiler: command category '+CommandCategoryToolMenuName+' not found');
  RunMyCompilerCmd:=RegisterIDECommand(CmdCatTools, MenuName, MenuCaption,
    CleanIDEShortCut,CleanIDEShortCut,nil,@RunMyPascalCompiler);

  // add a menu item
  RegisterIDEMenuCommand(itmSecondaryTools,MenuName,MenuCaption,
    nil,nil,RunMyCompilerCmd);
end;

{ TMyPascalCompiler }

procedure TMyPascalCompiler.Execute;
var
  i, j: Integer;
  sl: TStringList;
  Tick: QWord;
begin
  Tool.UserThreadRunning;
  try
    if Tool.Stage<>etsRunning then
      exit;
    j:=1;
    sl:=TStringList.Create;
    try
      for i:=1 to 30 do begin
        // emulate some load
        Tick:=GetTickCount64;
        while GetTickCount64-Tick<100 do ;
        if Tool.Stage<>etsRunning then
          exit;

        sl.Add(Filename+'('+IntToStr(j)+',1) Hint: This could be from an internal compiler');
        inc(j);

        // feed output
        Tool.AddOutputLines(sl);
        sl.Clear;
      end;
    finally
      sl.Free;
    end;

    Tool.ExitCode:=0;
    Tool.ErrorMessage:='';
  finally
    if Tool.Stage<etsStopped then
      Tool.UserThreadStopped;
    Tool.UserThread:=nil;
    Tool.Release(Self);
    Tool:=nil;
  end;
end;

end.

