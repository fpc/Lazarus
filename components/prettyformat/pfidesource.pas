unit pfidesource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,LCLtype;

Procedure PrettyPrintSelection(Sender : TObject);
Procedure PrettyPrintFile(Sender : TObject);

Procedure Register;

implementation

uses menuintf, idecommands, srceditorintf, ptopu;

Const
  SCmdPFSelection = 'PrettyFormatSelection';
  SCmdPFFile      = 'PrettyFormatFile';
  SCatFormatting  = 'Formatting';

Resourcestring
  SDescrPFSelection = 'Pretty-Format Selection';
  SDescrPFFile      = 'Pretty-Format File';
  SDescrFormatting  = 'Formatting commands';

Var
  CmdFormatSelection : TIDECommand;
  CmdFormatFile      : TIDECommand;

Procedure Register;

Var
  Key : TIDEShortCut;
  Cat : TIDECommandCategory;
  
begin
  Key:=IDEShortCut(VK_P,[SSctrl,ssShift],VK_UNKNOWN,[]);
{$ifndef USECustomCategory}
  Cat:=IDECommandList.CreateCategory(Nil,
                                    SCatFormatting,
                                    SDescrFormatting,
                                    IDECmdScopeSrcEditOnly);
{$else}
  cat:=nil,
{$endif}
  CmdFormatSelection:=RegisterIDECommand(Cat,
                                         SCmdPFSelection,
                                         SDescrPFSelection,
                                         Key,nil,@PrettyPrintSelection);
  Key:=IDEShortCut(VK_P,[SSctrl,ssAlt],VK_UNKNOWN,[]);
  CmdFormatFile:=RegisterIDECommand(Cat,
                                    SCmdPFFile,
                                    SDescrPFFile,
                                    Key,nil,@PrettyPrintFile);
  RegisterIDEMenuCommand(SrcEditSubMenuRefactor,
                         SCmdPFSelection,
                         SDescrPFSelection,
                         Nil,nil,CmdFormatSelection);
  RegisterIDEMenuCommand(SrcEditSubMenuRefactor,
                         SCmdPFFile,
                         SDescrPFFile,
                         Nil,nil,CmdFormatFile);
  RegisterIDEMenuCommand(itmEditBlockIndentation,
                         SCmdPFSelection,
                         SDescrPFSelection,
                         Nil,nil,CmdFormatSelection);
  RegisterIDEMenuCommand(itmEditBlockIndentation,
                         SCmdPFFile,
                         SDescrPFFile,
                         Nil,nil,CmdFormatFile);
end;

Procedure PrettyPrintStream(SIn,SOut : TStream);

Var
  PP : TPrettyPrinter;

begin
  PP:=TPrettyPrinter.Create;
  Try
    PP.Source:=Sin;
    PP.Dest:=Sout;
    PP.PrettyPrint;
  Finally
    PP.Free;
  end;
end;

Procedure PrettyPrintSelection(Sender : TObject);

Var
  S1,S2 : TSTringStream;
  E : TSourceEditorInterface;

begin
  if Sender=nil then ;
  E:=SourceEditorWindow.ActiveEditor;
  If (E=Nil) or (Not E.SelectionAvailable) then
    Exit;
  S1:=TStringStream.Create(E.Selection);
  Try
    S2:=TStringStream.Create('');
    Try
      S1.Position:=0;
      PrettyPrintStream(S1,S2);
      E.Selection:=S2.DataString;
    Finally
      S2.Free;
    end;
  Finally
    S1.Free;
  end;
end;

Procedure PrettyPrintFile(Sender : TObject);

Var
  S1 : TMemoryStream;
  S2 : TStringStream;
  E  : TSourceEditorInterface;

begin
  if Sender=nil then ;
  E:=SourceEditorWindow.ActiveEditor;
  If (E=Nil) then
    Exit;
  S1:=TMemoryStream.Create;
  Try
    E.Lines.SaveToStream(S1);
    S1.Position:=0;
    S2:=TStringStream.Create('');
    Try
      PrettyPrintStream(S1,S2);
      S2.Position:=0;
      E.ReplaceLines(0, E.LineCount, S2.DataString);
    Finally
      S2.Free;
    end;
  Finally
    S1.Free;
  end;
end;

end.

