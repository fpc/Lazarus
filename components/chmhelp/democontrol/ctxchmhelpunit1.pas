{
Abstract:
  This example demonstrates the chm help components.

  TCHMLHelpDatabase handles help for a single chm file - it contains the
  mapping from Keyword to page.

  TLHelpConnector starts "lhelp" a chm viewer written in pure pascal.

How to run this program:
    Make sure the example.chm file is created
    You can create the example.chm with
    ../../../tools/chmaker/chmmaker.lpi
    (see the README.txt in the chmmaker directory).

    Run the program.

    Focus the edit field and press F1. lhelp will be started. lhelp will load
    example.chm and shows the page MainPage.html.

How was the example created:
    Put a TCHMHelpDatabase on a form.
    Set AutoRegister to true.
    Set KeywordPrefix to 'example'
    Set Filename to the path of the chm file '../../../tools/chmaker/example.chm'

    Put a TLHelpConnector on the form.
    Set AutoRegister to true.
    Set LHelpPath to '../lhelp/lhelp'.

    Put a TEdit on a form.
    Set HelpType to htKeyword
    Set HelpKeyword to 'example/MainPage.html'
}
unit CtxCHMHelpUnit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  HelpIntfs, LazHelpIntf, LazHelpCHM;

const
  {$IFDEF Darwin}
  HelpShortcut = #$e2#$8c#$98'?';
  {$ELSE}
  HelpShortcut = 'F1';
  {$ENDIF}

type

  { TForm1 }

  TForm1 = class(TForm)
    CHMHelpDatabase1: TCHMHelpDatabase;
    Edit1: TEdit;
    Edit2: TEdit;
    LHelpConnector1: TLHelpConnector;
    ShowHelpButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ShowHelpButtonClick(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  CreateLCLHelpSystem;

  Edit1.Text:='Edit1 - Press '+HelpShortcut+' for help';
  Edit2.Text:='Edit2 - Press '+HelpShortcut+' for help';
end;

procedure TForm1.ShowHelpButtonClick(Sender: TObject);
begin
  // This demonstrates how to show a help item manually:
  ShowHelpOrErrorForKeyword('','example/AboutLazarus.html');
end;

{$R *.lfm}

end.

