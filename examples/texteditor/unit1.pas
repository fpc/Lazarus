unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Memo1: TMemo;                 // Text editing area
    MainMenu1: TMainMenu;         // Main menu bar
    MenuItemFile: TMenuItem;      // 'File' menu
    MenuItemNew: TMenuItem;       // 'New' file
    MenuItemOpen: TMenuItem;      // 'Open' file
    MenuItemSave: TMenuItem;      // 'Save' file
    MenuItemExit: TMenuItem;      // 'Exit' application
    OpenDialog1: TOpenDialog;     // Dialog to select the file to be opened
    SaveDialog1: TSaveDialog;     // Dialog to select the file name for saving
    procedure FormCreate(Sender: TObject);
    procedure MenuItemFileClick(Sender: TObject);

    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemNewClick(Sender: TObject);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure MenuItemSaveClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm} // Links the .lfm form file

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Optional: Set dialog filters and default file extensions
  OpenDialog1.Filter := 'Text files (*.txt)|*.txt|All files (*.*)|*.*';
  SaveDialog1.Filter := 'Text files (*.txt)|*.txt|All files (*.*)|*.*';
end;


procedure TForm1.MenuItemExitClick(Sender: TObject);
begin
  // Closes the application
  Close;
end;

procedure TForm1.MenuItemFileClick(Sender: TObject);
begin
  // Shows the file open dialog
   if OpenDialog1.Execute then
   begin
     // Loads file contents into the memo
     Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
   end;
end;

procedure TForm1.MenuItemNewClick(Sender: TObject);
begin
  // Clears the text editor for a new file
  Memo1.Clear;
end;

procedure TForm1.MenuItemOpenClick(Sender: TObject);
begin
  // Shows the file open dialog
  if OpenDialog1.Execute then
  begin
    // Loads file contents into the memo
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
  end;
end;

procedure TForm1.MenuItemSaveClick(Sender: TObject);
begin
  // Shows the file save dialog
  if SaveDialog1.Execute then
  begin
    // Saves memo contents to the selected file
    Memo1.Lines.SaveToFile(SaveDialog1.FileName);
  end;
end;

end.

