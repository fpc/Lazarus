unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  stdctrls, ComCtrls, mrumanager;

Type
  TMyTabSheet = Class(TTabsheet)
  Public
    FileName : String;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    MIRecent: TMenuItem;
    MenuItem2: TMenuItem;
    MIQuit: TMenuItem;
    MISaveAs: TMenuItem;
    MISave: TMenuItem;
    MIOpen: TMenuItem;
    MINew: TMenuItem;
    MFile: TMenuItem;
    MRUMenuManager1: TMRUMenuManager;
    ODFile: TOpenDialog;
    PCFiles: TPageControl;
    SDFile: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MINewClick(Sender: TObject);
    procedure MIOpenClick(Sender: TObject);
    procedure MIQuitClick(Sender: TObject);
    procedure MISaveClick(Sender: TObject);
    procedure MRUMenuManager1RecentFile(Sender: TObject; const AFileName: String
      );
  private
    function CurrentFileName: string;
    procedure OpenFile(AFileName: String);
    procedure SaveFile(const AFileName: String);
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MIQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.MISaveClick(Sender: TObject);
begin
  If (CurrentFileName='') or (Sender=MISaveAs) then
    begin
    if SDFile.Execute then
      SaveFile(SDFile.FileName);
    end
  else
    SaveFile(CurrentFileName);
end;

procedure TForm1.MRUMenuManager1RecentFile(Sender: TObject;
  const AFileName: String);
begin
  OpenFile(AFileName);
end;

procedure TForm1.SaveFile(Const AFileName : String);

Var
  T : TMyTabSheet;
  M : TMemo;

begin
  if (PCFiles.ActivePage is TMyTabSheet) then
    begin
    T:=PCFiles.ActivePage as TMyTabSheet;
    if (T.ControlCount>0) and (T.Controls[0] is TMemo) then
      begin
      M:=T.Controls[0] as TMemo;
      M.Lines.SaveToFile(AFileName);
      MRUMenuManager1.AddTorecent(AFileName);
      end;
    end;
end;

Function TForm1.CurrentFileName : string;

begin
  if (PCFiles.ActivePage is TMyTabSheet) then
    Result:=(PCFiles.ActivePage as TMyTabSheet).FileName
  else
    Result:='';
end;


procedure TForm1.MIOpenClick(Sender: TObject);
begin
  If ODFIle.Execute then
    OpenFile(ODFile.FileName);
end;

procedure TForm1.MINewClick(Sender: TObject);
begin
  OpenFile('')
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MRUMenuManager1.LoadRecentFilesFromIni;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  MRUMenuManager1.SaveRecentFilesToIni;
end;


procedure TForm1.OpenFile(AFileName : String);

Var
  TS : TMyTabSheet;
  M : TMemo;

begin
  TS:=TMyTabSheet.Create(Self);
  TS.FileName:=AFileName;
  TS.Parent:=PCFiles;
  TS.PageControl:=PCFiles;
  if (AFileName<>'') then
    TS.Caption:=ExtractFileName(AFileName)
  else
    TS.Caption:='New file';
  M:=TMemo.Create(Self);
  M.Align:=AlClient;
  M.Parent:=TS;
  if (AFileName<>'') then
    M.Lines.LoadFromFile(AFileName);
  PCFIles.ActivePage:=TS;
  if (AFileName<>'') then
    MRUMenuManager1.AddToRecent(AFileName);
end;

end.

