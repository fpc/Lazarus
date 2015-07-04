unit fEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, LazUTF8, FrmMain;

type

  { TModuleEditor }

  TModuleEditor = class(TMainForm)
    procedure NewCreate(Sender: TObject);
  private
    procedure BuildRecent(const AUnit: string);
  public
    constructor CreateFor(const AUnit: string; TheOwner: TComponent);
  end;

var
  ModuleEditor: TModuleEditor;

implementation

uses
  LazDEOpts, IniFiles;

{$R *.lfm}

{ TModuleEditor }

procedure TModuleEditor.NewCreate(Sender: TObject);
begin
(* New OnCreate handler - do adjustments before calling the inherited handler.
*)
//set Recent to current unit
  //what, how?
  MainFormCreate(Sender);
end;

procedure TModuleEditor.BuildRecent(const AUnit: string);
const
  SecPrefs            = 'Preferences';
begin
  With TInifile.Create(UTF8ToSys(GetoptionFileName)) do
    try
      EraseSection('Recent');
      WriteInteger('Recent','Count',1);
      WriteString('Recent','File1',AUnit);
    //for LoadOptions
      WriteBool(SecPrefs, 'StartMaximized', False);
      WriteBool(SecPrefs, 'ReopenLast', True);
      UpdateFile;
    Finally
      Free;
    end;
end;

constructor TModuleEditor.CreateFor(const AUnit: string; TheOwner: TComponent);
begin
  MainForm := Self; //referenced in editor units
  BuildRecent(AUnit); //force loading of AUnit
  Create(TheOwner);
end;

end.

