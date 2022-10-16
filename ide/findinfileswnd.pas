{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Find in files non modal window.

ToDo:
  Resourcestrings

}
unit FindInFilesWnd;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, Buttons, ComCtrls, ExtCtrls, SynEdit,
  DividerBevel, MenuIntf, IDEWindowIntf, LazIDEIntf;

type

  { TLazFindInFilesWindow }

  TLazFindInFilesWindow = class(TForm)
    CaseSensitiveSpeedButton: TSpeedButton;
    AutoUpdateSpeedButton: TSpeedButton;
    DirsComboBox: TComboBox;
    DirsEditSpeedButton: TSpeedButton;
    FileMaskComboBox: TComboBox;
    FindCombobox: TComboBox;
    ShowReplaceSpeedButton: TSpeedButton;
    InEditorFilesSpeedButton: TSpeedButton;
    InProjectFilesSpeedButton1: TSpeedButton;
    PkgEditSpeedButton: TSpeedButton;
    ReplaceButton: TButton;
    WherePanel: TPanel;
    PkgComboBox: TComboBox;
    ReplaceComboBox: TComboBox;
    ResultsTreeView: TTreeView;
    SrcDividerBevel: TDividerBevel;
    StoreAndNewSpeedButton: TSpeedButton;
    RegularExpressionsSpeedButton: TSpeedButton;
    MultilineSpeedButton: TSpeedButton;
    SynEdit1: TSynEdit;
    WholeWordsSpeedButton: TSpeedButton;
    procedure ShowReplaceSpeedButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure InitImageList;
  public
  end;

var
  FindInFilesWindow: TLazFindInFilesWindow;

procedure RegisterFindInFilesWnd;

implementation

{$R *.lfm}

type

  { TRegisterDummy }

  TRegisterDummy = class
    procedure mnuFindInFilesWndClicked(Sender: TObject);
  end;
var
  RegisterDummy: TRegisterDummy;

procedure RegisterFindInFilesWnd;
var
  MenuCommand: TIDEMenuCommand;
begin
  RegisterDummy:=TRegisterDummy.Create;
  MenuCommand:=RegisterIDEMenuCommand(itmSearchFindReplace,'FindInFilesWndItem','New Find In Files');
  MenuCommand.OnClick:=@RegisterDummy.mnuFindInFilesWndClicked;
end;

{ TRegisterDummy }

procedure TRegisterDummy.mnuFindInFilesWndClicked(Sender: TObject);
begin
  if FindInFilesWindow=nil then
  begin
    IDEWindowCreators.CreateForm(FindInFilesWindow,TLazFindInFilesWindow,
       false,LazarusIDE.OwningComponent);
  end;
  IDEWindowCreators.ShowForm(FindInFilesWindow,true);
end;

{ TLazFindInFilesWindow }

procedure TLazFindInFilesWindow.FormCreate(Sender: TObject);
begin
  ShowReplaceSpeedButton.Caption:='';
  FindCombobox.TextHint:='Find text';
  StoreAndNewSpeedButton.Hint:='Store results in search results window and start a new search';
  CaseSensitiveSpeedButton.Hint:='Case Sensitive';
  WholeWordsSpeedButton.Hint:='Whole words';
  RegularExpressionsSpeedButton.Hint:='Regular Expression';
  MultilineSpeedButton.Hint:='Multi line';
  AutoUpdateSpeedButton.Hint:='Start searching while you type';

  ReplaceComboBox.TextHint:='Replace text';
  ReplaceButton.Hint:='Replace all found matches';

  FileMaskComboBox.TextHint:='*.pas;*.inc;*.txt;*.lfm';
  InEditorFilesSpeedButton.Hint:='Search in editor files';
  InProjectFilesSpeedButton1.Hint:='Search in project files';
  PkgComboBox.TextHint:='lcl;lazutils';
  PkgComboBox.Text:='do not search in packages';

  DirsComboBox.TextHint:='folder1;folder2';
  DirsComboBox.Text:='';

  SrcDividerBevel.Caption:='Preview';
  SynEdit1.Lines.Text:='No source selected';

  InitImageList;
end;

procedure TLazFindInFilesWindow.ShowReplaceSpeedButtonClick(Sender: TObject);
begin
  DisableAlign;
  try
    if ShowReplaceSpeedButton.Down then
    begin
      ReplaceComboBox.Visible:=true;
      ReplaceButton.Visible:=true;
    end else begin
      ReplaceComboBox.Visible:=false;
      ReplaceButton.Visible:=false;
    end;
  finally
    EnableAlign;
  end;
end;

procedure TLazFindInFilesWindow.FormDestroy(Sender: TObject);
begin

end;

procedure TLazFindInFilesWindow.InitImageList;
begin

end;

finalization
  FreeAndNil(RegisterDummy);

end.

