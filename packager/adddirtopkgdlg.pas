{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit AddDirToPkgDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr,
  // LCL
  Forms, Controls, Dialogs, StdCtrls, ButtonPanel, EditBtn,
  // LazUtils
  FileProcs, LazFileUtils, LazStringUtils,
  // IdeIntf
  IDEWindowIntf,
  // IDE
  LazarusIDEStrConsts, PackageDefs;

type

  { TAddDirToPkgDialog }

  TAddDirToPkgDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    DirEdit: TDirectoryEdit;
    OnlyTextCheckBox: TCheckBox;
    ExcludeFilterCombobox: TComboBox;
    ExcludeRegExCheckBox: TCheckBox;
    ExcludeGroupBox: TGroupBox;
    IncludeFilterCombobox: TComboBox;
    IncludeRegExCheckBox: TCheckBox;
    IncludeGroupBox: TGroupBox;
    SubDirCheckBox: TCheckBox;
    DirGroupBox: TGroupBox;
    procedure ButtonPanel1CancelClick(Sender: TObject);
    procedure ButtonPanel1OkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FFiles: TStrings;
    FLazPackage: TLazPackage;
    fIncludeFilterRE, fExcludeFilterRE: TRegExpr;
    procedure SetLazPackage(const AValue: TLazPackage);
    function GatherFiles(Directory: string; WithSubDirs: boolean;
      OnlyTextFiles: boolean): boolean;
    function UpdateFilter: boolean;
  public
    property LazPackage: TLazPackage read FLazPackage write SetLazPackage;
    property Files: TStrings read FFiles write FFiles;
  end;

function ShowAddDirToPkgDialog(APackage: TLazPackage; out Files: TStrings): TModalResult;

implementation

{$R *.lfm}

function ShowAddDirToPkgDialog(APackage: TLazPackage; out Files: TStrings): TModalResult;
var
  AddDirToPkgDialog: TAddDirToPkgDialog;
begin
  Files:=TStringList.Create;
  AddDirToPkgDialog:=TAddDirToPkgDialog.Create(nil);
  try
    AddDirToPkgDialog.LazPackage:=APackage;
    AddDirToPkgDialog.Files:=Files;
    Result:=AddDirToPkgDialog.ShowModal;
  finally
    AddDirToPkgDialog.Free;
  end;
end;

{ TAddDirToPkgDialog }

procedure TAddDirToPkgDialog.FormCreate(Sender: TObject);
begin
  Caption:=lisAddFilesInDirectory;
  DirGroupBox.Caption:=lisCodeToolsDefsInsertBehindDirectory;
  SubDirCheckBox.Caption:=lisFindFileIncludeSubDirectories;
  IncludeGroupBox.Caption:=lisFilter;
  IncludeRegExCheckBox.Caption:=lisRegularExpression;
  OnlyTextCheckBox.Caption:=lisFindFileOnlyTextFiles;
  ExcludeGroupBox.Caption:=lisExcludeFilter;
  ExcludeRegExCheckBox.Caption:=lisRegularExpression;
  ButtonPanel1.OKButton.OnClick:=@ButtonPanel1OkClick;
  ButtonPanel1.CancelButton.OnClick:=@ButtonPanel1CancelClick;

  with IncludeFilterCombobox.Items do begin
    BeginUpdate;
    Clear;
    Add('*.pas;*.pp;*.p;*.inc;*.lfm;*.dfm;*.lrs;*.txt;*.xml');
    Add('*.pas;*.pp;*.p;*.inc');
    EndUpdate;
  end;
  IncludeFilterCombobox.ItemIndex:=0;

  with ExcludeFilterCombobox.Items do begin
    BeginUpdate;
    Clear;
    Add('*.o;*.ppu;*.dcu;*.a;*.so;*.dll;*.compiled;*.po');
    EndFormUpdate;
  end;
  ExcludeFilterCombobox.ItemIndex:=0;

  // not needed for borderstyle dialog
  //IDEDialogLayoutList.ApplyLayout(Self,400,350);

  fIncludeFilterRE:=TRegExpr.Create;
  fExcludeFilterRE:=TRegExpr.Create;
end;

procedure TAddDirToPkgDialog.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fIncludeFilterRE);
  FreeAndNil(fExcludeFilterRE);
end;

procedure TAddDirToPkgDialog.ButtonPanel1CancelClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TAddDirToPkgDialog.ButtonPanel1OkClick(Sender: TObject);
begin
  if not GatherFiles(DirEdit.Text,SubDirCheckBox.Checked,
    OnlyTextCheckBox.Checked) then exit;
  ModalResult:=mrOk;
end;

procedure TAddDirToPkgDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TAddDirToPkgDialog.SetLazPackage(const AValue: TLazPackage);
begin
  if FLazPackage=AValue then exit;
  FLazPackage:=AValue;
  DirEdit.Text:=FLazPackage.Directory;
end;

function TAddDirToPkgDialog.GatherFiles(Directory: string;
  WithSubDirs: boolean; OnlyTextFiles: boolean): boolean;

  function FileCanBeAdded(AFilename: string): boolean;
  begin
    Result:=false;

    // check include filter
    if (fIncludeFilterRE.Expression<>'')
    and (not fIncludeFilterRE.Exec(ExtractFilename(AFilename))) then
      exit;

    // check exclude filter
    if (fExcludeFilterRE.Expression<>'')
    and (fExcludeFilterRE.Exec(ExtractFilename(AFilename))) then
      exit;

    // check binaries
    if OnlyTextFiles and (not FileIsText(AFilename)) then exit;

    Result:=true;
  end;

  function SearchDir(CurDir: string): boolean;
  var
    FileInfo: TSearchRec;
    CurFilename: String;
  begin
    Result:=false;
    CurDir:=TrimAndExpandDirectory(CurDir);
    if not DirPathExists(CurDir) then begin
      MessageDlg(lisEnvOptDlgDirectoryNotFound,
        Format(lisTheDirectoryWasNotFound, [CurDir]), mtError, [mbCancel], 0);
      exit;
    end;
    if FindFirstUTF8(CurDir+FileMask,faAnyFile,FileInfo)=0 then begin
      repeat
        // check if special file
        if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
          continue;
        CurFilename:=CurDir+FileInfo.Name;
        if (FileInfo.Attr and faDirectory)>0 then begin
          if WithSubDirs and not SearchDir(CurFilename) then exit;
        end else begin
          if FileCanBeAdded(CurFilename) then
            Files.Add(CurFilename);
        end;
      until FindNextUTF8(FileInfo)<>0;
    end;
    FindCloseUTF8(FileInfo);

    Result:=true;
  end;

begin
  Files.Clear;
  Result:=false;
  if not UpdateFilter then exit;
  Result:=SearchDir(Directory);
end;

function TAddDirToPkgDialog.UpdateFilter: boolean;
var
  Expr: String;
begin
  Expr:=IncludeFilterCombobox.Text;
  if not IncludeRegExCheckBox.Checked then
    Expr:=SimpleSyntaxToRegExpr(Expr);
  if Expr<>'' then begin
    Result:=false;
    try
      fIncludeFilterRE.Expression:=Expr;
      Result:=true;
    except
      on E: Exception do begin
        MessageDlg(lisInvalidFilter,
          Format(lisInvalidExpression, [LineEnding, Expr, LineEnding, E.Message]),
          mtError, [mbCancel], 0);
      end;
    end;
    if not Result then exit;
  end;
  Expr:=ExcludeFilterCombobox.Text;
  if not ExcludeRegExCheckBox.Checked then
    Expr:=SimpleSyntaxToRegExpr(Expr);
  if Expr<>'' then begin
    Result:=false;
    try
      fExcludeFilterRE.Expression:=Expr;
      Result:=true;
    except
      on E: Exception do begin
        MessageDlg(lisInvalidFilter,
          Format(lisInvalidExpression, [LineEnding, Expr, LineEnding, E.Message]),
          mtError, [mbCancel], 0);
      end;
    end;
    if not Result then exit;
  end;
  Result:=true;
end;

end.

