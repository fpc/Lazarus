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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Dialog and functions to change encodings (e.g. UTF-8) of projects and
    packages.
}
unit ChgEncodingDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileProcs, Forms, Controls, Graphics,
  Dialogs, LConvEncoding, ExtCtrls, StdCtrls, ComCtrls,
  IDEWindowIntf, SynRegExpr, SrcEditorIntf,
  CodeCache, CodeToolsStructs, CodeToolManager, AVL_Tree,
  // IDE
  IDEProcs, CodeBrowser, PackageDefs, PackageSystem, Project,
  LazarusIDEStrConsts, Buttons, IDEContextHelpEdit;

type

  { TChgEncodingDialog }

  TChgEncodingDialog = class(TForm)
    ApplyButton: TBitBtn;
    HelpButton: TBitBtn;
    BtnPanel: TPanel;
    CloseButton: TBitBtn;
    NewEncodingComboBox: TComboBox;
    FileFilterCombobox: TComboBox;
    NewEncodingLabel: TLabel;
    PreviewButton: TBitBtn;
    PreviewListView: TListView;
    PreviewGroupBox: TGroupBox;
    OwnerComboBox: TComboBox;
    ScopeGroupBox: TGroupBox;
    RegExprCheckBox: TCheckBox;
    FileFilterLabel: TLabel;
    NonUTF8FilesCheckBox: TCheckBox;
    UTF8FilesCheckBox: TCheckBox;
    FilesGroupBox: TGroupBox;
    procedure ApplyButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure PreviewButtonClick(Sender: TObject);
  private
    procedure GetFiles(out Tree: TFilenameToStringTree);
    procedure UpdatePreview;
  public
  end;

function ShowConvertEncodingDlg: TModalResult;

implementation

{$R *.lfm}

function ShowConvertEncodingDlg: TModalResult;
var
  ChgEncodingDialog: TChgEncodingDialog;
begin
  ChgEncodingDialog:=TChgEncodingDialog.Create(nil);
  Result:=ChgEncodingDialog.ShowModal;
  ChgEncodingDialog.Free;
end;

{ TChgEncodingDialog }

procedure TChgEncodingDialog.FormCreate(Sender: TObject);
var
  List: TStringList;
  Encoding: string;
  i: Integer;
begin
  IDEDialogLayoutList.ApplyLayout(Self);

  Caption:=lisConvertEncoding;

  ScopeGroupBox.Caption:=lisConvertProjectOrPackage;
  NewEncodingLabel.Caption:=lisNewEncoding;
  FilesGroupBox.Caption:=lisFileFilter;
  UTF8FilesCheckBox.Caption:=lisFilesInASCIIOrUTF8Encoding;
  NonUTF8FilesCheckBox.Caption:=lisFilesNotInASCIINorUTF8Encoding;
  FileFilterLabel.Caption:=lisFilter;
  RegExprCheckBox.Caption:=lisRegularExpression;

  PreviewButton.Caption:=lisUpdatePreview;
  CloseButton.Caption:=lisMenuClose;
  ApplyButton.Caption:=lisConvert;
  HelpButton.Caption:=dlgGroupHelp;
  PreviewButton.LoadGlyphFromLazarusResource('laz_refresh');
  CloseButton.LoadGlyphFromLazarusResource('btn_close');
  ApplyButton.LoadGlyphFromLazarusResource('btn_ok');
  HelpButton.LoadGlyphFromLazarusResource('btn_help');

  PreviewGroupBox.Caption:=dlgWRDPreview;
  PreviewListView.Column[0].Caption:=dlgEnvFiles;
  PreviewListView.Column[0].Width:=350;
  PreviewListView.Column[1].Caption:=uemEncoding;

  // get possible encodings
  List:=TStringList.Create;
  GetSupportedEncodings(List);
  for i:=List.Count-1 downto 0 do begin
    Encoding:=List[i];
    if (Encoding='') or (SysUtils.CompareText(Encoding,EncodingAnsi)=0) then
      List.Delete(i);
  end;
  List.Sort;
  NewEncodingComboBox.Items.Assign(List);
  List.Free;
  NewEncodingComboBox.Text:='UTF-8';

  // get possible filters
  List:=TStringList.Create;
  List.Add('*.pas;*.pp;*.p;*.inc;*.lpr;*.lfm;*.lrs;*.txt');
  List.Sort;
  FileFilterCombobox.Items.Assign(List);
  List.Free;
  FileFilterCombobox.Text:=FileFilterCombobox.Items[0];

  // get possible projects and packages
  List:=TStringList.Create;
  for i:=0 to PackageGraph.Count-1 do
    if (List.IndexOf(PackageGraph[i].Name)<0)
    and (not PackageGraph[i].ReadOnly)
    and (not PackageGraph[i].IsVirtual)
    then
      List.Add(PackageGraph[i].Name);
  List.Sort;
  if not Project1.IsVirtual then
    List.Insert(0,dlgEnvProject);
  OwnerComboBox.Items.Assign(List);
  List.Free;
  if OwnerComboBox.Items.Count>0 then
    OwnerComboBox.Text:=OwnerComboBox.Items[0]
  else
    OwnerComboBox.Text:='';
end;

procedure TChgEncodingDialog.ApplyButtonClick(Sender: TObject);
var
  Buf: TCodeBuffer;
  SrcEdit: TSourceEditorInterface;
  NewEncoding: String;
  OldEncoding: String;
  Files: TFilenameToStringTree;
  Node: TAVLTreeNode;
  Item: PStringToStringTreeItem;
  Filename: String;
  Encoding: String;
  HasChanged: boolean;
  li: TListItem;
begin
  GetFiles(Files);
  if Files=nil then exit;
  try
    NewEncoding:=NormalizeEncoding(NewEncodingComboBox.Text);

    PreviewListView.BeginUpdate;
    PreviewListView.Items.Clear;
    Node:=Files.Tree.FindLowest;
    while Node<>nil do begin
      Item:=PStringToStringTreeItem(Node.Data);
      Filename:=Item^.Name;
      Encoding:=Item^.Value;
      DebugLn(['TChgEncodingDialog.ApplyButtonClick Filename=',Filename,' Encoding=',Encoding]);
      Buf:=CodeToolBoss.LoadFile(Filename,true,false);
      if not Buf.ReadOnly then begin
        OldEncoding:=Buf.DiskEncoding;
        SrcEdit:=SourceEditorManagerIntf.SourceEditorIntfWithFilename(Filename);
        HasChanged:=true;
        if SrcEdit<>nil then begin
          DebugLn(['TChgEncodingDialog.ApplyButtonClick changing in source editor: ',Filename]);
          Buf.DiskEncoding:=NewEncoding;
          SrcEdit.Modified:=true;
        end else begin
          DebugLn(['TChgEncodingDialog.ApplyButtonClick changing on disk: ',Filename]);
          Buf:=CodeToolBoss.LoadFile(Filename,true,false);
          Buf.DiskEncoding:=NewEncoding;
          HasChanged:=Buf.Save;
          if not HasChanged then
            Buf.DiskEncoding:=OldEncoding;
        end;
      end;
      if not HasChanged then begin
        li:=PreviewListView.Items.Add;
        li.Caption:=Filename;
        li.SubItems.Add(Encoding);
      end;
      Node:=Files.Tree.FindSuccessor(Node);
    end;
    PreviewListView.EndUpdate;
    PreviewGroupBox.Caption:='Number of files failed to convert: '+IntToStr(PreviewListView.Items.Count);
  finally
    Files.Free;
  end;
end;

procedure TChgEncodingDialog.FormDestroy(Sender: TObject);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TChgEncodingDialog.HelpButtonClick(Sender: TObject);
begin
  ShowContextHelpForIDE(Self);
end;

procedure TChgEncodingDialog.PreviewButtonClick(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TChgEncodingDialog.GetFiles(out Tree: TFilenameToStringTree);
var
  AProject: TProject;
  SearchPath: String;
  APackage: TLazPackage;
  Dir: String;
  FileInfo: TSearchRec;
  CurFilename: String;
  Buf: TCodeBuffer;
  NewEncoding: String;
  IncludeFilterRegExpr: TRegExpr;
  CurOwner: TObject;
  Expr: String;
  ok: Boolean;
  p: Integer;
  CurEncoding: String;
begin
  Tree:=nil;

  // check owner
  if OwnerComboBox.Text=dlgEnvProject then
    CurOwner:=Project1
  else
    CurOwner:=PackageGraph.FindAPackageWithName(OwnerComboBox.Text,nil);
  if CurOwner=nil then begin
    DebugLn(['TChgEncodingDialog.UpdatePreview package not found: ',OwnerComboBox.Text]);
    exit;
  end;

  // find search paths
  if CurOwner is TProject then begin
    AProject:=TProject(CurOwner);
    SearchPath:=AProject.SourceDirectories.CreateSearchPathFromAllFiles;
    SearchPath:=MergeSearchPaths(SearchPath,AProject.CompilerOptions.GetIncludePath(false));
  end else begin
    APackage:=TLazPackage(CurOwner);
    SearchPath:=APackage.SourceDirectories.CreateSearchPathFromAllFiles;
    SearchPath:=MergeSearchPaths(SearchPath,APackage.CompilerOptions.GetIncludePath(false));
  end;

  // find files
  IncludeFilterRegExpr:=TRegExpr.Create;
  Expr:=FileFilterCombobox.Text;
  if not RegExprCheckBox.Checked then
    Expr:=SimpleSyntaxToRegExpr(Expr);
  ok:=false;
  try
    IncludeFilterRegExpr.Expression:=Expr;
    ok:=true;
  except
    on E: Exception do begin
      DebugLn('Invalid Include File Expression ',Expr,' ',E.Message);
      MessageDlg('Error in regular expression',
        E.Message,mtError,[mbCancel],0);
    end;
  end;
  if not ok then exit;

  NewEncoding:=NormalizeEncoding(NewEncodingComboBox.Text);
  Tree:=TFilenameToStringTree.Create(FilenamesCaseSensitive);
  p:=1;
  repeat
    Dir:=GetNextDirectoryInSearchPath(SearchPath,p);
    if p>length(SearchPath) then break;
    Dir:=AppendPathDelim(Dir);
    DebugLn(['TChgEncodingDialog.GetFiles Dir=',Dir]);
    if FindFirstUTF8(Dir+FileMask,faAnyFile,FileInfo)=0 then begin
      repeat
        // check if special file
        //DebugLn(['TChgEncodingDialog.GetFiles ',FileInfo.Name,' ... ']);
        if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
          continue;
        CurFilename:=Dir+FileInfo.Name;
        if Tree.Contains(CurFilename) then continue;
        if not IncludeFilterRegExpr.Exec(CurFilename) then begin
          DebugLn(['TChgEncodingDialog.GetFiles not matching filter: ',CurFilename]);
          continue;
        end;
        if not FileIsTextCached(CurFilename) then begin
          DebugLn(['TChgEncodingDialog.GetFiles not a text file: ',CurFilename]);
          continue;
        end;

        if (FileInfo.Attr and faDirectory)>0 then begin
          // skip directory
        end else begin
          Buf:=CodeToolBoss.LoadFile(CurFilename,true,false);
          if Buf<>nil then begin
            //DebugLn(['TChgEncodingDialog.GetFiles Filename=',CurFilename,' Encoding=',NormalizeEncoding(Buf.DiskEncoding)]);
            CurEncoding:=NormalizeEncoding(Buf.DiskEncoding);
            if CurEncoding=NewEncoding then continue;
            if (CurEncoding=EncodingUTF8) and (not UTF8FilesCheckBox.Checked) then
              continue;
            if (CurEncoding<>EncodingUTF8)
            and (not NonUTF8FilesCheckBox.Checked) then
              continue;
            Tree[CurFilename]:=Buf.DiskEncoding;
          end else begin
            DebugLn(['TChgEncodingDialog.UpdatePreview read error: ',CurFilename]);
          end;
        end;
      until FindNextUTF8(FileInfo)<>0;
    end;
    FindCloseUTF8(FileInfo);
  until false;
end;

procedure TChgEncodingDialog.UpdatePreview;
var
  Files: TFilenameToStringTree;
  Node: TAVLTreeNode;
  Item: PStringToStringTreeItem;
  Filename: String;
  Encoding: String;
  li: TListItem;
begin
  GetFiles(Files);
  if Files=nil then exit;
  try
    PreviewListView.BeginUpdate;
    PreviewListView.Items.Clear;
    Node:=Files.Tree.FindLowest;
    while Node<>nil do begin
      Item:=PStringToStringTreeItem(Node.Data);
      Filename:=Item^.Name;
      Encoding:=Item^.Value;
      DebugLn(['TChgEncodingDialog.UpdatePreview Filename=',Filename,' Encoding=',Encoding]);
      li:=PreviewListView.Items.Add;
      li.Caption:=Filename;
      li.SubItems.Add(Encoding);
      Node:=Files.Tree.FindSuccessor(Node);
    end;
    PreviewListView.EndUpdate;
    PreviewGroupBox.Caption:=Format(lisNumberOfFilesToConvert, [IntToStr(
      PreviewListView.Items.Count)]);
  finally
    Files.Free;
  end;
end;

end.

