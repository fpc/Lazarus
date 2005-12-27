{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Find in files dialog form.

}
unit FindInFilesDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LCLIntf, Controls, StdCtrls, Forms, Buttons,
  ExtCtrls, LResources, FileUtil, LazarusIDEStrConsts, Dialogs, SynEditTypes,
  InputHistory;

type

  { TLazFindInFilesDialog }

  TLazFindInFilesDialog = class(TForm)
    TextToFindLabel: TLabel;
    TextToFindComboBox: TComboBox;
    OptionsGroupBox: TGroupBox;
    CaseSensitiveCheckBox: TCheckBox;
    WholeWordsOnlyCheckBox: TCheckBox;
    RegularExpressionsCheckBox: TCheckBox;
    WhereRadioGroup: TRadioGroup;
    DirectoryOptionsGroupBox: TGroupBox;
    DirectoryLabel: TLabel;
    DirectoryComboBox: TComboBox;
    DirectoryBrowse: TBitBtn;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    FileMaskLabel: TLabel;
    FileMaskComboBox: TComboBox;
    IncludeSubDirsCheckBox: TCheckBox;
    OkButton: TButton;
    CancelButton: TButton;
    procedure LazFindInFilesDialogResize(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure DirectoryBrowseClick(Sender: TObject);
    procedure WhereRadioGroupChangeBounds(Sender: TObject);
    procedure WhereRadioGroupClick(Sender: TObject);
    procedure SetOptions(NewOptions: TLazFindInFileSearchOptions);
    function GetOptions: TLazFindInFileSearchOptions;
    function GetSynOptions: TSynSearchOptions;
    procedure SetSynOptions(NewOptions: TSynSearchOptions);
  private
    function GetFindText: string;
    procedure SetFindText(const NewFindText: string);
  public
    constructor Create(AOwner:TComponent); override;
    property Options: TLazFindInFileSearchOptions read GetOptions
                                                  write SetOptions;
    property FindText: string read GetFindText write SetFindText;
    property SynSearchOptions: TSynSearchOptions read GetSynOptions
                                                 write SetSynOptions;
  end;


var FindInFilesDialog: TLazFindInFilesDialog;


implementation

{ TLazFindInFilesDialog }

constructor TLazFindInFilesDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    Caption:=srkmecFindInFiles;
    Width:=320;
    Height:=430;
    Position:=poScreenCenter;
    OnResize:=@LazFindInFilesDialogResize;

    TextToFindLabel:=TLabel.Create(Self);
    with TextToFindLabel do begin
      Name:='TextToFindLabel';
      SetBounds(8,8,80,Height);
      Caption:=lisFindFileTextToFind;
      Parent:=Self;
    end;
    
    TextToFindComboBox:=TComboBox.Create(Self);
    with TextToFindComboBox do begin
      Name:='TextToFindComboBox';
      SetBounds(TextToFindLabel.Left+TextToFindLabel.Width+5,
        TextToFindLabel.Top-2,
        Self.ClientWidth-TextToFindLabel.Left-TextToFindLabel.Width-13,
        Height);
      Text:='';
      Parent:=Self;
    end;
    
    OptionsGroupBox:=TGroupBox.Create(Self);
    with OptionsGroupBox do begin
      Name:='OptionsGroupBox';
      SetBounds(8,TextToFindLabel.Top+TextToFindLabel.Height+10,
        Self.ClientWidth-20,95);
      Caption:=dlgFROpts;
      Parent:=Self;
    end;
    
    CaseSensitiveCheckBox:=TCheckBox.Create(Self);
    with CaseSensitiveCheckBox do begin
      Name:='CaseSensitiveCheckBox';
      SetBounds(8,2,OptionsGroupBox.ClientWidth-20,20);
      Caption:=lisFindFileCaseSensitive;
      Parent:=OptionsGroupBox;
    end;
    
    WholeWordsOnlyCheckBox:=TCheckBox.Create(Self);
    with WholeWordsOnlyCheckBox do begin
      Name:='WholeWordsOnlyCheckBox';
      SetBounds(CaseSensitiveCheckBox.Left,
           CaseSensitiveCheckBox.Top+CaseSensitiveCheckBox.Height+2,
           CaseSensitiveCheckBox.Width,20);
      Caption:=lisFindFileWholeWordsOnly;
      Parent:=OptionsGroupBox;
    end;
    
    RegularExpressionsCheckBox:=TCheckBox.Create(Self);
    with RegularExpressionsCheckBox do begin
      Name:='RegularExpressionsCheckBox';
      SetBounds(CaseSensitiveCheckBox.Left,
           WholeWordsOnlyCheckBox.Top+WholeWordsOnlyCheckBox.Height+2,
           CaseSensitiveCheckBox.Width,20);
      Caption:=lisFindFileRegularExpressions;
      Parent:=OptionsGroupBox;
    end;
    
    WhereRadioGroup:=TRadioGroup.Create(Self);
    with WhereRadioGroup do begin
      Name:='WhereRadioGroup';
      OnChangeBounds:=@WhereRadioGroupChangeBounds;
      SetBounds(8,OptionsGroupBox.Top+OptionsGroupBox.Height+10,
        Self.ClientWidth-20,90);
      Caption:=lisFindFileWhere;
      Items.BeginUpdate;
      Items.Add(lisFindFilesearchAllFilesInProject);
      Items.Add(lisFindFilesearchAllOpenFiles);
      Items.Add(lisFindFilesearchInDirectories);
      Items.EndUpdate;
      ItemIndex:=1;
      OnClick:= @WhereRadioGroupClick;
      Parent:=Self;
    end;
    
    DirectoryOptionsGroupBox:=TGroupBox.Create(Self);
    with DirectoryOptionsGroupBox do begin
      Name:='DirectoryOptionsGroupBox';
      SetBounds(8,WhereRadioGroup.Top+WhereRadioGroup.Height+10,
        Self.ClientWidth-20,135);
      Caption:=lisFindFileDirectoryOptions;
      Enabled:= False;
      Parent:=Self;
    end;
    
    DirectoryLabel:=TLabel.Create(Self);
    with DirectoryLabel do begin
      Name:='DirectoryLabel';
      SetBounds(8,5,80,Height);
      Caption:=lisCodeToolsDefsInsertBehindDirectory;
      Parent:=DirectoryOptionsGroupBox;
    end;
    
    DirectoryComboBox:=TComboBox.Create(Self);
    with DirectoryComboBox do begin
      Name:='DirectoryComboBox';
      Left:=DirectoryLabel.Left+DirectoryLabel.Width+5;
      Top:=DirectoryLabel.Top-2;
      Width:=Self.ClientWidth-Left-8-25-5;
      Text:='';
      Parent:=DirectoryOptionsGroupBox;
    end;
    
    DirectoryBrowse:=TBitBtn.Create(Self);
    with DirectoryBrowse do begin
      Name:='DirectoryBrowse';
      SetBounds(DirectoryComboBox.Left+DirectoryComboBox.Width+5,
        DirectoryComboBox.Top,25,25);
      Caption:='...';
      OnClick:=@DirectoryBrowseClick;
      Parent:=DirectoryOptionsGroupBox;
    end;
    
    SelectDirectoryDialog := TSelectDirectoryDialog.Create(Self);
    with SelectDirectoryDialog do
      // the whole selected directory must exist (ofPathMustExist is not enough)
      Options:= Options + [ofFileMustExist];

    FileMaskLabel:=TLabel.Create(Self);
    with FileMaskLabel do begin
      Name:='FileMaskLabel';
      SetBounds(8,DirectoryComboBox.Top+DirectoryComboBox.Height+5,200,Height);
      Caption:=lisFindFileFileMaskBak;
      Parent:=DirectoryOptionsGroupBox;
    end;
    
    FileMaskComboBox:=TComboBox.Create(Self);
    with FileMaskComboBox do begin
      Name:='FileMaskComboBox';
      SetBounds(FileMaskLabel.Left, FileMaskLabel.Top+FileMaskLabel.Height+3,
         Self.ClientWidth-20-5-25,Height);
      Text:='*.pas;*.pp;*.inc';
      Parent:=DirectoryOptionsGroupBox;
    end;
    
    IncludeSubDirsCheckBox:=TCheckBox.Create(Self);
    with IncludeSubDirsCheckBox do begin
      Name:='IncludeSubDirsCheckBox';
      SetBounds(8,FileMaskComboBox.Top+FileMaskComboBox.Height+10,
          150,Height);
      Caption:=lisFindFileIncludeSubDirectories;
      Parent:=DirectoryOptionsGroupBox;
    end;
    
    OkButton:=TButton.Create(Self);
    with OkButton do begin
      Name:='OkButton';
      SetBounds(Self.ClientWidth-200,Self.ClientHeight-40,80,Height);
      Caption:=lisLazBuildOk;
      OnClick:=@OkButtonClick;
      Default:=true;
      Parent:=Self;
    end;
    
    CancelButton:=TButton.Create(Self);
    with CancelButton do begin
      Name:='CancelButton';
      SetBounds(Self.ClientWidth-100,Self.ClientHeight-40,80,Height);
      Caption:=dlgCancel;
      OnClick:=@CancelButtonClick;
      Cancel := true;
      Parent:=Self;
    end;
    ActiveControl:=TextToFindComboBox;
  end;
  LazFindInFilesDialogResize(nil);
end;

procedure TLazFindInFilesDialog.LazFindInFilesDialogResize(Sender: TObject);
begin
  with TextToFindLabel do begin
    SetBounds(8,8,80,Height);
  end;

  with TextToFindComboBox do begin
    SetBounds(TextToFindLabel.Left+TextToFindLabel.Width+5,
      TextToFindLabel.Top-2,
      Self.ClientWidth-TextToFindLabel.Left-TextToFindLabel.Width-13,
      Height);
  end;

  with OptionsGroupBox do begin
    SetBounds(8,TextToFindLabel.Top+TextToFindLabel.Height+10,
      Self.ClientWidth-20,95);
  end;

  with WhereRadioGroup do begin
    SetBounds(8,OptionsGroupBox.Top+OptionsGroupBox.Height+10,
      Self.ClientWidth-20,90);
  end;

  with DirectoryOptionsGroupBox do begin
    SetBounds(8,WhereRadioGroup.Top+WhereRadioGroup.Height+10,
      Self.ClientWidth-20,135);
  end;

  with DirectoryComboBox do begin
    Left:=DirectoryLabel.Left+DirectoryLabel.Width+5;
    Top:=DirectoryLabel.Top-2;
    Width:=Parent.ClientWidth-Left-8-25-5;
  end;

  with DirectoryBrowse do begin
    SetBounds(DirectoryComboBox.Left+DirectoryComboBox.Width+5,
      DirectoryComboBox.Top,25,25);
  end;

  with FileMaskLabel do begin
    SetBounds(8,DirectoryComboBox.Top+DirectoryComboBox.Height+5,200,Height);
  end;

  with FileMaskComboBox do begin
    SetBounds(FileMaskLabel.Left, FileMaskLabel.Top+FileMaskLabel.Height+3,
       Self.ClientWidth-20-5-25,Height);
  end;

  with IncludeSubDirsCheckBox do begin
    SetBounds(8,FileMaskComboBox.Top+FileMaskComboBox.Height+10,
        150,Height);
  end;

  with OkButton do begin
    SetBounds(Self.ClientWidth-200,Self.ClientHeight-40,80,Height);
  end;

  with CancelButton do begin
    SetBounds(Self.ClientWidth-100,Self.ClientHeight-40,80,Height);
  end;
end;

procedure TLazFindInFilesDialog.OkButtonClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TLazFindInFilesDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TLazFindInFilesDialog.SetFindText(const NewFindText: string);
begin
  TextToFindComboBox.Text:= NewFindText;
  TextToFindComboBox.SelectAll;
  ActiveControl:=TextToFindComboBox;
end;

function TLazFindInFilesDialog.GetFindText: string;
begin
  Result:=TextToFindComboBox.Text;
end;

procedure TLazFindInFilesDialog.WhereRadioGroupClick(Sender: TObject);
begin
  DirectoryOptionsGroupBox.Enabled:= (WhereRadioGroup.ItemIndex = 2)
end;

procedure TLazFindInFilesDialog.DirectoryBrowseClick(Sender: TObject);
begin
  SelectDirectoryDialog.InitialDir:= GetCurrentDir;
  if SelectDirectoryDialog.Execute then
    DirectoryComboBox.Text:= SelectDirectoryDialog.FileName;
end;

procedure TLazFindInFilesDialog.WhereRadioGroupChangeBounds(Sender: TObject);
begin
  //DebugLn('TLazFindInFilesDialog.WhereRadioGroupChangeBounds ',dbgs(WhereRadioGroup.BoundsRect));
  if (WhereRadioGroup.Left>8) then begin
    RaiseGDBException('');
  end;
end;

procedure TLazFindInFilesDialog.SetOptions(
                                       NewOptions: TLazFindInFileSearchOptions);
begin
  CaseSensitiveCheckBox.Checked:=fifMatchCase in NewOptions;
  WholeWordsOnlyCheckBox.Checked:=fifWholeWord in NewOptions;
  RegularExpressionsCheckBox.Checked:=fifRegExpr in NewOptions;
  DirectoryOptionsGroupBox.Enabled:=fifSearchDirectories in NewOptions;
  IncludeSubDirsCheckBox.Checked:=fifIncludeSubDirs in NewOptions;
  if fifSearchProject in NewOptions then WhereRadioGroup.ItemIndex:= 0;
  if fifSearchOpen in NewOptions then WhereRadioGroup.ItemIndex:= 1;
  if fifSearchDirectories in NewOptions then WhereRadioGroup.ItemIndex:= 2;
end;

function TLazFindInFilesDialog.GetOptions: TLazFindInFileSearchOptions;
begin
  Result:=[];
  if CaseSensitiveCheckBox.Checked then Include(Result,fifMatchCase);
  if WholeWordsOnlyCheckBox.Checked then Include(Result,fifWholeWord);
  if RegularExpressionsCheckBox.Checked then Include(Result,fifRegExpr);
  if IncludeSubDirsCheckBox.Checked then Include(Result,fifIncludeSubDirs);

  case WhereRadioGroup.ItemIndex of
    0: Include(Result,fifSearchProject);
    1: Include(Result,fifSearchOpen);
    2: Include(Result,fifSearchDirectories);
  end;//case
end;

function TLazFindInFilesDialog.GetSynOptions: TSynSearchOptions;
begin
  Result:=[];
  if CaseSensitiveCheckBox.Checked then Include(Result,ssoMatchCase);
  if WholeWordsOnlyCheckBox.Checked then Include(Result,ssoWholeWord);
  if RegularExpressionsCheckBox.Checked then Include(Result,ssoRegExpr);
end;//GetSynOptions

procedure TLazFindInFilesDialog.SetSynOptions(NewOptions: TSynSearchOptions);
begin
  CaseSensitiveCheckBox.Checked:=ssoMatchCase in NewOptions;
  WholeWordsOnlyCheckBox.Checked:=ssoWholeWord in NewOptions;
  RegularExpressionsCheckBox.Checked:=ssoRegExpr in NewOptions;
end;//SetSynOptions

initialization
  FindInFilesDialog:=nil;

end.

