{
  Author: Mattias Gaertner

  Abstract:
    Project options dialog

}
unit ProjectOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLLinux, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls,
  Project, LResources, Buttons, CodeTools, Dialogs, IDEProcs;

type
  TProjectOptionsDialog = class(TForm)
    NoteBook: TNoteBook;

    // Application
    AppSettingsGroupBox: TGroupBox;
    TitleLabel: TLabel;
    TitleEdit: TEdit;
    OutputSettingsGroupBox: TGroupBox;
    TargetFileLabel: TLabel;
    TargetFileEdit: TEdit;

    // Forms
    FormsAutoCreatedLabel: TLabel;
    FormsAutoCreatedListBox: TListBox;
    FormsAvailFormsLabel: TLabel;
    FormsAvailFormsListBox: TListBox;
    FormsAddToAutoCreatedFormsBtn: TSpeedButton;
    FormsRemoveFromAutoCreatedFormsBtn: TSpeedButton;
    FormsMoveAutoCreatedFormUpBtn: TSpeedButton;
    FormsMoveAutoCreatedFormDownBtn: TSpeedButton;
    FormsAutoCreateNewFormsCheckBox: TCheckBox;

    // buttons at bottom
    OkButton: TButton;
    CancelButton: TButton;
    
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormsAddToAutoCreatedFormsBtnClick(Sender: TObject);
    procedure FormsRemoveFromAutoCreatedFormsBtnClick(Sender: TObject);
    procedure FormsMoveAutoCreatedFormUpBtnClick(Sender: TObject);
    procedure FormsMoveAutoCreatedFormDownBtnClick(Sender: TObject);
  private
    FProject: TProject;
    procedure SetProject(AProject: TProject);
    procedure SetupApplicationPage;
    procedure SetupFormsPage;
    function GetAutoCreatedFormsList: TStrings;
    procedure FillAutoCreateFormsListbox;
    procedure FillAvailFormsListBox;
    function IndexOfAutoCreateForm(FormName: string): integer;
    function FirstAutoCreateFormSelected: integer;
    function FirstAvailFormSelected: integer;
    procedure SelectOnlyThisAutoCreateForm(Index: integer);
    procedure SetAutoCreateForms;
  public
    constructor Create(AOwner: TComponent); override;
    property Project: TProject read FProject write SetProject;
  end;

function ShowProjectOptionsDialog(AProject: TProject): TModalResult;


implementation


function ShowProjectOptionsDialog(AProject: TProject): TModalResult;
var ProjectOptionsDialog: TProjectOptionsDialog;
begin
  ProjectOptionsDialog:=TProjectOptionsDialog.Create(Application);
  try
    with ProjectOptionsDialog do begin
      Project:=AProject;
      Result:=ShowModal;
    end;
  finally
    ProjectOptionsDialog.Free;
  end;
end;


{ TProjectOptionsDialog }

constructor TProjectOptionsDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    SetBounds((Screen.Width-440) div 2,(Screen.Height-385) div 2,430,375);
    Caption:='Project Options';
    
    NoteBook:=TNoteBook.Create(Self);
    with NoteBook do begin
      Name:='NoteBook';
      Parent:=Self;
      SetBounds(0,0,Self.ClientWidth,Self.ClientHeight-50);
      Pages[0]:='Application';
      Pages.Add('Forms');
      Visible:=true;
    end;

    SetupFormsPage;
    SetupApplicationPage;

    NoteBook.Show;

    CancelButton:=TButton.Create(Self);
    with CancelButton do begin
      Name:='CancelButton';
      Parent:=Self;
      Width:=70;
      Height:=23;
      Left:=Self.ClientWidth-Width-15;
      Top:=Self.ClientHeight-Height-15;
      Caption:='Cancel';
      OnClick:=@CancelButtonClick;
      Show;
    end;

    OkButton:=TButton.Create(Self);
    with OkButton do begin
      Name:='OkButton';
      Parent:=Self;
      Width:=CancelButton.Width;
      Height:=CancelButton.Height;
      Left:=CancelButton.Left-15-Width;
      Top:=CancelButton.Top;
      Caption:='Ok';
      OnClick:=@OkButtonClick;
      Show;
    end;
  end;
end;

procedure TProjectOptionsDialog.SetupApplicationPage;
var MaxX:integer;
begin
  MaxX:=ClientWidth-5;

  AppSettingsGroupBox:=TGroupBox.Create(Self);
  with AppSettingsGroupBox do begin
    Name:='AppSettingsGroupBox';
    Parent:=NoteBook.Page[0];
    Left:=5;
    Top:=5;
    Width:=MaxX-2*Left;
    Height:=60;
    Caption:='Application Settings';
    Visible:=true;
  end;

  TitleLabel:=TLabel.Create(Self);
  with TitleLabel do begin
    Name:='TitleLabel';
    Parent:=AppSettingsGroupBox;
    Left:=5;
    Top:=1;
    Width:=50;
    Height:=23;
    Caption:='Title:';
    Visible:=true;
  end;

  TitleEdit:=TEdit.Create(Self);
  with TitleEdit do begin
    Name:='TitleEdit';
    Parent:=AppSettingsGroupBox;
    Left:=TitleLabel.Left+TitleLabel.Width+2;
    Top:=TitleLabel.Top+4;
    Width:=AppSettingsGroupBox.ClientWidth-Left-10;
    Text:='';
    Visible:=true;
  end;

  OutputSettingsGroupBox:=TGroupBox.Create(Self);
  with OutputSettingsGroupBox do begin
    Name:='OutputSettingsGroupBox';
    Parent:=NoteBook.Page[0];
    Left:=AppSettingsGroupBox.Left;
    Top:=AppSettingsGroupBox.Top+AppSettingsGroupBox.Height+5;
    Width:=AppSettingsGroupBox.Width;
    Height:=60;
    Caption:='Output Settings';
    Visible:=true;
  end;

  TargetFileLabel:=TLabel.Create(Self);
  with TargetFileLabel do begin
    Name:='TargetFileLabel';
    Parent:=OutputSettingsGroupBox;
    Left:=5;
    Top:=1;
    Width:=100;
    Height:=23;
    Enabled:=false;
    Caption:='Target file name:';
    Visible:=true;
  end;

  TargetFileEdit:=TEdit.Create(Self);
  with TargetFileEdit do begin
    Name:='TargetFileEdit';
    Parent:=OutputSettingsGroupBox;
    Left:=TargetFileLabel.Left+TargetFileLabel.Width+5;
    Top:=TargetFileLabel.Top+4;
    Width:=OutputSettingsGroupBox.Width-Left-10;
    Text:='';
    Enabled:=false;
    Visible:=true;
  end;
end;

procedure TProjectOptionsDialog.SetupFormsPage;
begin
  FormsAutoCreatedLabel:=TLabel.Create(Self);
  with FormsAutoCreatedLabel do begin
    Name:='FormsAutoCreatedLabel';
    Parent:=NoteBook.Page[1];
    Left:=40;
    Top:=1;
    Width:=150;
    Height:=23;
    Caption:='Auto-create forms:';
    Visible:=true;
  end;

  FormsAutoCreatedListBox:=TListBox.Create(Self);
  with FormsAutoCreatedListBox do begin
    Name:='FormsAutoCreatedListBox';
    Parent:=NoteBook.Page[1];
    Left:=40;
    Top:=28;
    Width:=165;
    Height:=228;
    Visible:=true;
  end;

  FormsAvailFormsLabel:=TLabel.Create(Self);
  with FormsAvailFormsLabel do begin
    Name:='FormsAvailFormsLabel';
    Parent:=NoteBook.Page[1];
    Left:=FormsAutoCreatedListBox.Left+FormsAutoCreatedListBox.Width+45;
    Top:=FormsAutoCreatedLabel.Top;
    Width:=FormsAutoCreatedLabel.Width;
    Height:=FormsAutoCreatedLabel.Height;
    Caption:='Available forms:';
    Visible:=true;
  end;

  FormsAvailFormsListBox:=TListBox.Create(Self);
  with FormsAvailFormsListBox do begin
    Name:='FormsAvailFormsListBox';
    Parent:=NoteBook.Page[1];
    Left:=FormsAvailFormsLabel.Left;
    Top:=FormsAutoCreatedListBox.Top;
    Width:=FormsAutoCreatedListBox.Width;
    Height:=FormsAutoCreatedListBox.Height;
    Visible:=true;
  end;

  FormsAddToAutoCreatedFormsBtn:=TSpeedButton.Create(Self);
  with FormsAddToAutoCreatedFormsBtn do begin
    Name:='FormsAddToAutoCreatedFormsBtn';
    Parent:=NoteBook.Page[1];
    Left:=FormsAutoCreatedListBox.Left+FormsAutoCreatedListBox.Width+10;
    Top:=FormsAutoCreatedListBox.Top+80;
    Width:=25;
    Height:=25;
    OnClick:=@FormsAddToAutoCreatedFormsBtnClick;
    Visible:=true;
  end;

  FormsRemoveFromAutoCreatedFormsBtn:=TSpeedButton.Create(Self);
  with FormsRemoveFromAutoCreatedFormsBtn do begin
    Name:='FormsRemoveFromAutoCreatedFormsBtn';
    Parent:=NoteBook.Page[1];
    Left:=FormsAddToAutoCreatedFormsBtn.Left;
    Top:=FormsAddToAutoCreatedFormsBtn.Top
        +FormsAddToAutoCreatedFormsBtn.Height+10;
    Width:=25;
    Height:=25;
    OnClick:=@FormsRemoveFromAutoCreatedFormsBtnClick;
    Visible:=true;
  end;

  FormsMoveAutoCreatedFormUpBtn:=TSpeedButton.Create(Self);
  with FormsMoveAutoCreatedFormUpBtn do begin
    Name:='FormsMoveAutoCreatedFormUpBtn';
    Parent:=NoteBook.Page[1];
    Left:=FormsAutoCreatedListBox.Left-35;
    Top:=FormsAutoCreatedListBox.Top+80;
    Width:=25;
    Height:=25;
    OnClick:=@FormsMoveAutoCreatedFormUpBtnClick;
    Visible:=true;
  end;

  FormsMoveAutoCreatedFormDownBtn:=TSpeedButton.Create(Self);
  with FormsMoveAutoCreatedFormDownBtn do begin
    Name:='FormsMoveAutoCreatedFormDownBtn';
    Parent:=NoteBook.Page[1];
    Left:=FormsMoveAutoCreatedFormUpBtn.Left;
    Top:=FormsMoveAutoCreatedFormUpBtn.Top
        +FormsMoveAutoCreatedFormUpBtn.Height+10;
    Width:=25;
    Height:=25;
    OnClick:=@FormsMoveAutoCreatedFormDownBtnClick;
    Visible:=true;
  end;

  FormsAutoCreateNewFormsCheckBox:=TCheckBox.Create(Self);
  with FormsAutoCreateNewFormsCheckBox do begin
    Name:='FormsAutoCreateNewFormsCheckBox';
    Parent:=NoteBook.Page[1];
    Left:=FormsAutoCreatedListBox.Left+5;
    Top:=FormsAutoCreatedListBox.Top+FormsAutoCreatedListBox.Height+5;
    Width:=200;
    Height:=25;
    Caption:='Auto create new forms';
    Enabled:=false;
    Visible:=true;
  end;
end;

procedure TProjectOptionsDialog.SetProject(AProject: TProject);
begin
  FProject:=AProject;
  if AProject=nil then exit;
  with AProject do begin
    TitleEdit.Text:=Title;
  end;
  FillAutoCreateFormsListbox;
  FillAvailFormsListBox;
end;

procedure TProjectOptionsDialog.OkButtonClick(Sender: TObject);
begin
  with FProject do begin
    Title:=TitleEdit.Text;
  end;
  SetAutoCreateForms;
  ModalResult:=mrOk;
end;

procedure TProjectOptionsDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

function TProjectOptionsDialog.GetAutoCreatedFormsList: TStrings;
var i, j: integer;
begin
  if (FProject<>nil) and (FProject.MainUnit>=0) then begin
    Result:=ListAllCreateFormsInProgram(
           FProject.Units[FProject.MainUnit].Source.Source);
    if Result<>nil then begin
      // shorten lines of type 'FormName:TFormName' to simply 'FormName'
      for i:=0 to Result.Count-1 do begin
        j:=Pos(':',Result[i]);
        if j>0 then begin
          if 't'+lowercase(copy(Result[i],1,j-1))
            =lowercase(copy(Result[i],j+1,length(Result[i])-j)) then
          begin
            Result[i]:=copy(Result[i],1,j-1);
          end;
        end;
      end;
    end;
  end else begin
    Result:=nil;
  end;  
end;

procedure TProjectOptionsDialog.FillAutoCreateFormsListbox;
var sl: TStrings;
begin
  sl:=GetAutoCreatedFormsList;
  FormsAutoCreatedListBox.Items.BeginUpdate;
  FormsAutoCreatedListBox.Items.Clear;
  if sl<>nil then
    FormsAutoCreatedListBox.Items.Assign(sl);
  FormsAutoCreatedListBox.Items.EndUpdate;
  if sl<>nil then sl.Free;
end;

procedure TProjectOptionsDialog.FillAvailFormsListBox;
var sl: TStringList;
  i: integer;
begin
  if (FProject<>nil) then begin
    sl:=TStringList.Create;
    for i:=0 to FProject.UnitCount-1 do begin
      if (FProject.Units[i].IsPartOfProject)
      and (FProject.Units[i].FormName<>'') then begin
        if IndexOfAutoCreateForm(FProject.Units[i].FormName)<0 then begin
          sl.Add(FProject.Units[i].FormName);
        end;
      end;
    end;
    sl.Sort;
  end else begin
    sl:=nil;
  end;
  FormsAvailFormsListBox.Items.BeginUpdate;
  FormsAvailFormsListBox.Items.Clear;
  if sl<>nil then
    FormsAvailFormsListBox.Items.Assign(sl);
  FormsAvailFormsListBox.Items.EndUpdate;
end;

function TProjectOptionsDialog.IndexOfAutoCreateForm(FormName: string): integer;
var p: integer;
begin
  p:=Pos(':',FormName);
  if p>0 then FormName:=copy(FormName,1,p-1);
  Result:=FormsAutoCreatedListBox.Items.Count-1;
  while (Result>=0) do begin
    p:=Pos(':',FormsAutoCreatedListBox.Items[Result]);
    if p<1 then p:=length(FormsAutoCreatedListBox.Items[Result])+1;
    if AnsiCompareText(copy(FormsAutoCreatedListBox.Items[Result],1,p-1)
                      ,FormName)=0 then exit;
    dec(Result);
  end;
end;

function TProjectOptionsDialog.FirstAutoCreateFormSelected: integer;
begin
  Result:=0;
  while (Result<FormsAutoCreatedListBox.Items.Count)
  and (not FormsAutoCreatedListBox.Selected[Result]) do
    inc(Result);
  if Result=FormsAutoCreatedListBox.Items.Count then Result:=-1;
end;

function TProjectOptionsDialog.FirstAvailFormSelected: integer;
begin
  Result:=0;
  while (Result<FormsAvailFormsListBox.Items.Count)
  and (not FormsAvailFormsListBox.Selected[Result]) do
    inc(Result);
  if Result=FormsAvailFormsListBox.Items.Count then Result:=-1;
end;

procedure TProjectOptionsDialog.FormsAddToAutoCreatedFormsBtnClick(
  Sender: TObject);
var i: integer;
  NewFormName: string;
begin
  FormsAutoCreatedListBox.Items.BeginUpdate;
  with FormsAvailFormsListBox do begin
    Items.BeginUpdate;
    i:=0;
    while i<Items.Count do begin
      if Selected[i] then begin
        NewFormName:=Items[i];
        Items.Delete(i);
        FormsAutoCreatedListBox.Items.Add(NewFormName);
      end else
        inc(i);
    end;
    Items.EndUpdate;
  end;
  FormsAutoCreatedListBox.Items.EndUpdate;
end;

procedure TProjectOptionsDialog.FormsRemoveFromAutoCreatedFormsBtnClick(
  Sender: TObject);
var i, NewPos: integer;
  OldFormName: string;
begin
  FormsAutoCreatedListBox.Items.BeginUpdate;
  FormsAvailFormsListBox.Items.BeginUpdate;
  i:=0;
  while i<FormsAutoCreatedListBox.Items.Count do begin
    if FormsAutoCreatedListBox.Selected[i] then begin
      OldFormName:=FormsAutoCreatedListBox.Items[i];
      FormsAutoCreatedListBox.Items.Delete(i);
      NewPos:=0;
      while (NewPos<FormsAvailFormsListBox.Items.Count)
      and (AnsiCompareText(FormsAvailFormsListBox.Items[NewPos],OldFormName)<0)
      do inc(NewPos);
      FormsAvailFormsListBox.Items.Insert(NewPos,OldFormName);
    end else
      inc(i);
  end;
  FormsAvailFormsListBox.Items.EndUpdate;
  FormsAutoCreatedListBox.Items.EndUpdate;
end;

procedure TProjectOptionsDialog.FormsMoveAutoCreatedFormUpBtnClick(
  Sender: TObject);
var i: integer;
  h: string;
begin
  i:=FirstAutoCreateFormSelected;
  if i<1 then exit;
  with FormsAutoCreatedListBox do begin
    Items.BeginUpdate;
    h:=Items[i];
    Items[i]:=Items[i-1];
    Items[i-1]:=h;
    Items.EndUpdate;
  end;
  SelectOnlyThisAutoCreateForm(i-1);
end;

procedure TProjectOptionsDialog.FormsMoveAutoCreatedFormDownBtnClick(
  Sender: TObject);
var i: integer;
  h: string;
begin
  i:=FirstAutoCreateFormSelected;
  if (i<0) or (i>=FormsAutoCreatedListBox.Items.Count-1) then exit;
  with FormsAutoCreatedListBox do begin
    Items.BeginUpdate;
    h:=Items[i];
    Items[i]:=Items[i+1];
    Items[i+1]:=h;
    Items.EndUpdate;
  end;
  SelectOnlyThisAutoCreateForm(i+1);
end;

procedure TProjectOptionsDialog.SelectOnlyThisAutoCreateForm(
  Index: integer);
var i: integer;
begin
  with FormsAutoCreatedListBox do begin
    for i:=0 to Items.Count-1 do
      Selected[i]:=(i=Index);
  end;
end;

procedure TProjectOptionsDialog.SetAutoCreateForms;
var i, p, InsertPos, StartPos, EndPosition: integer;
  MainSrc: TSourceLog;
  OldList: TStrings;
  s, FormName, FormClassName: string;
begin
  if (Project.MainUnit<0) or (Project.ProjectType in [ptCustomProgram]) then
    exit;
  OldList:=GetAutoCreatedFormsList;
  if (OldList=nil) then exit;
  try
    if OldList.Count=FormsAutoCreatedListBox.Items.Count then begin
      i:=OldList.Count-1;
      while AnsiCompareText(OldList[i],FormsAutoCreatedListBox.Items[i])=0 do
        dec(i);
      if i<0 then exit;
    end;
    MainSrc:=Project.Units[Project.MainUnit].Source;
    StartPos:=SearchCodeInSource(MainSrc.Source,
        'application.createform(',1,EndPosition,false);
    if StartPos<1 then begin
      StartPos:=SearchCodeInSource(MainSrc.Source,
          'application.Initialize;',1,EndPosition,false);
      if StartPos<1 then begin
        MessageDlg('Unable to find "Application.Initialize;" '
             +'in project source. Auto-create-forms changes not applied.'
             ,mtError,[mbOk],0);
        exit;
      end;
      ReadNextPascalAtom(MainSrc.Source,EndPosition,InsertPos);
    end else 
      InsertPos:=StartPos;
    // remove all "Application.CreateForm(*,*);"
    for i:=0 to OldList.Count-1 do begin
      FormName:=OldList[i];
      p:=Pos(':',FormName);
      if p>1 then begin
        FormClassName:=copy(FormName,p+1,length(FormName)-p);
        FormName:=copy(FormName,1,p-1);
      end else begin
        FormClassName:='T'+FormName;
      end;
      RemoveCreateFormFromProgram(MainSrc,FormClassName,FormName);
    end;
    // add auto created forms
    s:='';
    for i:=0 to FormsAutoCreatedListBox.Items.Count-1 do begin
      FormName:=FormsAutoCreatedListBox.Items[i];
      p:=Pos(':',FormName);
      if p>1 then begin
        FormClassName:=copy(FormName,p+1,length(FormName)-p);
        FormName:=copy(FormName,1,p-1);
      end else begin
        FormClassName:='T'+FormName;
      end;
      s:=s+'Application.CreateForm('+FormClassName+','+FormName+');'+EndOfLine+'  ';
    end;
    if s<>'' then
      MainSrc.Insert(InsertPos,s);
  finally
    OldList.Free;
  end;
end;

end.
