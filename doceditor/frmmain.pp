unit frmmain; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ExtCtrls, ComCtrls, StdCtrls, pgeditor,process, fpdeutil;

type
  TNodeType = (ntfile,ntPackage,ntModule,ntElement,ntTopic);
  { TMainForm }

  TMainForm = class(TForm)
    AExtraBuild: TAction;
    AExtraOptions: TAction;
    AHelpAbout: TAction;
    AFormatFile: TAction;
    AFormatBold: TAction;
    AFormatItalic: TAction;
    AFormatUnderline: TAction;
    AFormatRemark: TAction;
    AFormatVariable: TAction;
    AFormatCode: TAction;
    AFormatParagraph: TAction;
    AInsertTable: TAction;
    AInsertPackage: TAction;
    AInsertModule: TAction;
    AInsertElement: TAction;
    AInsertTopic: TAction;
    AInsertLink: TAction;
    AExit: TAction;
    ANew: TAction;
    ANewFromFile: TAction;
    AOpen: TAction;
    ASave: TAction;
    ASaveAs: TAction;
    AClose: TAction;
    ALMain: TActionList;
    ILMain: TImageList;
    LIBuild: TMenuItem;
    MISeparate: TMenuItem;
    MISaveAs: TMenuItem;
    MISave: TMenuItem;
    MIAbout: TMenuItem;
    MIHelp: TMenuItem;
    MIExtraOptions: TMenuItem;
    MIExtra: TMenuItem;
    MIInsertPackage: TMenuItem;
    MIInsertModule: TMenuItem;
    MIInsertElement: TMenuItem;
    MIInsertLink: TMenuItem;
    MIInsertTopic: TMenuItem;
    MIInsertTable: TMenuItem;
    MIInsert: TMenuItem;
    MIClose: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MIRecent: TMenuItem;
    MINewFromFile: TMenuItem;
    MIFileOpen: TMenuItem;
    MIFileNew: TMenuItem;
    MFile: TMenuItem;
    MMain: TMainMenu;
    ODMain: TOpenDialog;
    PCFiles: TPageControl;
    SDMain: TSaveDialog;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    TBMain: TToolBar;
    TBNewModule: TToolButton;
    TBNewPackage: TToolButton;
    TBNew: TToolButton;
    TBNewTopic: TToolButton;
    TBNewElement: TToolButton;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    TBSaveAs: TToolButton;
    TBSave: TToolButton;
    ToolButton13: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure ACloseExecute(Sender: TObject);
    procedure AExitExecute(Sender: TObject);
    procedure AExtraBuildExecute(Sender: TObject);
    procedure AExtraOptionsExecute(Sender: TObject);
    procedure AFormatParagraphHint(var HintStr: string; var CanShow: Boolean);
    procedure AHelpAboutExecute(Sender: TObject);
    procedure CanFormat(Sender: TObject);
    procedure AInsertLinkExecute(Sender: TObject);
    procedure AInsertLinkUpdate(Sender: TObject);
    procedure AInsertTableExecute(Sender: TObject);
    procedure AInsertTableUpdate(Sender: TObject);
    procedure ANewExecute(Sender: TObject);
    procedure ANewFromFileExecute(Sender: TObject);
    procedure AOpenExecute(Sender: TObject);
    procedure ASaveAsExecute(Sender: TObject);
    procedure ASaveExecute(Sender: TObject);
    procedure DoFormat(Sender: TObject);
    procedure HaveEditor(Sender: TObject);
    procedure InsertStructure(Sender: TObject);
    procedure MainFormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure MainFormCreate(Sender: TObject);
    procedure MainFormDestroy(Sender: TObject);
  private
    { private declarations }
    FRecent : TStringList;
    // Editor functions.
    procedure BuildReopenList;
    Procedure AddTorecent(FN : String);
    Procedure OpenFile(FN : String);
    Procedure SaveEditorAs(E : TEditorPage);
    Procedure SaveEditor(E : TEditorPage);
    Function  CloseEditor(E : TEditorPage) : Boolean;
    Procedure FileReopen(Sender: TObject);
    Procedure NewFile;
    Procedure NewFromFile;
    Procedure LoadCommandLine;
    Procedure LoadRecent;
    Procedure SaveRecent;
    Function  CreatePage : TEditorPage;
    Function  Currenteditor : TEditorPage;
    Function  AllowClose : Boolean;
    Procedure InsertNode (NT : TNodeType);
    Procedure InsertLink;
    Procedure InsertTable;
    Procedure ShowAbout;
    Procedure GetCurrentFiles(List : TStrings);
  public
    { public declarations }
  end; 

Const
  NodeNames : Array[TNodeType] of String
            = ('file','package','module','element','topic');
var
  MainForm: TMainForm;

implementation

uses lazdeopts,lazdemsg,inifiles, frmmakeskel,frmOptions,frmNewNode,
     frmLink,frmTable,frmAbout,frmBuild;

{ Fixes & additions to LCL}

Const
  mbYesNo = [mbYes, mbNo];

function MessageDlg(Fmt: string; Args : Array of const; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;

begin
  Result:=Dialogs.MessageDlg(Format(Fmt,Args),DlgType,Buttons,HelpCtx);
end;

function MessageDlg(Fmt: string; Args : Array of const; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons): Integer;

begin
  Result:=Dialogs.MessageDlg(Format(Fmt,Args),DlgType,Buttons,0);
end;

function MessageDlg(Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): Integer;

begin
  Result:=Dialogs.MessageDlg(Msg,DlgType,Buttons,0);
end;


{ TMainForm }

Type
  TRecentMenuItem = Class (TMenuItem)
  public
    FileName : String;
  end;

Procedure TMainForm.FileReopen(Sender: TObject);

begin
  OpenFile((Sender as TRecentMenuItem).FileName);
end;

procedure TMainForm.MainFormCreate(Sender: TObject);
begin
  if Sender=nil then ;
  FRecent:=TStringList.Create;
  LoadCommandLine;
  LoadOptions;
  LoadRecent;
end;

procedure TMainForm.AOpenExecute(Sender: TObject);
begin
  if Sender=nil then ;
  With ODMain do
    If Execute then
      OpenFile(FileName);
end;

procedure TMainForm.AExitExecute(Sender: TObject);
begin
  if Sender=nil then ;
  Close;
end;

procedure TMainForm.AExtraBuildExecute(Sender: TObject);
begin
  if Sender=nil then ;
  With TBuildForm.Create(Self) do
    Try
      OnGetList:=@Self.GetCurrentFiles;
      ShowModal;
    Finally
      Free;
    end;
end;

procedure TMainForm.ACloseExecute(Sender: TObject);
begin
  if Sender=nil then ;
  CloseEditor(CurrentEditor);
end;

procedure TMainForm.AExtraOptionsExecute(Sender: TObject);
begin
  if Sender=nil then ;
  With TOptionsForm.Create(Self) do
    Try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TMainForm.AFormatParagraphHint(var HintStr: string;
  var CanShow: Boolean);
begin
  if (HintStr='') then ;
  if CanShow then ;
end;

procedure TMainForm.AHelpAboutExecute(Sender: TObject);
begin
  if Sender=nil then ;
  ShowAbout;
end;

procedure TMainForm.CanFormat(Sender: TObject);

Var
  B : Boolean;

begin
  if Sender=nil then ;
  B:=(CurrentEditor<>Nil);
  If B then
    B:=CurrentEditor.CanInsertTag(TTagType((Sender as TAction).Tag));
  (Sender as Taction).Enabled:=B;
end;

procedure TMainForm.AInsertLinkExecute(Sender: TObject);
begin
  if Sender=nil then ;
  InsertLink;
end;

procedure TMainForm.AInsertLinkUpdate(Sender: TObject);

Var
  B : Boolean;

begin
  if Sender=nil then ;
  B:=(CurrentEditor<>Nil);
  If B then
    B:=CurrentEditor.CanInsertTag(ttLink);
  (Sender as TAction).Enabled:=B;
end;

procedure TMainForm.AInsertTableExecute(Sender: TObject);

begin
  if Sender=nil then ;
  InsertTable;
end;

procedure TMainForm.AInsertTableUpdate(Sender: TObject);
Var
  B : Boolean;

begin
  if Sender=nil then ;
  B:=(CurrentEditor<>Nil);
  If B then
    B:=CurrentEditor.CanInsertTag(ttTable);
  (Sender as TAction).Enabled:=B;
end;

procedure TMainForm.ANewExecute(Sender: TObject);
begin
  if Sender=nil then ;
  NewFile;
end;

procedure TMainForm.ANewFromFileExecute(Sender: TObject);
begin
  if Sender=nil then ;
  NewFromFile;
end;

procedure TMainForm.ASaveAsExecute(Sender: TObject);

Var
  E : TEditorPage;

begin
  if Sender=nil then ;
  E:=CurrentEditor;
  If (E<>Nil) then
    SaveEditorAs(E);
end;

procedure TMainForm.ASaveExecute(Sender: TObject);

Var
  E : TEditorPage;

begin
  if Sender=nil then ;
  E:=CurrentEditor;
  If (E<>Nil) then
    SaveEditor(E);
end;

procedure TMainForm.DoFormat(Sender: TObject);
begin
  if Sender=nil then ;
  If Assigned(CurrentEditor) then
    CurrentEditor.InsertTag(TTagType((Sender as TAction).Tag));
end;

procedure TMainForm.HaveEditor(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(CurrentEditor<>Nil);
end;

procedure TMainForm.InsertStructure(Sender: TObject);

begin
  InsertNode(TNodeType((Sender as TAction).Tag));
end;

procedure TMainForm.MainFormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Sender=nil then ;
  CanClose:=AllowClose;
end;

procedure TMainForm.MainFormDestroy(Sender: TObject);
begin
  if Sender=nil then ;
  SaveRecent;
  FreeAndNil(FRecent);
end;


procedure TMainForm.BuildReopenList;

  Function NewRecentMenuItem (Nr : Integer;AFileName : string) : TRecentMenuItem;

  begin
    Result:=TRecentMenuItem.Create(Self);
    If Nr<10 then
      Result.Caption:='&'+IntToStr(Nr)+' '+AFileName
    else
      Result.Caption:=AFileName;
    Result.FileName:=AFileName;
    Result.OnCLick:=@FileReopen;
  end;

var I : integer;
    mi : TRecentMenuItem;

begin
  with MIRecent do
    begin
    CLear;
    for I:=FRecent.count-1 downto 0 do
      begin
      mi:=NewRecentMenuItem (I,FRecent[I]);
      Add(mi);
      end;
    end;
end;

procedure TMainForm.AddTorecent(FN: String);
Var
  Index : Integer;

begin
  FN:=ExpandFileName(FN);
  With FRecent do
    begin
    Index:=IndexOf(FN);
    If Index<>-1 then
      Delete(Index);
    Insert(0,FN);
    While Count>MaxRecentUsed do
      Delete(Count-1);
    end;
  BuildReopenList;
end;

procedure TMainForm.NewFile;

Const
  template = '<?xml version="1.0" encoding="ISO8859-1"?>'+LineEnding+
             '<fpdoc-descriptions>'+LineEnding+
             '</fpdoc-descriptions>'+LineEnding;

Var
  S : TStringStream;

begin
  With CreatePage do
    begin
    If FileExists(SFileTemplate) then
      LoadFromFile(SFileTemplate)
    else
      begin
      S:=TStringStream.Create(Template);
      Try
        LoadFromStream(S)
      finally
        S.Free;
      end;
      end;
    end;
end;

procedure TMainForm.OpenFile(FN: String);
begin
  IF (FN<>'') then
    begin
    If FileExists(FN) then
      With CreatePage do
        begin
        LoadFromFile(FN);
        AddToRecent(Fn);
        end;
    end;
end;

procedure TMainForm.SaveEditorAs(E: TEditorPage);

var
  Fn : String;

begin
  with SDMain do
    begin
    FileName:=E.FileName;
    if execute then
      begin
      FN:=FileName;
      If ExtractFileExt(FN)='' then
        FN:=FN+DefaultExtension;
      E.SaveToFile(FN);
      AddToRecent(FN);
      end;
    end;
end;

procedure TMainForm.SaveEditor(E: TEditorPage);

begin
  With E do
    begin
    if (FileName=SNewDocument) then
      SaveEditorAs(E)
    else
      SaveToFile(FileName);
    end;
end;

function TMainForm.CloseEditor(E: TEditorPage): Boolean;
begin
  Result:=Not E.Modified;
  If Not Result then
    Case MessageDlg(SFileModified,[E.FileName],mtConfirmation,mbYesNoCancel,0) of
      mrYes : begin
              SaveEditor(E);
              E.Free;
              Result:=True;
              end;
      mrNo  : begin
              E.Free;
              Result:=True;
              end;
    end
  else
    E.Free;
end;

procedure TMainForm.LoadCommandLine;
Var
  I : Integer;

begin
  I:=1;
  While I<=ParamCount do
    begin
    If FileExists(ParamStr(i)) then
      OpenFile(Paramstr(I));
    Inc(I);
    end;
end;

procedure TMainForm.LoadRecent;
Var
  I,Count : Integer;
  S : String;

begin
  FRecent.Clear;
  With TInifile.Create(GetoptionFileName) do
    begin
    Count:=ReadInteger('Recent','Count',0);
    For I:=1 to Count do
      begin
      S:=ReadString('Recent','File'+IntToStr(i),'');
      If S<>'' then
        FRecent.Add(S);
      end;
    end;
  BuildReopenList;
end;

procedure TMainForm.SaveRecent;
Var
  I : Integer;
begin
  With TInifile.Create(GetoptionFileName) do
    try
      EraseSection('Recent');
      WriteInteger('Recent','Count',FRecent.Count);
      For I:=1 to FRecent.Count do
        WriteString('Recent','File'+IntToStr(i),FRecent[i-1]);
      UpdateFile;
    Finally
      Free;
    end;
end;


function TMainForm.CreatePage: TEditorPage;
begin
  Result:=TEditorPage.Create(self);
  Result.Caption:=SNewDocument;
  Result.PageControl:=PCFiles;
  // This should go when it's fixed in the LCL.
  Result.Parent:=PCFiles;
  PCFiles.ActivePage:=Result;
end;

function TMainForm.Currenteditor: TEditorPage;
begin
  If PCFiles.PageCount=0 then
    Result:=Nil
  else
    Result:=PCFiles.ActivePage as TEditorPage;
end;

function TMainForm.AllowClose: Boolean;

begin
  Result:=True;
  While (PCFiles.PageCount>0) and Result do
    Result:=CloseEditor(PCFiles.Pages[PCFiles.PageCount-1] as TEditorPage);
end;

Type
  TSkeletonData = Record
    InputFile,
    OutputFile,
    PackageName,
    AdditionalOptions : String;
    DisableArguments,
    DisableResults,
    DisablePrivate,
    DisableProtected,
    DisableSeeAlso,
    DisableErrors : Boolean;
  end;


Function CreateSkeletonFile(Const S : TSkeletonData) : Boolean;

Var
  Cmd : String;

begin
  With S do
    begin
    cmd:=cmdMakeSkel+' ';
    cmd:=cmd+format('--input=''%s %s''',[Inputfile,Additionaloptions]);
    cmd:=cmd+' --output='+OutputFile;
    cmd:=cmd+' --package='+PackageName;
    If DisableErrors then
      cmd:=cmd+' --disable-errors';
    If DisableSeeAlso then
      cmd:=cmd+' --disable-seealso';
    If DisableProtected then
      cmd:=cmd+' --disable-protected'
    else if DisablePrivate then
      cmd:=cmd+' --disable-private';
    If DisableResults then
      cmd:=cmd+' --disable-function-results';
    If DisableArguments then
      cmd:=cmd+' --disable-arguments';
    Writeln(cmd);
    With TProcess.Create(Nil) do
      try
        CommandLine:=cmd;
        options:=[poWaitOnExit];
        Execute;
        If (ExitStatus<>0) then
          begin
          Writeln('error detected ',ExitStatus );
          If FileExists(OutputFile) then
            Result:=MessageDlg(SSkelErrorWithFile,[ExitStatus],mtWarning,mbYesNo,0)=mrYes
          else
            begin
            MessageDlg(SSkelErrorWithoutFile,[ExitStatus],mtError,[mbOk],0);
            Result:=False;
            end;
          end
        else
          Result:=FileExists(OutputFile);
      finally
        Free;
      end;
    end;
end;

Procedure TMainForm.NewFromFile;

Var
  SkeletonData : TSkeletonData;
  OK : Boolean;
  
begin
  With TMakeSkelform.Create(Self) do
    try
      Caption:=SMakeSkelFromSource;
      OK:=ShowModal=mrOK;
      If OK Then
        With SkeletonData do
          begin
          InputFile:=FEInputFile.Text;
          OutputFile:=FEOutputFile.Text;
          PackageName:=EPackage.Text;
          AdditionalOptions:=EAdditionalOptions.Text;
          DisableArguments:=CBDisableArguments.Checked;
          DisableResults:=CBDisableResults.Checked;
          DisablePrivate:=CBDisablePrivate.Checked;
          DisableProtected:=CBDisableProtected.Checked;
          DisableSeeAlso:=CBDisableSeeAlso.Checked;
          DisableErrors:=CBDisableErrors.Checked;
          end;
    Finally
      Free;
    end;
  If OK and CreateSkeletonFile(SkeletonData) then
    OpenFile(SkeletonData.OutPutFile)
end;

Procedure TMainForm.InsertNode (NT : TNodeType) ;

Var
  S : AnsiString;

begin
  If (CurrentEditor<>Nil) then
    With TNewNodeForm.Create(Self) do
      Try
        S:=SNew+' '+NodeNames[nt];
        Case nt of
          ntPackage : S:=S+SForFile+ExtractFileName(CurrentEditor.FileName);
          ntModule: If (CurrentEditor.CurrentPackage<>Nil) then
                       S:=S+SForPackage+CurrentEditor.CurrentPackage['name'];
          ntElement: begin
                     If (CurrentEditor.CurrentModule<>Nil) then
                        S:=S+SForModule+CurrentEditor.CurrentModule['name'];
                     If Assigned(CurrentEditor.CurrentElement) then
                       ENodeName.SelText:=CurrentEditor.CurrentElement['name'];
                     end;
          ntTopic : begin
                    if (CurrentEditor.CurrentTopic<>Nil) then
                        S:=S+SForTopic+CurrentEditor.CurrentPackage['name']
                    else if (CurrentEditor.CurrentModule<>Nil) then
                       S:=S+SForModule+CurrentEditor.CurrentModule['name']
                    else if (CurrentEditor.CurrentPackage<>Nil) then
                        S:=S+SForPackage+CurrentEditor.CurrentPackage['name']
                    end;
        end;
        Caption:=S;
        S:='';
        If ShowModal=mrOK Then
          begin
          S:=ENodeName.Text;
          Case nt of
            ntPackage : CurrentEditor.NewPackage(S);
            ntModule  : CurrentEditor.NewModule(S);
            ntElement : CurrentEditor.NewElement(S);
            ntTopic   : CurrentEditor.NewTopic(S);
          end;
          end;
      finally
        Free;
      end;
end;

Procedure TMainForm.InsertLink;

begin
  If Assigned(CurrentEditor) then
    With TLinkForm.Create(Self) do
      Try
        Caption:=SInsertLink;
        ELinkText.Text:=CurrentEditor.CurrentSelection;
        CBTarget.Items.BeginUpdate;
        Try
          CurrentEditor.GetElementList(CBTarget.Items);
        Finally
          CBTarget.Items.EndUpdate;
        end;
        If ShowModal=mrOK Then
          CurrentEditor.InsertLink(CBTarget.Text,ELinkText.Text);
      Finally
        free;
      end;
end;

Procedure TMainForm.InsertTable;

Var
  R,C : Integer;

begin
  With TTableForm.Create(Self) do
    try
      Caption:=SInsertTable;
      SERows.Text:=IntToStr(3);
      SEColumns.Text:=IntToStr(3);
      If ShowModal=mrOK Then
        begin
        R:=StrToIntDef(SERows.Text,3);
        C:=StrToIntDef(SEColumns.Text,3);
        CurrentEditor.InsertTable(C,R,CBUseHeaderRow.Checked);
        end;
    Finally
      Free;
    end;
end;

procedure TMainForm.ShowAbout;

begin
  With TAboutForm.Create(Self) do
    Try
      ShowModal
    finally
      Free;
    end;
end;

procedure TMainForm.GetCurrentFiles(List: TStrings);

Var
  I : Integer;

begin
  For I:=0 to PCFiles.PageCount-1 do
    List.Add(TEditorPage(PCFiles.Pages[i]).FileName);
end;


initialization
  {$I frmmain.lrs}

end.

