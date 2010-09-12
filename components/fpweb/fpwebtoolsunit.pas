unit fpWebToolsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProjectIntf, LCLType;

type

   { THtmlFileDescriptor }

   THtmlFileDescriptor = class(TProjectFileDescriptor)
   public
     constructor Create; override;
     function GetLocalizedName: string; override;
     function GetLocalizedDescription: string; override;
     function GetResourceSource(const ResourceName: string): string; override;
     function CreateSource(const Filename, SourceName,
                           ResourceName: string): string; override;
     procedure UpdateDefaultPascalFileExtension(const DefPasExt: string); override;
   end;

   { TJSFileDescriptor }

   TJSFileDescriptor = class(TProjectFileDescriptor)
   public
     constructor Create; override;
     function GetLocalizedName: string; override;
     function GetLocalizedDescription: string; override;
     function GetResourceSource(const ResourceName: string): string; override;
     function CreateSource(const Filename, SourceName,
                           ResourceName: string): string; override;
     procedure UpdateDefaultPascalFileExtension(const DefPasExt: string); override;
   end;

   { TCSSFileDescriptor }

   TCSSFileDescriptor = class(TProjectFileDescriptor)
   public
     constructor Create; override;
     function GetLocalizedName: string; override;
     function GetLocalizedDescription: string; override;
     function GetResourceSource(const ResourceName: string): string; override;
     function CreateSource(const Filename, SourceName,
                           ResourceName: string): string; override;
     procedure UpdateDefaultPascalFileExtension(const DefPasExt: string); override;
   end;

procedure Register;

//---
procedure ProcHTMLTextBold(Sender: TObject);
procedure ProcHTMLTextItalic(Sender: TObject);
procedure ProcHTMLTextUnderline(Sender: TObject);
procedure ProcHTMLHR(Sender: TObject);
procedure ProcHTMLBR(Sender: TObject);
procedure ProcHTMLNBSP(Sender: TObject);
procedure ProcHTMLComment(Sender: TObject);
procedure ProcHTMLImageTag(Sender: TObject);
procedure ProcHTMLTextAlignLeft(Sender: TObject);
procedure ProcHTMLTextAlignRight(Sender: TObject);
procedure ProcHTMLTextAlignCenter(Sender: TObject);
procedure ProcHTMLTextAlignJustify(Sender: TObject);

//--
procedure ProcHTMLTable(Sender: TObject);
procedure ProcHTMLGenList(Sender: TObject);

procedure ProcHTMLTableRow(Sender: TObject);
procedure ProcHTMLTableData(Sender: TObject);

procedure ProcHTMLTableRowWD(Sender: TObject);
procedure ProcHTMLTableDataWD(Sender: TObject);

procedure ProcHTMLTextHeader1(Sender: TObject);
procedure ProcHTMLTextHeader2(Sender: TObject);
procedure ProcHTMLTextHeader3(Sender: TObject);
procedure ProcHTMLTextHeader4(Sender: TObject);
procedure ProcHTMLTextHeader5(Sender: TObject);
procedure ProcHTMLColor(Sender: TObject);


procedure ProcHTMLForm(Sender: TObject);
procedure ProcHTMLInputTag(Sender: TObject);
procedure ProcHTMLInputSubmitTag(Sender: TObject);
procedure ProcHTMLInputResetTag(Sender: TObject);

procedure ProcOtherInsertFileName(Sender: TObject);

procedure InsertTextToCurEditor(const S:string);
implementation

uses LResources, NewItemIntf, Forms, Controls, IDECommands,
  MenuIntf, SrcEditorIntf, Dialogs,
  //HTML
  fpWebNewHTMLFileUnit, fpWebStrConsts, fpWebNewHtmlTableUnit,
  fpwebNewHTMLListUnit, fpwebNewHtmlTagTRUnit, fpwebNewHTMLFormUnit,
  fpwebNewHTMLInputUnit, fpwebNewHTMLImgUnit,
  //Other
  fpIDEExtEditorInsertFileNameUnit;

var
  //Standart items
  CmdHTMLBold : TIDECommand;
  CmdHTMLItalic : TIDECommand;
  CmdHTMLUnderline : TIDECommand;
  //----
  CmdHTMLBR : TIDECommand;
  //Paragraph
  CmdHTMLNBSP : TIDECommand;
  //----
  //Link
  CmdHTMLIMG : TIDECommand;
  CmdHTMLHR : TIDECommand;
  CmdHTMLComment : TIDECommand;
  //----
  CmdHTMLTextAlignLeft : TIDECommand;
  CmdHTMLTextAlignRight : TIDECommand;
  CmdHTMLTextAlignCenter : TIDECommand;
  CmdHTMLTextAlignJustify : TIDECommand;
  //Span text


  CmdHTMLTable : TIDECommand;
  CmdHTMLList : TIDECommand;

  CmdHTMLTableRow : TIDECommand;
  CmdHTMLTableData : TIDECommand;

  CmdHTMLTableRowWD : TIDECommand;
  CmdHTMLTableDataWD : TIDECommand;

  CmdHTMLTextStyleH1 : TIDECommand;
  CmdHTMLTextStyleH2 : TIDECommand;
  CmdHTMLTextStyleH3 : TIDECommand;
  CmdHTMLTextStyleH4 : TIDECommand;
  CmdHTMLTextStyleH5 : TIDECommand;
  CmdHTMLColor : TIDECommand;



  CmdHTMLFrom : TIDECommand;
  CmdHTMLInput : TIDECommand;
  CmdHTMLInputSubmit : TIDECommand;
  CmdHTMLInputReset : TIDECommand;


  CmdOtherInsFileName : TIDECommand;

procedure CreateHTMLToolsMenu;
var
  Key: TIDEShortCut;
  Cat: TIDECommandCategory;
  mnuHTMLMain : TIDEMenuSection;
  mnuHTMLSection : TIDEMenuSection;

  mnuHTMLStandart : TIDEMenuSection;
  mnuHTMLStyles : TIDEMenuSection;
  mnuHTMLTables : TIDEMenuSection;
  mnuHTMLLists : TIDEMenuSection;
  mnuHTMLForms : TIDEMenuSection;
  mnuHTMLOther : TIDEMenuSection;
begin
  Key:=IDEShortCut(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  Cat:=IDECommandList.CreateCategory(nil, 'HTMLEditor', SmiHTMLEditor,
    IDECmdScopeSrcEditOnly);
  //--
  CmdHTMLBold := RegisterIDECommand(Cat, 'HTMLTable', SmiHTMLTextBold, Key, nil, @ProcHTMLTextBold);
  CmdHTMLItalic := RegisterIDECommand(Cat, 'HTMLTable', SmiHTMLTextItalic, Key, nil, @ProcHTMLTextItalic);
  CmdHTMLUnderline := RegisterIDECommand(Cat, 'HTMLTable', SmiHTMLTextUnderline, Key, nil, @ProcHTMLTextUnderline);
  CmdHTMLBR := RegisterIDECommand(Cat, 'HTMLBR', SmiHTMLInsertBR, Key, nil, @ProcHTMLBR);
  //Para
  CmdHTMLNBSP := RegisterIDECommand(Cat, 'HTMLNBSP', SmiHTMLInsertBR, Key, nil, @ProcHTMLNBSP);

  CmdHTMLHR := RegisterIDECommand(Cat, 'HTMLHR', SmiHTMLInsertHR, Key, nil, @ProcHTMLHR);
  CmdHTMLComment := RegisterIDECommand(Cat, 'HTMLComment', SmiHTMLInsertComment, Key, nil, @ProcHTMLComment);
  CmdHTMLIMG := RegisterIDECommand(Cat, 'HTMLIMG', SmiHTMLInsertIMG, Key, nil, @ProcHTMLImageTag);

  CmdHTMLTextAlignLeft :=RegisterIDECommand(Cat, 'HTMLTextAlignLeft', SmiHTMLTextAlignLeft, Key, nil, @ProcHTMLTextAlignLeft);
  CmdHTMLTextAlignRight :=RegisterIDECommand(Cat, 'HTMLTextAlignRight', SmiHTMLTextAlignRight, Key, nil, @ProcHTMLTextAlignRight);
  CmdHTMLTextAlignCenter :=RegisterIDECommand(Cat, 'HTMLTextAlignCenter', SmiHTMLTextAlignCenter, Key, nil, @ProcHTMLTextAlignCenter);
  CmdHTMLTextAlignJustify :=RegisterIDECommand(Cat, 'HTMLTextAlignJustify', SmiHTMLTextAlignJustify, Key, nil, @ProcHTMLTextAlignJustify);

  //--
  CmdHTMLTable := RegisterIDECommand(Cat, 'HTMLTable', SmiHTMLInsertTable, Key, nil, @ProcHTMLTable);
  CmdHTMLList := RegisterIDECommand(Cat, 'HTMLList', SmiHTMLInsertList, Key, nil, @ProcHTMLGenList);

  CmdHTMLTableRow := RegisterIDECommand(Cat, 'HTMLTableRow', SmiHTMLInsertTableRow, Key, nil, @ProcHTMLTableRow);
  CmdHTMLTableData := RegisterIDECommand(Cat, 'HTMLTableData', SmiHTMLInsertTableData, Key, nil, @ProcHTMLTableData);

  CmdHTMLTableRowWD  := RegisterIDECommand(Cat, 'HTMLTableRowWD', SmiHTMLInsertTableRowWD, Key, nil, @ProcHTMLTableRowWD);
  CmdHTMLTableDataWD := RegisterIDECommand(Cat, 'HTMLTableDataWD', SmiHTMLInsertTableDataWD, Key, nil, @ProcHTMLTableDataWD);

  CmdHTMLTextStyleH1 := RegisterIDECommand(Cat, 'HTMLTextStyleH1', SmiHTMLInsertHeader1Level, Key, nil, @ProcHTMLTextHeader1);
  CmdHTMLTextStyleH2 := RegisterIDECommand(Cat, 'HTMLTextStyleH2', SmiHTMLInsertHeader2Level, Key, nil, @ProcHTMLTextHeader2);
  CmdHTMLTextStyleH3 := RegisterIDECommand(Cat, 'HTMLTextStyleH3', SmiHTMLInsertHeader3Level, Key, nil, @ProcHTMLTextHeader3);
  CmdHTMLTextStyleH4 := RegisterIDECommand(Cat, 'HTMLTextStyleH4', SmiHTMLInsertHeader4Level, Key, nil, @ProcHTMLTextHeader4);
  CmdHTMLTextStyleH5 := RegisterIDECommand(Cat, 'HTMLTextStyleH5', SmiHTMLInsertHeader5Level, Key, nil, @ProcHTMLTextHeader5);
  CmdHTMLColor := RegisterIDECommand(Cat, 'HTMLColor', SmiHTMLInsertColor, Key, nil, @ProcHTMLColor);


  CmdHTMLFrom := RegisterIDECommand(Cat, 'HTMLForm', SmiHTMLInsertForm, Key, nil, @ProcHTMLForm);
  CmdHTMLInput:= RegisterIDECommand(Cat, 'HTMLInput', SmiHTMLInsertInput, Key, nil, @ProcHTMLInputTag);
  CmdHTMLInputSubmit := RegisterIDECommand(Cat, 'HTMLInputSubmit', SmiHTMLInsertInputSubmit, Key, nil, @ProcHTMLInputSubmitTag);
  CmdHTMLInputReset := RegisterIDECommand(Cat, 'HTMLInputReset', SmiHTMLInsertInputReset, Key, nil, @ProcHTMLInputResetTag);

  CmdOtherInsFileName:=RegisterIDECommand(Cat, 'OtherInsFN', SmiOtherInsertFN, Key, nil, @ProcOtherInsertFileName);

  //  mnuHTMLMain := RegisterIDEMenuRoot('HTML');//RegisterIDEMenuSection(itmCustomTools, 'HTMLEditor');
  mnuHTMLMain := RegisterIDEMenuSection(mnuMain, 'HTMLEditor');

  mnuHTMLSection:=RegisterIDESubMenu(mnuHTMLMain, 'HTMLEditor', SmiHTMLEditor, nil, nil);

  mnuHTMLStandart := RegisterIDESubMenu(mnuHTMLSection, 'HTMLStandart', SmiHTMLStandart, nil, nil);
    RegisterIDEMenuCommand(mnuHTMLStandart, 'HTMLBold', SmiHTMLTextBold, nil, nil, CmdHTMLBold, 'tag_bold');
    RegisterIDEMenuCommand(mnuHTMLStandart, 'HTMLItalic', SmiHTMLTextItalic, nil, nil, CmdHTMLItalic, 'tag_i');
    RegisterIDEMenuCommand(mnuHTMLStandart, 'HTMLUnderline', SmiHTMLTextUnderline, nil, nil, CmdHTMLUnderline, 'tag_u');
    RegisterIDEMenuCommand(mnuHTMLStandart, '', '-', nil, nil, nil, '');
    RegisterIDEMenuCommand(mnuHTMLStandart, 'HTMLBR', SmiHTMLInsertBR, nil, nil, CmdHTMLBR, 'tag_br');
    //Paragraph
    RegisterIDEMenuCommand(mnuHTMLStandart, 'HTMLNBSP', SmiHTMLInsertNBSP, nil, nil, CmdHTMLNBSP, 'tag_nbsp');
    RegisterIDEMenuCommand(mnuHTMLStandart, '', '-', nil, nil, nil, '');
    //Link
    RegisterIDEMenuCommand(mnuHTMLStandart, 'HTMLIMG', SmiHTMLInsertIMG, nil, nil, CmdHTMLIMG, 'tag_image');
    RegisterIDEMenuCommand(mnuHTMLStandart, 'HTMLHR', SmiHTMLInsertHR, nil, nil, CmdHTMLHR, 'tag_hr');
    RegisterIDEMenuCommand(mnuHTMLStandart, 'HTMLComment', SmiHTMLInsertComment, nil, nil, CmdHTMLComment, 'tag_comm');
    RegisterIDEMenuCommand(mnuHTMLStandart, '', '-', nil, nil, nil, '');
    //--
    RegisterIDEMenuCommand(mnuHTMLStandart, 'HTMLTextAlignLeft', SmiHTMLTextAlignLeft, nil, nil, CmdHTMLTextAlignLeft, 'div_left');
    RegisterIDEMenuCommand(mnuHTMLStandart, 'HTMLTextAlignRight', SmiHTMLTextAlignRight, nil, nil, CmdHTMLTextAlignRight, 'div_right');
    RegisterIDEMenuCommand(mnuHTMLStandart, 'HTMLTextAlignCenter', SmiHTMLTextAlignCenter, nil, nil, CmdHTMLTextAlignCenter, 'div_center');
    RegisterIDEMenuCommand(mnuHTMLStandart, 'HTMLTextAlignJustify', SmiHTMLTextAlignJustify, nil, nil, CmdHTMLTextAlignJustify, 'div_justify');
    //Span text

  //List's
  mnuHTMLLists := RegisterIDESubMenu(mnuHTMLSection, 'HTMLLists', SmiHTMLLists, nil, nil);
    RegisterIDEMenuCommand(mnuHTMLLists, 'HTMLList', SmiHTMLInsertList, nil, nil,
      CmdHTMLList, 'HTMLList');

  //Style's
  mnuHTMLStyles :=RegisterIDESubMenu(mnuHTMLSection, 'HTMLStyle', SmiHTMLStyle, nil, nil);
    RegisterIDEMenuCommand(mnuHTMLStyles, 'HTMLTextHeader1', SmiHTMLInsertHeader1Level, nil, nil,
      CmdHTMLTextStyleH1, 'tag_h1');
    RegisterIDEMenuCommand(mnuHTMLStyles, 'HTMLTextHeader2', SmiHTMLInsertHeader2Level, nil, nil,
      CmdHTMLTextStyleH2, 'tag_h2');
    RegisterIDEMenuCommand(mnuHTMLStyles, 'HTMLTextHeader3', SmiHTMLInsertHeader3Level, nil, nil,
      CmdHTMLTextStyleH3, 'tag_h3');
    RegisterIDEMenuCommand(mnuHTMLStyles, 'HTMLTextHeader4', SmiHTMLInsertHeader4Level, nil, nil,
      CmdHTMLTextStyleH4, 'tag_h4');
    RegisterIDEMenuCommand(mnuHTMLStyles, 'HTMLTextHeader5', SmiHTMLInsertHeader5Level, nil, nil,
      CmdHTMLTextStyleH5, 'tag_h5');
    RegisterIDEMenuCommand(mnuHTMLStyles, 'HTMLColor', SmiHTMLInsertColor, nil, nil,
      CmdHTMLColor, 'color-picker');

  //Table menus
  mnuHTMLTables := RegisterIDESubMenu(mnuHTMLSection, 'HTMLTables', SmiHTMLTables, nil, nil);
    RegisterIDEMenuCommand(mnuHTMLTables, 'HTMLTable', SmiHTMLInsertTable, nil, nil,
      CmdHTMLTable, 'quick_table');
    RegisterIDEMenuCommand(mnuHTMLTables, 'HTMLTableRow', SmiHTMLInsertTableRow, nil, nil,
      CmdHTMLTableRow, 'tag_tr');
    RegisterIDEMenuCommand(mnuHTMLTables, 'HTMLTableData', SmiHTMLInsertTableData, nil, nil,
      CmdHTMLTableData, 'tag_td');
    RegisterIDEMenuCommand(mnuHTMLTables, 'HTMLTableRowWD', SmiHTMLInsertTableRowWD, nil, nil,
      CmdHTMLTableRowWD, 'tag_table_row');
    RegisterIDEMenuCommand(mnuHTMLTables, 'HTMLTableDataWD', SmiHTMLInsertTableDataWD, nil, nil,
      CmdHTMLTableDataWD, 'tag_table_data');


  mnuHTMLForms := RegisterIDESubMenu(mnuHTMLSection, 'HTMLForms', SmiHTMLForms, nil, nil);
    RegisterIDEMenuCommand(mnuHTMLForms, 'HTMLForm', SmiHTMLInsertForm, nil, nil,
      CmdHTMLFrom, 'HTMLForm');
    RegisterIDEMenuCommand(mnuHTMLForms, 'HTMLInput', SmiHTMLInsertInput, nil, nil,
      CmdHTMLInput, 'lineedit');
    RegisterIDEMenuCommand(mnuHTMLForms, 'HTMLInputSubmit', SmiHTMLInsertInputSubmit, nil, nil,
      CmdHTMLInputSubmit, 'submit');
    RegisterIDEMenuCommand(mnuHTMLForms, 'HTMLInputReset', SmiHTMLInsertInputReset, nil, nil,
      CmdHTMLInputReset, 'reset');

  mnuHTMLOther :=RegisterIDESubMenu(mnuHTMLSection, 'HTMLOther', SmiHTMLOther, nil, nil);
    RegisterIDEMenuCommand(mnuHTMLOther, 'OtherInsFN', SmiOtherInsertFN, nil, nil,
      CmdOtherInsFileName, '');

end;

procedure Register;
begin
  RegisterNewItemCategory(TNewIDEItemCategory.Create(SHtmlDesign));
  RegisterProjectFileDescriptor(THtmlFileDescriptor.Create, SHtmlDesign);
  RegisterProjectFileDescriptor(TJSFileDescriptor.Create, SHtmlDesign);
  RegisterProjectFileDescriptor(TCSSFileDescriptor.Create, SHtmlDesign);

  CreateHTMLToolsMenu;
end;

Procedure InsertHTMLSnippet(Const AText : String);

begin
  if Assigned(SourceEditorManagerIntf) and Assigned(SourceEditorManagerIntf.ActiveEditor) then
    With SourceEditorManagerIntf.ActiveEditor do
      Selection:=AText;
end;

Procedure InsertHTMLTag(Const ATag : String; Const AAttribute : String = '');

begin
  if Assigned(SourceEditorManagerIntf) and Assigned(SourceEditorManagerIntf.ActiveEditor) then
    With SourceEditorManagerIntf.ActiveEditor do
      if (AAttribute='') then
        Selection:='<'+ATag+'>'+Selection+'</'+ATag+'>'
      else
        Selection:='<'+ATag+' '+AAttribute+' >'+Selection+'</'+ATag+'>';
end;

procedure ProcHTMLTableRow(Sender: TObject);
begin
  InsertHTMLTag('tr');
end;

procedure ProcHTMLTableData(Sender: TObject);
begin
  InsertHTMLTag('td');
end;

procedure ProcHTMLTextHeader1(Sender: TObject);
begin
  InsertHTMLTag('H1');
end;

procedure ProcHTMLTextHeader2(Sender: TObject);
begin
  InsertHTMLTag('H2');
end;

procedure ProcHTMLTextHeader3(Sender: TObject);
begin
  InsertHTMLTag('H3');
end;

procedure ProcHTMLTextHeader4(Sender: TObject);
begin
  InsertHTMLTag('H4');
end;

procedure ProcHTMLTextHeader5(Sender: TObject);
begin
  InsertHTMLTag('H5');
end;


procedure ProcHTMLTextBold(Sender: TObject);
begin
  InsertHTMLTag('strong');
end;

procedure ProcHTMLTextItalic(Sender: TObject);
begin
  InsertHTMLTag('em');
end;

procedure ProcHTMLTextUnderline(Sender: TObject);
begin
  InsertHTMLTag('u');
end;

procedure ProcHTMLHR(Sender: TObject);
begin
  InsertHTMLSnippet('<HR>');
end;

procedure ProcHTMLBR(Sender: TObject);
begin
  InsertHTMLSnippet('<BR>');
end;

procedure ProcHTMLNBSP(Sender: TObject);
begin
  InsertHTMLSnippet(' &nbsp; ');
end;

procedure ProcHTMLTextAlignLeft(Sender: TObject);
begin
  InsertHTMLTag('div','align="left"');
end;

procedure ProcHTMLTextAlignRight(Sender: TObject);
begin
  InsertHTMLTag('div','align="right"');
end;

procedure ProcHTMLTextAlignCenter(Sender: TObject);
begin
  InsertHTMLTag('div','align="center"');
end;

procedure ProcHTMLTextAlignJustify(Sender: TObject);
begin
  InsertHTMLTag('div','align="justify"');
end;

procedure ProcHTMLComment(Sender: TObject);
begin
  if Assigned(SourceEditorManagerIntf) and Assigned(SourceEditorManagerIntf.ActiveEditor) then
    with SourceEditorManagerIntf.ActiveEditor do
      Selection:='<!-- '+Selection+' -->';
end;

procedure ProcHTMLColor(Sender: TObject);
var
  Dlg:TColorDialog;
begin
  Dlg:=TColorDialog.Create(Application);
  try
    if Dlg.Execute then
      InsertHTMLSnippet('#'+IntToHex(Dlg.Color,6));
  finally
    Dlg.Free;
  end;
end;

procedure ProcHTMLTable(Sender: TObject);
begin
  With TfpWebNewHtmlTableForm.Create(Application) do
    try
      if ShowModal = mrOk then
         InsertHTMLSnippet(HtmlText);
    finally
      fpWebNewHtmlTableForm.Free;
    end;
end;

procedure ProcHTMLGenList(Sender: TObject);
begin
  With TfpWebNewHTMLListForm.Create(Application) do
    try
      if ShowModal = mrOk then
         InsertHTMLSnippet(HtmlText);
    finally
      Free;
    end;
end;

procedure ProcHTMLTableRowWD(Sender: TObject);
begin
  With TfpWebNewHtmlTagTRForm.Create(Application) do
    try
      if ShowModal = mrOk then
         InsertHTMLSnippet(HtmlText);
    Finally
      Free;
    end
end;

procedure ProcHTMLTableDataWD(Sender: TObject);
begin

end;

procedure ProcHTMLForm(Sender: TObject);
begin
  With TfpWebNewHTMLFormForm.Create(Application) do
    try
      InsertHTMLSnippet(HtmlText(SourceEditorManagerIntf.ActiveEditor.Selection));
    finally
      Free;
    end;
end;

procedure ProcHTMLInputTag(Sender: TObject);
begin
  With TfpWebNewHTMLInputForm.Create(Application) do
    try
      if ShowModal = mrOk then
         InsertHTMLSnippet(HtmlText);
    finally
      Free;
    end;
end;

procedure ProcHTMLInputSubmitTag(Sender: TObject);
begin
  With TfpWebNewHTMLInputForm.Create(Application) do
    try
      cbType.Text:='submit';
      edtValue.Text:=SHTMLTagCaptionSubmit;
      if ShowModal = mrOk then
         InsertHTMLSnippet(HtmlText);
    finally
      Free;
    end;
end;

procedure ProcHTMLInputResetTag(Sender: TObject);
begin
  With TfpWebNewHTMLInputForm.Create(Application) do
    try
      cbType.Text:='reset';
      edtValue.Text:=SHTMLTagCaptionReset;
      if ShowModal = mrOk then
         InsertHTMLSnippet(HtmlText);
    finally
      Free;
    end;
end;

procedure ProcHTMLImageTag(Sender: TObject);
begin
  With TfpWebNewHTMLImgForm.Create(Application) do
    try
      if ShowModal = mrOk then
       InsertHTMLSnippet(HtmlText);
    finally
      Free;
    end;
end;


procedure ProcOtherInsertFileName(Sender: TObject);
begin
  With TfpIDEExtEditorInsertFileNameForm.Create(Application) do
    try
      if ShowModal = mrOk then
        InsertHTMLSnippet(SelectedFile);
    finally
      Free;
    end;
end;

procedure InsertTextToCurEditor(const S: string);
var
  i:integer;
begin
  { TODO -oalexs : Add code for positiopn cursor to position of char |, also insert curent selection into new string at position of %s }

{  if Assigned(SourceEditorManagerIntf) and Assigned(SourceEditorManagerIntf.ActiveEditor) then
  begin
    if Pos('|', S)
    SourceEditorManagerIntf.ActiveEditor.Selection:=fpWebIDEExtEditorInsertFileNameForm.SelectedFile;
  end;}
end;

{ THtmlFileDescriptor }

constructor THtmlFileDescriptor.Create;
begin
  inherited Create;
  Name:='Html file';
  DefaultFilename:='index.html';
  DefaultResFileExt:='';
  DefaultFileExt:='.html';
  VisibleInNewDialog:=true;
end;

function THtmlFileDescriptor.GetLocalizedName: string;
begin
  Result:=SHtmlFile;
end;

function THtmlFileDescriptor.GetLocalizedDescription: string;
begin
  Result:=SHtmlFileDesc;
end;

function THtmlFileDescriptor.GetResourceSource(const ResourceName: string
  ): string;
begin
  Result:='';
end;

function THtmlFileDescriptor.CreateSource(const Filename, SourceName,
  ResourceName: string): string;
begin
  Result:='';
  fpWebNewHTMLFileForm:=TfpWebNewHTMLFileForm.Create(Application);
  if fpWebNewHTMLFileForm.ShowModal = mrOk then
  begin
    Result:=fpWebNewHTMLFileForm.HtmlText;
  end;
  fpWebNewHTMLFileForm.Free;
end;

procedure THtmlFileDescriptor.UpdateDefaultPascalFileExtension(
  const DefPasExt: string);
begin
  //inherited UpdateDefaultPascalFileExtension(DefPasExt);
end;

{ TJSFileDescriptor }

constructor TJSFileDescriptor.Create;
begin
  inherited Create;
  Name:='Java script file';
  DefaultFilename:='functions.js';
  DefaultResFileExt:='';
  DefaultFileExt:='.js';
  VisibleInNewDialog:=true;
end;

function TJSFileDescriptor.GetLocalizedName: string;
begin
  Result:=SJSFile;
end;

function TJSFileDescriptor.GetLocalizedDescription: string;
begin
  Result:=SJSFileDesc;
end;

function TJSFileDescriptor.GetResourceSource(const ResourceName: string
  ): string;
begin
  Result:='';
end;

function TJSFileDescriptor.CreateSource(const Filename, SourceName,
  ResourceName: string): string;
begin
  Result:='/* '+SJSSource+ '*/';
end;

procedure TJSFileDescriptor.UpdateDefaultPascalFileExtension(
  const DefPasExt: string);
begin
  //
end;

{ TCSSFileDescriptor }

constructor TCSSFileDescriptor.Create;
begin
  inherited Create;
  Name:='CSS file';
  DefaultFilename:='styles.css';
  DefaultResFileExt:='';
  DefaultFileExt:='.css';
  VisibleInNewDialog:=true;
end;

function TCSSFileDescriptor.GetLocalizedName: string;
begin
  Result:=SCSSFile;
end;

function TCSSFileDescriptor.GetLocalizedDescription: string;
begin
  Result:=SCSSFileDesc;
end;

function TCSSFileDescriptor.GetResourceSource(const ResourceName: string
  ): string;
begin
  Result:='';
end;

function TCSSFileDescriptor.CreateSource(const Filename, SourceName,
  ResourceName: string): string;
begin
  Result:='/* '+SCSSSource+' */';
end;

procedure TCSSFileDescriptor.UpdateDefaultPascalFileExtension(
  const DefPasExt: string);
begin
  //
end;

initialization
  {$I fpWeb_images.inc}
end.

