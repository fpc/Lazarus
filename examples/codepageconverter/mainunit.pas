{
      Author: BARKO, OPINFOS d.o.o., SLOVENIA http://www.opinfos.com
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
}
unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ComCtrls, FileFind, Menus;

type
  TLazConverterForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    FileSearch1: TFileSearch;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ListView1: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    ProgressBar1: TProgressBar;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FileSearch1ChangeFolder(fullpath: string; info: TSearchRec);
    procedure FileSearch1FileFind(fullpath: string; info: TSearchRec);
    procedure FileSearch1Finish(Sender: TObject);
    procedure Form1Show(Sender: TObject);
    procedure LazConverterFormResize(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  LazConverterForm: TLazConverterForm;

implementation uses viewunit;


function origpath:string;
var tmp:string;
begin
  tmp:=StrPas(argv[0]);
  tmp:=ExpandFileName(tmp);
  tmp:=ExtractFileDir(tmp);
  tmp:=IncludeTrailingPathDelimiter(tmp);
  result:=tmp;
end;

function replace (source: string; src, rep: string):string;
begin
  result:=StringReplace(source,src,rep,[rfReplaceAll]);
end;


{ TLazConverterForm }

procedure TLazConverterForm.Button1Click(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
  begin
    edit1.text:=IncludeTrailingPathDelimiter(ExtractFileDir(SelectDirectoryDialog1.FileName));
  end;
end;

procedure TLazConverterForm.Button2Click(Sender: TObject);
var tmp:string;
begin
  listview1.items.clear;
  tmp:=trim(Edit1.text);
  if tmp='' then
  begin
    button1click(sender);abort;
  end;
  edit1.text:=IncludeTrailingPathDelimiter(tmp);
  if DirectoryExists(tmp) then
  begin
    tmp:=tmp+'*.pas;*.lfm;*.lrs;*.inc';
    FileSearch1.SearchFile:=tmp;
    FileSearch1.Start;
  end;
end;

procedure TLazConverterForm.Button3Click(Sender: TObject);
var tmp:tstringlist;
    i:integer;
    ok:boolean;

               function ConvertMe(var tmp:tstringlist):boolean;
               var cpin,cpout:tstringlist;
                   ok:boolean;
                   i:integer;
               begin
                 ok:=true;result:=true;
                 if trim(tmp.text)='' then result:=false else
                 begin
                   cpin:=tstringlist.create;
                   cpout:=tstringlist.create;
                   try
                     try
                       cpin.LoadFromFile(origpath+combobox1.text);
                     except
                       ok:=false;
                     end;
                     try
                       cpout.LoadFromFile(origpath+combobox2.text);
                     except
                       ok:=false;
                     end;
                     if ok then
                     begin
                       if cpin.Count=cpout.Count then
                       begin
                         for i:=0 to cpin.Count-1 do
                         begin
                           cpin[i]:=trim(cpin[i]);
                           cpout[i]:=trim(cpout[i]);
                           if (cpin[i]<>'') and (cpout[i]<>'') then
                           begin
                             tmp.text:=Replace(tmp.text,char(strtoint(cpin[i])),char(strtoint(cpout[i])));
                           end;
                         end;
                       end else result:=false;
                     end else result:=false;
                   finally
                     cpin.free;
                     cpout.free;
                   end;
                 end;
               end;
    
               function ConvertMeLRS(var tmp:tstringlist):boolean;
               var cpin,cpout:tstringlist;
                   ok:boolean;
                   i:integer;
               begin
                 result:=true;
                 ok:=true;
                 if trim(tmp.text)='' then result:=false else
                 begin
                   cpin:=tstringlist.create;
                   cpout:=tstringlist.create;
                   try
                     try
                       cpin.LoadFromFile(origpath+combobox1.text);
                     except
                      ok:=false;
                     end;
                     try
                       cpout.LoadFromFile(origpath+combobox2.text);
                     except
                       ok:=false;
                     end;
                     if ok then
                     begin
                       if cpin.Count=cpout.Count then
                       begin
                         for i:=0 to cpin.Count-1 do
                         begin
                           cpin[i]:=trim(cpin[i]);
                           cpout[i]:=trim(cpout[i]);
                           if (cpin[i]<>'') and (cpout[i]<>'') then
                           begin
                             tmp.text:=Replace(tmp.text,'#'+cpin[i],'#'+cpout[i]);
                           end;
                         end;
                       end else result:=false;
                     end else result:=false;
                   finally
                     cpin.free;
                     cpout.free;
                   end;
                 end;
               end;

begin
  if FileSearch1.filesfound.Count-1=-1 then abort;
  if trim(combobox1.text)='' then abort;
  if trim(combobox2.text)='' then abort;
  tmp:=tstringlist.create;
  try
    ProgressBar1.Min:=0;
    ProgressBar1.Max:=FileSearch1.filesfound.count;
    ProgressBar1.StepBy(1);
    for i:=0 to FileSearch1.filesfound.count-1 do
    begin
      ok:=true;
      try
        tmp.LoadFromFile(filesearch1.filesfound[i]);
      except
        ok:=false;
      end;
      if ok then
      begin
        if pos('.LRS',uppercase(filesearch1.filesfound[i]))<>0 then
        begin
          if ConvertMeLRS(tmp) then
          begin
            ok:=true;
            try
              tmp.SaveToFile(filesearch1.filesfound[i]);
            except ok:=false; end;
          end;
        end else
        begin
          if ConvertMe(tmp) then
          begin
            ok:=true;
            try
              tmp.SaveToFile(filesearch1.filesfound[i]);
            except ok:=false; end;
          end;
        end;
      end;
      ProgressBar1.StepIt;
    end;
  finally
    ProgressBar1.Position:=0;
    tmp.free;
    label6.caption:='Done!';
  end;
end;

procedure TLazConverterForm.Button4Click(Sender: TObject);
begin
  if listview1.selected<>NIL then
  begin
    filesearch1.FilesFound.Delete(filesearch1.filesfound.IndexOf(listview1.selected.caption));
    listview1.selected.delete;
    filesearch1finish(sender);
  end;
end;

procedure TLazConverterForm.Button5Click(Sender: TObject);
begin
  try
   if listview1.selected<>nil then
   begin
     viewform.caption:='VIEW : '+listview1.selected.caption;
     viewform.SynMemo1.Lines.LoadFromFile(listview1.selected.caption);
     viewform.showmodal;
   end;
  except end;
end;

procedure TLazConverterForm.CheckBox1Click(Sender: TObject);
begin
  FileSearch1.RecurseSubFolders:=CheckBox1.Checked;
end;

procedure TLazConverterForm.FileSearch1ChangeFolder(fullpath: string; info: TSearchRec);
begin
  label6.caption:=fullpath;
end;

procedure TLazConverterForm.FileSearch1FileFind(fullpath: string; info: TSearchRec);
begin
  listview1.items.add;
  listview1.items[listview1.items.count-1].caption:=fullpath+info.Name;
  listview1.items[listview1.items.count-1].subitems.add(UpperCase(ExtractFileExt(info.Name)));
end;

procedure TLazConverterForm.FileSearch1Finish(Sender: TObject);
begin
  label6.caption:='Searching done... press CONVERT button! Files: '+inttostr(filesearch1.filesfound.count);
end;

procedure TLazConverterForm.Form1Show(Sender: TObject);
begin
  if paramcount<>0 then edit1.text:=paramstr(1) else edit1.text:='';
  try
    ComboBox1.items.LoadFromFile(origpath+'codepages.ini');
  except end;
  try
    ComboBox2.items.LoadFromFile(origpath+'codepages.ini');
  except end;
  ComboBox1.text:='';
  ComboBox2.text:='';
  CheckBox1.Checked:=True;
  Edit1.SetFocus;
  listview1.Columns[0].Width:=Width-70;
  button4.enabled:=false;
  button5.enabled:=false;
  MenuItem1.Enabled:=false;
  MenuItem3.Enabled:=false;
end;

procedure TLazConverterForm.LazConverterFormResize(Sender: TObject);
begin
  listview1.Columns[0].Width:=Width-70;
end;

procedure TLazConverterForm.ListView1SelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  button4.enabled:=selected;
  button5.enabled:=selected;
  MenuItem1.Enabled:=selected;
  MenuItem3.Enabled:=selected;
end;

initialization
  {$I mainunit.lrs}

end.

