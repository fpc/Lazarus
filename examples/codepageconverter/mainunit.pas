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
  FileUtil, StdCtrls, Buttons, ComCtrls, Menus,
  DefaultTranslator, FileCtrl;

type

  { TLazConverterForm }

  TLazConverterForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    FileListBox1: TFileListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    ProgressBar1: TProgressBar;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Form1Show(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure CountFiles;
  end; 

var
  LazConverterForm: TLazConverterForm;
  
const msgdone = 'Searching done... press CONVERT button! Files: ';

implementation

{$R *.lfm}

function replace (source: string; src, rep: string):string;
begin
  result:=StringReplace(source,src,rep,[rfReplaceAll]);
end;

function origpath:string;
var tmp:string;
begin
  tmp:=StrPas(argv[0]);
  tmp:=ExpandFileNameUTF8(tmp);
  tmp:=ExtractFileDir(tmp);
  tmp:=IncludeTrailingPathDelimiter(tmp);
  result:=tmp;
end;

procedure TLazConverterForm.CountFiles;
begin
  if FileListBox1.Items.Count-1<>-1 then
  begin
    label6.caption:=msgdone+inttostr(FileListBox1.Items.Count-1);
  end else label6.caption:=msgdone+'0';
end;

procedure TLazConverterForm.Button1Click(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
  begin
    edit1.text:=IncludeTrailingPathDelimiter(SelectDirectoryDialog1.FileName);
  end;
end;

procedure TLazConverterForm.Button2Click(Sender: TObject);
begin
  if trim(edit1.text)='' then
  begin
    button1click(sender);abort;
  end;
  FileListBox1.Mask:='';
  FileListBox1.Directory:='';
  FileListBox1.Clear;
  FileListBox1.Directory:=Edit1.text;
  FileListBox1.Mask:='*.pas;*.lfm;*.lrs;*.inc;*.lrt';
  CountFiles;
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
           cpin.LoadFromFile(UTF8ToSys(origpath+combobox1.text));
         except
           ok:=false;
         end;
         try
           cpout.LoadFromFile(UTF8ToSys(origpath+combobox2.text));
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
           cpin.LoadFromFile(UTF8ToSys(origpath+combobox1.text));
         except
          ok:=false;
         end;
         try
           cpout.LoadFromFile(UTF8ToSys(origpath+combobox2.text));
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
  if FileListBox1.Items.count-1=-1 then abort;
  if trim(combobox1.text)='' then abort;
  if trim(combobox2.text)='' then abort;
  tmp:=tstringlist.create;
  try
    ProgressBar1.Min:=0;
    ProgressBar1.Max:=FileListBox1.Items.count;
    ProgressBar1.StepBy(1);
    for i:=0 to FileListBox1.Items.count-1 do
    begin
      ok:=true;
      try
        tmp.LoadFromFile(UTF8ToSys(FileListBox1.Directory+FileListBox1.Items[i]));
      except
        ok:=false;
      end;
      if ok then
      begin
        if uppercase(ExtractFileExt(FileListBox1.Items[i]))='.LRS' then
        begin
          if ConvertMeLRS(tmp) then
          begin
            ok:=true;
            try
              tmp.SaveToFile(UTF8ToSys(FileListBox1.Directory+FileListBox1.Items[i]));
            except ok:=false; end;
          end;
        end else
        begin
          if ConvertMe(tmp) then
          begin
            ok:=true;
            try
              tmp.SaveToFile(UTF8ToSys(FileListBox1.Directory+FileListBox1.Items[i]));
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
    FileListBox1.Mask:='';
    FileListBox1.Directory:='';
    FileListBox1.Clear;
  end;
end;

procedure TLazConverterForm.Form1Show(Sender: TObject);
begin
  if paramcount<>0 then edit1.text:=ParamStrUTF8(1) else edit1.text:='';
  try
    ComboBox1.items.LoadFromFile(UTF8ToSys(origpath+'codepages.ini'));
  except end;
  try
    ComboBox2.items.LoadFromFile(UTF8ToSys(origpath+'codepages.ini'));
  except end;
  ComboBox1.text:='';
  ComboBox2.text:='';
  Edit1.SetFocus;
  FileListBox1.Mask:='';
  FileListBox1.Directory:='';
  FileListBox1.Clear;
end;

procedure TLazConverterForm.MenuItem1Click(Sender: TObject);
begin
 if FileListBox1.Items.count-1=-1 then abort;
 try
  FileListBox1.Items.Delete(FileListBox1.ItemIndex);
 except end;
 CountFiles;
end;

end.

