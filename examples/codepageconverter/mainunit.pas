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
  StdCtrls, Buttons, ComCtrls, Menus
  {$IFDEF TRANSLATESTRING}, DefaultTranslator{$ENDIF};

type

  { TLazConverterForm }

  TLazConverterForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Form1Show(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }

  end; 

var
  LazConverterForm: TLazConverterForm;
  allfiles:tstringlist;

implementation


function replace (source: string; src, rep: string):string;
begin
  result:=StringReplace(source,src,rep,[rfReplaceAll]);
end;

function origpath:string;
var tmp:string;
begin
  tmp:=StrPas(argv[0]);
  tmp:=ExpandFileName(tmp);
  tmp:=ExtractFileDir(tmp);
  tmp:=IncludeTrailingPathDelimiter(tmp);
  result:=tmp;
end;

Function LocWord(StartAT,Wordno:integer;Str:string):integer;
{local proc used by PosWord and Extract word}
var
 W,L: integer;
 Spacebefore: boolean;
begin
   If (Str = '') or (wordno < 1) or (StartAT > length(Str)) then
   begin
       LocWord := 0;
       exit;
   end;
   SpaceBefore := true;
   W := 0;
   L := length(Str);
   StartAT := pred(StartAT);
   While (W < Wordno) and (StartAT <= length(Str)) do
   begin
       StartAT := succ(StartAT);
       If SpaceBefore and (Str[StartAT] <> ' ') then
       begin
           W := succ(W);
           SpaceBefore := false;
       end
       else
           If (SpaceBefore = false) and (Str[StartAT] = ' ') then
               SpaceBefore := true;
   end;
   If W = Wordno then
      LocWord := StartAT
   else
      LocWord := 0;
end;


Function ExtractWords(StartWord,NoWords:integer;Str:string):string;
var Start, finish : integer;
begin
   If Str = '' then
   begin
       ExtractWords := '';
       exit;
   end;
   Start := LocWord(1,StartWord,Str);
   If Start <> 0 then
      finish := LocWord(Start,succ(NoWords),Str)
   else
   begin
       ExtractWords := '';
       exit;
   end;
   If finish = 0 then {5.02A}
      finish := succ(length(Str));
   Repeat
       finish := pred(finish);
   Until Str[finish] <> ' ';
   ExtractWords := copy(Str,Start,succ(finish-Start));
end;  {Func ExtractWords}


Function WordCnt(Str:string):integer;
var
 W,I: integer;
 SpaceBefore: boolean;
begin
   If Str = '' then
   begin
       WordCnt := 0;
       exit;
   end;
   SpaceBefore := true;
   W := 0;
   For  I :=  1 to length(Str) do
   begin
       If SpaceBefore and (Str[I] <> ' ') then
       begin
           W := succ(W);
           SpaceBefore := false;
       end
       else
           If (SpaceBefore = false) and (Str[I] = ' ') then
               SpaceBefore := true;
   end;
   WordCnt := W;
end;



function IsIn(Iskano:string; baza:string):boolean;
var
    index,mindex,counter:integer;
    tmp1:string;
    tmp2:string;
begin
  result:=false;
  tmp1:=uppercase(iskano);
  tmp2:=uppercase(baza);
  counter:=0;
  if pos('"',tmp1)<>0 then
  begin
    mindex:=1;
    tmp1:=Replace(tmp1,'"','');
    if pos(tmp1,tmp2)<>0 then
    begin
      counter:=1;
    end;
  end else
  begin
    mindex:=WordCnt(tmp1);
    if mindex>0 then
    begin
      for index:=1 to mindex do
      begin
        if Pos(ExtractWords(index,1,tmp1),tmp2)<>0 then inc(counter);
      end;
    end;
  end;
  if counter=mindex then
  begin
    result:=true;
  end;
end;


procedure GetAllFiles(mask: string; unignore:string; subdirs:boolean);
var
 search: TSearchRec;
 directory: string;
 tmp:string;

const
{$ifdef unix}
 msk='*';
 delimeter='/';
 {$else}
 msk='*.*';
 delimeter='\';
{$endif}

begin
 directory := ExtractFilePath(mask);

 // find all files
 if FindFirst(mask, faAnyFile-faDirectory, search) = 0 then
 begin
   repeat
     // add the files to the listbox
     tmp:=trim(ExtractFileExt(search.Name));
     if tmp<>'' then
     begin
       if pos('.',tmp)=1 then delete(tmp,1,1);
       if IsIn(tmp,unignore) then
       begin
         allfiles.add(directory + search.Name);
       end;
     end;
   until FindNext(search) <> 0;
 end;
 if subdirs then
 begin
   // Subdirectories/ Unterverzeichnisse
   if FindFirst(directory + msk, faDirectory, search) = 0 then
   begin
     repeat
       if ((search.Attr and faDirectory) = faDirectory) and (search.Name[1] <> '.') then
         GetAllFiles(directory + search.Name + delimeter + ExtractFileName(mask),unignore,subdirs);
     until FindNext(search) <> 0;
     FindClose(search);
   end;
 end;
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

const
{$ifdef unix}
  msk='*';
{$else}
  msk='*.*';
{$endif}
begin
  memo1.text:='';
  tmp:=trim(Edit1.text);
  if tmp='' then
  begin
    button1click(sender);abort;
  end;
  edit1.text:=IncludeTrailingPathDelimiter(tmp);
  if DirectoryExists(tmp) then
  begin
    AllFiles.Text:='';
    GetAllFiles(tmp+msk,'pas lfm lrs inc',checkbox1.checked);
    label6.caption:='Searching done... press CONVERT button! Files: '+inttostr(allfiles.count);
    memo1.text:=allfiles.text;
    caption:=allfiles[0];
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
  if allfiles.Count-1=-1 then abort;
  if trim(combobox1.text)='' then abort;
  if trim(combobox2.text)='' then abort;
  tmp:=tstringlist.create;
  try
    ProgressBar1.Min:=0;
    ProgressBar1.Max:=allfiles.count;
    ProgressBar1.StepBy(1);
    for i:=0 to allfiles.count-1 do
    begin
      ok:=true;
      try
        tmp.LoadFromFile(allfiles[i]);
      except
        ok:=false;
      end;
      if ok then
      begin
        if pos('.LRS',uppercase(allfiles[i]))<>0 then
        begin
          if ConvertMeLRS(tmp) then
          begin
            ok:=true;
            try
              tmp.SaveToFile(allfiles[i]);
            except ok:=false; end;
          end;
        end else
        begin
          if ConvertMe(tmp) then
          begin
            ok:=true;
            try
              tmp.SaveToFile(allfiles[i]);
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
end;

initialization
  {$I mainunit.lrs}
  allfiles:=tstringlist.create;

finalization
  allfiles.free;
end.

