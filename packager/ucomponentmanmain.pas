{  $Id$  }
{***************************************************************************
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

  Author: Anthony Maro
}

unit UComponentManMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, FileUtil, IDEProcs, UFrmAddComponent;
  
Type
  TRComponent = class(TObject)
  public
    Name: String;
    Page: String;
    unit_name: String;
  end;

type

  { TFrmComponentMan }

  TFrmComponentMan = class(TForm)
    BtnCancel: TBITBTN;
    Bitbtn2: TBITBTN;
    BtnRemove: TBitBtn;
    Button1: TBitBtn;
    DlgLoad: TOPENDIALOG;
    TxtPage: TEDIT;
    Label1: TLABEL;
    Label2: TLABEL;
    Label3: TLABEL;
    Label4: TLABEL;
    LblUnit: TLABEL;
    LblComponent: TLABEL;
    ListComps: TLISTBOX;
    procedure Bitbtn2CLICK(Sender: TObject);
    procedure BtnCancelCLICK(Sender: TObject);
    procedure BtnRemoveCLICK(Sender: TObject);
    procedure Button1CLICK(Sender: TObject);
    procedure FrmMainCREATE(Sender: TObject);
    procedure FrmMainDESTROY(Sender: TObject);
    procedure FrmMainSHOW(Sender: TObject);
    procedure ListCompsCLICK(Sender: TObject);
    procedure PopulateList;
    procedure ParseRegister(ALine: String);
    procedure TxtPageCHANGE(Sender: TObject);
    procedure MakeHeader;
    procedure MakeUses;
    procedure MakeRegister;
    function CountComponents: Integer;
    function GetComponent(I: Integer): String;
    function FindUnitName: String;
  private
    FLazPath: String;
    MyFile: TStringList;
    procedure SetLazPath(const AValue: String);
    { private declarations }
  public
    { public declarations }
    property LazPath: String read FLazPath write SetLazPath;
  end; 

function ShowConfigureCustomComponentDlg(const LazarusDir: string): TModalResult;

implementation

{$R *.lfm}

function ShowConfigureCustomComponentDlg(
  const LazarusDir: string): TModalResult;
var
  FrmComponentMan: TFrmComponentMan;
begin
  FrmComponentMan:=TFrmComponentMan.Create(nil);
  FrmComponentMan.SetLazPath(LazarusDir);
  Result:=FrmComponentMan.ShowModal;
  FrmComponentMan.Free;
end;

{ TFrmComponentMan }

procedure TFrmComponentMan.BtnCancelCLICK(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TFrmComponentMan.BtnRemoveCLICK(Sender: TObject);
var
  MyObj: TRComponent;
begin

  if ListComps.ItemIndex > -1 then begin
    MyObj := ListComps.Items.Objects[ListComps.ItemIndex] as TRComponent;
    if assigned(MyObj) then MyObj.Free;
    ListComps.Items.Delete(ListComps.ItemIndex);
  end;

end;

procedure TFrmComponentMan.Button1CLICK(Sender: TObject);
var
  I, J, NewCompCnt: Integer;
  Found: Boolean;
  MyObj: TRComponent;
begin
  DlgLoad.InitialDir := AppendPathDelim(FLazPath)+'components/custom';
  if DlgLoad.Execute then begin
    // load in and parse the source
    try
      MyFile.Clear;
      MyFile.LoadFromFile(UTF8ToSys(DlgLoad.Filename));
    except
      MessageDlg('Error loading unit: '+DlgLoad.Filename,mtError,[mbCancel],0);
      exit;
    end;
    
    NewCompCnt:=CountComponents;
    //MessageDlg('Found '+inttostr(NewCompCnt), mtInformation,[mbOk],0);
    if NewCompCnt > 30 then begin
      // just to save face if something goes wrong...
      MessageDlg('More than 30 components is not supported.', mtError, [mbCancel],0);
      exit;
    end;

    if NewCompCnt<1 then begin
       MessageDlg('No components found.', mtError, [mbCancel],0);
       exit;
    end;
    
    if FrmAddComponent=nil then
      FrmAddComponent := TFrmAddComponent.Create(Self);
    FrmAddComponent.ListCompAdd.Clear;
    for I := 1 to NewCompCnt do begin
      //MessageDlg(GetComponent(I),mtInformation,[mbOk],0);
      // only add if not already in the list
      Found := False;
      if FrmAddComponent.ListCompAdd.Items.Count > 0 then begin
        for J := 0 to FrmAddComponent.ListCompAdd.Items.Count -1 do begin
          if uppercase(FrmAddComponent.ListCompAdd.Items[J]) = uppercase(trim(GetComponent(I)))
          then Found := True;
        end;
      end;
      if not(Found) then FrmAddComponent.ListCompAdd.Items.Add(trim(GetComponent(I)));
    end;
    if FrmAddComponent.ShowModal = mrOk then begin
      // add selected items...
      for I := 0 to FrmAddComponent.ListCompAdd.Items.Count-1 do begin
        if FrmAddComponent.ListCompAdd.Selected[i] then begin
          // add this one if not already there...
          Found := False;
          if ListComps.Items.Count > 0 then begin
            for J := 0 to ListComps.Items.Count -1 do begin
              if Uppercase(FrmAddComponent.ListCompAdd.Items[i]) = uppercase(ListComps.Items[J]) then Found := True;
            end;
          end; // if listcomps contains items already
          if not(Found) then begin
            MyObj := TRComponent.Create;
            MyObj.Name := FrmAddComponent.ListCompAdd.Items[i];
            MyObj.Page := 'Custom';
            MyObj.unit_name := FindUnitName;
            ListComps.Items.AddObject(MyObj.Name, MyObj);
          end; // if not found
        end; // if listcompadd selected
      end; // for I
    end; // if showmodal

  end;
    
end;

procedure TFrmComponentMan.Bitbtn2CLICK(Sender: TObject);
begin
  // save the file...
  MyFile.Clear;
  MakeHeader;
  MakeUses;
  MakeRegister;
  try
    MyFile.SaveToFile(UTF8ToSys(FLazPath+'components/custom/customidecomps.pas'));
  except
    MessageDlg('Error saving customidecomps.pas!',mtError,[mbCancel],0);
    exit;
  end;
  
  MessageDlg('Changes saved.  Now return to Lazarus and Build the IDE',mtInformation,[mbOk],0);
  ModalResult:=mrOk;
end;

procedure TFrmComponentMan.FrmMainCREATE(Sender: TObject);
begin
  MyFile := TStringList.Create;
  Caption:='Custom Component Manager (No packages!)';
  Label1.Caption:='Installed Custom Components';
end;

procedure TFrmComponentMan.FrmMainDESTROY(Sender: TObject);
begin
  MyFile.Free;
end;

procedure TFrmComponentMan.FrmMainSHOW(Sender: TObject);
var
  RegisterFilename: String;
begin

  LblComponent.Caption := '';
  TxtPage.Text := '';
  LblUnit.Caption := '';
  
  // try to load the current customidecomps files
  RegisterFilename:=
    AppendPathDelim(FLazPath)+'components/custom/customidecomps.pas';
  try
    MyFile.LoadFromFile(UTF8ToSys(RegisterFilename));
  except
    on E: Exception do begin
      if messagedlg('Error loading '+RegisterFilename+': '+E.Message+#13#10+
                  'Will start with blank file.', mtError, [mbOk,mbAbort],0)
        <>mrOk
      then begin
        ModalResult:=mrCancel;
      end;
      MyFile.Clear;
      exit;
    end;
  end;
  if ((MyFile.Count < 1)
  or (MyFile[0] <> '{ CustomIDEComps generated by Component Manager'))
  then begin
    if MessageDlg('This appears to be the first time you have used Component Manager.'+#13#10+
                'Your original file will be backed up.',
                mtConfirmation, [mbOk,mbAbort],0)
    <>mrOk
    then begin
      ModalResult:=mrCancel;
      MyFile.Clear;
      exit;
    end;
    MyFile.SaveToFile(UTF8ToSys(ChangeFileExt(RegisterFilename,'.orig')));
  end;
  
  // okay got a good file here...
  PopulateList;
  ListCompsClick(Self);

end;

procedure TFrmComponentMan.ListCompsCLICK(Sender: TObject);
var
  MyObj: TRComponent;
begin
  if ListComps.ItemIndex < 0 then exit;
  
  MyObj := ListComps.Items.Objects[ListComps.ItemIndex] as TRComponent;
  if assigned(MyObj) then begin
    LblComponent.Caption := Myobj.Name;
    TxtPage.Text := MyObj.Page;
    LblUnit.Caption := MyObj.unit_name;
  end;

end;


procedure TFrmComponentMan.PopulateList;
var
  I: Integer;
begin

  // search the file for the REGISTER entry and list all components found
  for I := 0 to MyFile.Count - 1 do begin
    if uppercase(copy(trim(MyFile[I]),1,18)) = 'REGISTERCOMPONENT(' then begin
      // got the start here...  parse it
      ParseRegister(MyFile[I]);
    end
  end;

end;

procedure TFrmComponentMan.ParseRegister(ALine: String);
var
  MyObj: TRComponent;
  CompName, CompUnit, CompPage, TempStr, TempLine: String;
begin
  // given a line, parse and add the object to the list
  TempLine := Trim(ALine);
  if copy(TempLine,1,18) = 'RegisterComponent(' then begin
    TempStr := copy(TempLine,20,255);
    CompPage := copy(TempStr,1,pos('''', TempStr)-1);
    TempStr := copy(TempStr,pos('''', TempStr)+3,255);
    CompUnit := copy(TempStr,1,pos('''', TempStr)-1);
    TempStr := copy(TempStr,pos('''', TempStr)+2,255);
    CompName := copy(TempStr,1,pos(')', TempStr)-1);

    MyObj := TRComponent.Create;
    MyObj.Name := CompName;
    MyObj.Page := CompPage;
    MyObj.Unit_Name := CompUnit;
    ListComps.Items.AddObject(CompName, MyObj);
  end;
  

end;

procedure TFrmComponentMan.TxtPageCHANGE(Sender: TObject);
var
  MyObj: TRComponent;
begin

  if ListComps.ItemIndex >= 0 then begin
    MyObj := ListComps.Items.Objects[ListComps.ItemIndex] as TRComponent;
    if assigned(MyObj) then begin
      MyObj.Page := TxtPage.Text;
    end;
  end;

end;

procedure TFrmComponentMan.MakeHeader;
begin

  with MyFile do begin
    Add('{ CustomIDEComps generated by Component Manager');
    Add('  Last generated '+formatdatetime('MM/DD/YYYY', Now()));
    Add('  Component Manager written by Anthony Maro');
    Add('  http://tony.maro.net/       tony@maro.net');
    Add('}');
    Add('');
    Add('unit CustomIDEComps;');
    Add('');
    Add('{$mode objfpc}{$H+}');
    Add('');
    Add('interface');
    Add('');
  end;

end;

procedure TFrmComponentMan.MakeUses;
var
  I, J: Integer;
  MyObj, MyObj2: TRComponent;
  Found: Boolean;
begin
  with MyFile do begin
    Add('//USES SECTION');
    Add('uses');
    if ListComps.Items.Count > 0 then begin
      Add('  Classes,');
      for I := 0 to ListComps.Items.Count-1 do begin
        MyObj := ListComps.Items.Objects[I] as TRComponent;
        if assigned(MyObj) then begin
          Found := False;
          if I > 0 then begin
            for J := 0 to I-1 do begin
              // see if we already got this one...
              MyObj2 := ListComps.Items.Objects[J] as TRComponent;
              //messagedlg('Comparing object '+MyObj.Name+' at '+inttostr(I)+' with '+MyObj2.Name, mtInformation,[mbOk],0);
              if assigned(MyOBj2) then begin
                Found := AnsiCompareText(MyObj2.Unit_Name,Myobj.Unit_Name)=0;
              end; // if assigned
            end; // for J
          end; // For I
          if not(Found) then add('  '+MyObj.Unit_Name+',');
        end;
      end; // for I
      // remove last comma
      MyFile[MyFile.Count-1] := copy(MyFile[MyFile.Count-1],1,length(MyFile[MyFile.Count-1])-1);
    end else add('  Classes');
    Add('  ;');
    Add('');
    Add('type');
    Add('  TRegisterComponentProc = procedure(const Page, TheUnitName:ShortString;');
    Add('    ComponentClass: TComponentClass);');
    Add('');
    Add('procedure RegisterCustomComponents(RegisterComponent: TRegisterComponentProc);');
    Add('');
    Add('implementation');
    Add('');
    Add('procedure RegisterCustomComponents(RegisterComponent: TRegisterComponentProc);');
    Add('begin');
    Add('');
  end; // with MyFile

end;

procedure TFrmComponentMan.MakeRegister;
var
  I: Integer;
  MyObj: TRComponent;
begin
  MyFile.Add('//REGISTER');
  if ListComps.Items.Count > 0 then begin
    for I := 0 to ListComps.Items.Count - 1 do begin
      MyObj := ListComps.Items.Objects[I] as TRComponent;
      if assigned(MyObj) then begin
        MyFile.Add('  RegisterComponent('''+MyObj.Page+''','''+MyObj.unit_name+''','+MyObj.Name+');');
      end;
    end;
  end;
  MyFile.Add('//ENDREGISTER');
  MyFile.Add('');
  MyFile.Add('end;');
  MyFile.Add('');
  MyFile.Add('end.');
end;

function TFrmComponentMan.CountComponents: Integer;
var
  I, J: integer;
  Count: Integer;
begin
  Count := 0;
  if MyFile.Count < 0 then begin
    Result := 0;
    exit;
  end;
  for I := 0 to MyFile.Count -1 do begin
    // find start of TYPE
    if pos('TYPE', uppercase(MyFile[i])) > 0 then begin
      //messagedlg('Found TYPE at '+inttostr(i),mtInformation,[mbOk],0);
      for J := I+1 to MyFile.Count -1 do begin
        if ((pos('= CLASS', uppercase(MyFile[j])) > 0) or
          (pos('=CLASS', uppercase(MyFile[j])) > 0)) then begin
          // found one!
          //messagedlg('Found CLASS'+#13#10+MyFile[J],mtInformation,[mbOk],0);
          Count := Count + 1;
        end; // if class
        if 'IMPLEMENTATION' = uppercase(MyFile[J]) then begin
          // that's it
          //messagedlg('Found IMPLEMENTATION at '+inttostr(J),mtInformation,[mbOk],0);
          Result := Count;
          exit;
        end;
      end; // For J
      Result := Count;
      exit;
    end; // if pos('TYPE');
  end; // for I

end;

function TFrmComponentMan.GetComponent(I: Integer): String;
var
  K, J: integer;
  TempStr: String;
  Count: Integer;
begin
  Count := 0;
  if MyFile.Count < 0 then begin
    Result := '';
    exit;
  end;
  for K := 0 to MyFile.Count -1 do begin
    // find start of TYPE
    if pos('TYPE', uppercase(MyFile[K])) > 0 then begin
      for J := K+1 to MyFile.Count -1 do begin
        if ((pos('= CLASS', uppercase(MyFile[j])) > 0) or
          (pos('=CLASS', uppercase(MyFile[j])) > 0)) then begin
          // found one!
          Count := Count + 1;
          if Count = I then begin
            TempStr := Trim(copy(MyFile[J],1,pos('=',MyFile[j])-1));
            Result := TempStr;
            exit;
          end;
        end; // if class
        if 'IMPLEMENTATION' = uppercase(MyFile[J]) then begin
          // that's it
          //messagedlg('Found IMPLEMENTATION at '+inttostr(J),mtInformation,[mbOk],0);
          Result := '';
          exit;
        end;
      end; // For J
      Result := '';
      exit;
    end; // if pos('TYPE');
  end; // for I
  
end;

function TFrmComponentMan.FindUnitName: String;
var
  I: Integer;
  TempStr: String;
begin
  if MyFile.Count < 1 then exit;
  for I := 0 to MyFile.Count - 1 do begin
    if uppercase(copy(MyFile[i],1,4)) = 'UNIT' then begin
      TempStr := copy(MyFile[i],5,64);
      TempStr := trim(TempStr);
      if copy(TempStr,length(TempStr),1) = ';' then TempStr := copy(TempStr,1,length(TempStr)-1);
      Result := TempStr;
      exit;
    end; // if UNIT found
  end; // for I
end;

procedure TFrmComponentMan.SetLazPath(const AValue: String);
begin
  if FLazPath=AValue then exit;
  FLazPath:=AValue;
end;

end.

