{
 /***************************************************************************
                         project.pp  -  project utility class file
                             -------------------
                   TCompiler is responsible for configuration and running
                   the PPC386 compiler.


                   Initial Revision  : Sun Mar 28 23:15:32 CST 1999


 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}
unit project;

{$mode objfpc}

interface

uses
  Classes, Global, SysUtils, IdeEditor, IniFiles,mwCustomEdit, FORMS,dlgMessage;

type

  TProject = class(TObject)
  private
    FUnits: TList;
    FLibrary: TStrings;
    FName: String;
  public
    constructor Create;
    function SaveProject(const SaveName: string) : Boolean;
    function OpenProject(const OpenName: string) : Boolean;
    procedure AddUnit(AUnit: TUnitInfo);
    procedure AddLibraryPath(const LibPath: String);
    function GetLibraryPaths: TStrings;
    function GetUnits: TList;
    property UnitList: TList read FUnits write FUnits;
    property Name: String read FName write FName;
  end;

var
  Project1 : TProject;
  
implementation
uses
  Main;


{------------------------------------------------------------------------------}
constructor TProject.Create;
begin
  inherited Create;
  FUnits := TList.Create;
  FLibrary := TStringList.Create;
end;

{------------------------------------------------------------------------------}
function TProject.SaveProject(const SaveName: String) : Boolean;
var
  iniFile : TIniFile;
  Texts : String;
  TempName : String;
  I,X : Integer;
  TempEditor : TmwCustomEdit;
begin
  Result := True;
  if SaveName = '' then Exit;

  //create the .dsk file based on the project name
  Assert(False, 'Trace:INIFILE CREATE');
  if FileExists(SaveName + '.dsk') then
  begin
    TempName := SaveName + '.~dsk';
    RenameFile(SaveName + '.dsk', TempName);
  end;

  INiFile := TiniFile.Create(Savename+'.dsk');
  Assert(False, 'Trace:INIFILE CREATE');

  for i := 0 to FUnits.Count -1 do
  begin
    INIFIle.WriteString('Modules','Module'+inttostr(I),TUnitInfo(FUnits.items[i]).Filename);
    INIFile.WriteString(TUnitInfo(FUnits.items[i]).Filename,'ModuleType','SourceModule');
    INIFile.WriteString(TUnitInfo(FUnits.items[i]).Filename,'FormState','0');
    INIFile.WriteString(TUnitInfo(FUnits.items[i]).Filename,'FormOnTop','0');
  end;

  INIFIle.WriteInteger('Modules','Count',FUnits.Count);
  
  //have to check this later
  INIFIle.WriteString('Modules','EditWindowCount','1');

  INIFile.WriteInteger('EditWindow0','ViewCount',IdeEditor1.Notebook1.Pages.Count);
  INIFile.WriteInteger('EditWindow0','CurrentView',IdeEditor1.Notebook1.PageIndex);
  for I := 0 to ideEditor1.Notebook1.Pages.Count-1 do
  begin
  
    //Determine what entry each notebook is displaying.
    Assert(False, 'Trace:*******************');
    Assert(False, 'Trace:*******************');
    Assert(False, 'Trace:***I := '+Inttostr(i));
    Assert(False, 'Trace:*******************');
    Assert(False, 'Trace:*******************');
    for X := 0 to FUnits.Count-1 do
  	begin
      Assert(False, 'Trace:X = '+Inttostr(x));
      Assert(False, 'Trace:Page = '+inttostr(TUnitInfo(FUnits.Items[x]).Page));
      if TUnitInfo(FUnits.Items[x]).Page = I 
      then begin
        INIFile.WriteInteger('EditWindow0','View'+Inttostr(I),X);
        //write out the View section while here.
        IniFile.WriteString('View'+Inttostr(I),'Module',TUnitInfo(FUnits.Items[x]).Filename);
        TempEditor := ideEditor1.GetEditorfromPage(i);
        if TempEditor = nil then Break;
        IniFile.WriteInteger('View'+Inttostr(I),'CursorX',TempEditor.CaretX);
        IniFile.WriteInteger('View'+Inttostr(I),'CursorY',TempEditor.CaretY);
        IniFile.WriteInteger('View'+Inttostr(I),'TopLine',TempEditor.TopLine);
        IniFile.WriteInteger('View'+Inttostr(I),'LeftCol',tempEditor.LeftChar);
        Break;
       end;
    end;
  end;
  if IdeEditor1.Visible 
  then INIFile.WriteInteger('EditWindow0','Visible',1)
  else INIFile.WriteInteger('EditWindow0','Visible',0);

//Write out screen coords
  INIFile.WriteInteger('EditWindow0','Left',ideEditor1.Left);
  INIFile.WriteInteger('EditWindow0','Top',ideEditor1.Top);
  INIFile.WriteInteger('EditWindow0','Height',ideEditor1.Height);
  INIFile.WriteInteger('EditWindow0','Width',ideEditor1.Width);
  INIFile.WriteInteger('EditWindow0','CurrentView',ideEditor1.Notebook1.Pageindex);

  INIFile.WriteInteger('Main Window','Create',1);
  INIFile.WriteInteger('Main Window','Visible',1);
  INIFile.WriteInteger('Main Window','State',0);  //0 = normal?
  INIFile.WriteInteger('Main Window','Left',Form1.Left);
  INIFile.WriteInteger('Main Window','Top',Form1.Top);
  INIFile.WriteInteger('Main Window','Width',Form1.Width);
  INIFile.WriteInteger('Main Window','Height',Form1.Height);

  INIFile.WriteInteger('Message Window','Create',1);
  if Messagedlg.Visible 
  then INIFile.WriteInteger('Message WIndow','Visible',1)
  else INIFile.WriteInteger('Message WIndow','Visible',0);

  INIFile.WriteInteger('Message Window','State',0);  //0 = normal?
  INIFile.WriteInteger('Message Window','Left',Form1.Left);
  INIFile.WriteInteger('Message Window','Top',Form1.Top);
  INIFile.WriteInteger('Message Window','Width',Form1.Width);
  INIFile.WriteInteger('Message Window','Height',Form1.Height);


  IniFile.Free;
end;

{------------------------------------------------------------------------------}
function TProject.OpenProject(const OpenName: String) : Boolean;
var
  tempInt: Integer;
  tempStr: String;
  Count : Integer;
  EditCount : Integer;
  ViewCount : Integer;
  tempEditor : TmwCustomEdit;
  SList : TUnitInfo;
  INIFile : TIniFile;
  I,X : Integer;
begin
  Result := True;
  //open the .dsk file based on the project name
  Assert(False, 'Trace:INIFILE CREATE '+Openname+'.dsk');
  INiFile := TiniFile.Create(OpenName+'.dsk');
  Assert(False, 'Trace:INIFILE CREATE');
  
  Assert(False, 'Trace:Read count');
  count := INIFIle.ReadInteger('Modules','Count',0);
  Assert(False, 'Trace:Count = '+Inttostr(count));
  
  for i := 0 to Count - 1 do
  begin
    tempStr := INIFIle.ReadString('Modules','Module'+inttostr(I),'');
    if TempStr <> '' 
    then Begin  //a name exists
      Assert(False, 'Trace:TempStr = '+TempStr);
      SList := TUnitInfo.Create;
      SList.Source := TStringList.Create;
      SList.Filename := TempStr;
      SList.Source.LoadFromFile(SList.Filename);
      SList.Page := -1;
      Form1.SetFlags(SList);
      Form1.SetName_Form(SList);
      FUnits.Add(SList);
    end;
  end;

  Assert(False, 'Trace:Read EditCount');
  EditCount := INIFile.ReadInteger('Modules','EditWindowCount',0);

  for I := 0 to EditCount -1 do
  begin
    Assert(False, 'Trace:Read ViewCount');
   
    ViewCount := INIFIle.ReadInteger('EditWindow'+inttostr(i),'ViewCount',0);
    if ViewCount > 0 
    then begin
      for x := 0 to ViewCount-1 do
      begin
        Assert(False, 'Trace:Read View'+inttostr(x));
        TempInt := INIFIle.ReadInteger('EditWindow'+inttostr(i),'View'+InttoStr(x),0);
        TUnitInfo(FUnits.Items[TempInt]).Page := x;
        //add the page here
        {debug}
        Assert(False, 'Trace:NAME = '+TUnitInfo(FUnits.Items[TempInt]).Name);
        
        ideEditor1.AddPage(TUnitInfo(FUnits.Items[TempInt]).Name,TUnitInfo(FUnits.Items[TempInt]).Source);
      end;
      TempInt := INIFIle.ReadInteger('EditWindow'+inttostr(i),'CurrentView',0);
      ideEditor1.Notebook1.Pageindex := tempint;
    end;
    
    Assert(False, 'Trace:Read Visible');
    Assert(False, 'Trace:I = '+Inttostr(i));

    ideEditor1.Visible := (INIFile.ReadInteger('EditWindow'+inttostr(i),'Visible',0) = 1);
    ideEditor1.Left := INIFIle.ReadInteger('EditWindow'+inttostr(i),'Left',0);
    ideEditor1.Top := INIFIle.ReadInteger('EditWindow'+inttostr(i),'Top',0);
    ideEditor1.Width := INIFIle.ReadInteger('EditWindow'+inttostr(i),'Width',300);
    ideEditor1.Height := INIFIle.ReadInteger('EditWindow'+inttostr(i),'Height',400);

  end;

  Assert(False, 'Trace:For loop : ViewCount = '+Inttostr(ViewCount));

  for i := 0 to ViewCount - 1 do
  begin
    TempEditor := ideEditor1.GetEditorfromPage(i);
    tempEditor.CaretX := INIFIle.ReadInteger('View'+inttostr(i),'CursorX',0);
    tempEditor.CaretY := INIFIle.ReadInteger('View'+inttostr(i),'CursorY',0);
    tempEditor.TopLine := INIFIle.ReadInteger('View'+inttostr(i),'TopLine',0);
    tempEditor.LeftChar := INIFIle.ReadInteger('View'+inttostr(i),'LeftCol',0);
  end;

  Form1.Visible := (INIFile.ReadInteger('Main Window','Visible',1) = 1);
  Form1.Left := INIFile.ReadInteger('Main Window','Left',0);
  Form1.Top := INIFile.ReadInteger('Main Window','Top',0);
  Form1.Height := INIFile.ReadInteger('Main Window','Height',100);
  Form1.Width := INIFile.ReadInteger('Main Window','Width',800);

  Messagedlg.Visible := (INIFile.ReadInteger('Message WIndow','Visible',1) = 1);
  Messagedlg.Left := INIFile.ReadInteger('Message Window','Left',0);
  Messagedlg.Top := INIFile.ReadInteger('Message Window','Top',0);
  Messagedlg.Width := InIfile.ReadInteger('Message Window','Width',(Screen.Width div 2));
  Messagedlg.Height := INIFile.ReadInteger('Message Window','Height',100);

  INiFile.Free;
end;

{------------------------------------------------------------------------------}
procedure TProject.AddUnit(AUnit: TUnitInfo);
begin
  if (AUnit <> nil) then FUnits.Add(AUnit);
end;

{------------------------------------------------------------------------------}
function TProject.GetUnits: TList;
begin
  Result := FUnits;
end;

{------------------------------------------------------------------------------}
procedure TProject.AddLibraryPath(const LibPath: String);
begin
  FLibrary.Add(LibPath);
end;

{------------------------------------------------------------------------------}
function TProject.GetLibraryPaths: TStrings;
begin
  GetLibraryPaths := FLibrary;
end;

end.
{
  $Log$
  Revision 1.1  2000/07/13 10:27:48  michael
  + Initial import

  Revision 1.14  2000/07/09 20:18:55  lazarus
  MWE:
    + added new controlselection
    + some fixes
    ~ some cleanup

  Revision 1.13  2000/05/10 02:34:43  lazarus
  Changed writelns to Asserts except for ERROR and WARNING messages.   CAW

  Revision 1.12  2000/04/18 20:06:39  lazarus
  Added some functions to Compiler.pp

  Revision 1.11  2000/04/17 19:50:05  lazarus
  Added some compiler stuff built into Lazarus.
  This depends on the path to your compiler being correct in the compileroptions
  dialog.
  Shane

  Revision 1.10  2000/03/07 16:52:58  lazarus
  Fixxed a problem with the main.pp unit determining a new files FORM name.
  Shane

  Revision 1.9  2000/03/03 20:22:02  lazarus
  Trying to add TBitBtn
  Shane

  Revision 1.8  2000/03/01 21:54:05  lazarus
  90% finished with SAVE PROJECT and OPEN PROJECT
  Shane

  Revision 1.6  1999/05/14 18:44:17  lazarus
  *** empty log message ***

  Revision 1.5  1999/05/14 14:53:10  michael
  + Removed objpas from uses clause

  Revision 1.4  1999/05/14 14:39:44  michael
  All file stuff now uses sysutils. Win32 compiles

  Revision 1.3  1999/05/07 05:46:54  lazarus
  *** empty log message ***

  Revision 1.2  1999/04/28 05:29:37  lazarus
  *** empty log message ***

  Revision 1.1  1999/04/27 05:08:28  lazarus
  *** empty log message ***

  Revision 1.3  1999/04/20 02:56:42  lazarus
  *** empty log message ***

  Revision 1.2  1999/04/18 05:42:05  lazarus
  *** empty log message ***

  Revision 1.1  1999/04/14 07:31:44  michael
  + Initial implementation

}
