{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit RegLazThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProjectIntf, LazIdeIntf;

type
  { TThreadFileDescriptor }
  TThreadFileDescriptor = class(TFileDescPascalUnit)
  private
    FThreadCount:integer;
    FThreadName :string;
    function GetNextThreadName:string;
    function GetCurrentThreadName:string;
  public
    constructor Create; override;
    function GetLocalizedName : string; override;
    function GetLocalizedDescription : string; override;
    function GetInterfaceSource(const Filename, SourceName, ResourceName: string): string; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string; override;
  end;

procedure register;

implementation

uses
  Controls, ThreadOptionsDialog;

resourcestring
  SThreadName        = 'Thread Object';
  SThreadDescription = 'A Pascal unit with a subclass of TThread class';

//---------------//
procedure register;
begin
  RegisterProjectFileDescriptor(TThreadFileDescriptor.Create, FileDescGroupName);
end;

{ TThreadFileDescriptor }

//-------------------------------------//
constructor TThreadFileDescriptor.Create;
begin
  inherited Create;
  Name := SThreadName;
  FThreadCount := 1;
  FThreadName  := TThread.ClassName;
end;

//-----------------------------------------------------//
function TThreadFileDescriptor.GetNextThreadName: string;
var
  ThreadOptionsDialog:TThreadOptionsDialog;
begin
  //Create Default Thread Class Name
  FThreadName := TThread.ClassName + IntToStr(FThreadCount);
  FThreadCount := (FThreadCount mod MaxInt) + 1;
  
  //Show Thread Options Dialog
  with(TThreadOptionsDialog.Create(nil))do
    begin
    ThreadNameEdit.Text := FThreadName;
    if((ShowModal = mrOK)and(ThreadNameEdit.Text <> ''))then
      begin
      FThreadName := ThreadNameEdit.Text;
      end;
    Free;
    end;
    
  //Return Thread Class Name
  Result := FThreadName;
end;

//--------------------------------------------------------//
function TThreadFileDescriptor.GetCurrentThreadName: string;
begin
  Result := FThreadName;
end;

//----------------------------------------------------//
function TThreadFileDescriptor.GetLocalizedName: string;
begin
  Result := SThreadName;
end;

//-----------------------------------------------------------//
function TThreadFileDescriptor.GetLocalizedDescription: string;
begin
  Result := SThreadDescription;
end;

//--------------------------------------------------------------------------------------------------------//
function TThreadFileDescriptor.GetInterfaceSource(const Filename, SourceName, ResourceName: string): string;
begin
  with(TStringList.Create)do
    begin
    Add('type');
    Add('  ' + GetNextThreadName + ' = class(TThread)');
    Add('  private');
    Add('    { Private declarations }');
    Add('  protected');
    Add('    { Protected declarations }');
    Add('    procedure Execute; override;');
    Add('  end;');
    Add('');
    Result := Text;
    Free;
  end;
end;

//-------------------------------------------------------------------------------------------------------------//
function TThreadFileDescriptor.GetImplementationSource(const Filename, SourceName, ResourceName: string): string;
begin
  with(TStringList.Create)do
    begin
    Add('{ ' + GetCurrentThreadName + ' }');
    Add('');
    Add('procedure ' + GetCurrentThreadName + '.Execute;');
    Add('begin');
    Add('  { Write your thread code here }');
    Add('end;');
    Add('');
    Result := Text;
    Free;
  end;
end;

end.

