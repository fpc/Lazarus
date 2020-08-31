unit FileReader;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is FileReader.pas, released April 2000.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2000 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{ Created AFS 27 November 1999
  reader for Code formatting util

  The method is to first read the entire file into a string
  This is a textbook optimization - 1 read for the whole file
  instead of 1 per char. The file may be large
  (the largest file that ships with Delphi5, excel2000.pas, is 4Mb!!!!)
  but even this should fit into memory
  This technique is not optimised for files of that size,
  but hey, that is not the normal case,
  and code like that has got to be machine-generated anyway.
  Why would it need machine-reformatting?

  8 Jan 2K - the original code is now split into
  TReader (base class) and TFileReader (read from file
  so that another subclass (TIDEReader) can be made for the IDE pluggin
  with the same interface
}

{$I JcfGlobal.inc}

interface

uses
  SysUtils,
  CodeReader;

type
  TFileReader = class(TCodeReader)
  private
    { working vars }
    { property implementation }
    FsSourceFileName: string;

    procedure SetSourceFileName(const psValue: string);

  protected
    procedure ReadFromSource; override;
  public
    procedure Clear; override;

    property SourceFileName: string Read FsSourceFileName Write SetSourceFileName;
  end;

implementation

uses
  JclAnsiStrings;

{ TFileReader }

procedure TFileReader.Clear;
begin
  inherited;
  FsSourceFileName := '';
end;

procedure TFileReader.SetSourceFileName(const psValue: string);
begin
  FsSourceFileName := psValue;
end;

procedure TFileReader.ReadFromSource;
begin
  if fbHasRead then
    exit;

  // Open the file
  Assert(FileExists(SourceFileName), 'No file ' + SourceFileName);

  fsSource := FileToString(SourceFileName);

  fiSourceLength := Length(fsSource);

  fiReadIndex    := 1;
  fiBufferLength := 1;
  fbHasRead      := True;
end;


end.
