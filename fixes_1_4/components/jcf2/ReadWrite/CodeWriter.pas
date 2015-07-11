{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is CodeWriter.pas, released April 2000.
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

unit CodeWriter;

{ AFS 28 November 1999
  Writer - final output stage of code formattter

  AFS 22 July 2K - optimised by using a string to store tokens,
  and writing the file all at once at the end
  This is best in the usual-case scenario of a file < 10k

  AFS 8 Jan 2K
  divided into TCodeWriter and TFileWriter, so that another subclass, TIDEWriter,
  can be made with the same interface for the IDW pluggin

  Now called  TCodeWriter not TWriter to avoid a name clash with Classes.Writer
  }

{$I JcfGlobal.inc}

interface

uses SourceToken, ParseTreeNode;

type
  TCodeWriter = class(TObject)
  private
    fiTokensWritten: integer;

    { properties }
    fcRoot: TParseTreeNode;

    { worker procs }
    procedure WriteTree(const pcRoot: TParseTreeNode);
  protected
    { working vars }
    fbBOF: boolean;
    fsDestText: string;

    // common to both
    procedure BeforeWrite;

    procedure WriteOut(const st: string); overload;
    procedure WriteOut(const pt: TSourceToken); overload;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear;
    procedure Close; virtual;

    procedure WriteAll;

    property Root: TParseTreeNode Read fcRoot Write fcRoot;
    property BOF: boolean Read fbBOF;
  end;

implementation

const
  MAX_TOKENS = 100000;

  { TCodeWriter }

constructor TCodeWriter.Create;
begin
  inherited;
  fbBOF  := True;
  fcRoot := nil;
end;

destructor TCodeWriter.Destroy;
begin
  Close;
  inherited;
end;

procedure TCodeWriter.WriteOut(const st: string);
begin
  //Assert(st <> #0);
  if (st = '') or (st = #0) then
    exit;

  fsDestText := fsDestText + st;
  fbBOF      := False;
end;


procedure TCodeWriter.WriteOut(const pt: TSourceToken);
begin
  WriteOut(pt.SourceCode);
  Inc(fiTokensWritten);
end;


procedure TCodeWriter.Close;
begin
  Assert(False, ClassName + ' must override TCodeWriter.Close');
end;

procedure TCodeWriter.Clear;
begin
  fsDestText := '';
end;

procedure TCodeWriter.BeforeWrite;
begin

end;


procedure TCodeWriter.WriteAll;
begin
  Assert(Root <> nil);

  WriteTree(Root);
end;

procedure TCodeWriter.WriteTree(const pcRoot: TParseTreeNode);
var
  liLoop: integer;
begin
  Assert(pcRoot <> nil);

  // is it a leaf with source?
  if (pcRoot is TSourceToken) then
  begin
    WriteOut(pcRoot as TSourceToken);
  end
  else
  begin
    // write out all child nodes
    for liLoop := 0 to pcRoot.ChildNodeCount - 1 do
    begin
      WriteTree(pcRoot.ChildNodes[liLoop])
    end;
  end;

end;

end.
