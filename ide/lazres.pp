program lazres;
{
  Author: Mattias Gaertner

  Name:
       lazres - creates an lazarus resource file from files

  Synopsis:
       lazres resourcefilename filename1 [filename2 ... filenameN]

  Description:
       lazres creates a lazarus resource file from filenameXXX.

}

{$mode objfpc}

uses Classes, SysUtils, LResources;

var
  ResourceFilename,BinFilename,BinExt,ResourceName,ResourceType:AnsiString;
  a:integer;
  ResFileStream,BinFileStream:TFileStream;
  ResMemStream,BinMemStream:TMemoryStream;

begin
  if ParamCount<2 then begin
    writeln('Usage: ',ExtractFileName(ParamStr(0))
       ,' resourcefilename filename1 [filename2 ... filenameN]');
  end else begin
    ResourceFilename:=ParamStr(1);
    try
      ResFileStream:=TFileStream.Create(ResourceFilename,fmCreate);
    except
      writeln('ERROR: unable to create file '''+ResourceFilename+'''');
      halt(1);
    end;
    ResMemStream:=TMemoryStream.Create;
    try
      for a:=2 to ParamCount do begin
        BinFilename:=ParamStr(a);
        write(BinFilename);
        try
          BinFileStream:=TFileStream.Create(BinFilename,fmOpenRead);
          BinMemStream:=TMemoryStream.Create;
          try
            BinMemStream.CopyFrom(BinFileStream,BinFileStream.Size);
            BinMemStream.Position:=0;
            BinExt:=uppercase(ExtractFileExt(BinFilename));
            if (BinExt='.LFM') or (BinExt='.DFM') then begin
              ResourceType:='FORMDATA';
              ResourceName:=FindLFMClassName(BinMemStream);
              if ResourceName='' then begin
                writeln(' ERROR: no resourcename');
                halt(2);
              end;
              write(
                ' ResourceName='''+ResourceName+''' Type='''+ResourceType+'''');
              LFMtoLFCstream(BinMemStream,ResMemStream);
            end else begin
              ResourceType:=copy(BinExt,2,length(BinExt)-1);
              ResourceName:=ExtractFileName(BinFilename);
              ResourceName:=copy(ResourceName,1
                 ,length(ResourceName)-length(BinExt));
              if ResourceName='' then begin
                writeln(' ERROR: no resourcename');
                halt(2);
              end;
              write(
                ' ResourceName='''+ResourceName+''' Type='''+ResourceType+'''');
              BinaryToLazarusResourceCode(BinMemStream,ResMemStream
                 ,ResourceName,ResourceType);
            end;
          finally
            BinFileStream.Free;
            BinMemStream.Free;
          end;
        except
          writeln('  ERROR: unable to read file '''+BinFilename+'''');
          halt(3);
        end;
        writeln('');
      end;
      ResMemStream.Position:=0;
      ResFileStream.CopyFrom(ResMemStream,ResMemStream.Size);
    finally
      ResMemStream.Free;
      ResFileStream.Free;
    end;
  end;
end.

