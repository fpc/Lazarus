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
  ResStream,BinStream:TFileStream;

begin
  if ParamCount<2 then begin
    writeln('Usage: ',ExtractFileName(ParamStr(0))
       ,' resourcefilename filename1 [filename2 ... filenameN]');
  end else begin
    ResourceFilename:=ParamStr(1);
    try
      ResStream:=TFileStream.Create(ResourceFilename,fmCreate);
    except
      writeln('ERROR: unable to create file '''+ResourceFilename+'''');
      halt(1);
    end;
    try
      for a:=2 to ParamCount do begin
        BinFilename:=ParamStr(a);
        write(BinFilename);
        try
          BinStream:=TFileStream.Create(BinFilename,fmOpenRead);
          try
            BinExt:=uppercase(ExtractFileExt(BinFilename));
            if BinExt='.LFM' then begin
              ResourceType:='FORMDATA';
              ResourceName:=FindLFMClassName(BinStream);
              if ResourceName='' then begin
                writeln(' ERROR: no resourcename');
                halt(2);
              end;
              write(
                ' ResourceName='''+ResourceName+''' Type='''+ResourceType+'''');
              LFMtoLFCstream(BinStream,ResStream);
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
              BinaryToLazarusResourceCode(BinStream,ResStream
                 ,ResourceName,ResourceType);
            end;
          finally
            BinStream.Free;
          end;
        except
          writeln('  ERROR: unable to read file '''+BinFilename+'''');
          halt(3);
        end;
        writeln('');
      end;
    finally
      ResStream.Free;
    end;
  end;
end.

