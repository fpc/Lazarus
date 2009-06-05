{%MainUnit unit1.pas}
procedure Do1;
var
  FileInfo: TSearchRec;
begin
  if FindFirstUTF8(Dir+FileMask,faAnyFile,FileInfo)=0 then begin
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
      then
        continue;

      if FilenameIsPascalUnit(FileInfo.Name,false) then begin
        List.Add(Dir+FileInfo.Name);
      end else if (FileInfo.Attr and faDirectory)>0 then begin
        CollectUnits(Dir+);
      end;
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
end;

