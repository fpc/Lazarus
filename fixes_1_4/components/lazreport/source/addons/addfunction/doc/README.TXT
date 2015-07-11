
  frAddFunctionLibrary is set of additional functions for
FastReport 2.4 (www.fastreport.ru).

  Functions are borrowed from RxLib (www.rxlib.com), Delphi and my StLib.

  There is four function categories in this set:
  - string functions;
  - date and time functions;
  - numeric functions;
  - SQL functions.

Few words about SQL functions:
  These functions are helper for creating dynamic SQL queries. They converts
dates, strings or numeric values to the string that can be inserted into
SQL clause. 
  To use CreateDate function, fill the frAddFunctionLibrary.FormatDate property
with the value that used by your DB server, for instance:
FormatDate := 'yyyy.mm.dd'.

Some examples of using SQL functions in the script:

Query.SQL.Add('select * from MYTABLE where CSTRING='+CreateStr(Edit1.Text));
Query.SQL.Add('select * from MYTABLE where CDATE='+CreateDate(DateEdit1.Text));
Query.SQL.Add('select * from MYTABLE where CNUM='+CreateNum(Edit1.Text));
Query.SQL.Add('select * from MYTABLE where CFLOAT='+CreateFloat(Edit1.Text));

instead of
Query.SQL.Add('select * from MYTABLE where CSTRING='+''''+Edit1.Text+'''');
Query.SQL.Add('select * from MYTABLE where CDATE='+''''+FormatDate('yyyy.mm.dd',DateEdit1.Date)+'''');
Query.SQL.Add('select * from MYTABLE where CNUM='+Edit1.Text);
Query.SQL.Add('select * from MYTABLE where CFLOAT='+Edit1.Text);


  Other functions can be viewed in the Expression Builder.


(c) Stalker <stalker@miac.dp.ua>
