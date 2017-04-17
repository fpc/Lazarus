unit fdt_proc_basetypes;

{$mode objfpc}{$H+}

interface

procedure DoIt{#boolean}(p: boolean); external;
procedure DoIt{#byte}(p: byte); external;
procedure DoIt{#shortint}(p: shortint); external;
procedure DoIt{#word}(p: word); external;
procedure DoIt{#smallint}(p: smallint); external;
procedure DoIt{#cardinal}(p: cardinal); external;
procedure DoIt{#longint}(p: longint); external;
procedure DoIt{#qword}(p: qword); external;
procedure DoIt{#int64}(p: int64); external;
procedure DoIt{#single}(p: single); external;
procedure DoIt{#double}(p: double); external;
procedure DoIt{#char}(p: char); external;
procedure DoIt{#string}(p: string); external;
procedure DoIt{#widechar}(p: widechar); external;
procedure DoIt{#unicodestring}(p: unicodestring); external;
procedure DoIt{#widestring}(p: widestring); external;
procedure DoIt{#rawbytestring}(p: rawbytestring); external;
procedure DoIt{#pointer}(p: pointer); external;

implementation

var
  bo: boolean;
  by: byte;
  shi: shortint;
  w: word;
  smi: smallint;
  ca: cardinal;
  l: longint;
  qw: qword;
  i64: int64;
  si: single;
  d: double;
  ch: char;
  st: string;
  wc: widechar;
  ws: widestring;
  us: unicodestring;
  rs: rawbytestring;
  p: pointer;
initialization
  DoIt{@boolean}(bo);
  DoIt{@byte}(by);
  DoIt{@shortint}(shi);
  DoIt{@word}(w);
  DoIt{@smallint}(smi);
  DoIt{@cardinal}(ca);
  DoIt{@longint}(l);
  DoIt{@qword}(qw);
  DoIt{@int64}(i64);
  DoIt{@single}(si);
  DoIt{@double}(d);
  DoIt{@char}(ch);
  DoIt{@string}(st);
  DoIt{@widechar}(wc);
  DoIt{@widestring}(ws);
  DoIt{@unicodestring}(us);
  DoIt{@rawbytestring}(rs);
  DoIt{@pointer}(p);
end.

