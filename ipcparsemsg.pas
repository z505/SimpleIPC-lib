{ IPC parsing functions for dissecting messages such as 1234;somestring

  These should be fast routines as parsing the first integers don't take many cpu
  cycles since it just checks for semi colons then exits.
  Then the rest of the string copied using a fast Copy() function or RightStr()
  However no benchmarks have been done. Todo: try test speed after thousands of
  calls, and compare to a binary delimited mechanism instead of plain string

  Copyright Z505 Software

  License: bsd/mit }


unit IpcParseMsg;

{$mode objfpc}{$H+}

// Disable this normally, will make program bigger if including verification tests
{...$DEFINE TESTINGON}

interface

uses
  SysUtils, strutils;

function ParseIntAndStr(const s: string; out outi: integer; out outs: string): boolean;
function IntBeforeSemi(const s: string; out outi: integer): integer;
function ParseXYAndStr(const s: string; out outx: integer; out outy: integer;
  out outs: string): boolean;
function Parse4IntsAndStr(const s: string;
  out x1: integer; out x2: integer; out x3: integer; out x4: integer;
  out outs: string): boolean;
function ParseInts(s: string; out val1, val2, val3, val4: integer): boolean;
function Parse2Ints(s: string; out val1, val2: integer): boolean;

{$ifdef TESTINGON}
  procedure ParseIntTests;
  procedure ParseIntStrTests;
{$endif}

procedure defaultdbugln(s: string);

var dbugln: procedure(s: string) = @defaultdbugln;

implementation

procedure defaultdbugln(s: string);
begin
  // user can assign his own statusln variable, this is a dummy
end;

// parse an integer and a string delimited by semicolon,
// i.e. 1343;some string here
(* old function
function ParseIntAndStr(const s: string; out outi: integer; out outs: string): boolean;
var i, len: integer;
    tmp: string;
begin
  result := false;
  len := length(s);
  outs := '';
  i := 2; // minimum semi colon position position two in string, i.e. 1;somestring
  while i <= len do begin
    if s[i] = ';' then begin
      tmp := LeftStr(s, i-1);
      if TryStrToInt(tmp, outi) then begin
        outs := RightStr(s, len-i);
        result := true;
      end;
      break;
    end;
    inc(i);
  end;
end;
*)

{
1;2;st
length:6
start:3
i: 4
}
// parse an integer and a string delimited by semicolon from a starting point
function ParseIntAndStrFrom(start: integer; const s: string; out outi: integer; out outs: string): boolean;
var i, len: integer;
    tmp: string;
begin
  result := false;
  len := length(s);
  outs := '';
  i := start;
  while i <= len do begin
    if s[i] = ';' then begin
      tmp := MidStr(s, start, i-start);
      if TryStrToInt(tmp, outi) then begin
        outs := MidStr(s, i+1, len-i);
        result := true;
      end;
      break;
    end;
    inc(i);
  end;
end;

// default, start position at 1
function ParseIntAndStr(const s: string; out outi: integer; out outs: string): boolean;
begin
  result := ParseIntAndStrFrom(1,s,outi,outs);
end;

function intBeforeSemiFrom(start: integer; const s: string; out outi: integer): integer;
var i, len: integer;
    tmp: string;
begin
  result := 0;
  len := length(s);
  i := start; // minimum semi colon position position two in string, i.e. 1;somestring
  while i <= len do begin
    if s[i] = ';' then begin
      tmp := MidStr(s, start, i-start);
      if TryStrToInt(tmp, outi) then result := i;
      break;
    end;
    inc(i);
  end;
end;


// extract first integer before a semi colon in a string
// returns position of semicolon, 0 if not found
function intBeforeSemi(const s: string; out outi: integer): integer;
begin
  result := intBeforeSemiFrom(1,s,outi);
end;

{ parse this:
 1234;567;some string
 2 semi colons means 2 integers
 everything after second semi colon is a string }
function ParseXYAndStr(const s: string; out outx: integer; out outy: integer;
  out outs: string): boolean;
var semiColPos: integer;
begin
  result := false;
  semiColPos := intBeforeSemi(s, outx);
  if semiColPos > 0 then begin
    // parse the rest of the string
    // result := ParseIntAndStr(RightStr(s, length(s)-semiColPos), outy, outs);
    result := ParseIntAndStrFrom(semiColPos+1, s, outy, outs);
  end;
end;

{ parse this:
  452;9098;8763;762;some string
  everything after fourth semi colon is a string  }
function Parse4IntsAndStr(const s: string;
  out x1: integer; out x2: integer; out x3: integer; out x4: integer;
  out outs: string): boolean;
var semiColPos: integer;
begin
  result := false;
  semiColPos := intBeforeSemi(s, x1);
  semiColPos := intBeforeSemiFrom(semiColPos+1,s, x2);
  if semiColPos = 0 then exit;
  semiColPos := intBeforeSemiFrom(semiColPos+1, s, x3);
  if semiColPos = 0 then exit;
  if semiColPos > 0 then begin
    // parse the rest of the string
    result := parseIntAndStrFrom(semiColPos+1,s, x4, outs);
  end;
end;

{ Converts string to 4 integers, separated by ;  ...i.e. 3;35;1;9833
  No trailing semi colon at very end
  todo: could just send binary instead of string, fixed length padding, might
  be more efficient (how much?) }
function ParseInts(s: string; out val1, val2, val3, val4: integer): boolean;
var i, delimcnt: integer;
    tmp: string = '';
    success: boolean;
begin
  result := true;
  delimcnt := 1;
  i := 0;
  val1 := 0; val2 := 0;  val3 := 0; val4 := 0;
  while i < length(s) do begin
    inc(i);
    if s[i] = ';' then begin
      inc(delimcnt);
      tmp := '';
      continue;
    end else begin
      tmp := tmp + s[i];
    end;
    case delimcnt of
      1: begin
           success := trystrtoint(tmp, val1);
           if not success then exit(false);
         end;
      2: begin
           success := trystrtoint(tmp, val2);
           if not success then exit(false);
         end;
      3: begin
           success := trystrtoint(tmp, val3);
           if not success then exit(false);
         end;
      4: begin
           success := trystrtoint(tmp, val4);
           if not success then exit(false);
         end;
    end;
  end;
end;

// x,y (2 integers), wraps around 4 integer function
function Parse2Ints(s: string; out val1, val2: integer): boolean;
var dummy1, dummy2: integer;
begin
  result := ParseInts(s, val1, val2, dummy1, dummy2);
end;

(*
 Old code: try speed tests to see if below is much slower on big string data
 as s + s[i] concatenations should be slower than LeftStr and RightStr

 { Returns false if couldn't parse integer and string combination, delimited by
   semicolon i.e. 7632;somestring of text. TODO: could just do binary, fixed
   length padding, if faster }
 function parseIntStr(inputstr: string; out outI: integer; var outS: string): boolean;
 var i: integer;
     tmp: string = '';
     FirstSemiColFound, success: boolean;
 begin
   result := false;
   FirstSemiColFound := false;
   outI := 0;
   outS := '';
   i := 0;
   while i < length(inputstr) do begin
     inc(i);
     if not FirstSemiColFound then begin
       if inputstr[i] = ';' then begin
         FirstSemiColFound := true;
         // end of integer reached
         success := TryStrToInt(tmp, outI);
         if not success then exit;
         tmp := '';
         continue;
       end;
       tmp := tmp + inputstr[i];
     end else begin
       tmp := tmp + inputstr[i];
     end;
     if (FirstSemiColFound) and (i = length(inputstr)) then begin
       outS := tmp;
     end;
   end;
   result := true;
 end;

*)

{$ifdef TESTINGON}
// verification tests to ensure parsing is functional
procedure ParseIntTests;
var x,y,x1,x2,x3,x4: integer;
    success: boolean;
begin
  writeln('----- Parse Int Tests -----');
  writeln('should succeed:');
  success := parseInts('324;87689;65;54', x1,x2,x3,x4);
  writeln('ints: (',x1,',',x2,',',x3,',',x4,')'+ 'success: ' + BoolToStr(success, true) );
  writeln('should succeed:');
  success := parse2ints('324;87689', x, y);
  writeln('ints: (',x,',',y,')'+ 'success: ' + BoolToStr(success, true) );
  writeln('should fail:');
  success := parseInts('324a;hk87689;65;54', x1,x2,x3,x4);
  writeln('ints: (',x1,',',x2,',',x3,',',x4,')'+ 'success: ' + BoolToStr(success, true) );
  writeln('should fail:');
  success :=parse2ints('abc;87689', x, y);
  writeln('test2 ints: (',x,',',y,')'+ 'success: ' + BoolToStr(success, true) );
  writeln('should fail:');
  success := parse2ints('324;abc', x, y);
  writeln('test3 ints: (',x,',',y,')'+ 'success: ' + BoolToStr(success, true) );
  writeln('should fail:');
  success := parse2ints('324a;87689', x, y);
  writeln('test4 ints: (',x,',',y,')' + 'success: ' + BoolToStr(success, true) );
  writeln('should fail:');
  success := parse2ints('324;87689a', x, y);
  writeln('test5 ints: (',x,',',y,')' + 'success: ' + BoolToStr(success, true) );
end;

procedure ParseIntStrTests;
var i: integer;
    s: string;
    success: boolean;
begin
  writeln('----- Parse Int Str Tests -----');
  writeln('should succeed:');
  success := ParseIntAndStr('8732;a string is here', i, s);
  writeln('test 1: success: ', success, ' int: ', i, ' string: "', s,'"');
  writeln('should fail:');
  success := ParseIntAndStr('8732acx;a string is here', i, s);
  writeln('test 2: success: ', success, ' int: ', i, ' string: "', s,'"');
  writeln('should succeed:');
  success := ParseIntAndStr('1234;a string ;is ;here', i, s);
  writeln('test 3: success: ', success, ' int: ', i, ' string: "', s,'"');
  writeln('should succeed:');
  success := ParseIntAndStr('1234;34;a string ;is ;here', i, s);
  writeln('test 4: success: ', success, ' int: ', i, ' string: "', s,'"');
end;
{$endif} // verification tests


end.

