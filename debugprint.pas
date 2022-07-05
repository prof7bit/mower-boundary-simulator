unit DebugPrint;

{$mode ObjFPC}{$H+}

interface

procedure Print(Txt: String);
procedure Print(Int: Integer);
procedure Print(Dbl: Double);
procedure Print(Fmt: String; Args: array of const);

implementation
uses
  Classes, SysUtils, LazTracer;

const
  FILENAME = 'debug.txt';

var
  F: TFileStream = nil;
  LineNumbers: Boolean = True;

function GetCallingLine: String;
var
  BP, OldBP: Pointer;
  Info: String;
  SL: TStringArray;
  Filename, LineNumber: String;
begin
  Result := '';
  BP := get_caller_frame(get_frame);
  while Assigned(BP) do begin
    Info := GetLineInfo(get_caller_addr(BP), True);
    if Pos('line', Info) = 0 then begin
      LineNumbers := False;
      exit;
    end;
    if Pos('debugprint.pas', Info) = 0 then begin
      SL := Info.Split(' ');
      Filename := SL[Length(SL) - 1];
      LineNumber := SL[Length(SL) - 3];
      Exit(Filename + ':' + LineNumber);
    end;
    OldBP := BP;
    BP := get_caller_frame(BP);
    if (BP <= OldBP) or (BP > (StackBottom + StackLength)) then
      BP := Nil;
  end;
end;

procedure Print(Txt: String);
var
  LN: String;
begin
  if LineNumbers then begin
    LN := GetCallingLine;
    if LN <> '' then begin
      Txt := '[' + LN + '] ' + Txt;
    end;;
  end;
  {$ifdef windows}
  if not Assigned(F) then
    F := TFileStream.Create(FILENAME, fmCreate);
  F.Write(Txt[1], Length(Txt));
  F.Write(LineEnding, Length(LineEnding));
  {$else}
  Writeln(Txt);
  {$endif}
end;

procedure Print(Int: Integer);
begin
  Print(IntToStr(Int));
end;

procedure Print(Dbl: Double);
begin
  Print(FloatToStr(Dbl));
end;

procedure Print(Fmt: String; Args: array of const);
begin
  Print(Format(Fmt, Args));
end;

finalization
if Assigned(F) then
  FreeAndNil(F);
end.

