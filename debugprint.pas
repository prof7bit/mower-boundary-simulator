unit DebugPrint;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

procedure Print(Txt: String);
procedure Print(Int: Integer);
procedure Print(Dbl: Double);
procedure Print(Fmt: String; Args: array of const);

implementation
const
  FILENAME = 'debug.txt';

var
  F: TFileStream;

procedure Print(Txt: String);
begin
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

