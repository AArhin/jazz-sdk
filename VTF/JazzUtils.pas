unit JazzUtils;

interface

uses
  Classes,
  SysUtils;

type
  TBlobByteArray = array of Byte;

function ClassToName(const ObjectClass: TClass): string;
function FormatTypeName(Name: string; ClassPrefix: Boolean = True): string;

procedure ExecuteAttach(const Observer: IInterface; const Subject: IInterface);
procedure ExecuteDetach(const Observer: IInterface; const Subject: IInterface);

function GenerateGUID: string;
function ItemInStringList(const Value: string; const List: TStringList): boolean;
function InterfaceInList(const Value: IInterface; const List: IInterfaceList): boolean;

function RemoveChars(const Value: string; const Count: Integer; const Start: Integer = 1; const ClearEol: boolean = True): string;
function StrGetToken(Text, Delimeter: string; var Position: Integer): string;

procedure BytesToStream(Bytes: TBlobByteArray; Stream: TStream);
function StreamToBytes(Stream: TStream; Count: Integer = 0): TBlobByteArray;

implementation

uses
  {$IFNDEF LINUX}
  ComObj,
  {$ENDIF}
  Variants,
  JazzConsts,
  JazzCoreConsts,
  JazzObserverIntf,
  JazzSubjectIntf;

function GenerateGUID: string;
begin
  {$IFNDEF LINUX}
  Result:= CreateClassID;
  {$ENDIF}
end;

procedure ExecuteAttach(const Observer: IInterface; const Subject: IInterface);
var
  LObserver: IObserver;
  LSubject: ISubject;
begin
  if Supports(Observer, IObserver, LObserver) and
     Supports(Subject, ISubject, LSubject) then
    LSubject.Attach(LObserver);
end;

procedure ExecuteDetach(const Observer: IInterface; const Subject: IInterface);
var
  LObserver: IObserver;
  LSubject: ISubject;
begin
  if Supports(Observer, IObserver, LObserver) and
     Supports(Subject, ISubject, LSubject) then
    LSubject.Detach(LObserver);
end;

function ItemInStringList(const Value: string; const List: TStringList): boolean;
begin
  Result:= List.IndexOf(Value) <> NotFound;
end;

function InterfaceInList(const Value: IInterface; const List: IInterfaceList): boolean;
begin
  Result:= List.IndexOf(Value) <> NotFound;
end;

function RemoveChars(const Value: string; const Count: Integer;
  const Start: Integer; const ClearEol: boolean): string;
var
  S: string;
begin
  S:= Value;
  if ClearEol then S:= StringReplace(S, EOL, EmptyStr, [rfReplaceAll]);
  Result:= Copy(S, Start, Length(S) - Count);
end;

function ClassToName(const ObjectClass: TClass): string;
const
  StartIndex = 2;
var
  Count: Integer;
begin
  Count:= Length(ObjectClass.ClassName) -1; 
  Result:= Copy(ObjectClass.ClassName, StartIndex, Count);
end;

function StrGetToken(Text, Delimeter: string; var Position: Integer): string;
var
  TempStr: string;
  EndStringPos: Integer;
begin
  Result:= EmptyStr;
  if Position <= 0 then
    Exit;
  if Position > Length(Text) then
  begin
    Position:= -1;
    Exit;
  end;

  TempStr:= Copy(Text, Position, Length(Text) + 1 - Position);
  if (Length(Delimeter) = 1) then
    EndStringPos:= Pos(Delimeter, TempStr)
  else
  begin
    Delimeter:= ' ' + Delimeter + ' ';
    EndStringPos:= Pos(UpperCase(Delimeter), UpperCase(TempStr));
  end;

  if EndStringPos <= 0 then
  begin
    Result:= TempStr;
    Position:= -1;
  end
  else
  begin
    Result:= Copy(TempStr, 1, EndStringPos - 1);
    Position:= Position + EndStringPos + Length(Delimeter) - 1;
  end;
end;

procedure BytesToStream(Bytes: TBlobByteArray; Stream: TStream);
begin
  Stream.Size := 0;
  Stream.Write(Bytes[0], Length(Bytes));
end;

function StreamToBytes(Stream: TStream; Count: Integer = 0): TBlobByteArray;
var
  LPos: Int64;
begin
  LPos:= Stream.Position;
  try
    Stream.Position := 0;
    if Count = 0 then
    begin
      Count := Stream.Size;
      Stream.Position := 0;
    end;
    SetLength(Result, Count);
    System.Move(TMemoryStream(Stream).Memory^, Result[0], Count);
  finally
    Stream.Position := LPos;
  end;
end;

function FormatTypeName(Name: string; ClassPrefix: Boolean = True): string;
  function CharUpper(Value: Char): Char;
  var
    S: string;
  begin
    S:= Uppercase(Value);
    Result:= S[1];
  end;
  
var
  I: Integer;
begin
  Result:= LowerCase(Name);
  Result[1]:= CharUpper(Result[1]);

  for I := Length(Result) downto 1 do
  begin
    if Result[I] = '_' then
    begin
      if I < Length(Result) then CharUpper(Result[I + 1]);
      Delete(Result, I, 1);
    end;
  end;

  if ClassPrefix then Result:= 'T' + Result
end;

end.

