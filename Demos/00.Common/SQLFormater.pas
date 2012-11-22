unit SQLFormater;

interface

uses
  Classes;

type
  TSimpleSQLFormater = class(TInterfacedObject)
  public
    class procedure Execute(SQL: TStrings);
  end;

implementation

uses
  JazzConsts,
  SysUtils;

{ TFormatSQL }

class procedure TSimpleSQLFormater.Execute(SQL: TStrings);
var
  I: Integer;
  S: string;
  LSQL: string;
begin
  for I:= 0 to SQL.Count -1 do
  begin
    S:= SQL[I];
    if Pos('ALTER TABLE', S) = 0 then
    begin
      S:= StringReplace(S, ' (', ' ('#13#10'  ', [rfReplaceAll]);
      S:= StringReplace(S, ', ', ', '#13#10'  ', [rfReplaceAll]);
      S:= StringReplace(S, ');', #13#10');', [rfReplaceAll]);
    end;

    LSQL:= LSQL + S + #13#10#13#10
  end;

  SQL.Text:= LSQL;
end;

end.

