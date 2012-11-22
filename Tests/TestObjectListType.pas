unit TestObjectListType;

interface

uses
  TestFramework,
  JazzValueType,
  JazzValueTypeIntf;

{$I JazzComp.inc}

type
  TTestObjectListType = class(TTestCase)
  published
    {$IFDEF D9_OR_HIGHER}
    procedure TestEnumerator;
    {$ENDIF}
  end;

implementation

uses
  PersonBO;

{ TestObjectValueType }

{$IFDEF D9_OR_HIGHER}
procedure TTestObjectListType.TestEnumerator;
const
  Names: array[0..2] of string = ('John Doe', 'Foo Bar', 'Jazz Rocks');
var
  People: IObjectListType;
  Person: IPerson;
  Obj: IObjectType;
  Name: string;
  Index: Integer;
begin
  People := TPerson.NewList;

  for Name in Names do
  begin
    Person := TPerson.Create;
    Person.Name := Name;
    People.Add(Person);
  end;

  Index := 0;
  for Obj in People do
  begin
    Person := Obj as IPerson;
    CheckEquals(Names[Index], Person.Name);
    Inc(Index);
  end;

  // To make sure all items were tested
  CheckEquals(High(Names) + 1, Index);
end;
{$ENDIF}

initialization
  RegisterTest(TTestObjectListType.Suite);

end.

