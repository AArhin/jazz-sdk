unit TestJazzObjectTable;

interface

uses
  TestFramework,
  JazzObjectTable,
  JazzSessionIntf,
  JazzPersister,
  JazzStreamMechanism;

type
  TestTObjectTable = class(TTestCase)
  private
    FStreamSession: ISession;
    FObjectTable: TObjectTable;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ObjectClassProp;
    procedure ObjectClassNameProp;
    procedure Append;
    procedure SessionNotDefined;
    procedure ObjectClassNotDefined;
  end;

implementation

uses
  PersonBO,
  SysUtils,
  JazzSession,
  JazzValueType;

procedure TestTObjectTable.Append;
begin
  FObjectTable.Session := FStreamSession;
  FObjectTable.ObjectClass := TPerson;
  FObjectTable.Open;

  CheckEquals(0, FObjectTable.RecordCount);

  FObjectTable.Append;
  FObjectTable.Post;

  CheckEquals(1, FObjectTable.RecordCount);

  FObjectTable.Append;
  FObjectTable.Post;

  CheckEquals(2, FObjectTable.RecordCount);
end;

procedure TestTObjectTable.SessionNotDefined;
begin
  ExpectedException := EJazzSessionNotDefined;
  FObjectTable.Open;
end;

procedure TestTObjectTable.ObjectClassNameProp;
var
  LObject: TClass;
begin
  // for design time support
  CheckEquals(EmptyStr, FObjectTable.ObjectClassName);
  LObject := nil;
  CheckEquals(LObject, FObjectTable.ObjectClass);

  FObjectTable.ObjectClassName := 'TPerson';

  CheckEquals(TPerson, FObjectTable.ObjectClass);
  CheckEquals('TPerson', FObjectTable.ObjectClassName);
end;

procedure TestTObjectTable.ObjectClassNotDefined;
begin
  ExpectedException := EJazzObjectClassNotDefined;
  FObjectTable.Session := FStreamSession;
  FObjectTable.Open;
end;

procedure TestTObjectTable.ObjectClassProp;
var
  LObject: TClass;
begin
  // for run time support
  CheckEquals(EmptyStr, FObjectTable.ObjectClassName);
  LObject := nil;
  CheckEquals(LObject, FObjectTable.ObjectClass);

  FObjectTable.ObjectClass := TPerson;

  CheckEquals(TPerson, FObjectTable.ObjectClass);
  CheckEquals('TPerson', FObjectTable.ObjectClassName);
end;

procedure TestTObjectTable.SetUp;
begin
  FStreamSession := NewSession(TStreamMechanism, cmNoCache);
  FStreamSession.Mechanism.Connected := True;

  FObjectTable := TObjectTable.Create(nil);
end;

procedure TestTObjectTable.TearDown;
begin
  FObjectTable.Free;
  FObjectTable := nil;
end;

initialization
  RegisterTest(TestTObjectTable.Suite);

end.

