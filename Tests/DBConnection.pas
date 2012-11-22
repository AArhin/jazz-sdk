unit DBConnection;

interface

uses JazzDatabaseMechanismIntf, TestExtensions;

type
  TConnection = class(TObject)
  private
    FDatabaseMechanism: IDatabaseMechanism;
  protected
    procedure DoConnect;
  public
    procedure Setup;
    procedure TearDown;
    property DatabaseMechanism: IDatabaseMechanism read FDatabaseMechanism;
  end;

  TConnectionSetup = class(TTestSetup)
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
  end;

var
  Connection: TConnection;

implementation

uses JazzSession, JazzPersister, JazzIBXMechanism, IBDataBase, SysUtils, Forms, classes;

procedure TConnection.DoConnect;
var
  LDBSchema: TStringList;
begin
  NewSession(TIBXMechanism);
  FDatabaseMechanism := ActiveSession.Mechanism as IDatabaseMechanism;
  TIBDatabase(FDatabaseMechanism.Connection).SQLDialect := 3;
  TIBDatabase(FDatabaseMechanism.Connection).DatabaseName := ExtractFilePath(Application.ExeName) +
    'database\JazzValueTypeTests.fdb';
  TIBDatabase(FDatabaseMechanism.Connection).Params.Values['user_name'] := 'SYSDBA';
  TIBDatabase(FDatabaseMechanism.Connection).Params.Values['password'] := 'masterkey';
  FDatabaseMechanism.Connected := True;
  LDBSchema := TStringList.Create;
  try
    FDatabaseMechanism.GenerateSchema(LDBSchema);
    LDBSchema.SaveToFile('FireBirdDataBaseSchema.sql');
  finally
    LDBSchema.Free;
  end;
end;

procedure TConnection.Setup;
begin
  DoConnect;
end;

procedure TConnection.TearDown;
begin
  FDatabaseMechanism.Connected := False;
  FDatabaseMechanism := nil;
end;

procedure TConnectionSetup.Setup;
begin
  inherited;
  Connection := TConnection.Create;
  Connection.Setup;
end;

procedure TConnectionSetup.TearDown;
begin
  Connection.TearDown;
  Connection.Free;
  inherited;
end;

end.

