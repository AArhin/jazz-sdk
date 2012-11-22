unit JazzZeosMechanism;

interface

uses
  JazzCriteriaIntf,
  JazzDatabaseMechanism,
  JazzDatabaseMechanismIntf,
  JazzDataSet,
  JazzDataSetIntf,
  JazzMappingIntf,
  JazzPersisterIntf,
  JazzSQLEngine,
  JazzValueTypeIntf,
  DB,
  Classes,
  ZConnection,
  ZDataSet,
  ZDbcIntfs;

type
  IZeosMechanism = interface(IDataBaseMechanism)
    ['{D72E5A75-06F7-4753-8DF7-EF79270E02EC}']
    procedure SetConnectionParams(Protocol, Host: string; Port: Integer;
      DataBase, User, Password: string);
  end;

  TZeosMechanism = class(TDatabaseMechanism, IZeosMechanism)
  private
    FProtocol: string;
    FHost: string;
    FPort: Integer;
    FDataBase: string;
    FUser: string;
    FPassword: string;
    FTransactionCount: Integer;
    FInternalConnection: TZConnection;
  protected
    procedure SetConnectionParams(Protocol, Host: string; Port: Integer; DataBase,
      User, Password: string);
    procedure SetConnected(const Value: boolean); override;

    function CommandClass: TObjectCommandClass; override;
    function QueryClass: TObjectQueryClass; override;
    function DriverClass: TDatabaseDriverClass; override;

    function CreateQuery(const Metadata: IObjectMeta; const Criteria: ICriteria):
      IObjectQuery; override;
    function CreateCommand(const Metadata: IObjectMeta = nil): IObjectCommand;
      override;
    function CreateDatabaseDriver: IDatabaseDriver; override;

    function InTransaction: boolean; override;
    procedure StartTransaction; override;
    procedure CommitTransaction(const ForceCommit: boolean = False); override;
    procedure RollbackTransaction; override;
  public
    constructor Create(const Session: IInterface); override;
    destructor Destroy; override;
  end;

  TZeosDriver = class(TSQLDriver, ISQLDriver)
  protected
    function GetDatabaseSchema: IMapping; override;
  public
    procedure AfterConstruction; override;
  end;

  TZeosCommand = class(TObjectCommand)
  private
    FZQuery: TZQuery;
  protected
    function GetParams: TParams; override;
    function CreateHandler: TObject; override;
    procedure SetStatement(const Value: string); override;
    function ExecuteCommand: boolean; override;
  end;

  TZeosQuery = class(TObjectQuery)
  private
    FZQuery: TZQuery;
    function InternalGetRecordCount: Integer;
  protected
    function GetParams: TParams; override;
    function GetRecordCount: Integer; override;

    function CreateHandler: TObject; override;
    function GetIsEmpty: boolean; override;

    procedure Open; override;
    procedure SetStatement(const Value: string); override;
  end;

implementation

uses
  JazzConsts,
  JazzPersisterConsts,
  SysUtils,
  Variants;

{ TZeosMechanism }

function TZeosMechanism.CommandClass: TObjectCommandClass;
begin
  Result := TZeosCommand;
end;

procedure TZeosMechanism.CommitTransaction(const ForceCommit: boolean);
begin
  if InTransaction then
  begin
    if ForceCommit then
    begin
      FInternalConnection.Commit;
      FTransactionCount := 0;
    end
    else
    begin
      if FTransactionCount > 0 then Dec(FTransactionCount);
      if FTransactionCount = 0 then
        FInternalConnection.Commit;
    end;
  end
  else
    FTransactionCount := 0
end;

constructor TZeosMechanism.Create(const Session: IInterface);
begin
  FPort := 0;
  FInternalConnection := TZConnection.Create(nil);
  FInternalConnection.TransactIsolationLevel := tiReadCommitted;
  Connection := FInternalConnection;
  inherited Create(Session);
end;

function TZeosMechanism.CreateQuery(const Metadata: IObjectMeta; const
  Criteria: ICriteria): IObjectQuery;
begin
  Result := QueryClass.Create(Self, Metadata, Criteria);
end;

function TZeosMechanism.CreateDatabaseDriver: IDatabaseDriver;
begin
  Result := DriverClass.Create(Self);
end;

function TZeosMechanism.CreateCommand(const Metadata: IObjectMeta = nil):
  IObjectCommand;
begin
  Result := CommandClass.Create(Self, Metadata, nil);
end;

destructor TZeosMechanism.Destroy;
begin
  if FInternalConnection <> nil then
    FreeAndNil(FInternalConnection);
  inherited;
end;

function TZeosMechanism.DriverClass: TDatabaseDriverClass;
begin
  Result := TZeosDriver;
end;

function TZeosMechanism.InTransaction: boolean;
begin
  Result := (FInternalConnection <> nil) and FInternalConnection.InTransaction;
end;

function TZeosMechanism.QueryClass: TObjectQueryClass;
begin
  Result := TZeosQuery;
end;

procedure TZeosMechanism.RollbackTransaction;
begin
  if InTransaction then
    FInternalConnection.RollBack;

  FTransactionCount := 0;
end;

procedure TZeosMechanism.StartTransaction;
begin
  if (FInternalConnection <> nil) and not InTransaction then
    FInternalConnection.StartTransaction;

  Inc(FTransactionCount);
end;

procedure TZeosMechanism.SetConnected(const Value: boolean);
begin
  if FInternalConnection = nil then
    raise Exception.Create(SConnectionNotDefined)
  else
  begin
    FInternalConnection.Protocol := FProtocol;
    FInternalConnection.HostName := FHost;
    FInternalConnection.Port := FPort;
    FInternalConnection.Database := FDataBase;
    FInternalConnection.User := FUser;
    FInternalConnection.Password := FPassword;
    FInternalConnection.Connected := Value;
    inherited SetConnected(FInternalConnection.Connected);
  end;
end;

procedure TZeosMechanism.SetConnectionParams(Protocol, Host: string; Port:
  Integer; DataBase, User, Password: string);
begin
  FProtocol := Protocol;
  FHost := Host;
  FPort := Port;
  FDataBase := DataBase;
  FUser := User;
  FPassword := Password;
end;

{ TZeosQuery }

function TZeosQuery.CreateHandler: TObject;
begin
  FZQuery := TZQuery.Create((Mechanism as IDatabasemechanism).Connection as
    TComponent);
  FZQuery.Connection := (Mechanism as IDatabasemechanism).Connection as TZConnection;
  Result := FZQuery;
end;

function TZeosQuery.GetRecordCount: Integer;
  function UseInternalCount: boolean;
  begin
    Result := (Pos('WHERE', FZQuery.SQL.Text) > 0) or
      (Pos('JOIN', FZQuery.SQL.Text) > 0);
  end;
begin
  if UseInternalCount then
    Result := InternalGetRecordCount
  else
    Result := FZQuery.RecordCount;
end;

function TZeosQuery.GetParams: TParams;
begin
  Result := FZQuery.Params;
end;

procedure TZeosQuery.Open;
begin
  inherited Open;
  FZQuery.Open;
end;

procedure TZeosQuery.SetStatement(const Value: string);
begin
  FZQuery.SQL.Text := Value;
end;

function TZeosQuery.GetIsEmpty: boolean;
begin
  Result := FZQuery.IsEmpty;
end;

function TZeosQuery.InternalGetRecordCount: Integer;
begin
  if FZQuery.IsEmpty then
    Result := -1
  else
    { call SELECT COUNT(*) FROM ... " if necessary, I still dont see why do that }
    Result := 1;
end;

{ TZeosCommand }

function TZeosCommand.CreateHandler: TObject;
begin
  FZQuery := TZQuery.Create((Mechanism as IDatabasemechanism).Connection as
    TComponent);
  FZQuery.Connection := (Mechanism as IDatabasemechanism).Connection as TZConnection;
  Result := FZQuery;
end;

function TZeosCommand.ExecuteCommand: boolean;
var
  LRowsAffected: Integer;
begin
  try
    FZQuery.ExecSQL;
    LRowsAffected := FZQuery.RowsAffected;
    SetRowsAffected(LRowsAffected);
    Result := True;
  except
    raise;
    Result := False;
  end;
end;

function TZeosCommand.GetParams: TParams;
begin
  Result := FZQuery.Params;
end;

procedure TZeosCommand.SetStatement(const Value: string);
begin
  FZQuery.SQL.Text := Value;
end;

{ TZeosDriver }

procedure TZeosDriver.AfterConstruction;
begin
  inherited;
  ColumnTypeForAutoInc := 'INTEGER';
  ColumnTypeForBlob := 'BLOB SUB_TYPE 0';
  ColumnTypeForBoolean := 'INTEGER';
  ColumnTypeForChar := 'CHAR(1)';
  ColumnTypeForCurrency := 'NUMERIC(15,2)';
  ColumnTypeForDate := 'DATE';
  ColumnTypeForDateTime := 'TIMESTAMP';
  ColumnTypeForFloat := 'NUMERIC(15,4)';
  ColumnTypeForInteger := 'INTEGER';
  ColumnTypeForLongInt := 'INTEGER';
  ColumnTypeForMemo := 'BLOB SUB_TYPE 1';
  ColumnTypeForSmallInt := 'INTEGER';
  ColumnTypeForString := 'VARCHAR(%d)';
  ColumnTypeForTime := 'TIME';
  ColumnTypeForTimeStamp := 'TIMESTAMP';
  ColumnTypeForWideString := 'VARCHAR(%d)';
end;

function TZeosDriver.GetDatabaseSchema: IMapping;
begin
  // TODO: Get Database Schema for Zeos
end;

end.

