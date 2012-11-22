unit JazzSQLDirectMechanism;
(*
  ******************************************************************************
    I have not tested SQLDirectMechanism, if you are using this please send
    your comments to "jazz@liws.com.br"
  ******************************************************************************

  * Centura (formerly, Gupta) SQLBase Server
  * IBM DB2 Universal Database
  * Informix Server
  * Interbase/Firebird Server
  * Microsoft SQL Server
  * MySQL Server
  * Oracle Database Server
  * PostgreSQL Server
  * Sybase Adaptive Server Enterprise and Adaptive Server Anywhere
  * ODBC datasources
  * OLEDB datasources
  http://www.sql-direct.com/
*)

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
  Classes,
  DB,
  SDEngine;

type
  TSQLDirectMechanism = class(TDatabaseMechanism)
  private
    FTransactionCount: Integer;
    FInternalConnection: TSDDatabase;
    FDestroyConnection: boolean;
  protected
    procedure SetConnected(const Value: boolean); override;
    procedure SetConnection(const Value: TObject); override;

	  function CommandClass: TObjectCommandClass; override;
	  function QueryClass: TObjectQueryClass; override;
	  function DriverClass: TDatabaseDriverClass; override;

	  function CreateQuery(const Metadata: IObjectMeta; const Criteria: ICriteria): IObjectQuery; override;
	  function CreateCommand(const Metadata: IObjectMeta = nil): IObjectCommand; override;
	  function CreateDatabaseDriver: IDatabaseDriver; override;

    function InTransaction: boolean; override;
    procedure StartTransaction; override;
    procedure CommitTransaction(const ForceCommit: boolean = False);  override;
    procedure RollbackTransaction; override;
  public
    constructor Create(const Session: IInterface); override;
    destructor Destroy; override;
  end;

  TSQLDirectDriver = class(JazzSQLEngine.TSQLDriver, JazzSQLEngine.ISQLDriver)
  private
    FSQLConnection: TSDDatabase;
    function GetSQLConnection: TSDDatabase;
  protected
    function GetDatabaseSchema: IMapping; override;
    function GetTableSchemaList(const TableSchemaList: IMapping): Boolean; override;
  public
    procedure AfterConstruction; override;
    property SQLConnection: TSDDatabase read GetSQLConnection;
  end;

  TSQLDirectCommand = class(TObjectCommand)
  private
    FSQLQuery: TSDQuery;
  protected
    function GetParams: TParams; override;
    function CreateHandler: TObject; override;
    procedure SetStatement(const Value: string); override;
    function ExecuteCommand: boolean; override;
  end;

  TSQLDirectQuery = class(TObjectQuery)
  private
    FSQLQuery: TSDQuery;
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

{ TSQLDirectMechanism }

function TSQLDirectMechanism.CommandClass: TObjectCommandClass;
begin
  Result := TSQLDirectCommand;
end;

procedure TSQLDirectMechanism.CommitTransaction(const ForceCommit: boolean);
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
    FTransactionCount := 0;
end;

constructor TSQLDirectMechanism.Create(const Session: IInterface);
begin
  inherited Create(Session);
  SetConnection(TSDDatabase.Create(nil));
  FDestroyConnection := True;
end;

function TSQLDirectMechanism.CreateQuery(const Metadata: IObjectMeta;
  const Criteria: ICriteria): IObjectQuery;
begin
  Result := QueryClass.Create(Self, Metadata, Criteria);
end;

function TSQLDirectMechanism.CreateDatabaseDriver: IDatabaseDriver;
begin
  Result := DriverClass.Create(Self); 
end;

function TSQLDirectMechanism.CreateCommand(const Metadata: IObjectMeta): IObjectCommand;
begin
  Result := CommandClass.Create(Self, Metadata, nil);
end;

destructor TSQLDirectMechanism.Destroy;
begin
  if FDestroyConnection then FreeAndNil(FInternalConnection);
  inherited;
end;

function TSQLDirectMechanism.DriverClass: TDatabaseDriverClass;
begin
  Result := TSQLDirectDriver;
end;

function TSQLDirectMechanism.InTransaction: boolean;
begin
  Result := (FInternalConnection <> nil) and FInternalConnection.InTransaction;
end;

function TSQLDirectMechanism.QueryClass: TObjectQueryClass;
begin
  Result := TSQLDirectQuery;
end;

procedure TSQLDirectMechanism.RollbackTransaction;
begin
  if InTransaction then
    FInternalConnection.RollBack;

  FTransactionCount := 0;
end;

procedure TSQLDirectMechanism.StartTransaction;
begin
  if (FInternalConnection <> nil) and not InTransaction then
    FInternalConnection.StartTransaction;

  Inc(FTransactionCount);
end;

procedure TSQLDirectMechanism.SetConnected(const Value: boolean);
begin
  if FInternalConnection = nil then
    raise Exception.Create(SConnectionNotDefined)
  else
  begin
    FInternalConnection.Connected := Value;
    inherited SetConnected(FInternalConnection.Connected);
  end;
end;

procedure TSQLDirectMechanism.SetConnection(const Value: TObject);
begin
  inherited SetConnection(Value);
  if (FInternalConnection <> Value) and (FInternalConnection <> nil) then
  begin
    FDestroyConnection := False;
    FreeAndNil(FInternalConnection);
  end;
  FInternalConnection := TSDDatabase(Value);
end;

{ TSQLDirectQuery }

function TSQLDirectQuery.CreateHandler: TObject;
begin
  FSQLQuery := TSDQuery.Create((Mechanism as IDatabasemechanism).Connection as TComponent);
  FSQLQuery.DatabaseName := TSDDatabase((Mechanism as IDatabasemechanism).Connection).DatabaseName;
  Result := FSQLQuery;
end;

function TSQLDirectQuery.GetRecordCount: Integer;
  function UseInternalCount: boolean;
  begin
    Result:= (Pos('WHERE', FSQLQuery.SQL.Text) > 0) or
             (Pos('JOIN' , FSQLQuery.SQL.Text) > 0); 
  end;
begin
  if UseInternalCount then
    Result := InternalGetRecordCount
  else
    Result := FSQLQuery.RecordCount;
end;

function TSQLDirectQuery.GetParams: TParams;
begin
  Result := FSQLQuery.Params;
end;

procedure TSQLDirectQuery.Open;
begin
  inherited Open;
  FSQLQuery.Open; 
end;

procedure TSQLDirectQuery.SetStatement(const Value: string);
begin
  FSQLQuery.SQL.Text := Value;
end;

function TSQLDirectQuery.GetIsEmpty: boolean;
begin
  Result := FSQLQuery.IsEmpty;
end;

function TSQLDirectQuery.InternalGetRecordCount: Integer;
begin
  if FSQLQuery.IsEmpty then
    Result := -1
  else
  { call SELECT COUNT(*) FROM ... " if necessary, I still dont see why do that }
    Result := 1;
end;

{ TSQLDirectCommand }

function TSQLDirectCommand.CreateHandler: TObject;
begin
  FSQLQuery := TSDQuery.Create((Mechanism as IDatabasemechanism).Connection as TComponent);
  FSQLQuery.DatabaseName := TSDDatabase((Mechanism as IDatabasemechanism).Connection).DatabaseName; //**
  Result := FSQLQuery;
end;

function TSQLDirectCommand.ExecuteCommand: boolean;
var
  LRowsAffected: Integer;
  LExecDirect: boolean;
begin
  try
    LExecDirect:= (Params.Count = 0); // if params.count = 0, do not prepare
    if not LExecDirect then
      FSQLQuery.Prepare;
    FSQLQuery.ExecSQL;
    LRowsAffected := FSQLQuery.RowsAffected;
    SetRowsAffected(LRowsAffected);
    Result:= True;
  except
    raise;
    Result := False;
  end;
end;

function TSQLDirectCommand.GetParams: TParams;
begin
  Result := FSQLQuery.Params;
end;

procedure TSQLDirectCommand.SetStatement(const Value: string);
begin
  FSQLQuery.SQL.Text := Value;
end;

{ TSQLDirectDriver }

procedure TSQLDirectDriver.AfterConstruction;
begin
  inherited;
  // TODO: evaluate correcty types for your database
  ColumnTypeForAutoInc   := 'INTEGER';
  ColumnTypeForBlob      := 'BLOB SUB_TYPE 0';
  ColumnTypeForBoolean   := 'INTEGER';
  ColumnTypeForChar      := 'CHAR(1)';
  ColumnTypeForCurrency  := 'NUMERIC(15,2)';
  ColumnTypeForDate      := 'DATE';
  ColumnTypeForDateTime  := 'TIMESTAMP';
  ColumnTypeForFloat     := 'NUMERIC(15,4)';
  ColumnTypeForInteger   := 'INTEGER';
  ColumnTypeForLongInt   := 'INTEGER';
  ColumnTypeForMemo      := 'BLOB SUB_TYPE 1';
  ColumnTypeForSmallInt  := 'INTEGER';
  ColumnTypeForString    := 'VARCHAR(%d)';
  ColumnTypeForTime      := 'TIME';
  ColumnTypeForTimeStamp := 'TIMESTAMP';
  ColumnTypeForWideString:= 'VARCHAR(%d)';
end;

function TSQLDirectDriver.GetDatabaseSchema: IMapping;
begin
  // TODO: Get Database Schema for SQLDirect
end;

function TSQLDirectDriver.GetSQLConnection: TSDDatabase;
begin
  if FSQLConnection = nil then
    FSQLConnection := TSDDatabase((Mechanism as IDatabaseMechanism).Connection);

  Result := FSQLConnection;
end;

function TSQLDirectDriver.GetTableSchemaList(
  const TableSchemaList: IMapping): Boolean;
var
  LTables: TStrings;
  I: Integer;
begin
  LTables := TStringList.Create;
  try
    SQLConnection.GetTableNames('', False, LTables); //** Needs completition

    for I := 0 to (LTables.Count - 1) do
    begin
      // TODO: Populate Mapping
    end;
  finally
    LTables.Free;
  end;
  Result := False;
end;

end.




