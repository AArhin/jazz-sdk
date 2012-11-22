unit JazzDBXMechanism;

interface

uses
  Classes,
  DB,
{$WARN UNIT_DEPRECATED OFF}
//  DBXpress,
{$WARN UNIT_DEPRECATED ON}
  SqlExpr,
  JazzCriteriaIntf,
  JazzDatabaseMechanism,
  JazzDatabaseMechanismIntf,
  JazzDataSet,
  JazzDataSetIntf,
  JazzMappingIntf,
  JazzPersisterIntf,
  JazzSQLEngine,
  JazzValueTypeIntf;

type
  TDBXMechanism = class(TDatabaseMechanism)
  private
    FTransactionCount: Integer;
    FInternalConnection: TSQLConnection;
    FInternalTransaction: TTransactionDesc;
    FDestroyConnection: boolean;
  protected
    procedure SetConnected(const Value: boolean); override;
    procedure SetConnection(const Value: TObject); override;  

	  function CommandClass: TObjectCommandClass; override;
	  function QueryClass: TObjectQueryClass; override;
	  function DriverClass: TDatabaseDriverClass; override;

	  function CreateQuery(const Meta: IObjectMeta; const Criteria: ICriteria): IObjectQuery; override;
	  function CreateCommand(const Meta: IObjectMeta = nil): IObjectCommand; override;
	  function CreateDatabaseDriver: IDatabaseDriver; override;

    function InTransaction: boolean; override;
    procedure StartTransaction; override;
    procedure CommitTransaction(const ForceCommit: boolean = False);  override;
    procedure RollbackTransaction; override;
  public
    constructor Create(const Session: IInterface); override;
    destructor Destroy; override;
  end;

  TDBXDriver = class(JazzSQLEngine.TSQLDriver, JazzSQLEngine.ISQLDriver)
  private
    FSQLConnection: TSQLConnection;
    function GetSQLConnection: TSQLConnection;
  protected
    function GetDatabaseSchema: IMapping; override;
    function GetTableSchemaList(const TableSchemaList: IMapping): boolean; override;
  public
    constructor Create(const Mechanism: IInterface); override;
    property SQLConnection: TSQLConnection read GetSQLConnection;
  end;

  TDBXCommand = class(TObjectCommand)
  private
    FSQLQuery: TSQLQuery;
  protected
    function CreateHandler: TObject; override;
    function ExecuteCommand: boolean; override;
    function GetParams: TParams; override;

    procedure SetStatement(const Value: string); override;
  end;

  TDBXQuery = class(TObjectQuery)
  private
    FSQLQuery: TSQLQuery;
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
{$IFDEF VER185}
  DBPlatform,
  DBXCommon,
{$ENDIF}
  SysUtils,
  Variants,
  JazzConsts,
  JazzPersisterConsts;

{$IFDEF VER185}
resourcestring
  SErrorCode = '%s. '#13#10'Error Code: %d (%s)';
{$ENDIF}

{ TDBXMechanism }

function TDBXMechanism.CommandClass: TObjectCommandClass; 
begin
  Result:= TDBXCommand;
end;

procedure TDBXMechanism.CommitTransaction(const ForceCommit: boolean);
begin
  if InTransaction then
  begin
    if ForceCommit then
    begin
{$WARN SYMBOL_DEPRECATED OFF}
      FInternalConnection.Commit(FInternalTransaction);
{$WARN SYMBOL_DEPRECATED ON}
      FTransactionCount:= 0;
    end
    else
    begin
      if FTransactionCount > 0 then Dec(FTransactionCount);
      if FTransactionCount = 0 then
{$WARN SYMBOL_DEPRECATED OFF}
        FInternalConnection.Commit(FInternalTransaction);
{$WARN SYMBOL_DEPRECATED ON}
    end;
  end
  else
    FTransactionCount:= 0
end;

constructor TDBXMechanism.Create(const Session: IInterface);
begin
  inherited Create(Session);
  SetConnection(TSQLConnection.Create(nil));
  FDestroyConnection:= True;
   
  if not InTransaction then
  begin
    FInternalTransaction.TransactionID:= 1;
    FInternalTransaction.IsolationLevel:= xilREADCOMMITTED;
  end;
end;

function TDBXMechanism.CreateQuery(const Meta: IObjectMeta;
  const Criteria: ICriteria): IObjectQuery;
begin
  Result:= QueryClass.Create(Self, Meta, Criteria); 
end;

function TDBXMechanism.CreateDatabaseDriver: IDatabaseDriver;
begin
  Result:= DriverClass.Create(Self); 
end;

function TDBXMechanism.CreateCommand(const Meta: IObjectMeta): IObjectCommand;
begin
  Result:= CommandClass.Create(Self, Meta, nil); 
end;

destructor TDBXMechanism.Destroy;
begin
  if FDestroyConnection then FreeAndNil(FInternalConnection);
  inherited;
end;

function TDBXMechanism.DriverClass: TDatabaseDriverClass;
begin
  Result:= TDBXDriver;
end;

function TDBXMechanism.InTransaction: boolean;
begin
  Result:= (FInternalConnection <> nil) and FInternalConnection.InTransaction;
end;

function TDBXMechanism.QueryClass: TObjectQueryClass;
begin
  Result:= TDBXQuery;
end;

procedure TDBXMechanism.RollbackTransaction;
begin
  if InTransaction then
{$WARN SYMBOL_DEPRECATED OFF}
    FInternalConnection.RollBack(FInternalTransaction);
{$WARN SYMBOL_DEPRECATED ON}

  FTransactionCount:= 0;
end;

procedure TDBXMechanism.StartTransaction;
begin
  if (FInternalConnection <> nil) and not InTransaction then
{$WARN SYMBOL_DEPRECATED OFF}
    FInternalConnection.StartTransaction(FInternalTransaction);
{$WARN SYMBOL_DEPRECATED ON}

  Inc(FTransactionCount);
end;

procedure TDBXMechanism.SetConnected(const Value: boolean);
begin
  if FInternalConnection = nil then
    raise Exception.Create(SConnectionNotDefined)
  else
  begin
    FInternalConnection.Connected:= Value;
    inherited SetConnected(FInternalConnection.Connected);
  end;
end;

procedure TDBXMechanism.SetConnection(const Value: TObject);
begin
  inherited SetConnection(Value);
  if (FInternalConnection <> Value) and (FInternalConnection <> nil) then
  begin
    FDestroyConnection:= False; 
    FreeAndNil(FInternalConnection);
  end;
  FInternalConnection:= TSQLConnection(Value);
end;

{ TDBXQuery }

function TDBXQuery.CreateHandler: TObject;
begin
  FSQLQuery:= TSQLQuery.Create(nil);
  FSQLQuery.SQLConnection:= TSQLConnection((Mechanism as IDatabasemechanism).Connection);
  Result:= FSQLQuery;
end;

function TDBXQuery.GetRecordCount: Integer;
  function UseInternalCount: boolean;
  var
    LSQL: string;
  begin
    LSQL:= FSQLQuery.SQL.Text;
    Result:= (Pos('WHERE', LSQL) > 0) or // do not localize
             (Pos('JOIN' , LSQL) > 0);  // do not localize
  end;
begin
{-------------------------------------------------------------------------------
   Do not read RecordCount if:
     The dataset represents stored procedure.
     The dataset represents a query that contains parameters.
     The dataset represents a multi-table join.

   source: http://bdn.borland.com/article/28494
-------------------------------------------------------------------------------}
  if UseInternalCount then
    Result:= InternalGetRecordCount
  else
    Result:= FSQLQuery.RecordCount;
end;

function TDBXQuery.GetParams: TParams;
begin
  Result:= FSQLQuery.Params;
end;

procedure TDBXQuery.Open;
begin
  inherited Open;
  FSQLQuery.Open; 
end;

procedure TDBXQuery.SetStatement(const Value: string);
begin
  FSQLQuery.SQL.Text:= Value;
end;

function TDBXQuery.GetIsEmpty: boolean;
begin
  Result:= FSQLQuery.IsEmpty;
end;

function TDBXQuery.InternalGetRecordCount: Integer;
begin
  if FSQLQuery.IsEmpty then
    Result:= -1
  else
  { call SELECT COUNT(*) FROM ... " if necessary, I still dont see why do that }
    Result:= 1;
end;

{ TDBXCommand }

function TDBXCommand.CreateHandler: TObject;
begin
  FSQLQuery:= TSQLQuery.Create(nil);
  FSQLQuery.SQLConnection:= TSQLConnection((Mechanism as IDatabasemechanism).Connection);
  Result:= FSQLQuery;
end;

function TDBXCommand.ExecuteCommand: boolean;
var
  LRowsAffected: Integer;
  LExecDirect: boolean;
begin
  try
    LExecDirect:= (Params.Count = 0); // if params.count = 0, do not prepare 
    LRowsAffected:= FSQLQuery.ExecSQL(LExecDirect);
    SetRowsAffected(LRowsAffected);
    Result:= True;
  except
{$IFDEF VER185}
    on DBXError: TDBXError do
    begin
      raise Exception.CreateFmt(SErrorCode, [
        DBXError.Message, DBXError.ErrorCode,
        TDBXError.ErrorCodeToString(DBXError.ErrorCode)]);
      Result:= False;
    end
    else
    begin
      raise;
      Result:= False;
    end;
{$ELSE}
    raise;
    Result:= False;
{$ENDIF}
  end;
end;

function TDBXCommand.GetParams: TParams;
begin
  Result:= FSQLQuery.Params;
end;

procedure TDBXCommand.SetStatement(const Value: string);
begin
  FSQLQuery.SQL.Text:= Value;
end;

function TDBXDriver.GetDatabaseSchema: IMapping;
begin
  // TODO: Get Database Schema for DBExpress
end;

function TDBXDriver.GetTableSchemaList(const TableSchemaList: IMapping):
  boolean;
var
  LTables: TStrings;
  I: Integer;
begin
  LTables:= TStringList.Create;
  try
    SQLConnection.GetTableNames(LTables, False);

    for I:= 0 to (LTables.Count - 1) do
    begin
      // TODO: Populate Mapping
    end;
  finally
    LTables.Free;
  end;
  Result:= False;
end;

function TDBXDriver.GetSQLConnection: TSQLConnection;
begin
  if FSQLConnection = nil then
    FSQLConnection:= TSQLConnection((Mechanism as IDatabaseMechanism).Connection);

  Result:= FSQLConnection;
end;

constructor TDBXDriver.Create(const Mechanism: IInterface);
begin
  inherited;
  ColumnTypeForAutoInc   := 'INTEGER'; // do not localize
  ColumnTypeForBlob      := 'BLOB SUB_TYPE 0'; // do not localize
  ColumnTypeForBoolean   := 'INTEGER'; // do not localize
  ColumnTypeForChar      := 'CHAR(1)'; // do not localize
  ColumnTypeForCurrency  := 'NUMERIC(15,2)'; // do not localize
  ColumnTypeForDate      := 'DATE'; // do not localize
  ColumnTypeForDateTime  := 'TIMESTAMP'; // do not localize
  ColumnTypeForFloat     := 'NUMERIC(15,4)'; // do not localize
  ColumnTypeForInteger   := 'INTEGER'; // do not localize
  ColumnTypeForLongInt   := 'INTEGER'; // do not localize
  ColumnTypeForMemo      := 'BLOB SUB_TYPE 1'; // do not localize
  ColumnTypeForSmallInt  := 'INTEGER'; // do not localize
  ColumnTypeForString    := 'VARCHAR(%d)'; // do not localize
  ColumnTypeForTime      := 'TIME'; // do not localize
  ColumnTypeForTimeStamp := 'TIMESTAMP'; // do not localize
  ColumnTypeForWideString:= 'VARCHAR(%d)'; // do not localize
end;

end.





