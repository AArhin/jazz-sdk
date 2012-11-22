unit JazzIBXMechanism;

interface

uses
  IBQuery,
  Classes,
  DB,
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
  TIBXMechanism = class(TDatabaseMechanism)
  private
    FTransactionCount: Integer;
    FOwnConnection: boolean;
  protected
    function GetConnection: TObject; override;
    procedure SetConnection(const Value: TObject); override;
    procedure SetConnected(const Value: boolean); override;
	  function CommandClass: TObjectCommandClass; override;
	  function QueryClass: TObjectQueryClass; override;
	  function DriverClass: TDatabaseDriverClass; override;

	  function CreateQuery(const Meta: IObjectMeta; const Criteria: ICriteria): IObjectQuery; override;
	  function CreateCommand(const Meta: IObjectMeta = nil): IObjectCommand; override;
	  function CreateDatabaseDriver: IDatabaseDriver; override;

    function LoadMember(const Member: IMemberType; const AutoLoad: boolean = False): boolean; override;
	  function LoadObject(const AObject: IObjectType; const Meta: IObjectMeta; const MembersProxy: TStrings = nil): boolean; override;
	  function LoadObjectList(const AObjectList: IObjectListType; const Criteria: ICriteria = nil): boolean; override;

    function InTransaction: boolean; override;
    procedure StartTransaction; override;
    procedure CommitTransaction(const ForceCommit: boolean = False);  override;
    procedure RollbackTransaction; override;
  public
    constructor Create(const Session: IInterface); override;
    destructor Destroy; override;
  end;

  TIBXDriver = class(TSQLDriver, ISQLDriver)
  private
    FConnection: TObject;
    function Connection: TObject;
    function GetQuery: TIBQuery;
    function GetTypeForColumn(const ColumnType: string; Precision, Scale: Integer): TItemType;
  protected
    function GetFieldType(const MemberType: TItemType): TFieldType; override;

    function GetTableSchemaList(const TableSchemaList: IMapping): boolean; override;
    function GetFieldSchema(const FieldSchemaList: IObjectMeta): boolean; override;
    function GetIndexesSchema(const IndexesSchema: IIndexMeta): boolean; override;
    function GetForeignKeysSchema(const ForeignKeysSchema: IIndexMeta): boolean; override;
  public
    procedure AfterConstruction; override;
  end;

  TIBXCommand = class(TObjectCommand)
  private
    FIBQuery: TIBQuery;
  protected
    function GetParams: TParams; override;
    function CreateHandler: TObject; override;
    procedure SetStatement(const Value: string); override;
    function ExecuteCommand: boolean; override;
  end;

  TIBXQuery = class(TObjectQuery)
  private
    FIBQuery: TIBQuery;
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
  IBDatabase,
  SysUtils,
  Variants,
  JazzConsts,
  JazzCriteria,
  JazzMapping,
  JazzFirebirdConsts,
  JazzPersisterConsts,
  JazzUtils;

function TIBXMechanism.CommandClass: TObjectCommandClass;
begin
  Result:= TIBXCommand;
end;

procedure TIBXMechanism.CommitTransaction(const ForceCommit: boolean);
begin
  if InTransaction then
  begin
    if ForceCommit then
    begin
      TIBDataBase(Connection).DefaultTransaction.Commit;
      FTransactionCount:= 0
    end
    else
    begin
      if FTransactionCount > 0 then Dec(FTransactionCount);
      if FTransactionCount = 0 then
        TIBDataBase(Connection).DefaultTransaction.Commit;
    end;
  end
  else
    FTransactionCount:= 0
end;

constructor TIBXMechanism.Create(const Session: IInterface);
begin
  FOwnConnection:= False;
  inherited Create(Session);
end;

function TIBXMechanism.CreateQuery(const Meta: IObjectMeta;
  const Criteria: ICriteria): IObjectQuery;
begin
  Result:= QueryClass.Create(Self, Meta, Criteria);
end;

function TIBXMechanism.CreateDatabaseDriver: IDatabaseDriver;
begin
  Result:= DriverClass.Create(Self); 
end;

function TIBXMechanism.CreateCommand(const Meta: IObjectMeta): IObjectCommand;
begin
  Result:= CommandClass.Create(Self, Meta, nil); 
end;

destructor TIBXMechanism.Destroy;
begin
  if (Connection <> nil) and FOwnConnection then
    TIBDataBase(Connection).Free;
  inherited;
end;

function TIBXMechanism.DriverClass: TDatabaseDriverClass;
begin
  Result:= TIBXDriver;
end;

function TIBXMechanism.GetConnection: TObject;
begin
  if (inherited GetConnection = nil) then
  begin
    SetConnection(TIBDatabase.Create(nil));
    TIBDataBase(Connection).DefaultTransaction:= TIBDataBase(Connection).InternalTransaction; 
    FOwnConnection:= True; 
    TIBDataBase(Connection).LoginPrompt:= False;
  end;

  Result:= inherited GetConnection;
end;

function TIBXMechanism.InTransaction: boolean;
begin
  Result:= TIBDataBase(Connection).DefaultTransaction.InTransaction;
end;

function TIBXMechanism.LoadMember(const Member: IMemberType;
  const AutoLoad: boolean): boolean;
begin
  Result:= False;
  StartTransaction;
  try
    Result:= inherited LoadMember(Member, AutoLoad);
    CommitTransaction;
  except
    RollbackTransaction;
  end;
end;

function TIBXMechanism.LoadObject(const AObject: IObjectType;
  const Meta: IObjectMeta; const MembersProxy: TStrings): boolean;
begin
  Result:= False;
  StartTransaction;
  try
    Result:= inherited LoadObject(AObject, Meta, MembersProxy);
    CommitTransaction;
  except
    RollbackTransaction;
  end;
end;

function TIBXMechanism.LoadObjectList(const AObjectList: IObjectListType;
  const Criteria: ICriteria): boolean;
begin
  Result:= False;
  StartTransaction;
  try
    Result:= inherited LoadObjectList(AObjectList, Criteria);
    CommitTransaction;
  except
    RollbackTransaction;
  end;
end;

function TIBXMechanism.QueryClass: TObjectQueryClass;
begin
  Result:= TIBXQuery;
end;

procedure TIBXMechanism.RollbackTransaction;
begin
  if InTransaction then TIBDataBase(Connection).DefaultTransaction.Rollback;
  FTransactionCount:= 0;
end;

procedure TIBXMechanism.StartTransaction;
begin
  if not InTransaction then
    TIBDataBase(Connection).DefaultTransaction.StartTransaction;
  Inc(FTransactionCount);
end;

procedure TIBXMechanism.SetConnected(const Value: boolean);
begin
  if Connection = nil then
    raise Exception.Create(SConnectionNotDefined)
  else
  begin
    TIBDataBase(Connection).Connected:= Value;
    inherited SetConnected(TIBDataBase(Connection).Connected);
  end;
end;

procedure TIBXMechanism.SetConnection(const Value: TObject);
var
  LOldConnection: TIBDatabase;
begin
  LOldConnection:= TIBDatabase(inherited GetConnection);
  if (LOldConnection <> nil) then FreeAndNil(LOldConnection);
  inherited SetConnection(Value);

  if TIBDataBase(Value).DefaultTransaction = nil then
    TIBDataBase(Value).DefaultTransaction:= TIBDataBase(Value).InternalTransaction;
end;

{ TIBXQuery }

function TIBXQuery.CreateHandler: TObject;
begin
  FIBQuery:= TIBQuery.Create(nil);
  FIBQuery.Database:= TIBDatabase((Mechanism as IDatabaseMechanism).Connection);
  Result:= FIBQuery;
end;

function TIBXQuery.GetRecordCount: Integer;
begin
  Result:= FIBQuery.RecordCount;
end;

function TIBXQuery.GetParams: TParams;
begin
  Result:= FIBQuery.Params;
end;

procedure TIBXQuery.Open;
begin
  inherited Open;
  FIBQuery.Open;
end;

procedure TIBXQuery.SetStatement(const Value: string);
begin
  FIBQuery.SQL.Text:= Value;
end;

function TIBXQuery.GetIsEmpty: boolean;
begin
  Result:= FIBQuery.IsEmpty;
end;

{ TIBXCommand }

function TIBXCommand.CreateHandler: TObject;
begin
  FIBQuery:= TIBQuery.Create(nil);
  FIBQuery.Database:= TIBDatabase((Mechanism as IDatabaseMechanism).Connection); 
  Result:= FIBQuery;
end;

function TIBXCommand.ExecuteCommand: boolean;
var
  LRowsAffected: Integer;
begin
  try
    // params.count = 0, then no need to prepare
    if Params.Count > 0 then FIBQuery.Prepare;
    FIBQuery.ExecSQL;
    LRowsAffected := FIBQuery.RowsAffected;
    SetRowsAffected(LRowsAffected);
    Result:= True;
  except
    raise;
    Result:= False;
  end;
end;

function TIBXCommand.GetParams: TParams;
begin
  Result:= FIBQuery.Params;
end;

procedure TIBXCommand.SetStatement(const Value: string);
begin
  FIBQuery.SQL.Text:= Value;
end;

{ TIBXDriver }

procedure TIBXDriver.AfterConstruction;
begin
  inherited;
  SetFirebirdColumnsType(Self);  
end;

function TIBXDriver.GetFieldType(const MemberType: TItemType): TFieldType;
begin
  if MemberType = itTimeStamp then
    Result:= ftDateTime
  else
    Result:= inherited GetFieldType(MemberType);
end;

function TIBXDriver.Connection: TObject;
begin
  if (FConnection = nil) and (Mechanism <> nil) then
    FConnection:= (Mechanism as IDatabaseMechanism).Connection;
  Result:= FConnection;
end;

function TIBXDriver.GetQuery: TIBQuery; 
begin
  Result:= TIBQuery((Mechanism as IDatabaseMechanism).CreateQuery(
    nil, nil).CreateHandler);
end;

function TIBXDriver.GetTypeForColumn(const ColumnType: string; Precision,
  Scale: Integer): TItemType;

  function CompareItem(ColumnText: string): TItemType;
  var
    I: Integer;
    LColumnType: string;
  begin
    Result:= itUnknow;
    for I := Integer(itAutoInc) to Integer(itImage) do
    begin
      LColumnType:= GetColumnTypeFor(TItemType(I));
      if SameText(LColumnType, ColumnText) then
      begin
        Result:= TItemType(I);
        Break;
      end;
    end;
  end;

var
  LColumnWithSize: string;
  LColumnWithScale: string;
begin
  Result:= CompareItem(ColumnType);

  if Result = itUnknow then
  begin
    LColumnWithScale:= ColumnType + Format(SPrecisionScale, [Precision, Scale]);
    Result:= CompareItem(LColumnWithScale);
    if Result = itUnknow then
    begin
      LColumnWithSize:= ColumnType + SColumnSizeDef;
      Result:= CompareItem(LColumnWithSize);
    end;
  end;
end;

function TIBXDriver.GetFieldSchema(const FieldSchemaList: IObjectMeta): boolean;
var
  LQuery: TIBQuery;
  LField: IMemberMeta;
  LName: string;
begin
  LQuery:= GetQuery;
  LQuery.SQL.Text:= Format(SQLFieldsSchema, [FieldSchemaList.TableName]);
  LQuery.Open;

  while not LQuery.EOF do
  begin
    LName:= Trim(LQuery.Fields[ColumnName].AsString); 
    LField:= FieldSchemaList.Add(FormatTypeName(LName, False), LName);
    LField.ColumnType:= GetTypeForColumn(
      Trim(LQuery.Fields[ColumnType].AsString),
      LQuery.Fields[ColumnPrecision].AsInteger,
      LQuery.Fields[ColumnScale].AsInteger);
    LField.Size:= LQuery.Fields[ColumnSize].AsInteger;
    LField.Precision:= LQuery.Fields[ColumnPrecision].AsInteger;
    LField.IsOID:= LQuery.Fields[ColumnPrimaryKey].AsBoolean;
    LField.Required:= LQuery.Fields[ColumnNotNull].AsBoolean;
    LQuery.Next;
  end;

  Result:= FieldSchemaList.Count > 0;
end;

function TIBXDriver.GetForeignKeysSchema(const ForeignKeysSchema: IIndexMeta):
  boolean;
begin
  // TODO: Populate Mapping = JazzFirebirdConsts.SQLForeignKeys
  Result:= False;
end;

function TIBXDriver.GetIndexesSchema(const IndexesSchema: IIndexMeta): boolean;
begin
  // TODO: Populate Mapping = JazzFirebirdConsts.SQLIndexesSchema
  Result:= False;
end;

function TIBXDriver.GetTableSchemaList(const TableSchemaList: IMapping): boolean;
var
  I: Integer;
  LTables: TStrings;
  LObject: IObjectMeta;
begin
  LTables:= TStringList.Create;
  try
    TIBDataBase(Connection).GetTableNames(LTables);
    for I := 0 to LTables.Count - 1 do
    begin
      LObject:= TableSchemaList.AddObject(FormatTypeName(LTables[I]), LTables[I]);
      GetFieldSchema(LObject);
    end;
      
    Result:= TableSchemaList.Count > 0;
  finally
    LTables.Free;
  end;
end;

end.


