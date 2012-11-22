unit JazzADOMechanism;

interface

uses
  ADODB_TLB,
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
  IADOMechanism = interface(IDatabaseMechanism)
    ['{E2C86AD8-22CF-4878-9864-C4C8337B7CCA}']
    function GetADOConnection: Connection;
    procedure SetConnectionString(const ConnectionString, UserName, Password: string);
  end;

  TADOMechanism = class(TDatabaseMechanism, IADOMechanism)
  private
    FConnectionString: string;
    FUserName: string;
    FPassword: string;
    FInternalConnection: Connection;
    FTransactionCount: Integer;
  protected
    function GetADOConnection: Connection;
    procedure SetConnected(const Value: boolean); override;

    function CommandClass: TObjectCommandClass; override;
    function QueryClass: TObjectQueryClass; override;
    function DriverClass: TDatabaseDriverClass; override;

    function CreateQuery(const Meta: IObjectMeta; const Criteria: ICriteria): IObjectQuery; override;
    function CreateCommand(const Meta: IObjectMeta = nil): IObjectCommand; override;
    function CreateDatabaseDriver: IDatabaseDriver; override;

    function InTransaction: boolean; override;
    procedure StartTransaction; override;
    procedure CommitTransaction(const ForceCommit: boolean = False); override;
    procedure RollbackTransaction; override;

    procedure SetConnectionString(const ConnectionString, UserName, Password: string);
  public
    destructor Destroy; override;
  end;

  TADODriver = class(TSQLDriver, ISQLDriver)
  protected
    function GetDatabaseSchema: IMapping; override;
  public
    constructor Create(const Mechanism: IInterface); override;
  end;

  TADOObjectCommand = class(TObjectCommand)
  private
    FParams: TParams;
    FCommand: Command;
    function GetADOConnection: Connection;
  protected
    function GetParams: TParams; override;

    function CreateHandler: TObject; override;
    function ExecuteCommand: boolean; override;
    procedure SetStatement(const Value: string); override;
  public
    destructor Destroy; override;
  end;

  TADOObjectQuery = class(TObjectQuery)
  private
    FParams: TParams;
    FQuery: Command;
    FRecordSet: RecordSet;
    function GetADOConnection: Connection;
    function FindADOField(const FieldName: string): Field;
  protected
    function CreateHandler: TObject; override;
    function GetFieldCount: Integer; override;
    function GetFieldValue(const FieldName: string): Variant; override;
    function GetIsEmpty: boolean; override;
    function GetParams: TParams; override;
    function GetRecordCount: Integer; override;
    procedure FieldIndexToMember(const AObject: IObjectType; const FieldIndex: Integer); override;
    procedure FieldToMember(const AObject: IObjectType; const MemberMeta: IMemberMeta); override;
    procedure SetStatement(const Value: string); override;
    procedure RowToObject(const AObject: IObjectType); override;
    function EOF: boolean; override;
    procedure First; override;
    procedure Next; override;
    procedure Open; override;
    procedure Close; override;
  public
    destructor Destroy; override;
  end;

var
  EmptyParam: OleVariant;

implementation

uses
  ActiveX,
  Math,
  SqlTimSt,
  SysUtils,
  Variants,
  Windows,
  JazzConsts,
  JazzIntfUtils,
  JazzMechanism,
  JazzPersister,
  JazzPersisterConsts,
  JazzSessionIntf,
  JazzTypes;

resourcestring
  SColumnNotFoundInRecordSet = 'Column %s not found in RecordSet';

procedure ClearADOParams(const Command: Command);
begin
  while Command.Parameters.Count > 0 do
    Command.Parameters.Delete(Command.Parameters.Count - 1);
end;

function GetADODataSize(const DataType: TFieldType; const Value: Variant): Integer;
begin
  if VarIsClear(Value) or VarIsNull(Value) then
    Result := 0
  else
    case DataType of
      ftBlob: Result := Value.Size;
      ftBoolean: Result := SizeOf(Integer);
      ftCurrency: Result := SizeOf(Currency);
      ftDate: Result := SizeOf(TDateTime);
      ftDateTime: Result := SizeOf(TDateTime);
      ftFloat: Result := SizeOf(Double);
      ftInteger: Result := SizeOf(Integer);
      ftMemo: Result := Length(Value.Text);
      ftSmallInt: Result := SizeOf(SmallInt);
      ftString: Result := Max(1, Length(Value));
      ftTime: Result := SizeOf(TDateTime);
      ftTimeStamp: Result := SizeOf(TSQLTimeStamp);
      ftWideString: Result := SizeOf(WideChar);
    else
      Result := 0;
    end;
end;

function GetADODataType(const ParamType: TFieldType): Integer;
begin
  case ParamType of
    ftBlob: Result := adLongVarBinary;
    ftBoolean: Result := adBoolean;
    ftCurrency: Result := adCurrency;
    ftDate: Result := adDate;
    ftDateTime: Result := adDate;
    ftFloat: Result := adDouble;
    ftInteger: Result := adInteger;
    ftMemo: Result := adVarChar;
    ftSmallInt: Result := adSmallInt;
    ftString: Result := adVarChar;
    ftTime: Result := adDate;
    ftTimeStamp: Result := adDate;
    ftWideString: Result := adWChar;
  else
    Result := 0;
  end;
end;

procedure SetADOParams(Command: Command; Params: TParams);
var
  I: Integer;
  LValue: OleVariant;
  LParam: Parameter;
begin
  ClearADOParams(Command);
  for I := 0 to Params.Count - 1 do
  begin
    LValue := Params[I].Value;

    LParam := Command.CreateParameter(
      Params[I].Name,
      GetADODataType(Params[I].DataType),
      adParamInput,
      GetADODataSize(Params[I].DataType, Params[I].Value),
      LValue);
    Command.Parameters.Append(LParam);
  end;
end;

{ TADOMechanism }

function TADOMechanism.CommandClass: TObjectCommandClass;
begin
  Result := TADOObjectCommand;
end;

procedure TADOMechanism.CommitTransaction(const ForceCommit: boolean);
begin
  if InTransaction then
  begin
    if ForceCommit then
    begin
      GetADOConnection.CommitTrans;
      FTransactionCount := 0
    end
    else
    begin
      if FTransactionCount > 0 then Dec(FTransactionCount);
      if FTransactionCount = 0 then GetADOConnection.CommitTrans;
    end;
  end
  else
    FTransactionCount := 0
end;

function TADOMechanism.CreateQuery(const Meta: IObjectMeta;
  const Criteria: ICriteria): IObjectQuery;
begin
  Result := QueryClass.Create(Self, Meta, Criteria);
end;

function TADOMechanism.CreateDatabaseDriver: IDatabaseDriver;
begin
  Result := DriverClass.Create(Self);
end;

function TADOMechanism.CreateCommand(const Meta: IObjectMeta): IObjectCommand;
begin
  Result := CommandClass.Create(Self, Meta, nil);
end;

destructor TADOMechanism.Destroy;
begin
  if FInternalConnection <> nil then FInternalConnection := nil;
  inherited;
end;

function TADOMechanism.DriverClass: TDatabaseDriverClass;
begin
  Result := TADODriver;
end;

function TADOMechanism.InTransaction: boolean;
begin
  Result := (GetADOConnection <> nil) and (FTransactionCount > 0)
end;

function TADOMechanism.QueryClass: TObjectQueryClass;
begin
  Result := TADOObjectQuery;
end;

procedure TADOMechanism.RollbackTransaction;
begin
  if InTransaction then GetADOConnection.RollbackTrans;
  FTransactionCount := 0;
end;

procedure TADOMechanism.StartTransaction;
begin
  if not InTransaction then
    GetADOConnection.BeginTrans;
  Inc(FTransactionCount);
end;

procedure TADOMechanism.SetConnected(const Value: boolean);
begin
  if Value then
  begin
    if (GetADOConnection.State = 0) then
      GetADOConnection.Open(FConnectionString, FUserName, FPassword, 0)
  end
  else if (GetADOConnection.State = 1) then
    GetADOConnection.Close;
  inherited SetConnected(Value);
end;

procedure TADOMechanism.SetConnectionString(const ConnectionString,
  UserName, Password: string);
begin
  FConnectionString := ConnectionString;
  FUserName := UserName;
  FPassword := Password;
end;

function TADOMechanism.GetADOConnection: Connection;
begin
  if FInternalConnection = nil then
  begin
    FInternalConnection := CoConnection.Create;
    SetConnection(Pointer(FInternalConnection));
  end;

  Result := FInternalConnection;
end;

{ TADOObjectQuery }

function TADOObjectQuery.CreateHandler: TObject;
begin
  FQuery := CoCommand.Create;
  FQuery.Set_ActiveConnection(GetADOConnection);
  Result := Pointer(FQuery);
  OwnHandler := False;
end;

function TADOObjectQuery.GetRecordCount: Integer;
begin
  Result := FRecordSet.RecordCount;
end;

function TADOObjectQuery.GetParams: TParams;
begin
  if FParams = nil then FParams := TParams.Create;
  Result := FParams;
end;

procedure TADOObjectQuery.Open;
var
  LRowsAffected: OleVariant;
begin
  inherited;
  SetADOParams(FQuery, Params);
  LRowsAffected := 0;
  FRecordSet := FQuery.Execute(LRowsAffected, EmptyParam, 0);
end;

procedure TADOObjectQuery.RowToObject(const AObject: IObjectType);
var
  I: Integer;
begin
  AObject.Loading := True;
  for I := 0 to FFieldList.Count - 1 do
    LoadMemberValue(AObject.Member[FFieldList[I]], FRecordSet.Fields[I].Value);
  (AObject as IObjectState).Loaded(Mechanism.Session);
end;

procedure TADOObjectQuery.SetStatement(const Value: string);
begin
  FQuery.CommandText := Value;
  FQuery.CommandType := adCmdText;
end;

function TADOObjectQuery.GetIsEmpty: boolean;
begin
  Result := (FRecordSet <> nil) and FRecordSet.BOF and FRecordSet.EOF;
end;

destructor TADOObjectQuery.Destroy;
begin
  if FParams <> nil then FreeAndNil(FParams);
  inherited;
end;

function TADOObjectQuery.GetADOConnection: Connection;
begin
  Result := (Mechanism as IADOMechanism).GetADOConnection;
end;

procedure TADOObjectQuery.Close;
begin
  FQuery.Cancel; // maybe not necessary, but do not call inherited.
end;

function TADOObjectQuery.EOF: boolean;
begin
  Result := (FRecordSet <> nil) and FRecordSet.EOF;
end;

procedure TADOObjectQuery.First;
begin
  if FRecordSet <> nil then FRecordSet.MoveFirst;
end;

procedure TADOObjectQuery.Next;
begin
  if FRecordSet <> nil then FRecordSet.MoveNext;
end;

function TADOObjectQuery.GetFieldValue(const FieldName: string): Variant;
var
  LField: Field;
begin
  LField := FindADOField(FieldName);
  if LField <> nil then
    Result := LField.Value
  else
    raise Exception.CreateFmt(SColumnNotFoundInRecordSet, [FieldName]);
end;

procedure TADOObjectQuery.FieldIndexToMember(const AObject: IObjectType;
  const FieldIndex: Integer);
var
  LField: Field;
  LMemberMeta: IMemberMeta;
begin
  LField := FRecordSet.Fields[FieldIndex];
  LMemberMeta := Meta.FindMember(LField.Name, True, True);
  LoadMemberValue(AObject.Member[LMemberMeta.MemberName], LField.Value);
end;

procedure TADOObjectQuery.FieldToMember(const AObject: IObjectType;
  const MemberMeta: IMemberMeta);
var
  LField: Field;
begin
  LField := FindADOField(MemberMeta.ColumnName);
  if LField <> nil then
    LoadMemberValue(AObject.Member[MemberMeta.MemberName], LField.Value)
  else if not MemberMeta.LazyLoad then
    raise Exception.CreateFmt(SColumnNotMapped, [MemberMeta.ColumnName])
end;

function TADOObjectQuery.GetFieldCount: Integer;
begin
  Result := FRecordSet.Fields.Count;
end;

function TADOObjectQuery.FindADOField(const FieldName: string): Field;
var
  I: Integer;
begin
  for I := 0 to FRecordSet.Fields.Count - 1 do
  begin
    Result := FRecordSet.Fields[I];
    if AnsiCompareText(Result.Name, FieldName) = 0 then Exit;
  end;
  Result := nil;
end;

{ TADOObjectCommand }

function TADOObjectCommand.CreateHandler: TObject;
begin
  FCommand := CoCommand.Create;
  FCommand.Set_ActiveConnection(GetADOConnection);
  Result := Pointer(FCommand);
  OwnHandler := False;
end;

destructor TADOObjectCommand.Destroy;
begin
  if FParams <> nil then FreeAndNil(FParams);
  inherited;
end;

function TADOObjectCommand.ExecuteCommand: boolean;
var
  LRowsAffected: OleVariant;
begin
  try
    SetADOParams(FCommand, Params);
    LRowsAffected := 0;
    FCommand.Execute(LRowsAffected, EmptyParam, 0);
    SetRowsAffected(LRowsAffected);
    Result := LRowsAffected > 0;
  except
    raise;
    Result := False;
  end;
end;

function TADOObjectCommand.GetADOConnection: Connection;
begin
  Result := (Mechanism as IADOMechanism).GetADOConnection;
end;

function TADOObjectCommand.GetParams: TParams;
begin
  if FParams = nil then FParams := TParams.Create;
  Result := FParams;
end;

procedure TADOObjectCommand.SetStatement(const Value: string);
begin
  FCommand.CommandText := Value;
  FCommand.CommandType := adCmdText;
end;

constructor TADODriver.Create(const Mechanism: IInterface);
begin
  inherited;
  ColumnTypeForAutoInc := 'AUTOINC'; // do not localize
  ColumnTypeForBlob := 'BINARY'; // do not localize
  ColumnTypeForBoolean := 'BIT'; // do not localize
  ColumnTypeForChar := 'CHAR(%d)'; // do not localize
  ColumnTypeForCurrency := 'MONEY'; // do not localize
  ColumnTypeForDate := 'DATETIME'; // do not localize
  ColumnTypeForDateTime := 'DATETIME'; // do not localize
  ColumnTypeForFloat := 'FLOAT'; // do not localize
  ColumnTypeForInteger := 'INT'; // do not localize
  ColumnTypeForLongInt := 'INT'; // do not localize
  ColumnTypeForMemo := 'TEXT'; // do not localize
  ColumnTypeForSmallInt := 'SMALLINT'; // do not localize
  ColumnTypeForString := 'VARCHAR(%d)'; // do not localize
  ColumnTypeForTime := 'DATETIME'; // do not localize
  ColumnTypeForTimeStamp := 'DATETIME'; // do not localize
  ColumnTypeForWideString := 'NVARCHAR(%d)'; // do not localize
end;

function TADODriver.GetDatabaseSchema: IMapping;
begin
  // TODO: Get Database Schema for ADO
end;

initialization
  CoInitialize(nil);

finalization
  CoUninitialize;

end.

