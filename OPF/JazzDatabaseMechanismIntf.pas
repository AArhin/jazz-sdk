unit JazzDatabaseMechanismIntf;

interface

uses
  Classes,
  DB,
  JazzConsts,
  JazzCriteriaIntf,
  JazzDataSet,
  JazzDataSetIntf,
  JazzMappingIntf,
  JazzMechanismIntf,
  JazzPersisterIntf,
  JazzTypes,
  JazzValueTypeIntf;

type
  IDatabaseMechanism = interface;
  INotifySession = interface;

  TDatabaseDriverClass = class of TDatabaseDriver;
  IDatabaseDriver = interface(IInterface)
    ['{211785D5-0A59-4EE6-9042-0EF69A9931D8}']
    function GetBeginQuote: string;
    function GetEndQuote: string;
    function GetQuoteIdentifiers: boolean;
    function GetWildChar: string;
    procedure SetBeginQuote(const Value: string);
    procedure SetEndQuote(const Value: string);
    procedure SetQuoteIdentifiers(const Value: boolean);
    procedure SetWildChar(const Value: string);

    function GetFieldType(const MemberType: TItemType): TFieldType; 
    function GetColumnTypeFor(const Index: TItemType): string;
    procedure SetColumnTypeFor(const Index: TItemType; const Value: string);

    function FormatIdentifier(const Indentifier: string): string; 
    function GetColumnName(const Member: IMemberMeta; const FullName: boolean = True; const Format: boolean = True): string; overload;
    function GetColumnName(const Meta: IObjectMeta; const MemberName: string; const FullName: boolean = True; const Format: boolean = True): string; overload;
    function GetTableName(const Meta: IObjectMeta; const Format: boolean = True): string;

    function GetDatabaseSchema: IMapping; 
	  procedure GenerateSchema(const Schema: TStrings; const OnlyDiff: boolean = False); 

    function GetTableSchemaList(const TableSchemaList: IMapping): boolean;
    function GetFieldSchema(const FieldSchemaList: IObjectMeta): boolean;
    function GetPrimaryKeysSchema(const PrimaryKeysSchema: IIndexMeta): boolean;
    function GetForeignKeysSchema(const ForeignKeysSchema: IIndexMeta): boolean;
    function GetIndexesSchema(const IndexesSchema: IIndexMeta): boolean;

    function CreateTable(const TableSchema: IObjectMeta): string;
    function DropTable(const TableSchema: IObjectMeta): string;
    function AddField(const FieldSchema: IMemberMeta; const Prefix: string = EmptyStr): string;
    function DropField(const FieldSchema: IMemberMeta; const Prefix: string = EmptyStr): string;
    function CopyFromTmpField(const FieldSchema: IMemberMeta; const Prefix: string): string;
    function CopyToTmpField(const FieldSchema: IMemberMeta; const Prefix: string): string;
    function AddPrimaryKey(const TableSchema: IObjectMeta): string;
    function DropPrimaryKey(const TableSchema: IObjectMeta): string;
    function AddForeignKey(const RelationshipSchema: IRelationshipMeta): string;
    function DropForeignKey(const RelationshipSchema: IRelationshipMeta): string;
    function AddIndex(const IndexesSchema: IIndexMeta): string;
    function DropIndex(const IndexesSchema: IIndexMeta): string;

    function InsertStatement(const AObject: IObjectType; const Meta: IObjectMeta): string;
    function DeleteStatement(const AObject: IObjectType; const Meta: IObjectMeta): string;
    function UpdateStatement(const AObject: IObjectType; const Meta: IObjectMeta): string;
    function SelectStatement(const DataSet: IObjectQuery; const Meta: IObjectMeta; FieldList: TStrings = nil): string;
    function SelectCountStatement(const Query: IObjectQuery; const Meta: IObjectMeta): string;

    property QuoteIdentifiers: boolean read GetQuoteIdentifiers write SetQuoteIdentifiers;
    property BeginQuote: string read GetBeginQuote write SetBeginQuote;
    property EndQuote: string read GetEndQuote write SetEndQuote;
    property WildChar: string read GetWildChar write SetWildChar;
  end;
  
  TExecuteStatementEvent = procedure(const Mechanism: IDatabaseMechanism; const Command: string; const Params: TParams = nil) of object;

  IDatabaseMechanism = interface(IMechanism)
    ['{C3E2D9BB-4B76-49FF-9EC3-942AC56C2C49}']
    function GetConnection: TObject;
    function GetDatabaseDriver: IDatabaseDriver;
    function GetOnExecuteStateMent: TExecuteStatementEvent;
    function GetNotifySession: INotifySession;
    procedure SetConnection(const Value: TObject);
    procedure SetOnExecuteStatement(const Value: TExecuteStatementEvent);
    procedure SetNotifySession(const Value: INotifySession);

	  function CommandClass: TObjectCommandClass;
	  function QueryClass: TObjectQueryClass;
	  function DriverClass: TDatabaseDriverClass;

	  function CreateQuery(const Meta: IObjectMeta; const Criteria: ICriteria): IObjectQuery;
	  function CreateCommand(const Meta: IObjectMeta = nil): IObjectCommand;
	  function CreateDatabaseDriver: IDatabaseDriver;

    procedure DoExecuteStatement(var Command: string; const Params: TParams);
	  procedure GenerateSchema(const Schema: TStrings; const OnlyDiff: boolean = False);

    function LoadMember(const Member: IMemberType; const AutoLoad: boolean = False): boolean;
	  function LoadObject(const AObject: IObjectType; const Meta: IObjectMeta; const MembersProxy: TStrings = nil): boolean;
	  function LoadObjectList(const AObjectList: IObjectListType; const Criteria: ICriteria = nil): boolean;

    procedure InsertObject(const AObject: IObjectType; const Meta: IObjectMeta; const Command: IObjectCommand = nil);
	  procedure DeleteObject(const AObject: IObjectType; const Meta: IObjectMeta; const Command: IObjectCommand = nil);
	  procedure UpdateObject(const AObject: IObjectType; const Meta: IObjectMeta; const Command: IObjectCommand = nil);

	  procedure DeleteObjectList(const  AObjectList: IObjectListType);

    procedure Save(const AObject: IMemberType);
	  procedure SaveObject(const AObject: IObjectType; const Meta: IObjectMeta = nil); overload;
	  procedure SaveObject(const AObject: IObjectType; const Meta: IObjectMeta; const Command: IObjectCommand); overload;
	  procedure SaveObjectList(const  AObjectList: IObjectListType);

    function InTransaction: boolean;
    procedure StartTransaction;
    procedure CommitTransaction(const ForceCommit: boolean = False);
    procedure RollbackTransaction;

	  property Connection: TObject read GetConnection write SetConnection;
    property DatabaseDriver: IDatabaseDriver read GetDatabaseDriver;
    property NotifySession: INotifySession read GetNotifySession write SetNotifySession;
    property OnExecuteStatement: TExecuteStatementEvent read GetOnExecuteStateMent write SetOnExecuteStatement;
  end;

  TSessionOperation = (
   soDeleted,
   soPersisted
  );

  INotifySessionItem = interface(IInterface)
    ['{C1BCB8A2-0A0D-41B0-B2AE-29E9C7C6660E}']
    function GetInstance: IMemberType;
    function GetOperation: TSessionOperation;
    procedure SetInstance(const Value: IMemberType);
    procedure SetOperation(const Value: TSessionOperation);
    property Instance: IMemberType read GetInstance write SetInstance;
    property Operation: TSessionOperation read GetOperation write SetOperation;
  end;
  
  INotifySession = interface(IInterface)
    ['{2F875DDB-BA91-44C9-A5F0-052973CF8805}']
    function AddItem(const Instance: IMemberType; const Operation: TSessionOperation): INotifySessionItem;
    procedure NotifyAll;
    procedure ClearItems;
  end;

  TDatabaseDriver = class(TInterfacedObject, IDatabaseDriver)
  private
    FColumnType: array[TItemType] of string;
    FBeginQuote: string;
    FEndQuote: string;
    FQuoteIndentifiers: boolean;
    FWildChar: string;
    FMechanism: Pointer;
    function GetMechanism: IInterface;
  protected
    function GetBeginQuote: string;
    function GetColumnTypeFor(const Index: TItemType): string;
    function GetFieldType(const MemberType: TItemType): TFieldType; virtual;
    function GetEndQuote: string;
    function GetQuoteIdentifiers: boolean;
    function GetWildChar: string;
    procedure SetBeginQuote(const Value: string);
    procedure SetColumnTypeFor(const Index: TItemType; const Value: string);
    procedure SetEndQuote(const Value: string);
    procedure SetQuoteIdentifiers(const Value: boolean);
    procedure SetWildChar(const Value: string);

    function FormatIdentifier(const Indentifier: string): string; virtual;
    function GetColumnName(const Meta: IObjectMeta; const MemberName: string; const FullName: boolean = True; const Format: boolean = True): string; overload;
    function GetColumnName(const Member: IMemberMeta; const FullName: boolean = True; const Format: boolean = True): string; overload;
    function GetTableName(const Meta: IObjectMeta; const Format: boolean = True): string; virtual;

    function GetDatabaseSchema: IMapping; virtual;
	  procedure GenerateSchema(const Schema: TStrings; const OnlyDiff: boolean = False); virtual;

    function GetTableSchemaList(const TableSchemaList: IMapping): boolean; virtual;
    function GetFieldSchema(const FieldSchemaList: IObjectMeta): boolean; virtual;
    function GetPrimaryKeysSchema(const PrimaryKeysSchema: IIndexMeta): boolean; virtual;
    function GetForeignKeysSchema(const ForeignKeysSchema: IIndexMeta): boolean; virtual;
    function GetIndexesSchema(const IndexesSchema: IIndexMeta): boolean; virtual;

    function CreateTable(const TableSchema: IObjectMeta): string; virtual; abstract;
    function DropTable(const TableSchema: IObjectMeta): string; virtual; abstract;

    function AddField(const FieldSchema: IMemberMeta; const Prefix: string = EmptyStr): string; virtual; abstract;
    function DropField(const FieldSchema: IMemberMeta; const Prefix: string = EmptyStr): string; virtual; abstract;
    function CopyFromTmpField(const FieldSchema: IMemberMeta; const Prefix: string): string; virtual; abstract;
    function CopyToTmpField(const FieldSchema: IMemberMeta; const Prefix: string): string; virtual; abstract;

    function AddPrimaryKey(const TableSchema: IObjectMeta): string; virtual; abstract;
    function DropPrimaryKey(const TableSchema: IObjectMeta): string; virtual; abstract;

    function AddForeignKey(const RelationshipSchema: IRelationshipMeta): string; virtual; abstract;
    function DropForeignKey(const RelationshipSchema: IRelationshipMeta): string; virtual; abstract;

    function AddIndex(const IndexesSchema: IIndexMeta): string; virtual; abstract;
    function DropIndex(const IndexesSchema: IIndexMeta): string; virtual; abstract;

    function InsertStatement(const AObject: IObjectType; const Meta: IObjectMeta): string; virtual; abstract;
    function DeleteStatement(const AObject: IObjectType; const Meta: IObjectMeta): string; virtual; abstract;
    function UpdateStatement(const AObject: IObjectType; const Meta: IObjectMeta): string; virtual; abstract;
    function SelectStatement(const Query: IObjectQuery; const Meta: IObjectMeta; FieldList: TStrings = nil): string; virtual; abstract;
    function SelectCountStatement(const Query: IObjectQuery; const Meta: IObjectMeta): string; virtual; abstract;

    property QuoteIdentifiers: boolean read GetQuoteIdentifiers write SetQuoteIdentifiers;
    property BeginQuote: string read GetBeginQuote write SetBeginQuote;
    property EndQuote: string read GetEndQuote write SetEndQuote;
    property WildChar: string read GetWildChar write SetWildChar;

    property ColumnTypeForAutoInc: string index itAutoInc read GetColumnTypeFor write SetColumnTypeFor;
    property ColumnTypeForBlob: string index itBlob read GetColumnTypeFor write SetColumnTypeFor;
    property ColumnTypeForBoolean: string index itBoolean read GetColumnTypeFor write SetColumnTypeFor;
    property ColumnTypeForChar: string index itChar read GetColumnTypeFor write SetColumnTypeFor;
    property ColumnTypeForCurrency: string index itCurrency read GetColumnTypeFor write SetColumnTypeFor;
    property ColumnTypeForDate: string index itDate read GetColumnTypeFor write SetColumnTypeFor;
    property ColumnTypeForDateTime: string index itDateTime read GetColumnTypeFor write SetColumnTypeFor;
    property ColumnTypeForFloat: string index itFloat read GetColumnTypeFor write SetColumnTypeFor;
    property ColumnTypeForInteger: string index itInteger read GetColumnTypeFor write SetColumnTypeFor;
    property ColumnTypeForLongInt: string index itLongInt read GetColumnTypeFor write SetColumnTypeFor;
    property ColumnTypeForMemo: string index itMemo read GetColumnTypeFor write SetColumnTypeFor;
    property ColumnTypeForSmallInt: string index itSmallInt read GetColumnTypeFor write SetColumnTypeFor;
    property ColumnTypeForString: string index itString read GetColumnTypeFor write SetColumnTypeFor;
    property ColumnTypeForTime: string index itTime read GetColumnTypeFor write SetColumnTypeFor;
    property ColumnTypeForTimeStamp: string index itTimeStamp read GetColumnTypeFor write SetColumnTypeFor;
    property ColumnTypeForWideString: string index itWideString read GetColumnTypeFor write SetColumnTypeFor;
    property Mechanism: IInterface read GetMechanism;
  public
    constructor Create(const Mechanism: IInterface); reintroduce; virtual;
  end;

implementation

uses
  SysUtils,
  Variants,
  JazzMapping,
  JazzPersisterConsts,
  JazzSQLEngine,
  JazzUtils;

{ TDatabaseDriver }

constructor TDatabaseDriver.Create(const Mechanism: IInterface);
begin
  inherited Create;
  FMechanism:= Pointer(Mechanism);
  FBeginQuote:= '"';
  FEndQuote:= '"';
  FQuoteIndentifiers:= False;
  FWildChar:= '%';
end;


function TDatabaseDriver.FormatIdentifier(const Indentifier: string): string;
begin
  if QuoteIdentifiers then
    Result:= BeginQuote + Indentifier + EndQuote
  else
    Result:= Indentifier;
end;

procedure TDatabaseDriver.GenerateSchema(const Schema: TStrings; const OnlyDiff: boolean);
var
  I: Integer;
  LObjectMeta: IObjectMeta;
  LMapping: IMapping;
  LGenerated: TStrings;
begin
  if OnlyDiff then
  begin
    LMapping:= GetDatabaseSchema;
    // TODO: Compare DatabaseSchema with Mapping
  end
  else
    LMapping:= (Mechanism as IDatabaseMechanism).Mapping;

  if not LMapping.Loaded then LMapping.LoadMapping;
  LGenerated:= TStringList.Create;
  try
    for I:= 0 to LMapping.Count -1 do
    begin
      LObjectMeta:= LMapping.Items[I];
      if LGenerated.IndexOf(LObjectMeta.TableName) = NotFound then
      begin
        LGenerated.Add(LObjectMeta.TableName);
        Schema.Add(CreateTable(LObjectMeta));
        Schema.Add(AddPrimaryKey(LObjectMeta));
      end;
    end;
  finally
    LGenerated.Free;
  end;
end;

function TDatabaseDriver.GetBeginQuote: string;
begin
  Result:= FBeginQuote;
end;

function TDatabaseDriver.GetColumnTypeFor(const Index: TItemType): string;
begin
  Result:= FColumnType[Index];
end;

function TDatabaseDriver.GetDatabaseSchema: IMapping;
{
var
  I: Integer;
}
begin
  // TODO: Implement GetDatabaseSchema
  Result:= TMapping.Create(nil, True);
  
  if GetTableSchemaList(Result) then
  begin
  {
    for I:= 0 to TableSchemaList.Count - 1 do
      if GetSchemaInfoFields(TableSchemaList[I].Fields) then
      begin
        GetSchemaInfoPrimaryKeys(TableSchemaList[I].PrimaryKeys);
        GetSchemaInfoIndexes(TableSchemaList[I].Indexes);
        GetSchemaInfoForeignKeys(TableSchemaList[I].ForeignKeys);
      end;
  }
  end;
end;

function TDatabaseDriver.GetEndQuote: string;
begin
  Result:= FEndQuote;
end;

function TDatabaseDriver.GetFieldSchema(const FieldSchemaList: IObjectMeta): boolean;
begin
  Result:= False;
end;

function TDatabaseDriver.GetForeignKeysSchema(const ForeignKeysSchema: IIndexMeta): boolean;
begin
  Result:= False;
end;

function TDatabaseDriver.GetIndexesSchema(const IndexesSchema: IIndexMeta): boolean;
begin
  Result:= False;
end;

function TDatabaseDriver.GetMechanism: IInterface;
begin
  Result:= IInterface(FMechanism);
end;

function TDatabaseDriver.GetPrimaryKeysSchema(const PrimaryKeysSchema: IIndexMeta): boolean;
begin
  Result:= False;
end;

function TDatabaseDriver.GetQuoteIdentifiers: boolean;
begin
  Result:= FQuoteIndentifiers;
end;

function TDatabaseDriver.GetTableSchemaList(const TableSchemaList: IMapping): boolean;
begin
  Result:= False;
end;

function TDatabaseDriver.GetWildChar: string;
begin
  Result:= FWildChar;
end;

procedure TDatabaseDriver.SetBeginQuote(const Value: string);
begin
  FBeginQuote:= Value;
end;

procedure TDatabaseDriver.SetColumnTypeFor(const Index: TItemType;
  const Value: string);
begin
  FColumnType[Index]:= Value;
end;

procedure TDatabaseDriver.SetEndQuote(const Value: string);
begin
  FEndQuote:= Value;
end;

procedure TDatabaseDriver.SetQuoteIdentifiers(const Value: boolean);
begin
  FQuoteIndentifiers:= Value;
end;

procedure TDatabaseDriver.SetWildChar(const Value: string);
begin
  FWildChar:= Value;
end;

function TDatabaseDriver.GetColumnName(const Meta: IObjectMeta; const MemberName: string; const
  FullName: boolean; const Format: boolean): string;
var
  LMember: IMemberMeta;
begin
  LMember:= Meta.FindMember(MemberName, True);
  Result:= GetColumnName(LMember, FullName, Format);
end;

function TDatabaseDriver.GetColumnName(const Member: IMemberMeta; const
  FullName, Format: boolean): string;
var
  LTableName: string;
begin
  if Member <> nil then
  begin
    if FullName and (Member.Owner <> nil) then
      LTableName:= GetTableName(Member.Owner as IObjectMeta, Format) + '.'
    else
      LTableName:= EmptyStr;

    if Format then
      Result:= LTableName + FormatIdentifier(Member.ColumnName)
    else
      Result:= LTableName + Member.ColumnName;
  end;
end;

function TDatabaseDriver.GetTableName(const Meta: IObjectMeta; const Format: boolean): string;
begin
  if Format then
    Result:= FormatIdentifier(Meta.TableName)
  else
    Result:= Meta.TableName;
end;

function TDatabaseDriver.GetFieldType(const MemberType: TItemType): TFieldType;
begin
  case MemberType of
    itAutoInc: Result:= ftAutoInc;
    itBlob: Result:= ftBlob;
    itBoolean: Result:= ftBoolean;
    itChar: Result:= ftString;
    itCurrency: Result:= ftCurrency;
    itDate: Result:= ftDate;
    itDateTime: Result:= ftDateTime;
    itFloat: Result:= ftFloat;
    itImage: Result:= ftGraphic;
    itInteger: Result:= ftInteger;
    itLongInt: Result:= ftLargeint;
    itMemo: Result:= ftMemo;
    itSmallInt: Result:= ftSmallint;
    itString: Result:= ftString;
    itTime: Result:= ftTime;
    itTimeStamp: Result:= ftTimeStamp;
    itWideString: Result:= ftWideString;
  else
    Result:= ftUnknown;
  end;
end;


end.






