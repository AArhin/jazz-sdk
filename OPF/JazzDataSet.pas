unit JazzDataSet;

interface

uses
  Classes,
  DB,
  JazzDataSetIntf,
  JazzMappingIntf,
  JazzValueTypeIntf,
  JazzCriteriaIntf,
  JazzConsts,
  JazzPersisterIntf,
  JazzMechanismIntf;

type
  TObjectDataSet = class(TInterfacedObject, IObjectDataSet)
  private
    FCriteria: Pointer;
    FHandler: TObject;
    FMeta: Pointer;
    FMechanism: Pointer;
    FOwnHandler: Boolean;
    function InternalCreateParam(const MemberMeta: IMemberMeta; ParamName: string = EmptyStr): TParam;
    procedure CreateParamsFromCriteria;
    procedure CreateParamsFromObject(const ObjectParams: IObjectType; const CommandType: TCommandType);
    procedure CreateParamAndSetValue(const MemberMeta: IMemberMeta; const Member: IMemberType; const ParamName: string = JazzConsts.EmptyStr);
    procedure CreateParamsFromID(const ObjectParams: IObjectType; const ParamName: string = JazzConsts.EmptyStr);
    procedure CreateParamsForUpdate(const ObjectParams: IObjectType);
  protected
    function GetCriteria: ICriteria;
    function GetHandler: TObject;
    function GetMechanism: IMechanism;
    function GetMeta: IObjectMeta;
    function GetOwnHandler: Boolean;
    function GetParams: TParams; virtual; abstract;
    procedure SetCriteria(const Value: ICriteria);
    procedure SetMechanism(const Value: IMechanism);
    procedure SetMeta(const Value: IObjectMeta);
    procedure SetOwnHandler(const Value: Boolean);

    function CreateHandler: TObject; virtual; abstract;
    procedure SetParamList(const ParamList: IObjectType; const CommandType: TCommandType); virtual;
    procedure SetStatement(const Value: string); virtual; abstract;

    procedure SetParamValue(Param: TParam; const Member: IMemberType); virtual;
    procedure LoadBlobParam(Param: TParam; const Member: IMemberType); virtual;

    property Criteria: ICriteria read GetCriteria write SetCriteria;
    property Handler: TObject read GetHandler;
    property Meta: IObjectMeta read GetMeta write SetMeta;
    property Mechanism: IMechanism read GetMechanism write SetMechanism;
    property OwnHandler: Boolean read GetOwnHandler write SetOwnHandler;
    property Params: TParams read GetParams;
  public
    constructor Create(const Mechanism: IMechanism; const Mapping: IObjectMeta; const Criteria: ICriteria); virtual;
    destructor Destroy; override;
  end;

  TObjectQueryClass = class of TObjectQuery;
  TObjectQuery = class(TObjectDataSet, IObjectQuery)
  private
    FCurrentLoaded: IObjectType;
  protected
    FFieldList: TStrings;
    function GetActive: Boolean; virtual; abstract;
    function GetRecordCount: Integer; virtual; abstract;
    function GetCurrentLoaded: IObjectType; virtual;
    procedure SetActive(const Value: Boolean); virtual; abstract;
    procedure SetCurrentLoaded(const Value: IObjectType); virtual;
    procedure RowToObject(const AObject: IObjectType); virtual;
    procedure SetLoadedValue(const Member: IMemberType; NewValue: Variant); virtual;

    function GetCurrentFromCache: IObjectType; virtual;
    function GetFieldValue(const FieldName: string): Variant; virtual;
    procedure LoadObject(const AObject: IObjectType); virtual;

    function GetFieldCount: Integer; virtual;
    procedure FieldIndexToMember(const AObject: IObjectType; const FieldIndex: Integer); virtual;
    procedure FieldToMember(const AObject: IObjectType; const MemberMeta: IMemberMeta); virtual;

    procedure LoadMemberValue(const Member: IMemberType; const NewValue: Variant); virtual;

    procedure Open; virtual;
    procedure Close; virtual;

    function EOF: Boolean; virtual;
    function GetIsEmpty: Boolean; virtual; abstract;
    procedure First; virtual;
    procedure Next; virtual;

    property Active: Boolean read GetActive write SetActive;

    property CurrentLoaded: IObjectType read GetCurrentLoaded write SetCurrentLoaded;
    property IsEmpty: Boolean read GetIsEmpty;
    property RecordCount: Integer read GetRecordCount;
    function GetFieldList: TStrings;
    property FieldList: TStrings read GetFieldList;
  public
    constructor Create(const Mechanism: IMechanism; const Mapping: IObjectMeta; const Criteria: ICriteria); override;
    destructor Destroy; override;
  end;

  TObjectCommandClass = class of TObjectCommand;
  TObjectCommand = class(TObjectDataSet, IObjectCommand)
  private
    FSQL: string;
    FCommandType: TCommandType;
    FRowsAffected: Integer;
  protected
    function GetRowsAffected: Integer; virtual;
    procedure SetRowsAffected(const Value: Integer); virtual;

    function ExecuteCommand: Boolean; virtual; abstract;
    function Execute(const SQLCommand: string): Boolean; overload; virtual;
    function Execute(const AObject: IObjectType; const CommandType: TCommandType): Boolean; overload; virtual;

    property RowsAffected: Integer read GetRowsAffected write SetRowsAffected;
  end;

implementation

uses
  DateUtils,
  SysUtils,
  SqlTimSt,
  Variants,
  JazzDatabaseMechanismIntf,
  JazzTypes,
  JazzSessionIntf,
  JazzVarUtils,
  JazzUtils,
  JazzPersisterConsts;

constructor TObjectDataSet.Create(const Mechanism: IMechanism;
  const Mapping: IObjectMeta; const Criteria: ICriteria);
begin
  inherited Create;
  FOwnHandler:= True;
  SetCriteria(Criteria);
  SetMechanism(Mechanism);
  SetMeta(Mapping);
  FHandler:= CreateHandler;
end;

destructor TObjectDataSet.Destroy;
begin
  if FOwnHandler and (FHandler <> nil) then FreeAndNil(FHandler);
  inherited;
end;

procedure TObjectDataSet.CreateParamsFromCriteria;
var
  I: Integer;
  J: Integer;
  LCriterion: ICriterion;
  lParam: TParam;
  LMemberMeta: IMemberMeta;
begin
  for I:= 0 to (Criteria.Count - 1) do
  begin
    LCriterion:= Criteria[I];
    if LCriterion.CriterionType in
      [ctIsNull, ctOrderBy, ctOrderByDesc, ctStartGrouping, ctEndGrouping] then
      Continue;

    for J:= 0 to LCriterion.Values.Count - 1 do
    begin
      LMemberMeta:= Meta.FindMember(LCriterion.MemberName, True);

      if LMemberMeta <> nil then
      begin
        lParam:= InternalCreateParam(LMemberMeta);
        SetParamValue(lParam, LCriterion.Values[J] as IMemberType);
      end;
    end;
  end;
end;

procedure TObjectDataSet.CreateParamAndSetValue(const MemberMeta: IMemberMeta;
  const Member: IMemberType; const ParamName: string);
begin
  SetParamValue(InternalCreateParam(MemberMeta, ParamName), Member);
end;

procedure TObjectDataSet.CreateParamsFromID(const ObjectParams: IObjectType;
  const ParamName: string);
var
  I: Integer;
  LMember: IMemberType;
  LMemberMeta: IMemberMeta;
begin
  for I:= 0 to Meta.OID.Count - 1 do
  begin
    LMemberMeta:= nil;
    LMember:= nil;
    LMemberMeta:= Meta.FindMember(Meta.OID[I].MemberName);
    LMember:= ObjectParams.Member[LMemberMeta.MemberName];
    CreateParamAndSetValue(LMemberMeta, LMember, ParamName);
  end;
end;

procedure TObjectDataSet.CreateParamsFromObject(const ObjectParams: IObjectType;
  const CommandType: TCommandType);
var
  I: Integer;
  LMember: IMemberType;
  LMemberMeta: IMemberMeta;
begin
  case CommandType of
    ctDelete:
      CreateParamsFromID(ObjectParams);
    ctUpdate:
      CreateParamsForUpdate(ObjectParams);
  else
    for I:= 0 to (Meta.Items.Count - 1) do
    begin
      LMemberMeta:= Meta.Items[I] as IMemberMeta;
      if (LMemberMeta = nil) then Continue;
      LMember:= ObjectParams.Member[LMemberMeta.MemberName];
      if (LMember = nil) or (LMember.IsNull) then Continue;
      CreateParamAndSetValue(LMemberMeta, LMember);
    end;
  end;
end;

function TObjectDataSet.GetCriteria: ICriteria;
begin
  Result:= ICriteria(FCriteria);
end;

function TObjectDataSet.GetHandler: TObject;
begin
  Result:= FHandler;
end;

function TObjectDataSet.GetMechanism: IMechanism;
begin
  Result:= IMechanism(FMechanism);
end;

function TObjectDataSet.GetMeta: IObjectMeta;
begin
  Result:= IObjectMeta(FMeta);
end;

function TObjectDataSet.InternalCreateParam(const MemberMeta: IMemberMeta;
  ParamName: string): TParam;
var
  LParamName: string;
begin
  if ParamName <> EmptyStr then
    LParamName:= ParamName
  else
    LParamName:= Format(SParamName, [Params.Count, MemberMeta.ColumnName]);
  Result:= Params.CreateParam(MemberMeta.GetFieldType, LParamName, ptInput);
end;

procedure TObjectDataSet.LoadBlobParam(Param: TParam; const Member:
  IMemberType);
var
  LBlobData: TMemoryStream;
begin
  with TDataSet(FHandler) do
  begin
    LBlobData:= TMemoryStream.Create;
    try
      (Member as IBlobType).SaveToStream(LBlobData);
      LBlobData.Position:= 0;
      Param.LoadFromStream(LBlobData, ftBlob);
    finally
      FreeAndNil(LBlobData);
    end;
  end;
end;

procedure TObjectDataSet.SetCriteria(const Value: ICriteria);
begin
  FCriteria:= Pointer(Value);
end;

procedure TObjectDataSet.SetMechanism(const Value: IMechanism);
begin
  FMechanism:= Pointer(Value);
end;

procedure TObjectDataSet.SetMeta(const Value: IObjectMeta);
begin
  FMeta:= Pointer(Value);
end;

procedure TObjectDataSet.SetParamList(const ParamList: IObjectType; const
  CommandType: TCommandType);
begin
  Params.Clear;
  if (ParamList <> nil) then
    CreateParamsFromObject(ParamList, CommandType)
  else if (Criteria <> nil) then
    CreateParamsFromCriteria;
end;

procedure TObjectDataSet.SetParamValue(Param: TParam; const Member:
  IMemberType);
var
  LValue: Variant;
begin
  LValue:= Param.Value;
  if not (Member.ValueTypeKind = vtBlob) then
  begin
    MemberToVariant(Member, LValue, Param.DataType = ftTimeStamp);
    Param.Value:= LValue;
  end
  else
    LoadBlobParam(Param, Member);
end;

{ TObjectQuery }

procedure TObjectQuery.Close;
begin
  TDataSet(FHandler).Close;
end;

constructor TObjectQuery.Create(const Mechanism: IMechanism;
  const Mapping: IObjectMeta; const Criteria: ICriteria);
begin
  inherited;
  FFieldList:= TStringList.Create;
end;

destructor TObjectQuery.Destroy;
begin
  FFieldList.Free;
  inherited;
end;

function TObjectQuery.EOF: Boolean;
begin
  Result:= TDataSet(FHandler).EOF;
end;

procedure TObjectQuery.First;
begin
  TDataSet(FHandler).First;
end;

function TObjectQuery.GetCurrentLoaded: IObjectType;
begin
  Result:= FCurrentLoaded;
end;

function TObjectQuery.GetFieldValue(const FieldName: string): Variant;
begin
  Result:= TDataSet(FHandler).FieldByName(FieldName).Value
end;

function TObjectQuery.GetCurrentFromCache: IObjectType;
var
  I: Integer;
  LMemberMeta: IMemberMeta;
  LPath: string;
begin
  for I:= 0 to Meta.OID.Count - 1 do
  begin
    LMemberMeta:= IMemberMeta(Pointer(Meta.OID[I]));
    if LPath <> EmptyStr then
      LPath:= LPath + '.' + VarTypeToStr(GetFieldValue(LMemberMeta.ColumnName))
    else
      LPath:= VarTypeToStr(GetFieldValue(LMemberMeta.ColumnName));
  end;

  Result:= (Mechanism.Session as ISession).Cache.Find(Meta.ObjectClassName, LPath);
end;

procedure TObjectQuery.Next;
begin
  TDataSet(FHandler).Next;
end;

procedure TObjectQuery.Open;
var
  LSQL: string;
  LMechanism: IDatabaseMechanism;
begin
  Supports(Mechanism, IDatabaseMechanism, LMechanism);
  Close;
  LSQL:= LMechanism.DatabaseDriver.SelectStatement(Self, Meta, FFieldList);
  SetStatement(LSQL);
  SetParamList(nil, ctSelect); // paramlist = nil then criteria will be used
  LMechanism.DoExecuteStatement(LSQL, Params);
end;

function TObjectQuery.GetFieldCount: Integer;
begin
  Result:= TDataSet(FHandler).FieldCount;
end;

function TObjectQuery.GetFieldList: TStrings;
begin
  Result:= FFieldList;
end;

procedure TObjectQuery.LoadMemberValue(const Member: IMemberType; const NewValue:
  Variant);
begin
  if Member <> nil then
  try
    Member.Loading:= True;
    SetLoadedValue(Member, NewValue);
  finally
    Member.Loading:= False;
  end;
end;

procedure TObjectQuery.FieldIndexToMember(const AObject: IObjectType; const FieldIndex: Integer);
var
  LField: TField;
  LMemberMeta: IMemberMeta;
begin
  LField:= TDataSet(FHandler).Fields[FieldIndex];
  LMemberMeta:= Meta.FindMember(LField.FieldName, True, True);
  LoadMemberValue(AObject.Member[LMemberMeta.MemberName], LField.Value);
end;

procedure TObjectQuery.FieldToMember(const AObject: IObjectType; const MemberMeta: IMemberMeta);
var
  LField: TField;
begin
  LField:= TDataSet(FHandler).FindField(MemberMeta.ColumnName);

  if (LField <> nil) then
    LoadMemberValue(AObject.Member[MemberMeta.MemberName], LField.Value);
end;

procedure TObjectQuery.RowToObject(const AObject: IObjectType);
var
  I: Integer;
begin
  AObject.Loading:= True;
  for I := 0 to FFieldList.Count - 1 do
    LoadMemberValue(AObject.Member[FFieldList[I]], TDataSet(FHandler).Fields[I].Value);
  (AObject as IObjectState).Loaded(Mechanism.Session);
end;

procedure TObjectQuery.SetCurrentLoaded(const Value: IObjectType);
var
  LObject: IObjectState;
begin
  FCurrentLoaded:= Value;

  if Supports(FCurrentLoaded, IObjectState, LObject) then
  begin
    with LObject.State do
    begin
      if not Loaded and not Modified and not Persisted then
        RowToObject(FCurrentLoaded);
    end;
  end;
end;

procedure TObjectQuery.SetLoadedValue(const Member: IMemberType; NewValue:
  Variant);
var
  LStream: TStream;
begin
  if not (Member.ValueTypeKind = vtBlob) then
  begin
    VariantToMember(NewValue, Member);
  end
  else
  begin
    LStream:= TDataSet(FHandler).CreateBlobStream(
      TDataSet(FHandler).FieldByName(Member.Name), bmRead);
    try
     (Member as IBlobType).Value:= LStream
    finally
      FreeAndNil(LStream);
    end;
  end;
end;

{ TCommand }

function TObjectCommand.Execute(const AObject: IObjectType;
  const CommandType: TCommandType): Boolean;
begin
  with (Mechanism as IDatabasemechanism).DatabaseDriver do
  begin
    case CommandType of
      ctUpdate:
        FSQL:= UpdateStatement(AObject, Meta);
      ctInsert:
        FSQL:= InsertStatement(AObject, Meta);
      ctDelete:
        FSQL:= DeleteStatement(AObject, Meta);
    end;
    FCommandType:= CommandType;
    SetStatement(FSQL);
  end;

  if FSQL = EmptyStr then
    Result:= False
  else
  begin
    SetParamList(AObject, CommandType);
    (Mechanism as IDatabaseMechanism).DoExecuteStatement(FSQL, Params);
    Result:= ExecuteCommand;
  end;
end;

function TObjectCommand.Execute(const SQLCommand: string): Boolean;
begin
  FSQL:= SQLCommand;
  SetStatement(FSQL);
  (Mechanism as IDatabaseMechanism).DoExecuteStatement(FSQL, Params);
  Result:= ExecuteCommand;
end;

function TObjectCommand.GetRowsAffected: Integer;
begin
  Result:= FRowsAffected;
end;

procedure TObjectCommand.SetRowsAffected(const Value: Integer);
begin
  FRowsAffected:= Value;
end;

function TObjectDataSet.GetOwnHandler: Boolean;
begin
  Result:= FOwnHandler;
end;

procedure TObjectDataSet.SetOwnHandler(const Value: Boolean);
begin
  FOwnHandler:= Value;
end;

procedure TObjectDataSet.CreateParamsForUpdate(const ObjectParams: IObjectType);
var
  I, LCount: Integer;
  LMember: IMemberType;
  LMeta: IMemberMeta;
  LMembers, LParams: TStrings;
begin
  LParams:= TStringList.Create;
  LMembers:= TStringList.Create;
  try
    for I:= 0 to ObjectParams.MemberList.Count - 1 do
    begin
      LMember:= ObjectParams[I];
      LMeta:= Meta.FindMember(LMember.Name);
      if LMeta = nil then Continue;
      if not LMeta.IsOID and not (LMember as IMemberState).State.Modified then Continue;
      LParams.AddObject(Format(SParamName, [LParams.Count, LMeta.ColumnName]), Pointer(LMeta));
      LMembers.AddObject(LMember.Name, Pointer(LMember));
    end;
    LMember:= nil;
    LMeta:= nil;

    // move OID to the end of list
    LCount:= 0;
    for I:= LParams.Count - 1 downto 0 do
    begin
      Pointer(LMeta):= LParams.Objects[I];
      if not LMeta.IsOID then Continue;
      Inc(LCount);
      LParams.Move(I, LParams.Count - LCount);
      LMembers.Move(I, LMembers.Count - LCount);
    end;
    
    for I:= 0 to LParams.Count - 1 do
    begin
      Pointer(LMeta):= LParams.Objects[I];
      Pointer(LMember):= LMembers.Objects[I];
      CreateParamAndSetValue(LMeta, LMember, LParams[I]);
    end;
  finally
    Pointer(LMeta):= nil;
    Pointer(LMember):= nil;
    LMembers.Free;
    LParams.Free;
  end;
end;

procedure TObjectQuery.LoadObject(const AObject: IObjectType);
begin
  RowToObject(AObject);
  // should unique objects be in cache?
  //  if (Mechanism.Session as ISession).CacheMode <> cmNoCache then
  //  (Mechanism.Session as ISession).Cache.Add(AObject);
  (AObject as IObjectState).Loaded(Mechanism.Session);
end;

end.


