unit JazzDatabaseMechanism;

interface

uses
  DB,
  Classes,
  SyncObjs,
  Windows,
  JazzClasses,
  JazzCriteriaIntf,
  JazzDatabaseMechanismIntf,
  JazzDataSet,
  JazzDataSetIntf,
  JazzMappingIntf,
  JazzMechanism,
  JazzPersister,
  JazzPersisterIntf,
  JazzSessionIntf,
  JazzTypes,
  JazzValueType,
  JazzValueTypeIntf;

type
  TNotifySession = class;
  TDatabaseMechanism = class(TMechanism, IDatabaseMechanism)
  private
    FConnection: TObject;
    FDatabaseDriver: IDatabaseDriver;
    FNotifySession: INotifySession;
    FOnExecuteStatement: TExecuteStatementEvent;

    procedure LoadObjects(const Query: IObjectQuery; const AObjectList: IObjectListType);
    procedure PersistRelated(const AObject: IObjectType; const Meta: IObjectMeta; CommandType: TCommandType);
    procedure PersistSuper(const AObject: IObjectType; const Meta: IObjectMeta; const CommandType: TCommandType);
    procedure UpdateRelationMember(const Meta: IRelationshipMeta; const Master, Detail: IObjectType); overload;
    procedure UpdateRelationMember(const Meta: IRelationshipMeta; const Master: IObjectType; const Detail: IObjectListType); overload;
    procedure AddCriteriaItems(RelationshipMeta: IRelationshipMeta; const AObject: IObjectType; const ACriteria: ICriteria; FromFirst: boolean);
    procedure PersistRelatedObject(const AObject, ARelated: IObjectType; const Relationship: IRelationshipMeta; CommandType: TCommandType);
    procedure PersistRelatedObjectList(const AObject: IObjectType; const ARelatedList: IObjectListType; const Relationship: IRelationshipMeta; CommandType: TCommandType);
  protected
    function GetConnection: TObject; virtual;
    function GetDatabaseDriver: IDatabaseDriver; virtual;
    function GetOnExecuteStatement: TExecuteStatementEvent;
    function GetNotifySession: INotifySession;
    procedure SetConnection(const Value: TObject); virtual;
    procedure SetOnExecuteStatement(const Value: TExecuteStatementEvent);
    procedure SetNotifySession(const Value: INotifySession);

	  function CommandClass: TObjectCommandClass; virtual; abstract;
	  function QueryClass: TObjectQueryClass; virtual; abstract;
	  function DriverClass: TDatabaseDriverClass; virtual; abstract;

	  function CreateQuery(const Meta: IObjectMeta; const Criteria: ICriteria): IObjectQuery; virtual; abstract;
	  function CreateCommand(const Meta: IObjectMeta = nil): IObjectCommand; virtual; abstract;
	  function CreateDatabaseDriver: IDatabaseDriver; virtual; abstract;

    procedure DoExecuteStatement(var Command: string; const Params: TParams); virtual;
	  procedure GenerateSchema(const Schema: TStrings; const OnlyDiff: boolean = False); virtual;

    function LoadMember(const Member: IMemberType; const AutoLoad: boolean = False): boolean; override;
	  function LoadObject(const AObject: IObjectType; const Meta: IObjectMeta; const MembersProxy: TStrings = nil): boolean; override;
	  function LoadObjectList(const AObjectList: IObjectListType; const Criteria: ICriteria = nil): boolean; override;

    procedure InsertObject(const AObject: IObjectType; const Meta: IObjectMeta; const Command: IObjectCommand = nil); virtual;
	  procedure DeleteObject(const AObject: IObjectType; const Meta: IObjectMeta; const Command: IObjectCommand = nil); virtual;
	  procedure UpdateObject(const AObject: IObjectType; const Meta: IObjectMeta; const Command: IObjectCommand = nil); virtual;

	  procedure DeleteObjectList(const  AObjectList: IObjectListType); override;

    procedure Save(const AObject: IMemberType);
	  procedure SaveObject(const AObject: IObjectType; const Meta: IObjectMeta = nil); overload; override;
	  procedure SaveObject(const AObject: IObjectType; const Meta: IObjectMeta; const Command: IObjectCommand); reintroduce; overload;
	  procedure SaveObjectList(const  AObjectList: IObjectListType); override;

    function InTransaction: boolean; override; abstract;
    procedure StartTransaction; override; abstract;
    procedure CommitTransaction(const ForceCommit: boolean = False); override; abstract;
    procedure RollbackTransaction; override; abstract;
    
	  property Connection: TObject read GetConnection write SetConnection;
    property DatabaseDriver: IDatabaseDriver read GetDatabaseDriver;
    property NotifySession: INotifySession read GetNotifySession write SetNotifySession;
    property OnExecuteStatement: TExecuteStatementEvent read GetOnExecuteStateMent write SetOnExecuteStatement;
  end;

  TDatabaseParamList = class(TObjectType, IDatabaseParamList)
  protected
    function Add(const Name: string; const ParamType: TItemType): IMemberType;
  public
    constructor Create; reintroduce; virtual;
  end;

  TNotifySessionItem = class(TInterfacedObject, INotifySessionItem)
  private
    FInstance: IMemberType;
    FOperation: TSessionOperation;
    FMemberType: TMemberType;
  protected
    function GetInstance: IMemberType;
    function GetMemberType: TMemberType;
    function GetOperation: TSessionOperation;
    procedure SetInstance(const Value: IMemberType);
    procedure SetMemberType(const Value: TMemberType);
    procedure SetOperation(const Value: TSessionOperation);
  public
    property Instance: IMemberType read GetInstance write SetInstance;
    property Operation: TSessionOperation read GetOperation write SetOperation;
    property MemberType: TMemberType read GetMemberType write SetMemberType;
  end;

  TNotifySession = class(TInterfacedObject, INotifySession)
  private
    FList: IInterfaceList;
    FSession: Pointer;
    function GetSession: ISession;
  protected
    function LockList: IInterfaceList;
    procedure Unlock;
    property Session: ISession read GetSession;
  public
    constructor Create(const Session: ISession); virtual;

    function AddItem(const Instance: IMemberType; const Operation: TSessionOperation): INotifySessionItem;
    procedure NotifyAll; virtual;
    procedure ClearItems;
  end;

implementation

uses SysUtils, JazzCriteria, JazzConsts, JazzPersisterConsts, JazzMapping;

{ TDatabaseMechanism }

procedure TDatabaseMechanism.DeleteObject(const AObject: IObjectType;
  const Meta: IObjectMeta; const Command: IObjectCommand);
var
  LCommand: IObjectCommand;
begin
  if Command = nil then
    LCommand:= CreateCommand(Meta)
  else
    LCommand:= Command;
  LCommand.Execute(AObject, ctDelete);
  NotifySession.AddItem(AObject, soDeleted);
end;

procedure TDatabaseMechanism.DeleteObjectList(const AObjectList: IObjectListType);
var
  I: Integer;
begin
  AObjectList.BeginUpdate;
  AObjectList.Loading:= True;
  try
    for I:= AObjectList.Count -1 downto 0 do
      AObjectList[I].Delete;

    for I:= AObjectList.DeletingCount -1 downto 0 do
      SaveObject(AObjectList.DeletingItems[I]);
  finally
    AObjectList.Loading:= False;
    AObjectList.EndUpdate;
  end;
end;

procedure TDatabaseMechanism.DoExecuteStatement(var Command: string;
  const Params: TParams);
begin
  if Assigned(FOnExecuteStatement) then
    FOnExecuteStatement(Self, Command, Params);
end;

procedure TDatabaseMechanism.GenerateSchema(const Schema: TStrings; const OnlyDiff: boolean = False);
begin
  DatabaseDriver.GenerateSchema(Schema, OnlyDiff);
end;

function TDatabaseMechanism.GetConnection: TObject;
begin
  Result:= FConnection;
end;

function TDatabaseMechanism.GetDatabaseDriver: IDatabaseDriver;
begin
  if FDatabaseDriver = nil then FDatabaseDriver:= CreateDatabaseDriver;
  Result:= FDatabaseDriver;
end;

function TDatabaseMechanism.GetOnExecuteStatement: TExecuteStatementEvent;
begin
  Result:= FOnExecuteStatement;
end;

function TDatabaseMechanism.GetNotifySession: INotifySession;
begin
  if FNotifySession = nil then FNotifySession:= TNotifySession.Create(Session as ISession);
  Result:= FNotifySession;
end;

procedure TDatabaseMechanism.InsertObject(const AObject: IObjectType;
  const Meta: IObjectMeta; const Command: IObjectCommand);
var
  LCommand: IObjectCommand;
begin
  if Command = nil then
    LCommand:= CreateCommand(Meta)
  else
    LCommand:= Command;
  LCommand.Execute(AObject, ctInsert);
  NotifySession.AddItem(AObject, soPersisted);
end;

procedure TDatabaseMechanism.AddCriteriaItems(RelationshipMeta:
  IRelationshipMeta; const AObject: IObjectType; const ACriteria: ICriteria;
  FromFirst: boolean);
var
  I: Integer;
  LMembersFrom: TStrings;
  LMembersTo: TStrings;
  LValue: string;
begin
  if FromFirst then
  begin
    LMembersFrom:= RelationshipMeta.FromMembers;
    LMembersTo:= RelationshipMeta.ToMembers;
  end
  else
  begin
    LMembersFrom:= RelationshipMeta.ToMembers;
    LMembersTo:= RelationshipMeta.FromMembers;
  end;
  
  for I:= 0 to LMembersFrom.Count -1 do
  begin
    LValue:= AObject.Member[LMembersFrom[I]].AsString;
    ACriteria.Add(ctEqualTo, LMembersTo[I], LValue);
  end;
end;

function TDatabaseMechanism.LoadMember(const Member: IMemberType;
  const AutoLoad: boolean): boolean;
var
  I: Integer;
  LCriteria: ICriteria;
  LMembers: TStrings;
  LMeta: IObjectMeta;
  LObject: IObjectType;
  LObjectList: IObjectListType;
  LObjectMeta: IObjectMeta;
  LRelationship: IRelationshipMeta;
begin
  Result:= False;
  LObject:= Member.Owner as IObjectType;
  LMeta:= Mapping.Find(LObject.GetClassName);
  if not LMeta.OID.IsMapped then Exit;

  if Supports(Member, IObjectListType, LObjectList) then
  begin
    LObjectMeta:= (Session as ISession).Mapping.Find(LObject.GetClassName);
    if LObjectMeta = nil then Exit;
    LCriteria:= NewCriteria;

    for I:= 0 to LObjectMeta.Relationships.Count -1 do
    begin
      LRelationship:= LObjectMeta.Relationships[I] as IRelationshipMeta;
      if AutoLoad and not LRelationship.CascadeLoad then Continue;
      if LRelationship.FromClassName = LObjectList.ItemClass.ClassName then
        AddCriteriaItems(LRelationship, LObject, LCriteria, False)
      else
      if LRelationship.ToClassName = LObjectList.ItemClass.ClassName then
        AddCriteriaItems(LRelationship, LObject, LCriteria, True);
    end;
    if LCriteria.Count > 0 then Result:= LoadObjectList(LObjectList, LCriteria);
    Exit;
  end;
  
  LMembers:= TStringList.Create;
  try
    LMembers.Add(Member.Name);
    Result:= LoadObject(LObject, LMeta, LMembers);
  finally
    LMembers.Free;
  end;
end;

function TDatabaseMechanism.LoadObject(const AObject: IObjectType;
  const Meta: IObjectMeta; const MembersProxy: TStrings): boolean;
var
  I: Integer;
  LCriteria: ICriteria;
  LQuery: IObjectQuery;
  LMember: IMemberType;
begin
  Result:= False;
  if not Meta.OID.IsMapped then Exit;
  LCriteria:= TCriteria.Create;
  if MembersProxy <> nil then LCriteria.Proxy:= MembersProxy;

  LQuery:= CreateQuery(Meta, LCriteria);

  for I:= 0 to Meta.OID.Count -1 do
  begin
    LMember:= AObject.Member[Meta.OID[I].MemberName];
    LQuery.Criteria.Add(ctEqualTo, LMember.Name, [(LMember.AsString)]);
  end;

  LQuery.Open;
  Result:= not LQuery.IsEmpty;
  if Result then LQuery.LoadObject(AObject);
  LQuery.Close;
end;

function TDatabaseMechanism.LoadObjectList(const AObjectList: IObjectListType;
  const Criteria: ICriteria): boolean;
var
  LQuery: IObjectQuery;
  LMeta: IObjectMeta;
begin
  Result:= False;
  if (Criteria <> nil) and not Criteria.IsValid then
    raise Exception.Create(SInvalidCriteria);

  AObjectList.BeginUpdate;
  AObjectList.Loading:= True;
  try
    AObjectList.Clear;
    LMeta:= Mapping.Find(AObjectList.ItemClass.ClassName);
    if LMeta = nil then
      raise Exception.CreateFmt(SMappingNotDefined, [AObjectList.ItemClass.ClassName]);

    LQuery:= CreateQuery(LMeta, Criteria);
    LQuery.Open;
    Result:= not LQuery.IsEmpty;

    if Result then LoadObjects(LQuery, AObjectList);
    (AObjectList as IObjectListState).Loaded(Session);
  finally
    AObjectList.Loading:= False;
    AObjectList.EndUpdate;
  end;
end;

procedure TDatabaseMechanism.Save(const AObject: IMemberType);
var
  LObject: IObjectType;
  LObjectList: IObjectListType;
begin
  if Supports(AObject, IObjectType, LObject) then
    SaveObject(LObject)
  else
  if Supports(AObject, IObjectListType, LObjectList) then
    SaveObjectList(LObjectList);
end;

procedure TDatabaseMechanism.UpdateRelationMember(const Meta: IRelationshipMeta;
  const Master, Detail: IObjectType);
var
  I: Integer;
begin
  for I:= 0 to Meta.FromMembers.Count -1 do
    Detail.Member[Meta.ToMembers[I]].AsString:=
      Master.Member[Meta.FromMembers[I]].AsString;
end;

procedure TDatabaseMechanism.UpdateRelationMember(const Meta: IRelationshipMeta;
  const Master: IObjectType; const Detail: IObjectListType);
var
  I: Integer;
begin
  for I:= 0 to Detail.Count -1 do
    UpdateRelationMember(Meta, Master, Detail[I]);
end;

procedure TDatabaseMechanism.PersistRelatedObject(const AObject, ARelated:
  IObjectType; const Relationship: IRelationshipMeta; CommandType:
  TCommandType);
var
  LObjectMeta: IObjectMeta;
begin
  LObjectMeta:= Mapping.Find(ARelated.GetClassName);

  if (CommandType = ctDelete) and Relationship.CascadeDelete then
    DeleteObject(ARelated, LObjectMeta)
  else
  if Relationship.CascadeSave then
  begin
    UpdateRelationMember(Relationship, AObject, ARelated);
    SaveObject(ARelated, LObjectMeta);
  end;
end;

procedure TDatabaseMechanism.PersistRelatedObjectList(const AObject:
  IObjectType; const ARelatedList: IObjectListType; const Relationship:
  IRelationshipMeta; CommandType: TCommandType);
begin
  if (CommandType = ctDelete) and Relationship.CascadeDelete then
    DeleteObjectList(ARelatedList)
  else
  if Relationship.CascadeSave then
  begin
    UpdateRelationMember(Relationship, AObject, ARelatedList);
    SaveObjectList(ARelatedList);
  end;
end;

procedure TDatabaseMechanism.PersistRelated(const AObject: IObjectType;
  const Meta: IObjectMeta; CommandType: TCommandType);
var
  I: Integer;
  LRelationship: IRelationshipMeta;
  LMember: IMemberType;
  LObject: IObjectType;
  LObjectList: IObjectListType;
begin
  for I:= 0 to Meta.Relationships.Count -1 do
  begin
    LRelationship:= Meta.Relationships[I] as IRelationshipMeta;
    if LRelationship.RelationshipType = rtInheritance then Continue;
    LMember:= AObject.Member[LRelationship.MasterAttribute];
    if LMember = nil then Continue;
    
    if Supports(LMember, IObjectListType, LObjectList) then
      PersistRelatedObjectList(AObject, LObjectList, LRelationship, CommandType)
    else
    if Supports(LMember, IObjectType, LObject) then 
      PersistRelatedObject(AObject, LObject, LRelationship, CommandType)
  end;

end;

procedure TDatabaseMechanism.PersistSuper(const AObject: IObjectType;
  const Meta: IObjectMeta; const CommandType: TCommandType);
var
  I: Integer;
  LRelationship: IRelationshipMeta;
  LObject: IObjectType;
  LObjectMeta: IObjectMeta;
  LObjectList: IObjectListType;
begin
  for I:= 0 to Meta.Relationships.Count -1 do
  begin
    LRelationship:= Meta.Relationships[I] as IRelationshipMeta;
    if LRelationship.RelationshipType <> rtInheritance then Continue;

    LObjectMeta:= (Meta.Owner as IMapping).Find(LRelationship.ToClassName);
    LObject:= AObject;

    if (CommandType = ctDelete) and LRelationship.CascadeDelete then
    begin
      if LObjectList <> nil then
        DeleteObjectList(LObjectList)
      else
        DeleteObject(LObject, LObjectMeta);
    end
    else
    if LRelationship.CascadeSave then
    begin
      if LObjectList <> nil then
        SaveObjectList(LObjectList)
      else
        SaveObject(LObject, LObjectMeta);
    end;
  end;
end;

procedure TDatabaseMechanism.SaveObject(const AObject: IObjectType;
  const Meta: IObjectMeta; const Command: IObjectCommand);
var
  LMeta: IObjectMeta;
begin
  StartTransaction;
  try
    LMeta:= Meta;
    if LMeta = nil then LMeta:= Mapping.Find(AObject.GetClassName);
    if (AObject as IObjectState).State.Deleting then
    begin
      PersistRelated(AObject, LMeta, ctDelete);
      DeleteObject(AObject, LMeta, Command);
      PersistSuper(AObject, LMeta, ctDelete);
    end
    else
    if (AObject as IObjectState).State.Loaded   and
       (AObject as IObjectState).State.Modified then
    begin
      UpdateObject(AObject, LMeta, Command);
      PersistSuper(AObject, LMeta, ctUpdate);
      PersistRelated(AObject, LMeta, ctUpdate);
    end
    else
    if (AObject as IObjectState).State.Modified then
    begin
      NewOID(AObject);
      PersistSuper(AObject, LMeta, ctInsert);
      InsertObject(AObject, LMeta, Command);
      PersistRelated(AObject, LMeta, ctInsert);
    end;
    CommitTransaction;
    if not InTransaction then NotifySession.NotifyAll;
  except
    RollbackTransaction;
    NotifySession.ClearItems;
    raise;
  end;
end;

procedure TDatabaseMechanism.SaveObject(const AObject: IObjectType;
  const Meta: IObjectMeta);
begin
  SaveObject(AObject, Meta, nil);
end;


function CanContinue(const AObjectList: IObjectListType): boolean;
begin
  Result:= (AObjectList as IObjectListState).State.Modified or
    (AObjectList.DeletingCount > 0);
end;

procedure TDatabaseMechanism.SaveObjectList(const AObjectList: IObjectListType);
var
  I: Integer;
  LMeta: IObjectMeta;
  LCommand: IObjectCommand;
  LObject: IObjectType;
begin
  if not CanContinue(AObjectList) then Exit;
  StartTransaction;
  try
    LMeta:= Mapping.Find(AObjectList.ItemClass.ClassName);
    LCommand:= CreateCommand(LMeta);
    try
      for I:= AObjectList.Count -1 downto 0 do
      begin
        LObject:= AObjectList.Items[I];

        if (LObject as IObjectState).State.Modified then
          SaveObject(LObject, LMeta, LCommand);

        LObject:= nil;
      end;

      for I:= AObjectList.DeletingCount -1 downto 0 do
        SaveObject(AObjectList.DeletingItems[I], LMeta, LCommand);
    finally
      LCommand:= nil;
      LMeta:= nil;
    end;

    CommitTransaction;
    if not InTransaction then NotifySession.NotifyAll;
  except
    RollbackTransaction;
    NotifySession.ClearItems;
    raise;
  end;
end;

procedure TDatabaseMechanism.SetConnection(const Value: TObject);
begin
  FConnection:= Value;
end;

procedure TDatabaseMechanism.SetOnExecuteStatement(
  const Value: TExecuteStatementEvent);
begin
  FOnExecuteStatement:= Value;
end;

procedure TDatabaseMechanism.SetNotifySession(const Value: INotifySession);
begin
  FNotifySession:= Value;
end;

procedure TDatabaseMechanism.UpdateObject(const AObject: IObjectType;
  const Meta: IObjectMeta; const Command: IObjectCommand);
var
  LCommand: IObjectCommand;
begin
  if Command = nil then
    LCommand:= CreateCommand(Meta)
  else
    LCommand:= Command;
  LCommand.Execute(AObject, ctUpdate);
  NotifySession.AddItem(AObject, soPersisted);
end;

constructor TDatabaseParamList.Create;
begin
  inherited Create(nil);
end;

procedure TDatabaseMechanism.LoadObjects(const Query: IObjectQuery;
  const AObjectList: IObjectListType);
var
  LCurrent: IObjectType;
begin
  if Query.IsEmpty then Exit;
  Query.First;
  while not Query.EOF do
  begin
    if (Session as ISession).CacheMode <> cmNoCache then
    begin
      LCurrent:= nil;
      LCurrent:= Query.GetCurrentFromCache;

      if LCurrent = nil then
      begin
        Query.CurrentLoaded:= AObjectList.Add;
        (Session as ISession).Cache.Add(Query.CurrentLoaded);
        (Query.CurrentLoaded as IObjectState).Loaded(Session);
      end
      else
      begin
        AObjectList.Add(LCurrent);
        Query.CurrentLoaded:= LCurrent;
      end;
    end
    else
    begin
      Query.CurrentLoaded:= AObjectList.Add;
     (Query.CurrentLoaded as IObjectState).Loaded(Session);
    end;
    Query.Next;
  end;
end;

{ TDatabaseParamList }

function TDatabaseParamList.Add(const Name: string; const ParamType: TItemType): IMemberType;
begin
  case ParamType of
    itAutoInc, itInteger: AddMember(Result, Name, TIntegerType);
    itBlob: AddMember(Result, Name, TBlobType);
    itBoolean: AddMember(Result, Name, TBooleanType);
    itChar: AddMember(Result, Name, TCharType);
    itCurrency: AddMember(Result, Name, TCurrencyType);
    itDate, itDateTime, itTime, itTimeStamp: AddMember(Result, Name, TDateType);
    itFloat: AddMember(Result, Name, TFloatType);
    itLongInt: AddMember(Result, Name, TLongIntType);
    itMemo: AddMember(Result, Name, TMemoType);
    itSmallInt: AddMember(Result, Name, TSmallIntType);
    itString: AddMember(Result, Name, TStringType);
    itWideString: AddMember(Result, Name, TWideStringType);
  end;
end;

{ TNotifySessionItem }

function TNotifySessionItem.GetInstance: IMemberType;
begin
  Result:= FInstance;
end;

function TNotifySessionItem.GetMemberType: TMemberType;
begin
  Result:= FMemberType;
end;

procedure TNotifySessionItem.SetInstance(const Value: IMemberType);
begin
  FInstance:= Value;
end;

procedure TNotifySessionItem.SetMemberType(const Value: TMemberType);
begin
  FMemberType:= Value;
end;

function TNotifySessionItem.GetOperation: TSessionOperation;
begin
  Result:= FOperation;
end;

procedure TNotifySessionItem.SetOperation(const Value: TSessionOperation);
begin
  FOperation:= Value;
end;

{ TNotifySession }

function TNotifySession.AddItem(const Instance: IMemberType;
  const Operation: TSessionOperation): INotifySessionItem;
begin
  LockList;
  try
    Result:= TNotifySessionItem.Create;
    FList.Add(Result);
    Result.Instance:= Instance;
    Result.Operation:= Operation;
  finally
    Unlock;
  end;
end;

procedure TNotifySession.ClearItems;
begin
  with LockList do
  try
    Clear;
  finally
    UnLock;
  end;
end;

constructor TNotifySession.Create(const Session: ISession);
begin
  inherited Create;
  FSession:= Pointer(Session);
  FList:= TInterfaceList.Create;
end;

function TNotifySession.GetSession: ISession;
begin
  Result:= ISession(FSession); 
end;

function TNotifySession.LockList: IInterfaceList;
begin
  FList.Lock;
  Result:= FList;
end;

procedure TNotifySession.NotifyAll;
var
  LSessionItem: INotifySessionItem;
  LLast: Integer;
  LInstance: IMemberType;
begin
  LockList;
  try
    while FList.Count > 0 do
    begin
      LLast:= FList.Count -1;
      LSessionItem:= FList.Items[LLast] as INotifySessionItem;
      LInstance:= LSessionItem.Instance;

      case LSessionItem.Operation of
        soDeleted:
          (LInstance as IObjectState).Deleted;
        soPersisted:
          (LInstance as IPersisterState).Persisted(Session as IDataProvider);
        // TODO: add new objetos to Session.Cache
      end;
      FList.Delete(LLast);
    end;
  finally
    Unlock;
  end;
end;

procedure TNotifySession.Unlock;
begin
  FList.Unlock;
end;

end.


