unit JazzSession;

interface

uses
  JazzClasses,
  JazzCriteriaIntf,
  JazzDataFactory,
  JazzMappingIntf,
  JazzMechanism,
  JazzMechanismIntf,
  JazzSessionIntf,
  JazzValueTypeIntf;

type
  EJazzSessionNotDefined = class(EJazz)
  protected
    function GetDefaultMessage: string; override;
  end;

  TCache = class(TCustomObject, ICache)
  private
    FItems: INamedInterfaceList;
    function GetPath(const AObject: IObjectType): string;
    function GetItems(Index: Integer): IInterface;
  protected
    function Count: Integer;
    procedure Clear;
    function Find(const Path: string): IObjectType; overload;
    function Find(const ObjectClassName: string; const OID: string): IObjectType; overload;
    function Find(const AObject: IObjectType; const OID: string): boolean; overload;
    function Find(const AObject: IObjectType): boolean; overload;
    procedure Add(const AObject: IObjectType);
    procedure Remove(const AObject: IObjectType);
    property Items[Index: Integer]: IInterface read GetItems;
  public
    constructor Create(const AOwner: IInterface = nil); override;
  end;

  TSession = class(TCustomObject, ISession, IDataProvider)
  private
    FCache: ICache;
    FCacheMode: TCacheMode;
    FMapping: IMapping;
    FMechanism: IMechanism;
    FName: string;
    FDataFactory: IDataFactory;
  protected
    function GetCache: ICache;
    function GetCacheMode: TCacheMode;
    function GetMapping: IMapping;
    function GetMechanism: IMechanism;
    function GetName: string;
    procedure SetMapping(const Value: IMapping);
    procedure SetName(const Value: string);

    procedure ClearCache;
    function Load(const Member: IMemberType; const LoadAllMembers: boolean = True; const AutoLoad: boolean = False): boolean; overload;
    function Load(const AObjectList: IObjectListType; const Criteria: ICriteria = nil): boolean; overload;
    function Load(const AObject: IObjectType): boolean; overload;
    procedure Save(const AObjectList: IObjectListType; const DeleteList: boolean = False); overload;
    procedure Save(const AObject: IObjectType); overload;
    function GetDataFactory: IDataFactory;

    property Cache: ICache read GetCache;
    property CacheMode: TCacheMode read GetCacheMode;
    property Mechanism: IMechanism read GetMechanism;
    property Mapping: IMapping read GetMapping write SetMapping;
    property Name: string read GetName write SetName;
    property DataFactory: IDataFactory read GetDataFactory;
  public
    constructor Create(const AOwner: IInterface; MechanismClass: TClass; CacheMode: TCacheMode = cmShared); reintroduce; virtual;
    destructor Destroy; override;
  end;

// TODO: Event OnActivateSession -> observer/subject  
var
  ActiveSession: ISession = nil;

implementation

uses
  Classes,
  SysUtils,
  JazzConsts,
  JazzPersister,
  JazzPersisterConsts,
  JazzPersisterIntf;

var
  FSharedCache: ICache;

function GetSharedCache: ICache;
begin
  if FSharedCache = nil then
  begin
    if ActiveSession = nil then
      raise exception.Create(SNoActiveSession)
    else
      FSharedCache:= TCache.Create(ActiveSession);
  end;
  Result:= FSharedCache;
end;

{ TSession }

procedure TSession.ClearCache;
begin
  FCache.Clear;
end;

constructor TSession.Create(const AOwner: IInterface; MechanismClass: TClass; CacheMode: TCacheMode);
begin
  inherited Create(AOwner);
  FMechanism:= TMechanismClass(MechanismClass).Create(Self);
  FCacheMode:= CacheMode;

  if ActiveSession = nil then ActiveSession:= Self;
end;

destructor TSession.Destroy;
begin
  if CacheMode = cmShared then
    FCache:= nil
  else
  if (CacheMode = cmExclusive) and (FCache <> nil) then
  begin
    FCache.Clear;
    FCache:= nil;
  end;
  
  inherited;
end;

function TSession.GetCache: ICache;
begin
  if CacheMode = cmNoCache then
  begin
    if FCache <> nil then FCache:= nil;
    Exit;
  end;
  
  if (FCache = nil) then
  begin
    if FCacheMode = cmShared then
      FCache:= GetSharedCache
    else
      FCache:= TCache.Create(Self);
  end;
  
  Result:= FCache;
end;

function TSession.GetCacheMode: TCacheMode;
begin
  Result:= FCacheMode;
end;

function TSession.GetMapping: IMapping;
begin
  if FMapping = nil then FMapping:= (Owner as IPersister).DefaultMapping;
  Result:= FMapping;
end;

function TSession.GetMechanism: IMechanism;
begin
  Result:= FMechanism;
end;

function TSession.GetName: string;
begin
  Result:= FName;
end;

procedure TSession.SetMapping(const Value: IMapping);
begin
  FMapping:= Value;
end;

procedure TSession.SetName(const Value: string);
begin
  FName:= Value;
end;

function TSession.Load(const Member: IMemberType; const LoadAllMembers: boolean; const AutoLoad: boolean): boolean;
(* To be used in LazyLoad operations *)
var
  LObject: IObjectType;
  LMeta: IObjectMeta;
  LLazyItems: TStrings;
begin
  if LoadAllMembers then
  begin
    LObject:= Member.Owner as IObjectType;
    LMeta:= Mapping.Find(LObject.GetClassName);
    LLazyItems:= TStringList.Create;
    try
      LMeta.GetLazyItems(LLazyItems);
      if LLazyItems.Count > 0 then
        Result:= Mechanism.LoadObject(LObject, LMeta, LLazyItems)
      else
        Result:= Mechanism.LoadObject(LObject, LMeta);
    finally
      FreeAndNil(LLazyItems);
    end;
  end
  else
    Result:= Mechanism.LoadMember(Member as IMemberType, AutoLoad);
end;

function TSession.Load(const AObjectList: IObjectListType; const Criteria: ICriteria): boolean;
(* Load ObjectList by Criteria *)
begin
  Result:= Mechanism.LoadObjectList(AObjectList as IObjectListType, Criteria as ICriteria);
end;

function TSession.Load(const AObject: IObjectType): boolean;
(* Load object by ID *)
var
  LMeta: IObjectMeta;
begin
  LMeta:= Mapping.Find(AObject.GetClassName);
  Result:= Mechanism.LoadObject(AObject as IObjectType, LMeta);
end;

procedure TSession.Save(const AObjectList: IObjectListType; const DeleteList: boolean);
begin
  if DeleteList then
    Mechanism.DeleteObjectList(AObjectList as IObjectListType)
  else
    Mechanism.SaveObjectList(AObjectList as IObjectListType);
end;

procedure TSession.Save(const AObject: IObjectType);
begin
  Mechanism.SaveObject(AObject as IObjectType);
end;

function TSession.GetDataFactory: IDataFactory;
begin
  if FDataFactory = nil then FDataFactory:= TDataFactory.Create(Mapping);
  Result:= FDataFactory;
end;

{ TCache }

procedure TCache.Add(const AObject: IObjectType);
begin
  FItems.Add(GetPath(AObject), AObject);
end;

procedure TCache.Clear;
begin
  FItems.Clear;
end;

function TCache.Count: Integer;
begin
  Result:= FItems.Count;
end;

constructor TCache.Create(const AOwner: IInterface);
begin
  inherited Create(AOwner);
  FItems:= TNamedInterfaceList.Create;
end;

function TCache.Find(const ObjectClassName, OID: string): IObjectType;
begin
  Result:= Find(ObjectClassName + '.' + OID);
end;

function TCache.Find(const AObject: IObjectType; const OID: string): boolean;
begin
  Result:= Find(AObject.GetClassName, OID) <> nil;
end;

function TCache.Find(const AObject: IObjectType): boolean;
begin
  Result:= Find(GetPath(AObject)) <> nil;
end;

function TCache.Find(const Path: string): IObjectType;
var
  LIndex: Integer;
begin
  LIndex:= FItems.IndexOf(Path);
  if LIndex <> NotFound then Result:= FItems[LIndex] as IObjectType;
end;

function TCache.GetItems(Index: Integer): IInterface;
begin
  Result:= FItems[Index]; 
end;

function TCache.GetPath(const AObject: IObjectType): string;
var
  I: Integer;
  LMeta: IObjectMeta;
begin
  LMeta:= (Owner as ISession).Mapping.Find(AObject.GetClassName);

  for I:= 0 to LMeta.OID.Count -1 do
    Result:= Result + '.' + AObject.Member[LMeta.OID[I].MemberName].AsString;

  Result:= AObject.GetClassName + Result;
end;

procedure TCache.Remove(const AObject: IObjectType);
var
  LIndex: Integer;
begin
  LIndex:= FItems.IndexOf(AObject);
  if LIndex <> NotFound then FItems.Delete(LIndex);
end;

{ EJazzSessionNotDefined }

function EJazzSessionNotDefined.GetDefaultMessage: string;
begin
  Result:= SSessionNotDefined;

end;

end.


