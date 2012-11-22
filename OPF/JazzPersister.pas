unit JazzPersister;

interface

uses
  Classes,
  JazzValueTypeIntf,
  JazzPersisterIntf,
  JazzMappingIntf,
  JazzSessionIntf,
  JazzDataFactory,
  JazzValueType,
  JazzCriteriaIntf,
  JazzMechanismIntf,
  JazzClasses;

type
  TPersister = class(TInterfacedObject, IPersister)
  private
    FSessionList: INamedInterfaceList;
  protected
    function GetDefaultMapping: IMapping;
    function GetDefaultSession: ISession;
    function GetSession(const Name: string): ISession;
    function GetSessions(const Index: Integer): ISession;
    function GetSessionsCount: Integer;

    property DefaultMapping: IMapping read GetDefaultMapping;
    property SessionsCount: Integer read GetSessionsCount;
    property Session[const Name: string]: ISession read GetSession;
    property Sessions[const Index: Integer]: ISession read GetSessions;
    property DefaultSession: ISession read GetDefaultSession;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

function Persister: IPersister;
function NewSession(const MechanismClass: TClass; const CacheMode: TCacheMode = cmShared; const SessionName: string = ''): ISession;

implementation

uses
  SysUtils,
  JazzPersisterConsts,
  JazzMapping,
  JazzCriteria,
  JazzConsts,
  JazzSession,
  JazzIntfUtils,
  JazzUtils;

var
  FPersister: IPersister;

function Persister: IPersister;
begin
  if FPersister = nil then FPersister:= TPersister.Create;
  Result:= FPersister;
end;

function NewSession(const MechanismClass: TClass; const CacheMode: TCacheMode; const SessionName: string): ISession;
begin
  Assert(MechanismClass <> nil, SSessionParams);

  Result:= TSession.Create(Persister, MechanismClass, CacheMode);
  if SessionName <> EmptyStr then
    Result.Name:= SessionName
  else
    Result.Name:= ClassToName(MechanismClass);

  TPersister(InterfaceToObject(Persister)).FSessionList.Add(Result.Name, Result);
end;

constructor TPersister.Create;
begin
  if FPersister <> nil then
  begin
    raise Exception.Create(SPersisterLoaded);
    Exit;
  end;
  inherited Create;
  FSessionList:= TNamedInterfaceList.Create;
end;

destructor TPersister.Destroy;
begin
  FSessionList.Clear; 
  inherited;
end;

function TPersister.GetDefaultMapping: IMapping;
begin
  if ActiveMapping = nil then ActiveMapping:= TMapping.Create(ActiveSession);
  Result:= ActiveMapping;
end;

function TPersister.GetDefaultSession: ISession;
begin
  Result:= ActiveSession;
end;

function TPersister.GetSession(const Name: string): ISession;
var
  LIndex: Integer;
begin
  LIndex:= FSessionList.IndexOf(Name);
  if LIndex <> NotFound then Result:= Sessions[LIndex];
end;

function TPersister.GetSessions(const Index: Integer): ISession;
begin
  Result:= FSessionList[Index] as ISession; 
end;

function TPersister.GetSessionsCount: Integer;
begin
  Result:= FSessionList.Count;
end;

end.





