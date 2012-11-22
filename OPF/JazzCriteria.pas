unit JazzCriteria;

interface

uses
  Classes,
  JazzCriteriaIntf,
  JazzValueTypeIntf,
  JazzValueType;

type
  TCriteria = class(TInterfacedObject, ICriteria)
  private
    FDistinct: boolean;
    FList: IInterfaceList;
    FGroupBy: TStrings;
    FProxy: TStrings;
    FFirstRecord: Integer;
    FMaxRecord: Integer;
  protected
    function GetCount: Integer;
    function GetDistinct: boolean;
    function GetFirstRecord: Integer;
    function GetGroupBy: TStrings;
    function GetItems(const Index: Integer): ICriterion;
    function GetMaxRecords: Integer;
    function GetProxy: TStrings;
    procedure SetFirstRecord(const Value: Integer);
    procedure SetGroupBy(const Value: TStrings);
    procedure SetMaxRecords(const Value: Integer);
    procedure SetProxy(const Value: TStrings);

    function Add(const CriterionType: TCriterionType; const MemberName: string; const Values: array of string; const IsNot: boolean = False): ICriterion;
    function AddValues(const CriterionType: TCriterionType; const MemberName: string; const Values: array of IMemberType; const IsNot: boolean = False): ICriterion; 
    function AddOr(const CriterionType: TCriterionType; const MemberName: string; const Values: array of string; const IsNot: boolean = False): ICriterion;
    function AddValuesOr(const CriterionType: TCriterionType; const MemberName: string; const Values: array of IMemberType; const IsNot: boolean = False): ICriterion;
    function AddOrderBy(const MemberName: string; const SortOrder: TSortOrderType = soAscending): ICriterion;
    procedure Clear;

    function IsValid: boolean;

    procedure StartGrouping(const AndOr: TCriteriaAndOrType = aoAnd; const IsNot: boolean = False);
    procedure EndGrouping;

    procedure AddGroupBy(const MemberName: string);
    procedure AddProxy(const MemberName: string);

    property Count: Integer read GetCount;
    property Distinct: boolean read GetDistinct;
    property GroupBy: TStrings read GetGroupBy write SetGroupBy;
    property Items[const Index: Integer]: ICriterion read GetItems; default;
    property Proxy: TStrings read GetProxy write SetProxy;
    property FirstRecord: Integer read GetFirstRecord write SetFirstRecord;
    property MaxRecords: Integer read GetMaxRecords write SetMaxRecords;
  public
    constructor Create; reintroduce; overload;
    constructor Create(const CriterionType: TCriterionType; const MemberName: string; const Values: array of string; const IsNot: boolean = False); reintroduce; overload;
    constructor Create(const CriterionType: TCriterionType; const MemberName: string; const Values: array of IMemberType; const IsNot: boolean = False); reintroduce; overload;
    destructor Destroy; override;
  end;

  TCriterion = class(TObjectType, ICriterion)
  private
    FCriterionType: ISmallIntType;
    FIsNot: IBooleanType;
    FIsOr: IBooleanType;
    FMemberName: IStringType;
    FOwner: Pointer;
    FValues: IInterfaceList;
  protected
    function GetCriterionType: TCriterionType;
    function GetIsNot: boolean;
    function GetIsOr: boolean;
    function GetMemberName: string; 
    function GetValues: IInterfaceList;
    procedure SetCriterionType(const Value: TCriterionType);
    procedure SetIsNot(const Value: boolean);
    procedure SetIsOr(const Value: boolean);
    procedure SetMemberName(const Value: string);

    function AddValue(const Value: string): IStringType; overload;
    function AddValue(const Value: IMemberType): IMemberType; overload;
    property CriterionType: TCriterionType read GetCriterionType write SetCriterionType;
    property IsNot: boolean read GetIsNot write SetIsNot;
    property IsOr: boolean read GetIsOr write SetIsOr;
    property MemberName: string read GetMemberName write SetMemberName;
    property Values: IInterfaceList read GetValues;
  public
    constructor Create(const AOwner: ICriteria); reintroduce; virtual;
  end;

function NewCriteria: ICriteria; overload;
function NewCriteria(out Instance): ICriteria; overload;

implementation

uses
  SysUtils,
  JazzPersisterConsts;

function NewCriteria: ICriteria;
begin
  Result:= TCriteria.Create;
end;

function NewCriteria(out Instance): ICriteria;
begin
  Result:= NewCriteria;
  Result.QueryInterface(ICriteria, Instance);
end;

constructor TCriteria.Create;
begin
  inherited Create;
  FList:= TInterfaceList.Create;
  FGroupBy:= TStringList.Create;
  FProxy:= TStringList.Create;
end;

destructor TCriteria.Destroy;
begin
  FProxy.Free;
  FGroupBy.Free;
  inherited;
end;

{ TCriteria }

function TCriteria.Add(const CriterionType: TCriterionType; const MemberName: string; const Values: array of string; const IsNot: boolean = False): ICriterion;
var
  I: Integer;
begin
  Result:= TCriterion.Create(Self);
  Result.CriterionType:= CriterionType;
  Result.MemberName:= MemberName;
  Result.IsNot:= IsNot;
  Result.Values.Clear;

  if CriterionType = ctIsNull then
    Result.AddValue(SNull)
  else
    for I:= Low(Values) to High(Values) do Result.AddValue(Values[I]);
  FList.Add(Result);
end;

function TCriteria.AddValues(const CriterionType: TCriterionType; const MemberName: string; const Values: array of IMemberType; const IsNot: boolean = False): ICriterion;
var
  I: Integer;
begin
  Result:= TCriterion.Create(Self);
  Result.CriterionType:= CriterionType;
  Result.MemberName:= MemberName;
  Result.IsNot:= IsNot;
  Result.Values.Clear;

  if CriterionType = ctIsNull then
    Result.AddValue(SNull)
  else
    for I:= Low(Values) to High(Values) do Result.AddValue(Values[I]);
  FList.Add(Result);
end;

procedure TCriteria.AddGroupBy(const MemberName: string);
begin
  FGroupBy.Add(MemberName);
end;

function TCriteria.AddOr(const CriterionType: TCriterionType; const MemberName: string; const Values: array of string; const IsNot: boolean): ICriterion;
begin
  Result:= Add(CriterionType, MemberName, Values, IsNot);
  Result.IsOr:= True;
end;

function TCriteria.AddValuesOr(const CriterionType: TCriterionType; const MemberName: string; const Values: array of IMemberType; const IsNot: boolean = False): ICriterion;
begin
  Result:= AddValues(CriterionType, MemberName, Values, IsNot);
  Result.IsOr:= True;
end;

function TCriteria.AddOrderBy(const MemberName: string; const SortOrder: TSortOrderType = soAscending): ICriterion;
var
  LMember: IMemberType;
begin
  LMember:= nil;
  if SortOrder = soAscending then
    Result:= Add(ctOrderBy, MemberName, [])
  else
    Result:= Add(ctOrderByDesc, MemberName, []);
end;

procedure TCriteria.AddProxy(const MemberName: string);
begin
  FProxy.Add(MemberName);
end;

function TCriteria.GetCount: Integer;
begin
  Result:= FList.Count;
end;

function TCriteria.GetDistinct: boolean;
begin
  Result:= FDistinct;
end;

function TCriteria.GetGroupBy: TStrings;
begin
  Result:= FGroupBy;
end;

function TCriteria.GetItems(const Index: Integer): ICriterion;
begin
  Result:= FList[Index] as ICriterion;
end;

function TCriteria.GetProxy: TStrings;
begin
  Result:= FProxy;
end;

procedure TCriteria.SetGroupBy(const Value: TStrings);
begin
  FGroupBy.Assign(Value);
end;

procedure TCriteria.SetProxy(const Value: TStrings);
begin
  FProxy.Assign(Value);
end;

function TCriterion.AddValue(const Value: IMemberType): IMemberType;
begin
  AddMember(Result, Value.Name, TypeRegister.FindTypeInfo(Value).TypeClass);
  Result.Assign(Value);
  Values.Add(Result);
end;

constructor TCriterion.Create(const AOwner: ICriteria);
begin
  inherited Create(nil);
  FOwner:= Pointer(AOwner);
  AddMember(FCriterionType, 'CriterionType', TSmallIntType); // do not localize
  AddMember(FIsNot, 'IsNot', TBooleanType); // do not localize
  AddMember(FIsOr, 'IsOr', TBooleanType); // do not localize
  AddMember(FMemberName, 'MemberName', TStringType); // do not localize
  FValues:= TInterfaceList.Create;
end;

procedure TCriteria.Clear;
begin
  FList.Clear;
  FGroupBy.Clear;
  FProxy.Clear;
end;

constructor TCriteria.Create(const CriterionType: TCriterionType; const MemberName: string; const Values: array of IMemberType; const IsNot: Boolean = False);
begin
  Create;
  AddValues(CriterionType, MemberName, Values, IsNot);
end;

constructor TCriteria.Create(const CriterionType: TCriterionType; const MemberName: string; const Values: array of string; const IsNot: boolean);
begin
  Create;
  Add(CriterionType, MemberName, Values, IsNot);
end;

function TCriteria.GetFirstRecord: Integer;
begin
  Result:= FFirstRecord;
end;

function TCriteria.GetMaxRecords: Integer;
begin
  Result:= FMaxRecord;
end;

procedure TCriteria.SetFirstRecord(const Value: Integer);
begin
  FFirstRecord:= Value;
end;

procedure TCriteria.SetMaxRecords(const Value: Integer);
begin
  FMaxRecord:= Value;
end;

procedure TCriteria.EndGrouping;
begin
  Add(ctEndGrouping, EmptyStr, []);
end;

procedure TCriteria.StartGrouping(const AndOr: TCriteriaAndOrType; const IsNot: boolean);
begin
  if AndOr = aoAnd then
    Add(ctStartGrouping, EmptyStr, [], IsNot)
  else
    AddValuesOr(ctStartGrouping, EmptyStr, [], IsNot);
end;

function TCriteria.IsValid: boolean;
var
  I: Integer;
  LCount: Integer;
  LStart: Integer;
  LEnd: Integer;
  LItems: Integer;
  LType: TCriterionType;
begin
  LStart:= 0;
  LEnd:= 0;
  LItems:= 0;
  LCount:= 0;

  for I:= 0 to Count -1 do
  begin
    LType:= Items[I].CriterionType;
    case LType of
      ctStartGrouping:
        begin
          Inc(LStart);
          Inc(LCount);
        end;
      ctEndGrouping:
        begin
          Inc(LEnd);
          Dec(LCount);
        end
    else
      if not (LType in [ctOrderBy, ctOrderByDesc]) and (LStart > 0) then
        Inc(LItems)
    end;

    if LCount < 0 then
    begin
      Result:= False;
      Exit;
    end;
  end;

  if LStart > 0 then
    Result:= (LStart = LEnd) and (LItems > 0)
  else
    Result:= True;
end;

{ TCriterion }

function TCriterion.AddValue(const Value: string): IStringType;
var
  LName: string;
const
  SValue = 'Value%d';
begin
  LName:= Format(SValue, [Values.Count + 1]);
  AddMember(Result, LName, TStringType);
  if Value = SNull then
    Result.Clear
  else
    Result.Value:= Value;
  Values.Add(Result);
end;

function TCriterion.GetCriterionType: TCriterionType;
begin
  Result:= TCriterionType(FCriterionType.Value);
end;

function TCriterion.GetIsNot: boolean;
begin
  Result:= FIsNot.Value;
end;

function TCriterion.GetIsOr: boolean;
begin
  Result:= FIsOr.Value;
end;

function TCriterion.GetMemberName: string;
begin
  Result:= FMemberName.Value;
end;

function TCriterion.GetValues: IInterfaceList;
begin
  Result:= FValues;
end;

procedure TCriterion.SetCriterionType(const Value: TCriterionType);
begin
  FCriterionType.Value:= Integer(Value);
end;

procedure TCriterion.SetIsNot(const Value: boolean);
begin
  FIsNot.Value:= Value;
end;

procedure TCriterion.SetIsOr(const Value: boolean);
begin
  FIsOr.Value:= Value;
end;

procedure TCriterion.SetMemberName(const Value: string);
begin
  FMemberName.Value:= Value;
end;

end.
