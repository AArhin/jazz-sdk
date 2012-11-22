unit JazzCriteriaIntf;

interface

uses
  Classes,
  JazzValueTypeIntf,
  JazzConsts;

type
  TCriterionType = (
  (* Where Criteria *)
    ctEqualTo,            { Member matchs with Value                   }
    ctLike,               { Member matchs with Value using Like clause }
    ctGreaterOrEqual,     { Member is greater or equal to Value        }
    ctGreaterThan,        { Member is only greater than Value          }
    ctLessOrEqual,        { Member is less or equal to Value           }
    ctLessThan,           { Member is only less than Value             }
    ctBetween,            { Member is between two items in Value array }
    ctIn,                 { Member is in one item of Value array       }
    ctIsNull,             { Member is Null                             }
  (* OrderBy Criteria *)
    ctOrderBy,            { Define the OrderBy                         }
    ctOrderByDesc,        { Define the OrderBy Descentent              }
  (* Criteria Grouping *)
    ctStartGrouping,      { add "(" to user defined begin grouping     }
    ctEndGrouping         { add ")" to user defined end grouping       }
  );

  WhereCriteria = array[ctEqualTo..ctIsNull] of TCriterionType;

  TSortOrderType = (
    soAscending,
    soDescending
  );

  TCriteriaAndOrType = (
    aoAnd,
    aoOr
  );

  ICriterion = interface(IObjectType)
    ['{3E201AAC-49D9-4D9A-8C67-712FC94584CD}']
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
  end;

  ICriteria = interface(IInterface)
    ['{9BD6CC5E-2C88-4530-A151-19792BD0694B}']
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
    function AddOr(const CriterionType: TCriterionType; const MemberName: string; const Values: array of string; const IsNot: boolean = False): ICriterion; overload;
    function AddValuesOr(const CriterionType: TCriterionType; const MemberName: string; const Values: array of IMemberType; const IsNot: boolean = False): ICriterion; overload;
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
  end;

const
  CriteriaOperators: array[TCriterionType] of string = (
    '=',
    'LIKE',
    '>=',
    '>',
    '<=',
    '<',
    'AND',
    'IN',
    'IS NULL',
    EmptyStr,
    EmptyStr,
    '(',
    ')'
  );


implementation

end.
