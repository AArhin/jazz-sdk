unit ValueTypeTestObject;

interface

uses
  JazzValueType,

  ValueTypeTestObjectIntf, classes, JazzValueTypeIntf, OneToOneObjectIntf, Graphics,
  JazzMapping, JazzMappingIntf;

type
  TValueTypeTestObject = class(TObjectType, IValueTypeTestObject)
  private
    FBlobMember: IBlobType;
    FBooleanMember: IBooleanType;
    FCharMember: ICharType;
    FCurrencyMember: ICurrencyType;
    FDateMember: IDateType;
    FFloatMember: IFloatType;
    FIntegerMember: IIntegerType;
    FLongIntMember: ILongIntType;
    FMemoMember: IMemoType;
    FOneToManyMember: IObjectListType;
    FOneToOneMember: IOneToOneObject;
    FSmallintMember: ISmallIntType;
    FStringMember: IStringType;
    FTimeMember: ITimeType;
    FWideStringMember: IWideStringType;
    function GetBlobMember: TStream;
    function GetBooleanMember: Boolean;
    function GetCharMember: Char;
    function GetCurrencyMember: Currency;
    function GetDateMember: TDateTime;
    function GetFloatMember: Double;
    function GetIntegerMember: Integer;
    function GetLongIntMember: LongInt;
    function GetMemoMember: TStrings;
    function GetOneToManyMember: IObjectListType;
    function GetOneToOneMember: IOneToOneObject;
    function GetSmallintMember: SmallInt;
    function GetStringMember: string;
    function GetTimeMember: TDateTime;
    function GetWideStringMember: string;
    procedure SetBlobMember(const Value: TStream);
    procedure SetBooleanMember(const Value: Boolean);
    procedure SetCharMember(const Value: Char);
    procedure SetCurrencyMember(const Value: Currency);
    procedure SetDateMember(const Value: TDateTime);
    procedure SetFloatMember(const Value: Double);
    procedure SetIntegerMember(const Value: Integer);
    procedure SetLongIntMember(const Value: LongInt);
    procedure SetMemoMember(const Value: TStrings);
    procedure SetOneToManyMember(const Value: IObjectListType);
    procedure SetOneToOneMember(const Value: IOneToOneObject);
    procedure SetSmallintMember(const Value: SmallInt);
    procedure SetStringMember(const Value: string);
    procedure SetTimeMember(const Value: TDateTime);
    procedure SetWideStringMember(const Value: string);
  public
    procedure InitInstance; override;
    property BlobMember: TStream read GetBlobMember write SetBlobMember;
    property BooleanMember: Boolean read GetBooleanMember write SetBooleanMember;
    property CharMember: Char read GetCharMember write SetCharMember;
    property CurrencyMember: Currency read GetCurrencyMember write SetCurrencyMember;
    property DateMember: TDateTime read GetDateMember write SetDateMember;
    property FloatMember: Double read GetFloatMember write SetFloatMember;
    property IntegerMember: Integer read GetIntegerMember write SetIntegerMember;
    property LongIntMember: LongInt read GetLongIntMember write SetLongIntMember;
    property MemoMember: TStrings read GetMemoMember write SetMemoMember;
    property OneToManyMember: IObjectListType read GetOneToManyMember write SetOneToManyMember;
    property OneToOneMember: IOneToOneObject read GetOneToOneMember write SetOneToOneMember;
    property SmallintMember: SmallInt read GetSmallintMember write SetSmallintMember;
    property StringMember: string read GetStringMember write SetStringMember;
    property TimeMember: TDateTime read GetTimeMember write SetTimeMember;
    property WideStringMember: string read GetWideStringMember write SetWideStringMember;
  end;

  TValueTypeTestMapping = class(TMappingLoader)
  public
    procedure Execute(const Mapping: IMapping); override;
  end;

implementation

uses OneToOneObject;

function TValueTypeTestObject.GetBlobMember: TStream;
begin
  Result := FBlobMember.Value;
end;

function TValueTypeTestObject.GetBooleanMember: Boolean;
begin
  Result := FBooleanMember.Value;
end;

function TValueTypeTestObject.GetCharMember: Char;
begin
  Result := FCharMember.Value;
end;

function TValueTypeTestObject.GetCurrencyMember: Currency;
begin
  Result := FCurrencyMember.Value;
end;

function TValueTypeTestObject.GetDateMember: TDateTime;
begin
  Result := FDateMember.Value;
end;

function TValueTypeTestObject.GetFloatMember: Double;
begin
  Result := FFloatMember.Value;
end;

function TValueTypeTestObject.GetIntegerMember: Integer;
begin
  Result := FIntegerMember.Value;
end;

function TValueTypeTestObject.GetLongIntMember: LongInt;
begin
  Result := FLongIntMember.Value;
end;

function TValueTypeTestObject.GetMemoMember: TStrings;
begin
  Result := FMemoMember.Value;
end;

function TValueTypeTestObject.GetOneToManyMember: IObjectListType;
begin
  Result := FOneToManyMember;
end;

function TValueTypeTestObject.GetOneToOneMember: IOneToOneObject;
begin
  Result := FOneToOneMember;
end;

function TValueTypeTestObject.GetSmallintMember: SmallInt;
begin
  Result := FSmallintMember.Value;
end;

function TValueTypeTestObject.GetStringMember: string;
begin
  Result := FStringMember.Value;
end;

function TValueTypeTestObject.GetTimeMember: TDateTime;
begin
  Result := FTimeMember.Value;
end;

function TValueTypeTestObject.GetWideStringMember: string;
begin
  Result := FWideStringMember.Value;
end;

procedure TValueTypeTestObject.InitInstance;
begin
  inherited;
  AddMember(FBlobMember, 'BlobMember', TBlobType);
  AddMember(FBooleanMember, 'BooleanMember', TBooleanType);
  AddMember(FCharMember, 'CharMember', TCharType);
  AddMember(FCurrencyMember, 'CurrencyMember', TCurrencyType);
  AddMember(FDateMember, 'DateMember', TDateType);
  AddMember(FFloatMember, 'FloatMember', TFloatType);
  AddMember(FIntegerMember, 'IntegerMember', TIntegerType);
  AddMember(FLongIntMember, 'LongIntMember', TLongIntType);
  AddMember(FMemoMember, 'MemoMember', TMemoType);

  AddMember(FOneToManyMember, 'OneToManyMember', TObjectListType);
  AddMember(FOneToOneMember, 'OneToOneMember', TOneToOneObject, IOneToOneObject);

  AddMember(FSmallintMember, 'SmallintMember', TSmallIntType);
  AddMember(FStringMember, 'StringMember', TStringType);
  AddMember(FTimeMember, 'TimeMember', TTimeType);
  AddMember(FWideStringMember, 'WideStringMember', TWideStringType);
end;

procedure TValueTypeTestObject.SetBlobMember(const Value: TStream);
begin
  FBlobMember.Value := Value;
end;

procedure TValueTypeTestObject.SetBooleanMember(const Value: Boolean);
begin
  FBooleanMember.Value := Value;
end;

procedure TValueTypeTestObject.SetCharMember(const Value: Char);
begin
  FCharMember.Value := Value;
end;

procedure TValueTypeTestObject.SetCurrencyMember(const Value: Currency);
begin
  FCurrencyMember.Value := Value;
end;

procedure TValueTypeTestObject.SetDateMember(const Value: TDateTime);
begin
  FDateMember.Value := Value;
end;

procedure TValueTypeTestObject.SetFloatMember(const Value: Double);
begin
  FFloatMember.Value := Value;
end;

procedure TValueTypeTestObject.SetIntegerMember(const Value: Integer);
begin
  FIntegerMember.Value := Value;
end;

procedure TValueTypeTestObject.SetLongIntMember(const Value: LongInt);
begin
  FLongIntMember.Value := Value;
end;

procedure TValueTypeTestObject.SetMemoMember(const Value: TStrings);
begin
  FMemoMember.Value := Value;
end;

procedure TValueTypeTestObject.SetOneToManyMember(const Value: IObjectListType);
begin
  FOneToManyMember := Value;
end;

procedure TValueTypeTestObject.SetOneToOneMember(const Value: IOneToOneObject);
begin
  FOneToOneMember := Value;
end;

procedure TValueTypeTestObject.SetSmallintMember(const Value: SmallInt);
begin
  FSmallintMember.Value := Value;
end;

procedure TValueTypeTestObject.SetStringMember(const Value: string);
begin
  FStringMember.Value := Value;
end;

procedure TValueTypeTestObject.SetTimeMember(const Value: TDateTime);
begin
  FTimeMember.Value := Value;
end;

procedure TValueTypeTestObject.SetWideStringMember(const Value: string);
begin
  FWideStringMember.Value := Value;
end;

procedure TValueTypeTestMapping.Execute(const Mapping: IMapping);
begin
  inherited;
  with Mapping do
  begin
    with AddObject('TValueTypeTestObject', 'ValueTypeTest') do
    begin
      with Add('StringMember', 'StringMember') do
      begin
        IsOid := True;
        ColumnType := itString;
      end;
      Add('BlobMember', 'BlobMember', itImage);
      Add('BooleanMember', 'BooleanMember', itBoolean);
      Add('CharMember', 'CharMember', itChar, 1);
      Add('CurrencyMember', 'CurrencyMember', itCurrency);
      Add('DateMember', 'DateMember', itDate);
      Add('FloatMember', 'FloatMember', itFloat);
      Add('IntegerMember', 'IntegerMember', itInteger);
      Add('LongIntMember', 'LongIntMember', itLongInt);
      Add('MemoMember', 'MemoMember', itMemo);

      with AddRelationship do
      begin
        ToClassName := 'TOneToMany';
        MasterAttribute := 'OnyToManyMember';

        FromMembers.Add('StringMember');
        ToMembers.Add('IDOwner');

        CascadeDelete := True;
        CascadeLoad := True;
        CascadeSave := True;

        RelationshipType := rtOneToMany;
      end;

      Add('SmallIntMember', 'SmallIntMember', itSmallInt);
      Add('TimeMember', 'TimeMember', itTime);
      Add('WideStringMember', 'WideStringMember', itWideString, 50);

    end;
  end;
end;

initialization
  RegisterMapping(TValueTypeTestMapping);

end.

