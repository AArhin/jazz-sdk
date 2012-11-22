unit JazzObjectTypeTests;

interface
{ TODO -oEdsonLima -cTODO : Create tests for NamedInterfaceList }

uses
  JazzValueTypeIntf,
  JazzValueType,
  JazzClasses,
  TestFrameWork,
  SysUtils,
  OneToManyObjectIntf,
  OneToOneObjectIntf;

const
  SValueIncorrectForMember = 'Value incorrect for member %s';
  SNotIntilalized = '%s Instance not initialized';
  SNotAddedToMemberList = '%s Instance not added to MemberList';

type
  TAddMemberType = (atAddMembersAndTest, atAddMembersOnly);
  TObjectTypeTests = class(TTestCase)
  private
    FTestObject: IObjectType;
    FBooleanMember: IBooleanType;
    FCharMember: ICharType;
    FCurrencyMember: ICurrencyType;
    FDateMember: IDateType;
    FFloatMember: IFloatType;
    FIntegerMember: IIntegerType;
    FLongIntMember: ILongIntType;
    FBlobMember: IBlobType;
    FMemoMember: IMemoType;
    FOneToManyMember: IObjectListType;
    FOneToOneMember: IOneToOneObject;
    FSmallintMember: ISmallIntType;
    FStringMember: IStringType;
    FTimeMember: ITimeType;
    FWideStringMember: IWideStringType;

    procedure AddAllMembers(AddType: TAddMemberType = atAddMembersAndTest);
    procedure AddMemberTests(out Instance; const MemberName: string; MemberClass:
      TClass; ItemClass: TGUID; AddType: TAddMemberType = atAddMembersAndTest);
    procedure BlobMemberTest(const Member: IMemberType);
    procedure MemoMemberTest(const Member: IMemberType);
    procedure ObjectListTest;
    procedure ObjectMemberTest(const Member: IMemberType);

  protected

    procedure SetUp; override;
    procedure TearDown; override;
    procedure TestGetMemberException;
  public
  published

    procedure TestAddMember;
    procedure TestAccessMember;
    procedure TestGetMember;
  end;

implementation

uses OneToOneObject, OneToManyObject, Variants, Graphics, classes, JazzConsts;

procedure TObjectTypeTests.TestAccessMember;
var
  i: Integer;
  LMember: IMemberType;
  LTime: TDateTime;
begin
  { TODO -oEdsonLima -cTODO : Create tests for object relationship }
  AddAllMembers(atAddMembersOnly);
  for i := 0 to FTestObject.MembersCount - 1 do
  begin
    LMember := FTestObject.Members[i];
    case LMember.ValueTypeKind of
      vtValueType:
        begin
        end;
      vtObject:
        begin
          ObjectMemberTest(LMember);
        end;
      vtObjectList:
        begin
          ObjectListTest;
        end;
      vtBlob:
        begin
          BlobMemberTest(LMember);
        end;
      vtBoolean:
        begin
          (LMember as IBooleanType).Value := True;
          CheckTrue((LMember as IBooleanType).Value, Format(SValueIncorrectForMember,
            [LMember.Name]));
        end;
      vtChar:
        begin
          (LMember as ICharType).Value := 'A';
          CheckEquals('A', (LMember as ICharType).Value, Format(SValueIncorrectForMember,
            [LMember.Name]));
        end;
      vtCurrency:
        begin
          (LMember as ICurrencyType).Value := 922337203685477.5807;
          CheckEquals(922337203685477.5807, (LMember as ICurrencyType).Value,
            Format(SValueIncorrectForMember, [LMember.Name]));
        end;
      vtDate:
        begin
          (LMember as IDateType).Value := Date;
          CheckEquals(Date, (LMember as IDateType).Value, Format(SValueIncorrectForMember,
            [LMember.Name]));
        end;
      vtFloat:
        begin
          (LMember as IFloatType).Value := 1;
          CheckEquals(1, (LMember as IFloatType).Value,
            Format(SValueIncorrectForMember, [LMember.Name]));
        end;
      vtInteger:
        begin
          (LMember as IIntegerType).Value := 2147483647;
          CheckEquals(2147483647, (LMember as IIntegerType).Value, Format(SValueIncorrectForMember,
            [LMember.Name]));
        end;
      vtLongInt:
        begin
          (LMember as ILongIntType).Value := 2147483647;
          CheckEquals(2147483647, (LMember as ILongIntType).Value, Format(SValueIncorrectForMember,
            [LMember.Name]));
        end;
      vtMemo:
        begin
          MemoMemberTest(LMember);
        end;
      vtSmallInt:
        begin
          (LMember as ISmallIntType).Value := 32767;
          CheckEquals(32767, (LMember as ISmallIntType).Value, Format(SValueIncorrectForMember,
            [LMember.Name]));
        end;
      vtString:
        begin
          (LMember as IStringType).Value := 'String test';
          CheckEquals('String test', (LMember as IStringType).Value,
            Format(SValueIncorrectForMember, [LMember.Name]));
        end;
      vtTime:
        begin
          LTime := Time;
          (LMember as ITimeType).Value := LTime;
          CheckEquals(LTime, (LMember as ITimeType).Value, Format(SValueIncorrectForMember,
            [LMember.Name]));
        end;
      vtWideString:
        begin
          (LMember as IWideStringType).Value := 'Wide string test';
          CheckEquals('Wide string test', (LMember as IWideStringType).Value,
            Format(SValueIncorrectForMember, [LMember.Name]));
        end;
    end;
  end;
end;

procedure TObjectTypeTests.AddAllMembers(AddType: TAddMemberType = atAddMembersAndTest);
var
  LNullGUID: TGUID;
begin
  LNullGUID := StringToGUID(GUID_NULL);
  AddMemberTests(FStringMember, 'StringMember', TStringType, LNullGUID, AddType);
  AddMemberTests(FBlobMember, 'BlobMember', TBlobType, LNullGUID, AddType);
  AddMemberTests(FBooleanMember, 'BooleanMember', TBooleanType, LNullGUID, AddType);

  AddMemberTests(FCharMember, 'CharMember', TCharType, LNullGUID, AddType);
  AddMemberTests(FCurrencyMember, 'CurrencyMember', TCurrencyType, LNullGUID, AddType);
  AddMemberTests(FDateMember, 'DateMember', TDateType, LNullGUID, AddType);
  AddMemberTests(FFloatMember, 'FloatMember', TFloatType, LNullGUID, AddType);
  AddMemberTests(FIntegerMember, 'IntegerMember', TIntegerType, LNullGUID, AddType);
  AddMemberTests(FLongIntMember, 'LongIntMember', TLongIntType, LNullGUID, AddType);
  AddMemberTests(FMemoMember, 'MemoMember', TMemoType, LNullGUID, AddType);
  AddMemberTests(FSmallintMember, 'SmallintMember', TSmallIntType, LNullGUID, AddType);
  AddMemberTests(FTimeMember, 'TimeMember', TTimeType, LNullGUID, AddType);
  AddMemberTests(FWideStringMember, 'WideStringMember', TWideStringType, LNullGUID, AddType);
  AddMemberTests(FOneToOneMember, 'OneToOne', TOneToOneObject, IOneToOneObject, AddType);
  FTestObject.AddMember(FOneToManyMember, 'OneToManyMember', TObjectListType, TOneToManyObject);
  if AddType = atAddMembersAndTest then
  begin
    CheckNotNull(FOneToManyMember, Format(SNotIntilalized, ['OneToManyMember']));
    { TODO -oEdsonLima -cTODO : Move test to NamedInterfaceListTests }
    CheckEquals(FTestObject.MemberList.Count - 1, FTestObject.MemberList.IndexOf('OneToManyMember'),
      Format(SNotAddedToMemberList, ['OneToManyMember']));
  end;
//  AddMemberTests(FOneToManyMember, 'OneToManyMember', TObjectListType, LNullGUID, AddType);
end;

procedure TObjectTypeTests.AddMemberTests(out Instance; const MemberName:
  string; MemberClass: TClass; ItemClass: TGUID; AddType: TAddMemberType = atAddMembersAndTest);
var
  LNullGUID: TGUID;
begin
  LNullGUID := StringToGUID(GUID_NULL);

  if IsEqualGUID(ItemClass, LNullGUID) then
    FTestObject.AddMember(Instance, MemberName, MemberClass)
  else
    FTestObject.AddMember(Instance, MemberName, MemberClass, ItemClass);
  if AddType = atAddMembersAndTest then
  begin
    CheckNotNull(TMemberType(Instance), Format(SNotIntilalized, [MemberName]));
    { TODO -oEdsonLima -cTODO : Move test to NamedInterfaceListTests }
    CheckEquals(FTestObject.MemberList.Count - 1, FTestObject.MemberList.IndexOf(MemberName),
      Format(SNotAddedToMemberList, [MemberName]));
    { TODO -Cesar -cERROR : Test failure for IndexOf(Interface) }
//       CheckEquals(0, (FTestObject.MemberList as IMemberList).IndexOf(Instance as IInterface));
  end;
end;

procedure TObjectTypeTests.BlobMemberTest(const Member: IMemberType);
var
  LBitmap: TBitmap;
  LStream: TMemoryStream;
begin
  LBitmap := TBitmap.Create;
  LStream := TMemoryStream.Create;
  try
    LBitmap.LoadFromFile('.\Pescaria.bmp');
    LBitmap.SaveToStream(LStream);
    (Member as IBlobType).Value := LStream;
    CheckEquals(SizeOf(LStream), SizeOf((Member as IBlobType).Value),
      Format(SValueIncorrectForMember, [Member.Name]));
  finally
    LBitmap.Free;
    LStream.Free;
  end;
end;

procedure TObjectTypeTests.TestGetMember;
var
  LMember: IMemberType;
begin
  AddAllMembers(atAddMembersOnly);
  LMember := FTestObject.Member['StringMember'];
  CheckNotNull(LMember, Format('Member %s not loaded properly', ['StringMember']));
  CheckException(TestGetMemberException, EJazzMemberNotFound, 'Not raise exception for invalid member InvalidMember');
end;

procedure TObjectTypeTests.MemoMemberTest(const Member: IMemberType);
var
  LSampleFile: TStringList;
begin
  LSampleFile := TStringList.Create;
  try
    LSampleFile.LoadFromFile('.\Sample.txt');
    (Member as IMemoType).Value := LSampleFile;
    CheckEquals(LSampleFile.Text, (Member as IMemoType).Value.Text,
      Format(SValueIncorrectForMember, [Member.Name]));
  finally
    LSampleFile.Free;
  end;
end;

procedure TObjectTypeTests.ObjectListTest;
var
  i: Integer;
  LChildObject: IOneToManyObject;
begin
  for i := 1 to 5 do
  begin
    LChildObject := (FTestObject.Member['OneToManyMember'] as IObjectListType).Add as
      IOneToManyObject;
    LChildObject.ID := IntToStr(i);
    LChildObject.IDOwner := FTestObject.Member['StringMember'].AsString;
  end;
  Check((FTestObject.Member['OneToManyMember'] as IObjectListType).Count = 5);
end;

procedure TObjectTypeTests.ObjectMemberTest(const Member: IMemberType);
var
  LOneToOne: IOneToOneObject;
begin
  LOneToOne := TOneToOneObject.Create;
  LOneToOne.ID := 'ID';
  LOneToOne.IDOwner := FTestObject.Member['StringMember'].AsString;
  (Member as IOneToOneObject).Assign(LOneToOne);
  CheckEquals(LOneToOne.ID, (Member as IOneToOneObject).ID,
    Format(SValueIncorrectForMember, [Member.Name]));
end;

procedure TObjectTypeTests.SetUp;
begin
  FTestObject := TObjectType.Create;
end;

procedure TObjectTypeTests.TearDown;
begin
  FBooleanMember := nil;
  FCharMember := nil;
  FCurrencyMember := nil;
  FDateMember := nil;
  FFloatMember := nil;
  FIntegerMember := nil;
  FLongIntMember := nil;
  FBlobMember := nil;
  FMemoMember := nil;
  FOneToOneMember := nil;
  FSmallintMember := nil;
  FStringMember := nil;
  FTimeMember := nil;
  FWideStringMember := nil;
  FTestObject := nil;
  FOneToManyMember := nil;
end;

procedure TObjectTypeTests.TestAddMember;
begin
  AddAllMembers;
end;

procedure TObjectTypeTests.TestGetMemberException;
var
  LMember: IMemberType;
begin
  LMember := FTestObject.Member['InvalidMember'];
end;

initialization
  TestFramework.RegisterTest('JazzValueTypeTests - No OPF operations', TObjectTypeTests.Suite);

end.

