unit JazzObjectListTypeTests;

interface

uses
  JazzValueType,
  JazzValueTypeIntf,
  TestFrameWork,
  ValueTypeTestObjectIntf;

type
  TObjectListTypeTests = class(TTestCase)
  private
    FTestList: IObjectListType;
    procedure InsertInvalidObject;
    procedure ListSetAsString;
    { TODO -oEdsonLima -Todo : Create tests for this methods }
    procedure TestBeginUpdate;
    procedure TestIsUpdating;
    procedure TestEndUpdate;
    procedure TestNotify;
  protected

    procedure AddItemsToList(Count: Integer);
    function Populate(const TestObject: IValueTypeTestObject): string;
    procedure SetUp; override;
    procedure TearDown; override;
  published

    procedure TestAdd;
    procedure TestAddObject;
    procedure TestGetCount;
    procedure TestInsert;
    procedure TestNew;

    procedure TestExchange;
    procedure TestClear;

    procedure TestAddDeleting;
    procedure TestRemove;
    procedure TestRemoveObject;
    procedure TestGetDeletingCount;
    procedure TestGetDeletingItems;
    procedure TestCancelDeleting;
    procedure TestIndexOfDeleting;
    procedure TestRemoveDeletingIndex;
    procedure TestRemoveDeletingObject;

    procedure TestIndexOf;
    procedure TestFindObject;
    procedure TestFindObjectIndex;
    procedure TestGetItemClass;
    procedure TestGetItems;
    procedure TestAsString;
    procedure TestAssign;
    procedure TestClone;
    procedure TestValueTypeKind;
  end;

implementation

uses ValueTypeTestObject, SysUtils, JazzUtils;

procedure TObjectListTypeTests.AddItemsToList(Count: Integer);
var
  i: Integer;
  LTestObject: IValueTypeTestObject;
begin
  for i := 1 to Count do
  begin
    LTestObject := FTestList.Add as IValueTypeTestObject;
    Populate(LTestObject);
  end;
end;

procedure TObjectListTypeTests.InsertInvalidObject;
var
  LInvalidObject: IValueType;
begin
  LInvalidObject := TValueType.Create;
  FTestList.Insert(3, LInvalidObject);
end;

procedure TObjectListTypeTests.ListSetAsString;
begin
  FTestList.AsString := 'Test';
end;

function TObjectListTypeTests.Populate(const TestObject: IValueTypeTestObject):
  string;
begin
  Result := GenerateGUID;
  with TestObject do
  begin
    BooleanMember := True;
    CharMember := 'A';
    CurrencyMember := 98989.98;
    DateMember := Date;
    FloatMember := 98989.989;
    IntegerMember := 1234567;
    LongIntMember := 1234567;
    SmallintMember := 1234;
    StringMember := Result;
    TimeMember := Time;
    WideStringMember := Result;
  end;
end;

{ TObjectListTypeTests }

procedure TObjectListTypeTests.SetUp;
begin
  FTestList := TValueTypeTestObject.NewList;
end;

procedure TObjectListTypeTests.TearDown;
begin
  FTestList := nil;
end;

procedure TObjectListTypeTests.TestAdd;
var
  LTestObject: IValueTypeTestObject;
begin
  LTestObject := FTestList.Add as IValueTypeTestObject;
  Check(FTestList.Count = 1, 'Item not added to list');
  Check(FTestList.IndexOf(LTestObject) = 0, 'Object not found');
end;

procedure TObjectListTypeTests.TestAddDeleting;
var
  LTestObject: IValueTypeTestObject;
  LDelete1: IValueTypeTestObject;
  LDelete2: IValueTypeTestObject;
begin
  LTestObject := FTestList.Add as IValueTypeTestObject;
  LDelete1 := FTestList.Add as IValueTypeTestObject;
  LTestObject := FTestList.Add as IValueTypeTestObject;
  LDelete2 := FTestList.Add as IValueTypeTestObject;
  Check(FTestList.DeletingCount = 0);
  FTestList.AddDeleting(LDelete1);
  CheckEquals(1, FTestList.DeletingCount, 'Not add delete object 1');
  LDelete2.Delete;
  CheckEquals(2, FTestList.DeletingCount, 'Not add delete object 2');
  Check(FTestList.Count = 2, 'AddDeleting failed - Itens not deleted from list');
end;

procedure TObjectListTypeTests.TestAddObject;
var
  LTestObject: IValueTypeTestObject;
begin
  LTestObject := TValueTypeTestObject.Create;
  FTestList.Add(LTestObject);
  Check(FTestList.Count = 1, 'Item not added to list');
  CheckEquals(0, FTestList.IndexOf(LTestObject as IValueTypeTestObject), 'Object not found');
end;

procedure TObjectListTypeTests.TestAssign;
var
  LAssignedList: IObjectListType;
begin
  AddItemsToList(5);
  FTestList[3].Delete;
  LAssignedList := TValueTypeTestObject.NewList;
  LAssignedList.Assign(FTestList as IObjectListType);
  CheckEquals(FTestList.Count, LAssignedList.Count, 'Assign failed - Count');
  CheckEquals(FTestList.DeletingCount, LAssignedList.DeletingCount, 'Assign failed - DeletingCount');
end;

procedure TObjectListTypeTests.TestBeginUpdate;
begin
  FTestList.BeginUpdate;
end;

procedure TObjectListTypeTests.TestCancelDeleting;
var
  LTestObject: IValueTypeTestObject;
begin
  AddItemsToList(5);
  LTestObject := FTestList.Add as IValueTypeTestObject;
  FTestList.AddDeleting(FTestList[3]);
  LTestObject.Delete;
  Check(FTestList.DeletingCount = 2, 'Items not deleted');
  FTestList.CancelDeleting;
  Check(FTestList.DeletingCount = 0, 'CancelDeleting failed');
  CheckFalse((FTestList[3] as IObjectState).State.Deleted);
  CheckFalse((LTestObject as IObjectState).State.Deleted);
  LTestObject.Delete;
  Check(FTestList.DeletingCount = 1, 'LTestObject not deleted');
  FTestList.CancelDeleting(LTestObject);
  Check(FTestList.DeletingCount = 0, 'CancelDeleting(AObject) failed');
end;

procedure TObjectListTypeTests.TestClear;
begin
  AddItemsToList(5);
  FTestList.Clear;
  Check(FTestList.Count = 0, 'Object list clear failed');
end;

procedure TObjectListTypeTests.TestClone;
var
  LClone: IObjectListType;
begin
  AddItemsToList(5);
  FTestList[2].Delete;
  LClone := FTestList.Clone as IObjectListType;
  CheckEquals(FTestList.Count, LClone.Count, 'Clone failed - Count');
  CheckEquals(FTestList.DeletingCount, LClone.DeletingCount, 'Clone failed - DeletingCount');
end;

procedure TObjectListTypeTests.TestEndUpdate;
begin

end;

procedure TObjectListTypeTests.TestExchange;
var
  LTestObject: IValueTypeTestObject;
  LID: string;
  LExchange: string;
begin
  LTestObject := FTestList.Add as IValueTypeTestObject;
  LID := Populate(LTestObject);
  LTestObject := FTestList.Add as IValueTypeTestObject;
  LExchange := Populate(LTestObject);
  FTestList.Exchange(0, 1);
  CheckEquals(LExchange, (FTestList[0] as IValueTypeTestObject).StringMember, 'Exchange failed - Item 0');
  CheckEquals(LID, (FTestList[1] as IValueTypeTestObject).StringMember, 'Exchange failed - Item 1');
end;

procedure TObjectListTypeTests.TestFindObject;
var
  LTestObject: IValueTypeTestObject;
  LFindObject: IValueTypeTestObject;
begin
  AddItemsToList(10);
  LTestObject := FTestList[5] as IValueTypeTestObject;
  LFindObject := FTestList.FindObject(['StringMember'], [LTestObject.StringMember]) as IValueTypeTestObject;
  CheckNotNull(LFindObject, 'FindObject failed - Object not found');
  LFindObject := nil;
  LFindObject := FTestList.FindObject(['stringmember'], [LTestObject.StringMember]) as IValueTypeTestObject;
  CheckNotNull(LFindObject, 'FindObject failed - Case sensitive error');
end;

procedure TObjectListTypeTests.TestFindObjectIndex;
var
  LTestObject: IValueTypeTestObject;
  LFindObjectIndex: Integer;
begin
  AddItemsToList(10);
  LTestObject := FTestList[5] as IValueTypeTestObject;
  LFindObjectIndex := FTestList.FindObjectIndex(['StringMember'], [LTestObject.StringMember]);
  CheckEquals(5, LFindObjectIndex, 'FindObject failed');
end;

procedure TObjectListTypeTests.TestAsString;
begin
  CheckException(ListSetAsString, Exception);
  CheckEquals('0 items', FTestList.AsString);
end;

procedure TObjectListTypeTests.TestGetCount;
begin
  AddItemsToList(5);
  CheckEquals(5, FTestList.Count, 'ObjectList Count incorrect');
end;

procedure TObjectListTypeTests.TestGetDeletingCount;
var
  LDelete: IValueTypeTestObject;
begin
  AddItemsToList(8);
  LDelete := FTestList[3] as IValueTypeTestObject;
  FTestList.AddDeleting(LDelete);
  CheckEquals(1, FTestList.DeletingCount, 'Deleting count failed');
  LDelete := FTestList[6] as IValueTypeTestObject;
  FTestList.AddDeleting(LDelete);
  CheckEquals(2, FTestList.DeletingCount, 'Deleting count failed');
end;

procedure TObjectListTypeTests.TestGetDeletingItems;
var
  LDelete: IValueTypeTestObject;
  LDelete1: IValueTypeTestObject;
begin
  AddItemsToList(8);

  LDelete := FTestList[3] as IValueTypeTestObject;
  LDelete1 := FTestList[6] as IValueTypeTestObject;

  FTestList.AddDeleting(LDelete);
  FTestList.AddDeleting(LDelete1);

  CheckEquals(LDelete.StringMember, (FTestList.DeletingItems[0] as IValueTypeTestObject).StringMember, 'GetDeletingItens failed for item 0');
  CheckEquals(LDelete1.StringMember, FTestList.DeletingItems[1].Member['StringMember'].AsString, 'GetDeletingItens failed for item 1');
end;

procedure TObjectListTypeTests.TestGetItemClass;
begin
  CheckEquals(TValueTypeTestObject, FTestList.ItemClass);
end;

procedure TObjectListTypeTests.TestGetItems;
var
  LTestObject: IValueTypeTestObject;
begin
  AddItemsToList(5);
  LTestObject := FTestList.New as IValueTypeTestObject;
  Populate(LTestObject);
  FTestList.Add(LTestObject);
  CheckEquals(LTestObject.StringMember, (FTestList.Items[5] as IValueTypeTestObject).StringMember, 'Get items failed');
end;

procedure TObjectListTypeTests.TestValueTypeKind;
begin
  Check(FTestList.ValueTypeKind = vtObjectList, 'ValueTypeKind incorrect');
end;

procedure TObjectListTypeTests.TestIndexOf;
var
  LTestObject: IValueTypeTestObject;
begin
  LTestObject := FTestList.Add as IValueTypeTestObject;
  CheckEquals(0, FTestList.IndexOf(LTestObject as IValueTypeTestObject),
    'Incorrect IndexOf with Type Cast');
  CheckEquals(0, FTestList.IndexOf(LTestObject), 'Incorrect IndexOf');
end;

procedure TObjectListTypeTests.TestIndexOfDeleting;
var
  LTestObject: IValueTypeTestObject;
begin
  LTestObject := FTestList.Add as IValueTypeTestObject;
  Populate(LTestObject);
  LTestObject.Delete;
  AddItemsToList(3);
  CheckEquals(0, FTestList.IndexOfDeleting(LTestObject), 'IndexOfDeleting incorret');
end;

procedure TObjectListTypeTests.TestInsert;
var
  LTestObject: IValueTypeTestObject;
begin
  AddItemsToList(5);
  LTestObject := TValueTypeTestObject.Create;
  Populate(LTestObject);
  FTestList.Insert(2, LTestObject);
  CheckEquals(2, FTestList.IndexOf(LTestObject), 'Item not inserted properly');
  CheckException(InsertInvalidObject, EJazzInsert, 'Cannot raise excpetion for invalid item');
end;

procedure TObjectListTypeTests.TestIsUpdating;
begin

end;

procedure TObjectListTypeTests.TestNew;
var
  LTestObject: IValueTypeTestObject;
  LObjectType: IValueTypeTestObject;
begin
  LTestObject := FTestList.New as IValueTypeTestObject;
  CheckNotNull(LTestObject, 'New method failed');
  Supports(LTestObject, IValueTypeTestObject, LObjectType);
  CheckNotNull(LObjectType, 'Not create an IValueTypeTestObject');
  Check(FTestList.Count = 0, 'Item added to list');
end;

procedure TObjectListTypeTests.TestNotify;
begin

end;

procedure TObjectListTypeTests.TestRemove;
var
  LTestObject: IValueTypeTestObject;
  LGUID: string;
begin
  LTestObject := FTestList.Add as IValueTypeTestObject;
  LGUID := Populate(LTestObject);
  LTestObject := FTestList.Add as IValueTypeTestObject;
  Populate(LTestObject);
  FTestList.Remove(0);
  CheckNotEquals(LGUID, (FTestList[0] as IValueTypeTestObject).StringMember, 'Remove failed');
end;

procedure TObjectListTypeTests.TestRemoveDeletingIndex;
begin
  AddItemsToList(5);
  FTestList[2].Delete;
  FTestList[3].Delete;
  Check(FTestList.DeletingCount = 2, 'Item not deleted');
  FTestList.RemoveDeleting(0);
  Check(FTestList.DeletingCount = 1, 'RemoveDeletingIndex failed');
end;

procedure TObjectListTypeTests.TestRemoveDeletingObject;
var
  LTestObject: IValueTypeTestObject;
begin
  AddItemsToList(3);
  LTestObject := FTestList.Add as IValueTypeTestObject;
  Populate(LTestObject);
  LTestObject.Delete;
  Check(FTestList.DeletingCount = 1, 'Item not deleted');
  FTestList.RemoveDeleting(LTestObject);
  Check(FTestList.DeletingCount = 0, 'RemoveDeletingObject failed');
end;

procedure TObjectListTypeTests.TestRemoveObject;
var
  LTestObject: IValueTypeTestObject;
  LGUID: String;
begin
  AddItemsToList(5);
  LTestObject := FTestList.Add as IValueTypeTestObject;
  LGUID := Populate(LTestObject);
  AddItemsToList(5);
  FTestList.RemoveObject(LTestObject);
  CheckEquals(10, FTestList.Count, 'RemoveObject failed - Count');
  CheckEquals(-1, FTestList.IndexOf(LTestObject), 'RemoveObject failed - IndexOf');
  CheckNull(FTestList.FindObject(['StringMember'], [LGUID]), 'RemoveObject failed - Object found in the list')
end;

initialization
  TestFramework.RegisterTest('JazzValueTypeTests - No OPF operations', TObjectListTypeTests.Suite);

end.

