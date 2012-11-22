unit JazzVarUtils;

interface

uses
  Classes,
  JazzUtils,
  JazzValueTypeIntf;

procedure VariantToMember(Value: Variant; const Member: IMemberType);
procedure MemberToVariant(const Member: IMemberType; var Value: Variant; IsSqlTimeStamp: Boolean = False);

function VarTypeToStr(const Value: Variant): string;
function IsVarNull(const Value: Variant): boolean;

function MemberToStream(const Member: IMemberType; Value: TStream): Boolean;
function StreamToMember(Value: TStream; const Member: IMemberType): boolean;

implementation

uses
  DB,
  SysUtils,
  SqlTimSt,
  Variants,
  JazzConsts;

procedure VariantToMember(Value: Variant; const Member: IMemberType);
begin
  if IsVarNull(Value) then
  begin
    Member.Clear;
    Exit;
  end;

  case Member.ValueTypeKind of
    vtString, vtChar, vtWideString:
      Member.AsString:= Value;
    vtBoolean:
      begin
        if VarType(Value) = varBoolean then
          (Member as IBooleanType).Value:= Value
        else
          (Member as IBooleanType).AsString:= VarTypeToStr(Value)
      end;
    vtCurrency: (Member as ICurrencyType).Value:= Value;
    vtFloat: (Member as IFloatType).Value:= Value;
    vtInteger: (Member as IIntegerType).Value:= Value;
    vtLongInt: (Member as ILongIntType).Value:= Value;
    vtSmallInt: (Member as ISmallIntType).Value:= Value;
    vtDate: (Member as IDateType).Value:= Value;
    vtTime: (Member as ITimeType).Value:= Value;
    vtMemo: Member.AsString:= Value;
  end;
end;

procedure MemberToVariant(const Member: IMemberType; var Value: Variant;
  IsSqlTimeStamp: Boolean = False);
begin
  if Member.IsNull then
    Value:= Variants.Null
  else
    case Member.ValueTypeKind of
      vtString,
      vtChar,
      vtWidestring:
        Value:= Member.AsString;
      vtInteger:
        Value:= (Member as IIntegerType).Value;
      vtBoolean:
        Value:= (Member as IBooleanType).Value;
      vtCurrency:
        Value:= (Member as ICurrencyType).Value;
      vtDate:
        begin
          if IsSqlTimeStamp then
            VarSQLTimeStampCreate(Value, (Member as IDateType).AsSQLTimeStamp)
          else
            Value:= (Member as IDateType).Value;
        end;
      vtTime:
        Value:= (Member as ITimeType).Value;
      vtFloat:
        Value:= (Member as IFloatType).Value;
      vtSmallInt:
        Value:= (Member as ISmallIntType).Value;
      vtLongInt:
        Value:= (Member as ILongIntType).Value;
      vtMemo:
        Value:= (Member as IMemoType).Value.Text;
      vtBlob:
        raise Exception.Create(SBlobNotSupported);
    end;
end;

function VarTypeToStr(const Value: Variant): string;
begin
  if not VarIsClear(Value) then // VarIsClear call VarIsEmpty
    Result:= VarToStr(Value)    // VarToStr test VarIsNull and call VarToStrDef
  else
    Result:= NullAsStringValue;
end;

function IsVarNull(const Value: Variant): boolean;
begin
  Result:= VarIsClear(Value) or VarIsNull(Value);
  if not Result then Result:= (VarTypeToStr(Value) = EmptyStr);
end;

function MemberToStream(const Member: IMemberType; Value: TStream): Boolean;
begin
  Result := False;
  if not Member.IsNull then
  begin
    if (Member.ValueTypeKind = vtBlob) then
      Value.CopyFrom((Member as IBlobType).Value, (Member as IBlobType).Value.Size)
    else
      (Member as IMemoType).Value.SaveToStream(Value);
    Result := True;
  end;
end;

function StreamToMember(Value: TStream; const Member: IMemberType): boolean;
begin
  Assert(Member <> nil, SMemberCantBeNil);
  if (Value.Size > 0) then
  begin
    if (Member.ValueTypeKind = vtBlob) then
      (Member as IBlobType).Value.CopyFrom(Value, Value.Size)
    else
      (Member as IMemoType).Value.LoadFromStream(Value);
    Result:= True;
  end
  else
  begin
    Member.Clear;
    Result:= False;
  end;
end;



end.


