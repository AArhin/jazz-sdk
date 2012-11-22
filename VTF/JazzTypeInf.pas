unit JazzTypeInf;

interface

uses Classes;

type
  ITypeRegister = interface;

  ITypeInfo = interface(IInterface)
    ['{C5F70D61-019B-4FFD-8B11-C9EA9AF53117}']
    function GetTypeClass: TClass;
    function GetIID: TGUID;
    function GetSuperIID: TGUID;
    function GetTypeName: string;
    function GetOwner: ITypeRegister;
    procedure SetTypeClass(const Value: TClass);
    procedure SetIID(const Value: TGUID);
    procedure SetSuperIID(const Value: TGUID);
    procedure SetTypeName(const Value: string);

    property IID: TGUID read GetIID write SetIID;
    property TypeName: string read GetTypeName write SetTypeName;
    property TypeClass: TClass read GetTypeClass write SetTypeClass;
    property SuperIID: TGUID read GetSuperIID write SetSuperIID;
    property Owner: ITypeRegister read GetOwner;
  end;
  
  ITypeRegister = interface(IInterface)
    ['{EA3C15DB-3F8F-44D6-9B3C-E18A9FEA9750}']
    function GetTypeInfo(const IID: TGUID): ITypeInfo; overload;
    function GetTypeInfo(const TypeName: string): ITypeInfo; overload;
    function GetTypeInfo(const TypeClass: TClass): ITypeInfo; overload;

    function RegisterType(const IID: TGUID; const TypeName: string; const TypeClass: TClass; const SuperIID: TGUID): ITypeInfo;
  end;

  TTypeInfo = class(TInterfacedObject, ITypeInfo)
  private
    FTypeClass: TClass;
    FIID: TGUID;
    FSuperIID: TGUID;
    FTypeName: string;
    FOwner: Pointer;
  protected
    function GetTypeClass: TClass;
    function GetIID: TGUID;
    function GetSuperIID: TGUID;
    function GetTypeName: string;
    function GetOwner: ITypeRegister;
    procedure SetTypeClass(const Value: TClass);
    procedure SetIID(const Value: TGUID);
    procedure SetSuperIID(const Value: TGUID);
    procedure SetTypeName(const Value: string);

    property IID: TGUID read GetIID write SetIID;
    property TypeName: string read GetTypeName write SetTypeName;
    property TypeClass: TClass read GetTypeClass write SetTypeClass;
    property SuperIID: TGUID read GetSuperIID write SetSuperIID;
    property Owner: ITypeRegister read GetOwner;
  public
    constructor Create(const Owner: ITypeRegister); virtual;
  end;

  TTypeRegister = class(TInterfacedObject, ITypeRegister)
  private
    FItems: IInterfaceList;
  protected
    function GetTypeInfo(const IID: TGUID): ITypeInfo; overload;
    function GetTypeInfo(const TypeName: string): ITypeInfo; overload;
    function GetTypeInfo(const TypeClass: TClass): ITypeInfo; overload;

    function RegisterType(const IID: TGUID; const TypeName: string; const TypeClass: TClass; const SuperIID: TGUID): ITypeInfo;
  public
    constructor Create; virtual;
  end;


function TypeRegister: ITypeRegister;

implementation

uses SysUtils, JazzConsts;

var
  FTypeRegister: ITypeRegister;

function TypeRegister: ITypeRegister;
begin
  if FTypeRegister = nil then FTypeRegister:= TTypeRegister.Create;
  Result:= FTypeRegister;
end;

{ TTypeInfo }

function TTypeInfo.GetTypeClass: TClass;
begin
  Result:= FTypeClass;
end;

function TTypeInfo.GetIID: TGUID;
begin
  Result:= FIID;
end;

function TTypeInfo.GetSuperIID: TGUID;
begin
  Result:= FSuperIID;
end;

function TTypeInfo.GetTypeName: string;
begin
  Result:= FTypeName;
end;

procedure TTypeInfo.SetTypeClass(const Value: TClass);
begin
  FTypeClass:= Value;
end;

procedure TTypeInfo.SetIID(const Value: TGUID);
begin
  FIID:= Value;
end;

procedure TTypeInfo.SetSuperIID(const Value: TGUID);
begin
  FSuperIID:= Value;
end;

procedure TTypeInfo.SetTypeName(const Value: string);
begin
  FTypeName:= Value;
end;

function TTypeInfo.GetOwner: ITypeRegister;
begin
  Result:= ITypeRegister(FOwner);
end;

constructor TTypeInfo.Create(const Owner: ITypeRegister);
begin
  inherited Create;
  FOwner:= Pointer(Owner); 
end;

{ TTypeRegister }

constructor TTypeRegister.Create;
begin
  inherited Create;
  FItems:= TInterfaceList.Create;
end;

function TTypeRegister.GetTypeInfo(const IID: TGUID): ITypeInfo;
var
  I: Integer;
begin
  for I:= 0 to FItems.Count -1 do
  begin
    if IsEqualGUID((FItems[I] as ITypeInfo).IID, IID) then
    begin
      Result:= (FItems[I] as ITypeInfo);
      Break;
    end;
  end;

  if not Assigned(Result) then
    raise Exception.CreateFmt(STypeInfoNotFound, [GuidToString(IID)]);
end;

function TTypeRegister.GetTypeInfo(const TypeName: string): ITypeInfo;
var
  I: Integer;
begin
  for I:= 0 to FItems.Count -1 do
  begin
    if (FItems[I] as ITypeInfo).TypeName = TypeName then
    begin
      Result:= (FItems[I] as ITypeInfo);
      Break;
    end;
  end;

  if not Assigned(Result) then
    raise Exception.CreateFmt(STypeInfoNotFound, [TypeName]);
end;

function TTypeRegister.GetTypeInfo(const TypeClass: TClass): ITypeInfo;
var
  I: Integer;
begin
  for I:= 0 to FItems.Count -1 do
  begin
    if (FItems[I] as ITypeInfo).TypeClass = TypeClass then
    begin
      Result:= (FItems[I] as ITypeInfo);
      Break;
    end;
  end;

  if not Assigned(Result) then
    raise Exception.CreateFmt(STypeInfoNotFound, [TypeClass.ClassName]);
end;

function TTypeRegister.RegisterType(const IID: TGUID;
  const TypeName: string; const TypeClass: TClass;
  const SuperIID: TGUID): ITypeInfo;
begin
  Result:= TTypeInfo.Create(Self);
  Result.IID:= IID;
  Result.TypeName:= TypeName;
  Result.TypeClass:= TypeClass;
  Result.SuperIID:= SuperIID;
  FItems.Add(Result);
end;

end.
