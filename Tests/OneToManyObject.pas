unit OneToManyObject;

interface

uses JazzValueType,
  JazzValueTypeIntf,
  JazzMapping,
  JazzMappingIntf,

  OneToManyObjectIntf;

type
  TOneToManyObject = class(TObjectType, IOneToManyObject)
  private
    FID: IStringType;
    FIDOwner: IStringType;
  protected
    function GetID: string;
    function GetIDOwner: string;
    procedure SetID(const Value: string);
    procedure SetIDOwner(const Value: string);
  public
    procedure InitInstance; override;
    property ID: string read GetID write SetID;
    property IDOwner: string read GetIDOwner write SetIDOwner;
  end;

  TOneToManyMapping = class(TMappingLoader)
  public
    procedure Execute(const Mapping: IMapping); override;
  end;

implementation

function TOneToManyObject.GetID: string;
begin
  Result := FID.Value;
end;

function TOneToManyObject.GetIDOwner: string;
begin
  Result := FIDOwner.Value;
end;

procedure TOneToManyObject.InitInstance;
begin
  inherited;
  AddMember(FID, 'ID', TStringType);
  AddMember(FIDOwner, 'IDOwner', TStringType);
end;

procedure TOneToManyObject.SetID(const Value: string);
begin
  FID.Value := Value;
end;

procedure TOneToManyObject.SetIDOwner(const Value: string);
begin
  FIDOwner.Value := Value;
end;

procedure TOneToManyMapping.Execute(const Mapping: IMapping);
begin
  inherited;
  with Mapping do
  begin
    with AddObject('TOneToMany', 'OneToMany') do
    begin
      with Add('ID', 'ID') do
      begin
        IsOid := True;
        ColumnType := itString;
      end;
      Add('IDOwner', 'IDOwner', itString, 38);
    end;
  end;
end;

initialization
  RegisterMapping(TOneToManyMapping);

end.

