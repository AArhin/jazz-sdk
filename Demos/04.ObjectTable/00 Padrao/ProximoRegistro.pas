unit ProximoRegistro;

interface

uses
  JazzValueType, JazzValueTypeIntf, ProximoRegistro_Intf;

type
  TProximoRegistro = class(TObjectType, IProximoRegistro)
  private
    FTabela: IStringType;
    FUltimoRegistro: IIntegerType;

    function GetTabela: String;
    procedure SetTabela(const Value: String);
    function GetUltimoRegistro: Integer;
    procedure SetUltimoRegistro(const Value: Integer);
  protected
    procedure InitInstance; override;
  public
    property Tabela: String read GetTabela write SetTabela;
    property UltimoRegistro: Integer read GetUltimoRegistro write SetUltimoRegistro;
  end;

implementation

uses
  JazzTypeInfo;

{ TProximoRegistro }

function TProximoRegistro.GetTabela: String;
begin
  Result := FTabela.Value;
end;

function TProximoRegistro.GetUltimoRegistro: Integer;
begin
  Result := FUltimoRegistro.Value;
end;

procedure TProximoRegistro.InitInstance;
begin
  inherited;
  AddMember(FTabela, 'Tabela', TStringType); 
  AddMember(FUltimoRegistro, 'UltimoRegistro', TIntegerType);
end;

procedure TProximoRegistro.SetTabela(const Value: String);
begin
  FTabela.Value := Value;
end;

procedure TProximoRegistro.SetUltimoRegistro(const Value: Integer);
begin
  FUltimoRegistro.Value := Value;
end;

initialization
  RegisterType(IProximoRegistro, 'TProximoRegistro', TProximoRegistro);

end.
